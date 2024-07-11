/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use app_units::Au;
use atomic_refcell::{AtomicRefCell, AtomicRefMut};
use style::properties::longhands::flex_direction::computed_value::T as FlexDirection;
use style::properties::longhands::flex_wrap::computed_value::T as FlexWrap;
use style::properties::ComputedValues;
use style::values::generics::length::{GenericLengthPercentageOrAuto, LengthPercentageOrAuto};
use style::Zero;
use taffy::MaybeMath;
use taffy_stylo::{TaffyStyloStyle, TaffyStyloStyleRef};

use super::{FlexContainer, FlexLevelBox, FlexLevelBoxInner};
use crate::cell::ArcRefCell;
use crate::context::LayoutContext;
use crate::formatting_contexts::{Baselines, IndependentFormattingContext, IndependentLayout};
use crate::fragment_tree::{BoxFragment, CollapsedBlockMargins, Fragment};
use crate::geom::{LogicalRect, LogicalSides, LogicalVec2};
use crate::positioned::{AbsolutelyPositionedBox, PositioningContext};
use crate::sizing::ContentSizes;
use crate::style_ext::ComputedValuesExt;
use crate::ContainingBlock;

// FIMXE: “Flex items […] `z-index` values other than `auto` create a stacking context
// even if `position` is `static` (behaving exactly as if `position` were `relative`).”
// https://drafts.csswg.org/css-flexbox/#painting
// (likely in `display_list/stacking_context.rs`)

fn resolve_content_size(constraint: taffy::AvailableSpace, content_sizes: ContentSizes) -> f32 {
    match constraint {
        taffy::AvailableSpace::Definite(limit) => {
            let min = content_sizes.min_content.to_f32_px();
            let max = content_sizes.max_content.to_f32_px();
            limit.min(max).max(min)
        },
        taffy::AvailableSpace::MinContent => content_sizes.min_content.to_f32_px(),
        taffy::AvailableSpace::MaxContent => content_sizes.max_content.to_f32_px(),
    }
}

#[inline(always)]
fn with_independant_formatting_context<T>(
    item: &mut FlexLevelBoxInner,
    cb: impl FnOnce(&mut IndependentFormattingContext) -> T,
) -> T {
    match item {
        FlexLevelBoxInner::FlexItem(ref mut context) => cb(context),
        FlexLevelBoxInner::OutOfFlowAbsolutelyPositionedBox(ref abspos_box) => {
            let mut abspos_box = AtomicRefCell::borrow_mut(abspos_box);
            cb(&mut abspos_box.context)
        },
    }
}

/// Layout parameters and intermediate results about a flex container,
/// grouped to avoid passing around many parameters
struct FlexContext<'a> {
    source_child_nodes: &'a [ArcRefCell<FlexLevelBox>],
    layout_context: &'a LayoutContext<'a>,
    positioning_context: &'a mut PositioningContext,
    // For items. Style is on containing_block
    content_box_size_override: &'a ContainingBlock<'a>,
    style: &'a ComputedValues,
    // container_is_single_line: bool,
    // container_min_cross_size: Length,
    // container_max_cross_size: Option<Length>,
    // flex_axis: FlexAxis,
    // flex_direction_is_reversed: bool,
    // flex_wrap_reverse: bool,
    // main_start_cross_start_sides_are: MainStartCrossStart,
    // container_definite_inner_size: FlexRelativeVec2<Option<Length>>,
    // align_content: AlignContent,
    // align_items: AlignItems,
    // justify_content: JustifyContent,
}

struct ChildIter(std::ops::Range<usize>);
impl Iterator for ChildIter {
    type Item = taffy::NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|idx| taffy::NodeId::from(idx))
    }
}

impl taffy::TraversePartialTree for FlexContext<'_> {
    type ChildIter<'a> = ChildIter where Self: 'a;

    fn child_ids(&self, _node_id: taffy::NodeId) -> Self::ChildIter<'_> {
        ChildIter(0..self.source_child_nodes.len())
    }

    fn child_count(&self, _node_id: taffy::NodeId) -> usize {
        self.source_child_nodes.len()
    }

    fn get_child_id(&self, _node_id: taffy::NodeId, index: usize) -> taffy::NodeId {
        taffy::NodeId::from(index)
    }
}

impl taffy::LayoutPartialTree for FlexContext<'_> {
    type CoreContainerStyle<'a> = TaffyStyloStyleRef<'a> where Self: 'a;
    type CacheMut<'b> = AtomicRefMut<'b, taffy::Cache> where Self: 'b;

    fn get_core_container_style(&self, _node_id: taffy::NodeId) -> Self::CoreContainerStyle<'_> {
        TaffyStyloStyleRef(self.style)
    }

    fn set_unrounded_layout(&mut self, node_id: taffy::NodeId, layout: &taffy::Layout) {
        let id = usize::from(node_id);
        (*self.source_child_nodes[id]).borrow_mut().taffy_layout = *layout;
    }

    fn get_cache_mut(&mut self, node_id: taffy::NodeId) -> AtomicRefMut<'_, taffy::Cache> {
        let id = usize::from(node_id);
        let mut_ref: AtomicRefMut<'_, _> = (*self.source_child_nodes[id]).borrow_mut();
        AtomicRefMut::map(mut_ref, |node| &mut node.taffy_layout_cache)
    }

    fn compute_child_layout(
        &mut self,
        node_id: taffy::NodeId,
        inputs: taffy::LayoutInput,
    ) -> taffy::LayoutOutput {
        // compute_cached_layout(self, node_id, inputs, |parent, node_id, inputs| {
        let mut child = (*self.source_child_nodes[usize::from(node_id)]).borrow_mut();
        let child = &mut *child;

        fn option_f32_to_lpa(input: Option<f32>) -> LengthPercentageOrAuto<Au> {
            match input {
                None => LengthPercentageOrAuto::Auto,
                Some(length) => LengthPercentageOrAuto::LengthPercentage(Au::from_f32_px(length)),
            }
        }

        with_independant_formatting_context(
            &mut child.flex_level_box,
            |independent_context| -> taffy::LayoutOutput {
                let computed_size = match independent_context {
                    IndependentFormattingContext::Replaced(replaced) => {
                        // The containing block of a flex item is the content box of the flex container
                        let containing_block = &self.content_box_size_override;

                        // Adjust known_dimensions from border box to content box
                        let pbm = replaced.style.padding_border_margin(&containing_block);
                        let content_box_known_dimensions =
                            taffy::Size {
                                width: inputs.known_dimensions.width.map(|width| {
                                    width - pbm.padding_border_sums.inline.to_f32_px()
                                }),
                                height: inputs.known_dimensions.height.map(|height| {
                                    height - pbm.padding_border_sums.block.to_f32_px()
                                }),
                            };

                        let content_box_size_override = LogicalVec2 {
                            inline: option_f32_to_lpa(content_box_known_dimensions.width),
                            block: option_f32_to_lpa(content_box_known_dimensions.height),
                        };

                        let content_box_size = replaced.contents.used_size_as_if_inline_element(
                            &containing_block,
                            &replaced.style,
                            Some(content_box_size_override),
                            &pbm,
                        );

                        child.child_fragments = replaced
                            .contents
                            .make_fragments(&replaced.style, content_box_size);

                        taffy::Size {
                            width: inputs.known_dimensions.width.unwrap_or_else(|| {
                                (content_box_size.inline + pbm.padding_border_sums.inline)
                                    .to_f32_px()
                            }),
                            height: inputs.known_dimensions.height.unwrap_or_else(|| {
                                (content_box_size.block + pbm.padding_border_sums.block).to_f32_px()
                            }),
                        }
                    },

                    // TODO: better handling of flexbox items (which can't precompute inline sizes)
                    IndependentFormattingContext::NonReplaced(non_replaced) => {
                        // The containing block of a flex item is the content box of the flex container
                        let containing_block = &self.content_box_size_override;

                        // Adjust known_dimensions from border box to content box
                        let pbm = non_replaced.style.padding_border_margin(containing_block);
                        let content_box_known_dimensions =
                            taffy::Size {
                                width: inputs.known_dimensions.width.map(|width| {
                                    width - pbm.padding_border_sums.inline.to_f32_px()
                                }),
                                height: inputs.known_dimensions.height.map(|height| {
                                    height - pbm.padding_border_sums.block.to_f32_px()
                                }),
                            };

                        // Compute inline size
                        let inline_size = content_box_known_dimensions.width.unwrap_or_else(|| {
                            let inline_sizes =
                                non_replaced.inline_content_sizes(&self.layout_context);
                            resolve_content_size(inputs.available_space.width, inline_sizes)
                        });

                        let maybe_block_size =
                            option_f32_to_lpa(content_box_known_dimensions.height);
                        let content_box_size_override = ContainingBlock {
                            inline_size: Au::from_f32_px(inline_size),
                            block_size: maybe_block_size,
                            style: &non_replaced.style,
                        };

                        let layout = {
                            let mut child_positioning_context = PositioningContext::new_for_subtree(
                                self.positioning_context
                                    .collects_for_nearest_positioned_ancestor(),
                            );
                            let layout = non_replaced.layout(
                                &self.layout_context,
                                &mut child_positioning_context,
                                &content_box_size_override,
                                &containing_block,
                            );

                            self.positioning_context.append(child_positioning_context);

                            layout
                            // },
                        };

                        // TODO: make this work for replaced boxes
                        child.child_fragments = layout.fragments;

                        let block_size = layout.content_block_size.to_f32_px();

                        taffy::Size {
                            width: inline_size + pbm.padding_border_sums.inline.to_f32_px(),
                            height: block_size + pbm.padding_border_sums.block.to_f32_px(),
                        }
                    },
                };

                let size = inputs.known_dimensions.unwrap_or(computed_size);

                taffy::LayoutOutput {
                    size,
                    content_size: size,
                    ..taffy::LayoutOutput::DEFAULT
                }
            },
        )

        // })
    }
}

impl taffy::LayoutFlexboxContainer for FlexContext<'_> {
    type ContainerStyle<'a> = TaffyStyloStyleRef<'a>
    where
        Self: 'a;

    type ItemStyle<'a> = TaffyStyloStyle
    where
        Self: 'a;

    fn get_flexbox_container_style(
        &self,
        _node_id: taffy::prelude::NodeId,
    ) -> Self::ContainerStyle<'_> {
        TaffyStyloStyleRef(self.style)
    }

    // TODO: Make a RefCell variant of TaffyStyloStyle to avoid the Arc clone here
    fn get_flexbox_child_style(
        &self,
        child_node_id: taffy::prelude::NodeId,
    ) -> Self::ItemStyle<'_> {
        let id = usize::from(child_node_id);
        let child = (*self.source_child_nodes[id]).borrow();
        let style = child.style.clone();
        TaffyStyloStyle(style)
    }
}

impl FlexContainer {
    pub fn inline_content_sizes(
        &self,
        layout_context: &LayoutContext,
        style: &ComputedValues,
    ) -> ContentSizes {
        let flex_direction = style.get_position().flex_direction;
        let flex_wrap = style.get_position().flex_wrap;

        let base = ContentSizes {
            min_content: Au::zero(),
            max_content: Au::zero(),
        };

        let child_iter = self.children.iter().filter_map(|child| {
            let mut child = (**child).borrow_mut();
            match &mut child.flex_level_box {
                FlexLevelBoxInner::FlexItem(item) => {
                    Some(item.inline_content_sizes(layout_context))
                },
                FlexLevelBoxInner::OutOfFlowAbsolutelyPositionedBox(_) => None,
            }
        });

        match (flex_direction, flex_wrap) {
            (FlexDirection::Row | FlexDirection::RowReverse, FlexWrap::Nowrap) => {
                child_iter.fold(base, |mut acc, child_content_sizes| {
                    acc.min_content = acc.min_content + child_content_sizes.min_content;
                    acc.max_content = acc.max_content + child_content_sizes.max_content;
                    acc
                })
            },
            (
                FlexDirection::Row | FlexDirection::RowReverse,
                FlexWrap::Wrap | FlexWrap::WrapReverse,
            ) => child_iter.fold(base, |mut acc, child_content_sizes| {
                acc.min_content = acc.min_content.max(child_content_sizes.min_content);
                acc.max_content = acc.max_content + child_content_sizes.max_content;
                acc
            }),
            (FlexDirection::Column | FlexDirection::ColumnReverse, _) => {
                child_iter.fold(base, |mut acc, child_content_sizes| {
                    acc.min_content = acc.min_content.max(child_content_sizes.min_content);
                    acc.max_content = acc.max_content.max(child_content_sizes.max_content);
                    acc
                })
            },
        }
    }

    /// <https://drafts.csswg.org/css-flexbox/#layout-algorithm>
    pub(crate) fn layout(
        &self,
        layout_context: &LayoutContext,
        positioning_context: &mut PositioningContext,
        content_box_size_override: &ContainingBlock,
        containing_block: &ContainingBlock,
    ) -> IndependentLayout {
        let mut flex_context = FlexContext {
            layout_context,
            positioning_context,
            content_box_size_override,
            style: &content_box_size_override.style,
            source_child_nodes: &self.children,
        };

        fn auto_or_to_option<T>(input: GenericLengthPercentageOrAuto<T>) -> Option<T> {
            match input {
                LengthPercentageOrAuto::LengthPercentage(val) => Some(val),
                LengthPercentageOrAuto::Auto => None,
            }
        }

        let pbm = content_box_size_override
            .style
            .padding_border_margin(containing_block);

        let known_dimensions = taffy::Size {
            width: Some(
                (content_box_size_override.inline_size + pbm.padding_border_sums.inline)
                    .to_f32_px(),
            ),
            height: auto_or_to_option(content_box_size_override.block_size)
                .map(Au::to_f32_px)
                .maybe_add(pbm.padding_border_sums.block.to_f32_px()),
        };

        let taffy_containing_block = taffy::Size {
            width: Some(containing_block.inline_size.to_f32_px()),
            height: auto_or_to_option(containing_block.block_size).map(Au::to_f32_px),
        };

        let output = taffy::compute_flexbox_layout(
            &mut flex_context,
            taffy::NodeId::from(usize::MAX),
            taffy::LayoutInput {
                run_mode: taffy::RunMode::PerformLayout,
                sizing_mode: taffy::SizingMode::InherentSize,
                axis: taffy::RequestedAxis::Vertical,
                vertical_margins_are_collapsible: taffy::Line::FALSE,

                known_dimensions,
                parent_size: taffy_containing_block,
                available_space: taffy_containing_block.map(taffy::AvailableSpace::from),
            },
        );

        // Convert `taffy::Layout` into Servo `Fragment`s
        let fragments: Vec<Fragment> = self
            .children
            .iter()
            .map(|child| (**child).borrow())
            .map(|child| {
                fn rect_to_logical_sides<T>(rect: taffy::Rect<T>) -> LogicalSides<T> {
                    LogicalSides {
                        inline_start: rect.left,
                        inline_end: rect.right,
                        block_start: rect.top,
                        block_end: rect.bottom,
                    }
                }

                fn size_and_pos_to_logical_rect<T: Default>(
                    position: taffy::Point<T>,
                    size: taffy::Size<T>,
                ) -> LogicalRect<T> {
                    LogicalRect {
                        start_corner: LogicalVec2 {
                            inline: position.x,
                            block: position.y,
                        },
                        size: LogicalVec2 {
                            inline: size.width,
                            block: size.height,
                        },
                    }
                }

                let layout = &child.taffy_layout;

                let padding = rect_to_logical_sides(layout.padding.map(Au::from_f32_px));
                let border = rect_to_logical_sides(layout.border.map(Au::from_f32_px));
                let margin = rect_to_logical_sides(layout.margin.map(Au::from_f32_px));
                let collapsed_margin = CollapsedBlockMargins::from_margin(&margin);

                // Compute content box size and position.
                //
                // For the x/y position we have to correct for the difference between the
                // content box and the border box for both the parent and the child.
                let content_size = size_and_pos_to_logical_rect(
                    taffy::Point {
                        x: Au::from_f32_px(
                            layout.location.x + layout.padding.left + layout.border.left,
                        ) - pbm.padding.inline_start -
                            pbm.border.inline_start,
                        y: Au::from_f32_px(
                            layout.location.y + layout.padding.top + layout.border.top,
                        ) - pbm.padding.block_start -
                            pbm.border.block_start,
                    },
                    taffy::Size {
                        width: layout.size.width -
                            layout.padding.left -
                            layout.padding.right -
                            layout.border.left -
                            layout.border.right,
                        height: layout.size.height -
                            layout.padding.top -
                            layout.padding.bottom -
                            layout.border.top -
                            layout.border.bottom,
                    }
                    .map(Au::from_f32_px),
                );

                match &child.flex_level_box {
                    FlexLevelBoxInner::FlexItem(independent_box) => {
                        // TODO: propagate absolute/fixed boxes from child positioning context
                        // child_positioning_context.adjust_static_position_of_hoisted_fragments(
                        //     &fragment,
                        //     PositioningContextLength::zero(),
                        // );
                        // positioning_context.append(child_positioning_context);

                        Fragment::Box(BoxFragment::new(
                            independent_box.base_fragment_info(),
                            independent_box.style().clone(),
                            // TODO: Eliminate clone
                            child.child_fragments.clone(),
                            content_size,
                            padding,
                            border,
                            margin,
                            None, /* clearance */
                            collapsed_margin,
                        ))
                    },
                    FlexLevelBoxInner::OutOfFlowAbsolutelyPositionedBox(abs_pos_box) => {
                        let hoisted_box = AbsolutelyPositionedBox::to_hoisted(
                            abs_pos_box.clone(),
                            LogicalVec2::zero(),
                            containing_block,
                        );
                        let hoisted_fragment = hoisted_box.fragment.clone();
                        positioning_context.push(hoisted_box);
                        Fragment::AbsoluteOrFixedPositioned(hoisted_fragment)
                    },
                }
            })
            .collect();

        IndependentLayout {
            fragments,
            content_block_size: Au::from_f32_px(output.size.height) - pbm.padding_border_sums.block,
            content_inline_size_for_table: Some(
                Au::from_f32_px(output.size.width) - pbm.padding_border_sums.inline,
            ),
            baselines: Baselines::default(),
        }
    }
}
