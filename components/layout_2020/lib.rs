/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

#![deny(unsafe_code)]

mod cell;
pub mod context;
pub mod display_list;
pub mod dom;
mod dom_traversal;
mod flexbox;
pub mod flow;
mod formatting_contexts;
mod fragment_tree;
pub mod geom;
#[macro_use]
pub mod layout_debug;
mod lists;
mod positioned;
pub mod query;
mod replaced;
mod sizing;
mod style_ext;
pub mod table;
pub mod traversal;

use app_units::Au;
pub use flow::BoxTree;
pub use fragment_tree::FragmentTree;
use geom::AuOrAuto;
use style::logical_geometry::WritingMode;
use style::properties::ComputedValues;

use crate::geom::{LogicalVec2, SizeConstraint};

/// Represents the set of constraints that we use when computing the min-content
/// and max-content inline sizes of an element.
pub(crate) struct ConstraintSpace {
    pub block_size: SizeConstraint,
    pub writing_mode: WritingMode,
}

impl ConstraintSpace {
    fn new(block_size: SizeConstraint, writing_mode: WritingMode) -> Self {
        Self {
            block_size,
            writing_mode,
        }
    }

    fn new_for_style(style: &ComputedValues) -> Self {
        Self::new(SizeConstraint::default(), style.writing_mode)
    }
}

/// A variant of [`ContainingBlock`] that allows an indefinite inline size.
/// Useful for code that is shared for both layout (where we know the inline size
/// of the containing block) and intrinsic sizing (where we don't know it).
pub(crate) struct IndefiniteContainingBlock {
    pub size: LogicalVec2<AuOrAuto>,
    pub writing_mode: WritingMode,
}

impl From<&ConstraintSpace> for IndefiniteContainingBlock {
    fn from(constraint_space: &ConstraintSpace) -> Self {
        Self {
            size: LogicalVec2 {
                inline: AuOrAuto::Auto,
                block: constraint_space.block_size.to_auto_or(),
            },
            writing_mode: constraint_space.writing_mode,
        }
    }
}

impl<'a> From<&'_ ContainingBlock<'a>> for IndefiniteContainingBlock {
    fn from(containing_block: &ContainingBlock<'a>) -> Self {
        Self {
            size: LogicalVec2 {
                inline: AuOrAuto::LengthPercentage(containing_block.inline_size),
                block: containing_block.block_size,
            },
            writing_mode: containing_block.style.writing_mode,
        }
    }
}

impl<'a> From<&'_ DefiniteContainingBlock<'a>> for IndefiniteContainingBlock {
    fn from(containing_block: &DefiniteContainingBlock<'a>) -> Self {
        Self {
            size: containing_block
                .size
                .map(|v| AuOrAuto::LengthPercentage(*v)),
            writing_mode: containing_block.style.writing_mode,
        }
    }
}

pub struct ContainingBlock<'a> {
    inline_size: Au,
    block_size: AuOrAuto,
    style: &'a ComputedValues,
}

struct DefiniteContainingBlock<'a> {
    size: LogicalVec2<Au>,
    style: &'a ComputedValues,
}

impl<'a> From<&'_ DefiniteContainingBlock<'a>> for ContainingBlock<'a> {
    fn from(definite: &DefiniteContainingBlock<'a>) -> Self {
        ContainingBlock {
            inline_size: definite.size.inline,
            block_size: AuOrAuto::LengthPercentage(definite.size.block),
            style: definite.style,
        }
    }
}
