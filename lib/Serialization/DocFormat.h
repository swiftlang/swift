//===--- DocFormat.h - The internals of swiftdoc files ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file Contains various constants and helper types to deal with serialized
/// documentation info (swiftdoc files).
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_DOCFORMAT_H
#define SWIFT_SERIALIZATION_DOCFORMAT_H

#include "llvm/Bitcode/RecordLayout.h"

namespace swift {
namespace serialization {

using llvm::BCArray;
using llvm::BCBlob;
using llvm::BCFixed;
using llvm::BCGenericRecordLayout;
using llvm::BCRecordLayout;
using llvm::BCVBR;

/// Magic number for serialized documentation files.
const unsigned char SWIFTDOC_SIGNATURE[] = { 0xE2, 0x9C, 0xA8, 0x07 };

/// Serialized swiftdoc format major version number.
///
/// Increment this value when making a backwards-incompatible change, i.e. where
/// an \e old compiler will \e not be able to read the new format. This should
/// be rare. When incrementing this value, reset SWIFTDOC_VERSION_MINOR to 0.
///
/// See docs/StableBitcode.md for information on how to make
/// backwards-compatible changes using the LLVM bitcode format.
const uint16_t SWIFTDOC_VERSION_MAJOR = 1;

/// Serialized swiftdoc format minor version number.
///
/// Increment this value when making a backwards-compatible change that might be
/// interesting to test for. A backwards-compatible change is one where an \e
/// old compiler can read the new format without any problems (usually by
/// ignoring new information).
///
/// If the \e new compiler can treat the new and old format identically, or if
/// the presence of a new record, block, or field is sufficient to indicate that
/// the swiftdoc file is using a new format, it is okay not to increment this
/// value. However, it may be interesting for a new compiler to treat the \e
/// absence of information differently for the old and new formats; in this
/// case, the difference in minor version number can distinguish the two.
///
/// The minor version number does not need to be changed simply to track which
/// compiler generated a swiftdoc file; the full compiler version is already
/// stored as text and can be checked by running the \c strings command-line
/// tool on a swiftdoc file.
///
/// To ensure that two separate changes don't silently get merged into one in
/// source control, you should also update the comment to briefly describe what
/// change you made. The content of this comment isn't important; it just
/// ensures a conflict if two people change the module format. Don't worry about
/// adhering to the 80-column limit for this line.
const uint16_t SWIFTDOC_VERSION_MINOR = 1; // Last change: skipping 0 for testing purposes

/// The record types within the comment block.
///
/// Be very careful when changing this block; it must remain
/// backwards-compatible. Adding new records is okay---they will be ignored---
/// but modifying existing ones must be done carefully. You may need to update
/// the version when you do so. See docs/StableBitcode.md for information on how
/// to make backwards-compatible changes using the LLVM bitcode format.
///
/// \sa COMMENT_BLOCK_ID
namespace comment_block {
  enum RecordKind {
    DECL_COMMENTS = 1,
    GROUP_NAMES = 2,
  };

  using DeclCommentListLayout = BCRecordLayout<
    DECL_COMMENTS, // record ID
    BCVBR<16>,     // table offset within the blob (an llvm::OnDiskHashTable)
    BCBlob         // map from Decl USRs to comments
  >;

  using GroupNamesLayout = BCRecordLayout<
    GROUP_NAMES,    // record ID
    BCBlob          // actual names
  >;

} // namespace comment_block

} // end namespace serialization
} // end namespace swift

#endif
