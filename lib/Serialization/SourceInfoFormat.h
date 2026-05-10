//===--- SourceInfoFormat.h - Format swiftsourceinfo files ---*- c++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file Contains various constants and helper types to deal with serialized
/// source information (.swiftsourceinfo files).
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_SOURCEINFOFORMAT_H
#define SWIFT_SERIALIZATION_SOURCEINFOFORMAT_H

#include "llvm/Bitcode/BitcodeConvenience.h"

namespace swift {
namespace serialization {

using llvm::BCArray;
using llvm::BCBlob;
using llvm::BCFixed;
using llvm::BCGenericRecordLayout;
using llvm::BCRecordLayout;
using llvm::BCVBR;

/// Magic number for serialized source info files.
const unsigned char SWIFTSOURCEINFO_SIGNATURE[] = { 0xF0, 0x9F, 0x8F, 0x8E };

/// Serialized sourceinfo format major version number.
///
/// Increment this value when making a backwards-incompatible change, i.e. where
/// an \e old compiler will \e not be able to read the new format. This should
/// be rare. When incrementing this value, reset SWIFTSOURCEINFO_VERSION_MINOR to 0.
///
/// See docs/StableBitcode.md for information on how to make
/// backwards-compatible changes using the LLVM bitcode format.
const uint16_t SWIFTSOURCEINFO_VERSION_MAJOR = 3;

/// Serialized swiftsourceinfo format minor version number.
///
/// Increment this value when making a backwards-compatible change that might be
/// interesting to test for. A backwards-compatible change is one where an \e
/// old compiler can read the new format without any problems (usually by
/// ignoring new information).
const uint16_t SWIFTSOURCEINFO_VERSION_MINOR = 0; // add location offset

/// The hash seed used for the string hashes(llvm::djbHash) in a .swiftsourceinfo file.
const uint32_t SWIFTSOURCEINFO_HASH_SEED = 5387;

/// The record types within the DECL_LOCS block.
///
/// Though we strive to keep the format stable, breaking the format of
/// .swiftsourceinfo doesn't have consequences as serious as breaking the format
/// of .swiftdoc, because .swiftsourceinfo file is for local development use only.
///
/// When changing this block, backwards-compatible changes are preferred.
/// You may need to update the version when you do so. See docs/StableBitcode.md
/// for information on how to make backwards-compatible changes using the LLVM
/// bitcode format.
///
/// \sa DECL_LOCS_BLOCK_ID
namespace decl_locs_block {
  enum RecordKind {
    BASIC_DECL_LOCS = 1,
    DECL_USRS,
    TEXT_DATA,
    DOC_RANGES,
    SOURCE_FILE_LIST,
  };

  using SourceFileListLayout = BCRecordLayout<
    SOURCE_FILE_LIST, // record ID
    BCBlob            // An array of fixed size 'BasicSourceFileInfo' data.
  >;

  using BasicDeclLocsLayout = BCRecordLayout<
    BASIC_DECL_LOCS, // record ID
    BCBlob           // an array of fixed size location data
  >;

  using DeclUSRSLayout = BCRecordLayout<
    DECL_USRS,         // record ID
    BCVBR<16>,         // table offset within the blob (an llvm::OnDiskHashTable)
    BCBlob             // map from actual USR to USR Id
  >;

  using TextDataLayout = BCRecordLayout<
    TEXT_DATA,        // record ID
    BCBlob            // a list of 0-terminated string segments
  >;

  using DocRangesLayout = BCRecordLayout<
    DOC_RANGES,         // record ID
    BCBlob
  >;

} // namespace sourceinfo_block

} // end namespace serialization
} // end namespace swift

#endif
