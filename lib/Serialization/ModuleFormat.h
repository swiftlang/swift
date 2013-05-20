//===--- ModuleFormat.h - The internals of serialized modules ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file Contains various constants and helper types to deal with serialized
/// modules.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_MODULEFORMAT_H
#define SWIFT_SERIALIZATION_MODULEFORMAT_H

#include "swift/Serialization/BCRecordLayout.h"
#include "llvm/Bitcode/BitCodes.h"

namespace swift {
namespace serialization {

/// Magic number for serialized module files.
const unsigned char SIGNATURE[] = { 0xE2, 0x9C, 0xA8, 0x0E };

/// Serialized module format major version number.
///
/// When the format changes in such a way that older compilers will not be
/// able to read the file at all, this number should be incremented.
const unsigned VERSION_MAJOR = 1;

/// Serialized module format minor version number.
///
/// When the format changes in a backwards-compatible way, this number should
/// be incremented.
const unsigned VERSION_MINOR = 0;

/// The various types of blocks that can occur within a serialized Swift
/// module.
enum BlockID {
  /// The control block, which contains all of the information that needs to
  /// be validated prior to committing to loading the serialized module.
  ///
  /// \sa control_block
  CONTROL_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID,

  /// The input block, which contains all the files this module depends on.
  ///
  /// \sa input_block
  INPUT_BLOCK_ID,

  /// The "decls-and-types" block, which contains all of the declarations that
  /// come from this module.
  ///
  /// Types are also stored here, so that types that just wrap a Decl don't need
  /// a separate entry in the file.
  ///
  /// \sa decls_block
  DECLS_AND_TYPES_BLOCK_ID,

  /// The index block, which contains cross-referencing information for the
  /// module.
  ///
  /// \sa index_block
  INDEX_BLOCK_ID
};

/// The record types within the control block.
///
/// \sa CONTROL_BLOCK_ID
namespace control_block {
  enum {
    METADATA = 1
  };

  using MetadataLayout = BCRecordLayout<
    METADATA, // ID
    BCFixed<16>, // Module format major version
    BCFixed<16>, // Module format minor version
    BCBlob // misc. version information
  >;
}

/// The record types within the input block.
///
/// \sa INPUT_BLOCK_ID
namespace input_block {
  enum {
    SOURCE_FILE = 1
  };

  using SourceFileLayout = BCRecordLayout<
    SOURCE_FILE, // ID
    BCBlob // path
  >;
}

using DeclID = uint32_t;
using DeclIDField = BCFixed<32>;

using BitOffset = uint32_t;
using BitOffsetField = BCFixed<32>;

/// The record types within the "decls-and-types" block.
///
/// \sa DECLS_AND_TYPES_BLOCK_ID
namespace decls_block {
  enum {
    BUILTIN_TYPE = 1,

    TYPEALIAS_DECL = 100,

    NAME_HACK = 200
  };

  using BuiltinTypeLayout = BCRecordLayout<
    BUILTIN_TYPE,
    BCBlob // name of the builtin type
  >;

  using TypeAliasLayout = BCRecordLayout<
    TYPEALIAS_DECL,
    DeclIDField, // underlying type
    BCFixed<1>,  // generic flag
    BCArray<DeclIDField> // inherited types
  >;

  /// Names will eventually be uniqued in an identifier table, but for now we
  /// store them as trailing records.
  using NameHackLayout = BCRecordLayout<NAME_HACK, BCBlob>;
}

/// The record types within the index block.
///
/// \sa INDEX_BLOCK_ID
namespace index_block {
  enum {
    TYPE_OFFSETS = 1,
    DECL_OFFSETS
  };

  // FIXME: Merge these.
  using TypeOffsetsLayout = BCRecordLayout<
    TYPE_OFFSETS,
    DeclIDField, // first ID in this module
    BCArray<BitOffsetField>
  >;

  using DeclOffsetsLayout = BCRecordLayout<
    DECL_OFFSETS,
    DeclIDField, // first ID in this module
    BCArray<BitOffsetField>
  >;
}

} // end namespace serialization
} // end namespace swift

#endif
