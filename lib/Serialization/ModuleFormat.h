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
const char Signature[] = "SMod";

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
  INPUT_BLOCK_ID
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

} // end namespace serialization
} // end namespace swift

#endif
