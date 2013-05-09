//===--- Serialization.h - Read and write Swift modules ---------*- C++ -*-===//
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
/// \file A catch-all header file for Swift module reading and writing.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_H
#define SWIFT_SERIALIZATION_H

#include "swift/Basic/LLVM.h"
#include "llvm/Bitcode/BitCodes.h"

namespace swift {
namespace serialization {

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
  /// \sa ControlRecordType
  CONTROL_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID,

  /// The input block, which contains all the files this module depends on.
  ///
  /// \sa InputRecordType
  INPUT_BLOCK_ID
};

/// The record types within the control block.
///
/// \sa CONTROL_BLOCK_ID
enum ControlRecordType {
  METADATA = 1
};

/// The record types within the input block.
///
/// \sa INPUT_BLOCK_ID
enum InputRecordType {
  SOURCE_FILE = 1
};

} // end namespace serialization
} // end namespace swift

#endif
