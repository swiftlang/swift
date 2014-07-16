//===--- SideCarFormat.h - The internals of side car files ------*- C++ -*-===//
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
/// \file
/// \brief Contains various constants and helper types to deal with side car
/// files.
///
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SIDE_CAR_FORMAT_H
#define SWIFT_SIDE_CAR_FORMAT_H

#include "swift/Serialization/BCRecordLayout.h" // FIXME: layering

namespace swift {
namespace side_car {

using namespace swift::serialization;

/// Magic number for side car files.
const unsigned char SIDE_CAR_SIGNATURE[] = { 0xE2, 0x9C, 0xA8, 0x01 };

/// Side car file major version number.
///
const uint16_t VERSION_MAJOR = 0;

/// Side car file minor version number.
///
/// When the format changes IN ANY WAY, this number should be incremented.
const uint16_t VERSION_MINOR = 0;

using IdentifierID = Fixnum<31>;
using IdentifierIDField = BCVBR<16>;

using ClassID = Fixnum<31>;
using ClassIDField = BCVBR<31>;

using ModuleID = Fixnum<15>;
using ModuleIDField = BCVBR<8>;

using SelectorID = Fixnum<31>;
using SelectorIDField = BCVBR<16>;

/// The various types of blocks that can occur within a side car file.
///
/// These IDs must \em not be renumbered or reordered without incrementing
/// VERSION_MAJOR.
enum BlockID {
  /// The side car block, which contains all of the other blocks.
  SIDE_CAR_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID,

  /// The control block, which contains all of the information that needs to
  /// be validated prior to committing to loading the side car file.
  ///
  /// \sa control_block
  CONTROL_BLOCK_ID,

  /// The identifier data block, which maps identifier strings to IDs.
  IDENTIFIER_BLOCK_ID,

  /// The Objective-C class data block, which maps Objective-C class
  /// names to information about the class.
  OBJC_CLASS_BLOCK_ID,

  /// The Objective-C property data block, which maps Objective-C
  /// (class name, property name) pairs to information about the
  /// property.
  OBJC_PROPERTY_BLOCK_ID,

  /// The Objective-C property data block, which maps Objective-C
  /// (class name, selector, is_instance_method) tuples to information
  /// about the method.
  OBJC_METHOD_BLOCK_ID,

  /// The Objective-C selector data block, which maps Objective-C
  /// selector names (# of pieces, identifier IDs) to the selector ID
  /// used in other tables.
  OBJC_SELECTOR_BLOCK_ID,
};

namespace control_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum {
    METADATA = 1,
  };

  using MetadataLayout = BCRecordLayout<
    METADATA, // ID
    BCFixed<16>, // Module format major version
    BCFixed<16>  // Module format minor version
  >;
}

namespace identifier_block {
  enum {
    IDENTIFIER_DATA = 1,
  };

  using IdentifierDataLayout = BCRecordLayout<
    IDENTIFIER_DATA,  // record ID
    BCVBR<16>,  // table offset within the blob (see below)
    BCBlob  // map from identifier strings to decl kinds / decl IDs
  >;
}

namespace objc_class_block {
  enum {
    OBJC_CLASS_DATA = 1,
  };

  using ObjCClassDataLayout = BCRecordLayout<
    OBJC_CLASS_DATA,  // record ID
    BCVBR<16>,  // table offset within the blob (see below)
    BCBlob  // map from ObjC class names (as IDs) to ObjC class information
  >;
}

namespace objc_property_block {
  enum {
    OBJC_PROPERTY_DATA = 1,
  };

  using ObjCPropertyDataLayout = BCRecordLayout<
    OBJC_PROPERTY_DATA,  // record ID
    BCVBR<16>,  // table offset within the blob (see below)
    BCBlob  // map from ObjC (class name, property name) pairs to ObjC
            // property information
  >;
}

namespace objc_method_block {
  enum {
    OBJC_METHOD_DATA = 1,
  };

  using ObjCMethodDataLayout = BCRecordLayout<
    OBJC_METHOD_DATA,  // record ID
    BCVBR<16>,  // table offset within the blob (see below)
    BCBlob  // map from ObjC (class names, selector,
            // is-instance-method) tuples to ObjC method information
  >;
}

namespace objc_selector_block {
  enum {
    OBJC_SELECTOR_DATA = 1,
  };

  using ObjCSelectorDataLayout = BCRecordLayout<
    OBJC_SELECTOR_DATA,  // record ID
    BCVBR<16>,  // table offset within the blob (see below)
    BCBlob  // map from (# pieces, identifier IDs) to Objective-C selector ID.
  >;
}

} // end namespace side_car
} // end namespace swift

#endif // LLVM_SWIFT_SIDE_CAR_FORMAT_H
