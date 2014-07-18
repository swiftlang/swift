//===--- APINotesFormat.h - The internals of API notes files ------*- C++ -*-===//
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
/// \brief Contains various constants and helper types to deal with API notes
/// files.
///
//===----------------------------------------------------------------------===//
#ifndef SWIFT_API_NOTES_FORMAT_H
#define SWIFT_API_NOTES_FORMAT_H

#include "swift/Serialization/BCRecordLayout.h" // FIXME: layering
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
namespace api_notes {

using namespace swift::serialization;

/// Magic number for API notes files.
const unsigned char API_NOTES_SIGNATURE[] = { 0xE2, 0x9C, 0xA8, 0x01 };

/// API notes file major version number.
///
const uint16_t VERSION_MAJOR = 0;

/// API notes file minor version number.
///
/// When the format changes IN ANY WAY, this number should be incremented.
const uint16_t VERSION_MINOR = 1;

using IdentifierID = Fixnum<31>;
using IdentifierIDField = BCVBR<16>;

using SelectorID = Fixnum<31>;
using SelectorIDField = BCVBR<16>;

/// The various types of blocks that can occur within a API notes file.
///
/// These IDs must \em not be renumbered or reordered without incrementing
/// VERSION_MAJOR.
enum BlockID {
  /// The control block, which contains all of the information that needs to
  /// be validated prior to committing to loading the API notes file.
  ///
  /// \sa control_block
  CONTROL_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID,

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

/// A stored Objective-C selector.
struct StoredObjCSelector {
  unsigned NumPieces;
  llvm::SmallVector<IdentifierID, 2> Identifiers;
};

} // end namespace api_notes
} // end namespace swift

namespace llvm {
  template<>
  struct DenseMapInfo<swift::api_notes::StoredObjCSelector> {
    typedef DenseMapInfo<unsigned> UnsignedInfo;

    static inline swift::api_notes::StoredObjCSelector getEmptyKey() {
      return swift::api_notes::StoredObjCSelector{ 
               UnsignedInfo::getEmptyKey(), { } };
    }

    static inline swift::api_notes::StoredObjCSelector getTombstoneKey() {
      return swift::api_notes::StoredObjCSelector{ 
               UnsignedInfo::getTombstoneKey(), { } };
    }
    
    static unsigned getHashValue(
                      const swift::api_notes::StoredObjCSelector& value) {
      auto hash = llvm::hash_value(value.NumPieces);
      hash = hash_combine(hash, value.Identifiers.size());
      for (auto piece : value.Identifiers)
        hash = hash_combine(hash, static_cast<unsigned>(piece));
      // FIXME: Mix upper/lower 32-bit values together to produce
      // unsigned rather than truncating.
      return hash;
    }

    static bool isEqual(const swift::api_notes::StoredObjCSelector &lhs, 
                        const swift::api_notes::StoredObjCSelector &rhs) {
      return lhs.NumPieces == rhs.NumPieces && 
             lhs.Identifiers == rhs.Identifiers;
    }
  };
}

#endif // LLVM_SWIFT_API_NOTES_FORMAT_H
