//===--- StringNormalization.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

// TODO(String Comparison Prototype): internalize everything in this file after
// prototype is integrated

// A namespace for various heuristics
//
public // String Comparison Prototype
enum _Normalization {

	// When normalized in NFC, some segments may expand in size. This expansion is
	// capped by the maximum expansion factor of the normal form. For NFC, that is
	// 3x.
	public // String Comparison Prototype
  static let _maxNFCExpansionFactor = 3

  // A small output buffer to use for normalizing a single normalization
  // segment. Fits all but pathological arbitrary-length segments (i.e. zalgo-
  // segments)
  public // String Comparison Prototype
  typealias _SegmentOutputBuffer = _FixedArray16<UInt16>

  // A large output buffer to use for normalizing large portions of a String. It
  // should be used extremely carefully, as chunking is inherently tricky in
  // Unicode, even when aligned on a normalization boundary (see "Multi-Segment
  // Expanders" below)
  public // String Comparison Prototype
  typealias _ChunkOutputBuffer = _FixedArray64<UInt16>

  public // String Comparison Prototype
  static var _nfcNormalizer: OpaquePointer = {
    var err = __swift_stdlib_U_ZERO_ERROR
    let normalizer = __swift_stdlib_unorm2_getNFCInstance(&err)
    guard err.isSuccess else {
      // This shouldn't be possible unless some deep (unrecoverable) system
      // invariants are violated
      fatalError("Unable to talk to ICU")
    }
    return normalizer
  }()

  public // String Comparison Prototype
  static func _prenormalQuickCheckYes(
    _ buffer: UnsafeBufferPointer<UInt16>
  ) -> Bool {
	  var err = __swift_stdlib_U_ZERO_ERROR
	  let length = __swift_stdlib_unorm2_spanQuickCheckYes(
	    _Normalization._nfcNormalizer,
	    buffer.baseAddress._unsafelyUnwrappedUnchecked,
	    Int32(buffer.count),
	    &err)

    guard err.isSuccess else {
      // This shouldn't be possible unless some deep (unrecoverable) system
      // invariants are violated
      fatalError("Unable to talk to ICU")
    }
	  return length == buffer.count
	}
}

extension UnicodeScalar {
  public // String Comparison Prototype
	var _hasNormalizationBoundaryBefore: Bool {
	  _sanityCheck(Int32(exactly: self.value) != nil, "top bit shouldn't be set")
	  let value = Int32(bitPattern: self.value)
	  return 0 != __swift_stdlib_unorm2_hasBoundaryBefore(
	  	_Normalization._nfcNormalizer, value)
	}

  public // String Comparison Prototype
  var _isDefined: Bool {
    return __swift_stdlib_u_isdefined(Int32(self.value)) != 0
  }

  public // String Comparison Prototype
  var _hasFullCompExclusion: Bool {
	  _sanityCheck(Int32(exactly: self.value) != nil, "top bit shouldn't be set")
	  let value = Int32(bitPattern: self.value)
	  let prop = __swift_stdlib_UCHAR_FULL_COMPOSITION_EXCLUSION
	  return __swift_stdlib_u_hasBinaryProperty(value, prop) != 0
  }
}


