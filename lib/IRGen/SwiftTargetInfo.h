//===--- SwiftTargetInfo.h --------------------------------------*- C++ -*-===//
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
//
// This file declares the SwiftTargetInfo abstract base class. This class
// provides an interface to target-dependent attributes of interest to Swift.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_IRGEN_TARGET_INFO_H__
#define __SWIFT_IRGEN_TARGET_INFO_H__

#include "swift/Basic/LLVM.h"
#include "swift/Basic/ClusteredBitVector.h"
#include "llvm/ADT/Triple.h"
#include "IRGen.h"

namespace swift {
namespace irgen {
  class IRGenModule;

class SwiftTargetInfo {
  explicit SwiftTargetInfo(llvm::Triple::ObjectFormatType outputObjectFormat,
                           unsigned numPointerBits);

public:

  /// Produces a SwiftTargetInfo object appropriate to the target.
  static SwiftTargetInfo get(IRGenModule &IGM);

  /// True if the ObjC runtime for the chosen platform supports tagged pointers.
  bool hasObjCTaggedPointers() const;

  /// True if the ObjC runtime for the chosen platform requires ISA masking.
  bool hasISAMasking() const {
    return ObjCUseISAMask;
  }

  /// The target's object format type.
  llvm::Triple::ObjectFormatType OutputObjectFormat;
  
  /// The spare bit mask for pointers. Bits set in this mask are unused by
  /// pointers of any alignment.
  SpareBitVector PointerSpareBits;

  /// The spare bit mask for (ordinary C) thin function pointers.
  SpareBitVector FunctionPointerSpareBits;
  
  /// The reserved bit mask for Objective-C pointers. Pointer values with
  /// bits from this mask set are reserved by the ObjC runtime and cannot be
  /// used for Swift value layout when a reference type may reference ObjC
  /// objects.
  SpareBitVector ObjCPointerReservedBits;
  
  /// The alignment of heap objects.  By default, assume pointer alignment.
  Alignment HeapObjectAlignment;
  
  /// The least integer value that can theoretically form a valid pointer.
  /// By default, assume that there's an entire page free.
  ///
  /// This excludes addresses in the null page(s) guaranteed to be
  /// unmapped by the platform.
  ///
  /// Changes to this must be kept in sync with swift/Runtime/Metadata.h.
  uint64_t LeastValidPointerValue;

  /// The maximum number of scalars that we allow to be returned directly.
  /// FIXME Until rdar://14679857, this must be set such that 
  /// NSRect and NSPoint structs are returned correctly.
  unsigned MaxScalarsForDirectResult = 3;

  /// Inline assembly to mark a call to objc_retainAutoreleasedReturnValue.
  llvm::StringRef ObjCRetainAutoreleasedReturnValueMarker;

  /// The integer size of ObjC's BOOL type, i8 or i1.
  unsigned ObjCBoolTypeSize = 8;
  
  /// Some architectures have specialized objc_msgSend variants.
  bool ObjCUseStret = true;
  bool ObjCUseFPRet = false;
  bool ObjCUseFP2Ret = false;
  bool ObjCUseNullForEmptyVTable = false;
  bool ObjCUseISAMask = false;
  
  /// The value stored in a Builtin.once predicate to indicate that an
  /// initialization has already happened, if known.
  Optional<int64_t> OnceDonePredicateValue = None;
};

}
}

#endif

