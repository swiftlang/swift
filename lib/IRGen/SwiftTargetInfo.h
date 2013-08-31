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

#include "llvm/ADT/BitVector.h"
#include "IRGen.h"

namespace swift {
namespace irgen {
  class IRGenModule;

class SwiftTargetInfo {
public:
  SwiftTargetInfo(llvm::BitVector &&pointerSpareBits,
                  llvm::BitVector &&objcPointerReservedBits,
                  Alignment heapObjectAlignment,
                  uint64_t leastValidPointerValue)
    : PointerSpareBits(std::move(pointerSpareBits)),
      ObjCPointerReservedBits(std::move(objcPointerReservedBits)),
      HeapObjectAlignment(heapObjectAlignment),
      LeastValidPointerValue(leastValidPointerValue)
  {}
  
  /// Produces a SwiftTargetInfo object appropriate to the target.
  static SwiftTargetInfo get(IRGenModule &IGM);

  /// The spare bit mask for pointers. Bits set in this mask are unused by
  /// pointers of any alignment.
  const llvm::BitVector PointerSpareBits;
  
  /// The reserved bit mask for Objective-C pointers. Pointer values with
  /// bits from this mask set are reserved by the ObjC runtime and cannot be
  /// used for Swift value layout when a reference type may reference ObjC
  /// objects.
  const llvm::BitVector ObjCPointerReservedBits;
  
  /// The alignment of heap objects.
  const Alignment HeapObjectAlignment;
  
  /// The least integer value that can theoretically form a valid pointer.
  /// This excludes addresses in the null page(s) guaranteed to be unmapped by
  /// the platform.
  const uint64_t LeastValidPointerValue;
};

}
}

#endif