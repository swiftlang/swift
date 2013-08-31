//===--- SwiftTargetInfo.cpp --------------------------------------------*-===//
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
// This file defines the SwiftTargetInfo abstract base class. This class
// provides an interface to target-dependent attributes of interest to Swift.
//
//===----------------------------------------------------------------------===//

#include "SwiftTargetInfo.h"
#include "IRGenModule.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/DataLayout.h"
#include "swift/IRGen/Options.h"

using namespace swift;
using namespace irgen;

/// Creates a generic SwiftTargetInfo with conservative values that should
/// be valid for any platform absent more specific information.
static SwiftTargetInfo getGenericSwiftTargetInfo(IRGenModule &IGM) {
  auto pointerSize = IGM.DataLayout.getPointerSizeInBits();
  
  // Assume no spare bits in pointers.
  llvm::BitVector pointerSpareBits(pointerSize, false);
  // Assume all bit patterns are reserved by the ObjC runtime.
  llvm::BitVector objcReservedBits(pointerSize, true);
  // Assume no special alignment of heap objects.
  Alignment heapObjectAlignment(1);
  // Assume only zero is an invalid pointer.
  uint64_t leastValidPointerValue = 1;
  
  return SwiftTargetInfo(std::move(pointerSpareBits),
                         std::move(objcReservedBits),
                         heapObjectAlignment,
                         leastValidPointerValue);
}

/// Creates SwiftTargetInfo for X86-64 platforms.
static SwiftTargetInfo getX86_64SwiftTargetInfo(IRGenModule &IGM) {
  // User space only uses the low 47 bits of a pointer.
  // FIXME: In kernel mode, the highest bit is occupied.
  llvm::BitVector pointerSpareBits(47, false);
  pointerSpareBits.resize(64, true);
  
  // Objective-C reserves the lowest and highest bits for tagged pointers.
  llvm::BitVector objcReservedBits(64, false);
  objcReservedBits[0] = true;
  objcReservedBits[63] = true;
  
  // Heap objects are 16-byte-aligned.
  Alignment heapObjectAlignment(16);
  
  // The null 4K page is always unmapped.
  // FIXME: Are additional null pages always unmapped on some platforms?
  uint64_t leastValidPointerValue = 4096;
  
  return SwiftTargetInfo(std::move(pointerSpareBits),
                         std::move(objcReservedBits),
                         heapObjectAlignment,
                         leastValidPointerValue);
}

SwiftTargetInfo SwiftTargetInfo::get(IRGenModule &IGM) {
  llvm::Triple triple(IGM.Opts.Triple);
  
  switch (triple.getArch()) {
  case llvm::Triple::x86_64:
    return getX86_64SwiftTargetInfo(IGM);
  default:
    return getGenericSwiftTargetInfo(IGM);
  }
}