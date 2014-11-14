//===--- SwiftTargetInfo.cpp ----------------------------------------------===//
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
#include "swift/ABI/System.h"
#include "swift/AST/IRGenOptions.h"

using namespace swift;
using namespace irgen;

/// Initialize a bit vector to be equal to the given bit-mask.
static void setToMask(llvm::BitVector &bits, uint64_t mask) {
  // This is a ridiculously inefficient way of doing this.
  for (unsigned i = 0, e = bits.size(); i != e; ++i) {
    if (mask & (1ULL << i)) {
      bits.set(i);
    } else {
      bits.reset(i);
    }
  }
}

/// Configures target-specific information for arm64 platforms.
static void configureARM64(IRGenModule &IGM, const llvm::Triple &triple,
                           SwiftTargetInfo &target) {
  setToMask(target.PointerSpareBits,
            SWIFT_ABI_ARM64_SWIFT_SPARE_BITS_MASK);
  setToMask(target.ObjCPointerReservedBits,
            SWIFT_ABI_ARM64_OBJC_RESERVED_BITS_MASK);
  
  if (triple.isOSDarwin()) {
    target.LeastValidPointerValue =
      SWIFT_ABI_DARWIN_ARM64_LEAST_VALID_POINTER;
  }

  // CGPoint and CGRect are both returned in registers.
  target.MaxScalarsForDirectResult = 4;

  // arm64 has no special objc_msgSend variants, not even stret.
  target.ObjCUseStret = false;

  // arm64 requires marker assembly for objc_retainAutoreleasedReturnValue.
  target.ObjCRetainAutoreleasedReturnValueMarker =
    "mov\tfp, fp\t\t; marker for objc_retainAutoreleaseReturnValue";

  // arm64 requires ISA-masking.
  target.ObjCUseISAMask = true;
  
  // BOOL is _Bool on arm64.
  target.ObjCBoolTypeSize = 1;
}

/// Configures target-specific information for x86-64 platforms.
static void configureX86_64(IRGenModule &IGM, const llvm::Triple &triple,
                            SwiftTargetInfo &target) {
  setToMask(target.PointerSpareBits,
            SWIFT_ABI_X86_64_SWIFT_SPARE_BITS_MASK);
  setToMask(target.ObjCPointerReservedBits,
            SWIFT_ABI_X86_64_OBJC_RESERVED_BITS_MASK);
  
  if (triple.isOSDarwin()) {
    target.LeastValidPointerValue =
      SWIFT_ABI_DARWIN_X86_64_LEAST_VALID_POINTER;
  }

  // On simulator targets, use null instead of &_objc_empty_vtable.
  if (triple.isiOS())
    target.ObjCUseNullForEmptyVTable = true;
  
  // x86-64 has every objc_msgSend variant known to humankind.
  target.ObjCUseFPRet = true;
  target.ObjCUseFP2Ret = true;

  // x86-64 requires ISA-masking.
  target.ObjCUseISAMask = true;
}

/// Configures target-specific information for 32-bit x86 platforms.
static void configureX86(IRGenModule &IGM, const llvm::Triple &triple,
                         SwiftTargetInfo &target) {
  // On simulator targets, use null instead of &_objc_empty_vtable.
  if (triple.isiOS())
    target.ObjCUseNullForEmptyVTable = true;
  
  // x86 uses objc_msgSend_fpret but not objc_msgSend_fp2ret.
  target.ObjCUseFPRet = true;
}

/// Configures target-specific information for 32-bit arm platforms.
static void configureARM(IRGenModule &IGM, const llvm::Triple &triple,
                         SwiftTargetInfo &target) {
  // ARM requires marker assembly for objc_retainAutoreleasedReturnValue.
  target.ObjCRetainAutoreleasedReturnValueMarker =
    "mov\tr7, r7\t\t@ marker for objc_retainAutoreleaseReturnValue";
}

/// Configure a default target.
SwiftTargetInfo::SwiftTargetInfo(
  llvm::Triple::ObjectFormatType outputObjectFormat,
  unsigned numPointerBits)
  : OutputObjectFormat(outputObjectFormat),
    PointerSpareBits(numPointerBits, false),
    ObjCPointerReservedBits(numPointerBits, true),
    HeapObjectAlignment(numPointerBits / 8),
    LeastValidPointerValue(SWIFT_ABI_DEFAULT_LEAST_VALID_POINTER)
{
  setToMask(PointerSpareBits,
            SWIFT_ABI_DEFAULT_SWIFT_SPARE_BITS_MASK);
  setToMask(ObjCPointerReservedBits,
            SWIFT_ABI_DEFAULT_OBJC_RESERVED_BITS_MASK);
}

SwiftTargetInfo SwiftTargetInfo::get(IRGenModule &IGM) {
  llvm::Triple triple(IGM.Opts.Triple);
  auto pointerSize = IGM.DataLayout.getPointerSizeInBits();

  /// Prepare generic target information.
  SwiftTargetInfo target(triple.getObjectFormat(), pointerSize);
  
  switch (triple.getArch()) {
  case llvm::Triple::x86_64:
    configureX86_64(IGM, triple, target);
    break;

  case llvm::Triple::x86:
    configureX86(IGM, triple, target);
    break;

  case llvm::Triple::arm:
    configureARM(IGM, triple, target);
    break;

  case llvm::Triple::aarch64:
    configureARM64(IGM, triple, target);
    break;

  default:
    // FIXME: Complain here? Default target info is unlikely to be correct.
    break;
  }

  // The JIT does not support absolute symbols, so we have to use null
  // for &objc_empty_vtable.
  if (IGM.Opts.UseJIT) {
    target.ObjCUseNullForEmptyVTable = true;
  }

  return target;
}

bool SwiftTargetInfo::hasObjCTaggedPointers() const {
  return ObjCPointerReservedBits.any();
}
