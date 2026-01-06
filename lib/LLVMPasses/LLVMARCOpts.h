//===--- LLVMARCOpts.h - LLVM level ARC Opts Util. Declarations -*- C++ -*-===//
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
#ifndef SWIFT_LLVMPASSES_LLVMARCOPTS_H
#define SWIFT_LLVMPASSES_LLVMARCOPTS_H

#include "swift/Basic/LLVM.h"
#include "swift/Runtime/Config.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"

namespace swift {

enum RT_Kind {
#define KIND(Name, MemBehavior) RT_ ## Name,
#include "LLVMSwift.def"
};

inline RT_Kind classifyFunctionName(StringRef name) {
  return llvm::StringSwitch<RT_Kind>(name)
#define SWIFT_FUNC(Name, MemBehavior, TextualName) \
    .Case("swift_" #TextualName, RT_ ## Name)
#define SWIFT_INTERNAL_FUNC_NEVER_NONATOMIC(Name, MemBehavior, TextualName) \
    .Case("__swift_" #TextualName, RT_ ## Name)
#include "LLVMSwift.def"

    // Identify "Client" versions of reference counting entry points.
#define SWIFT_FUNC(Name, MemBehavior, TextualName) \
    .Case("swift_" #TextualName "Client", RT_ ## Name)
#define SWIFT_INTERNAL_FUNC_NEVER_NONATOMIC(Name, MemBehavior, TextualName) \
    .Case("__swift_" #TextualName "Client", RT_ ## Name)
#include "LLVMSwift.def"

    // Support non-atomic versions of reference counting entry points.
#define SWIFT_FUNC(Name, MemBehavior, TextualName) \
    .Case("swift_nonatomic_" #TextualName, RT_ ## Name)
#define OBJC_FUNC(Name, MemBehavior, TextualName) \
    .Case("objc_nonatomic_" #TextualName, RT_ ## Name)
#define SWIFT_INTERNAL_FUNC_NEVER_NONATOMIC(Name, MemBehavior, TextualName)
#include "LLVMSwift.def"

    .Default(RT_Unknown);
}

/// Whether to allow ARC optimizations for a function with the given name.
inline bool allowArcOptimizations(StringRef name) {
  switch (classifyFunctionName(name)) {
  case RT_UnknownObjectRetainN:
  case RT_BridgeRetainN:
  case RT_RetainN:
  case RT_UnknownObjectReleaseN:
  case RT_BridgeReleaseN:
  case RT_ReleaseN:
  case RT_UnknownObjectRetain:
  case RT_UnknownObjectRelease:
  case RT_Retain:
  case RT_ObjCRetain:
  case RT_ObjCRelease:
  case RT_RetainUnowned:
  case RT_Release:
  case RT_BridgeRetain:
  case RT_BridgeRelease:
    return false;

  case RT_Unknown:
  case RT_NoMemoryAccessed:
  case RT_CheckUnowned:
  case RT_AllocObject:
  case RT_FixLifetime:
  case RT_EndBorrow:
    return true;
  }
}

/// Take a look at the specified instruction and classify it into what kind of
/// runtime entrypoint it is, if any.
inline RT_Kind classifyInstruction(const llvm::Instruction &I) {
  if (!I.mayReadOrWriteMemory())
    return RT_NoMemoryAccessed;

  // Non-calls or calls to indirect functions are unknown.
  auto *CI = dyn_cast<llvm::CallInst>(&I);
  if (CI == 0) return RT_Unknown;

  // First check if we have an objc intrinsic.
  auto intrinsic = CI->getIntrinsicID();
  switch (intrinsic) {
  // This is an intrinsic that we do not understand. It can not be one of our
  // "special" runtime functions as well... so return RT_Unknown early.
  default:
    return RT_Unknown;
  case llvm::Intrinsic::not_intrinsic:
    // If we do not have an intrinsic, break and move onto runtime functions
    // that we identify by name.
    break;
#define OBJC_FUNC(Name, MemBehavior, TextualName)                              \
  case llvm::Intrinsic::objc_##TextualName:                                    \
    return RT_##Name;
#include "LLVMSwift.def"
  }

  llvm::Function *F = CI->getCalledFunction();
  if (F == nullptr)
    return RT_Unknown;

  return classifyFunctionName(F->getName());
}

} // end namespace swift
#endif
