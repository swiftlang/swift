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
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/StringSwitch.h"

#if defined(SWIFT_WRAPPER_PREFIX)
#define SWIFT_WRAPPER_NAME(Name) SWIFT_WRAPPER_PREFIX Name
#endif

namespace swift {

enum RT_Kind {
#define KIND(Name, MemBehavior) RT_ ## Name,
#include "LLVMSwift.def"
};

/// classifyInstruction - Take a look at the specified instruction and classify
/// it into what kind of runtime entrypoint it is, if any.
inline RT_Kind classifyInstruction(const llvm::Instruction &I) {
  if (!I.mayReadOrWriteMemory())
    return RT_NoMemoryAccessed;

  // Non-calls or calls to indirect functions are unknown.
  auto *CI = dyn_cast<llvm::CallInst>(&I);
  if (CI == 0) return RT_Unknown;
  llvm::Function *F = CI->getCalledFunction();
  if (F == 0) return RT_Unknown;

  return llvm::StringSwitch<RT_Kind>(F->getName())
#define SWIFT_FUNC(Name, MemBehavior, TextualName) \
    .Case("swift_" #TextualName, RT_ ## Name)
#define OBJC_FUNC(Name, MemBehavior, TextualName) \
    .Case("objc_" #TextualName, RT_ ## Name)
#define SWIFT_INTERNAL_FUNC_NEVER_NONATOMIC(Name, MemBehavior, TextualName) \
    .Case("__swift_" #TextualName, RT_ ## Name)
#include "LLVMSwift.def"

#if defined(SWIFT_WRAPPER_PREFIX)
#define SWIFT_FUNC(Name, MemBehavior, TextualName) \
    .Case(SWIFT_WRAPPER_NAME("swift_" #TextualName), RT_ ## Name)
#define OBJC_FUNC(Name, MemBehavior, TextualName) \
    .Case(SWIFT_WRAPPER_NAME("objc_" #TextualName), RT_ ## Name)
#define SWIFT_INTERNAL_FUNC_NEVER_NONATOMIC(Name, MemBehavior, TextualName)
#include "LLVMSwift.def"
#endif

    // Support non-atomic versions of reference counting entry points.
#define SWIFT_FUNC(Name, MemBehavior, TextualName) \
    .Case("swift_nonatomic_" #TextualName, RT_ ## Name)
#define OBJC_FUNC(Name, MemBehavior, TextualName) \
    .Case("objc_nonatomic_" #TextualName, RT_ ## Name)
#define SWIFT_INTERNAL_FUNC_NEVER_NONATOMIC(Name, MemBehavior, TextualName)
#include "LLVMSwift.def"

#if defined(SWIFT_WRAPPER_PREFIX)
#define SWIFT_FUNC(Name, MemBehavior, TextualName) \
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_" #TextualName), RT_ ## Name)
#define OBJC_FUNC(Name, MemBehavior, TextualName) \
    .Case(SWIFT_WRAPPER_NAME("objc_nonatomic_" #TextualName), RT_ ## Name)
#define SWIFT_INTERNAL_FUNC_NEVER_NONATOMIC(Name, MemBehavior, TextualName)
#include "LLVMSwift.def"
#endif
    .Default(RT_Unknown);
}

} // end namespace swift
#endif
