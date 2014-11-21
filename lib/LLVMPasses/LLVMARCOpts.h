//===- LLVMARCOpts.h - LLVM level ARC Opts Utility Declarations -*- C++ -*-===//
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

#include "swift/Basic/LLVM.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/StringSwitch.h"

namespace swift {

enum RT_Kind {
  /// An instruction with this classification is known to not access (read or
  /// write) memory.
  RT_NoMemoryAccessed,

  /// SwiftHeapObject *swift_retain(SwiftHeapObject *object)
  RT_Retain,

  // void swift_retain_noresult(SwiftHeapObject *object)
  RT_RetainNoResult,

  // (i64,i64,i64) swift_retainAndReturnThree(SwiftHeapObject *obj, i64,i64,i64)
  RT_RetainAndReturnThree,

  /// void swift_release(SwiftHeapObject *object)
  RT_Release,

  /// SwiftHeapObject *swift_allocObject(SwiftHeapMetadata *metadata,
  ///                                    size_t size, size_t alignment)
  RT_AllocObject,

  /// void objc_release(%objc_object* %P)
  RT_ObjCRelease,

  /// %objc_object* objc_retain(%objc_object* %P)
  RT_ObjCRetain,

  /// void swift_unknownRetain(%swift.refcounted* %P)
  RT_UnknownRetain,

  /// void swift_unknownRelease(%swift.refcounted* %P)
  RT_UnknownRelease,

  /// void swift_fixLifetime(%swift.refcounted* %P)
  RT_FixLifetime,

  /// void swift_bridgeRetain(%swift.refcounted* %P)
  RT_BridgeRetain,

  /// void swift_bridgeRelease(%swift.refcounted* %P)
  RT_BridgeRelease,

  /// This is not a runtime function that we support.  Maybe it is not a call,
  /// or is a call to something we don't care about.
  RT_Unknown,
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
    .Case("swift_retain", RT_Retain)
    .Case("swift_retain_noresult", RT_RetainNoResult)
    .Case("swift_release", RT_Release)
    .Case("swift_allocObject", RT_AllocObject)
    .Case("swift_retainAndReturnThree", RT_RetainAndReturnThree)
    .Case("objc_release", RT_ObjCRelease)
    .Case("objc_retain", RT_ObjCRetain)
    .Case("swift_bridgeObjectRetain", RT_BridgeRetain)
    .Case("swift_bridgeObjectRelease", RT_BridgeRelease)
    .Case("swift_unknownRetain", RT_UnknownRetain)
    .Case("swift_unknownRelease", RT_UnknownRelease)
    .Case("swift_fixLifetime", RT_FixLifetime)
    .Default(RT_Unknown);
}

} // end namespace swift
