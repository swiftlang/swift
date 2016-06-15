//===--- LLVMARCOpts.h - LLVM level ARC Opts Util. Declarations -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
  /// An instruction with this classification is known to not access (read or
  /// write) memory.
  RT_NoMemoryAccessed,

  /// void swift_retain(SwiftHeapObject *object)
  RT_Retain,

  /// void swift_retain_n(SwiftHeapObject *object)
  RT_RetainN,

  /// void swift::swift_retainUnowned(HeapObject *object)
  RT_RetainUnowned,
  
  /// void swift_checkUnowned(HeapObject *object)
  RT_CheckUnowned,
  
  /// void swift_release(SwiftHeapObject *object)
  RT_Release,

  /// void swift_release_n(SwiftHeapObject *object)
  RT_ReleaseN,

  /// SwiftHeapObject *swift_allocObject(SwiftHeapMetadata *metadata,
  ///                                    size_t size, size_t alignment)
  RT_AllocObject,

  /// void objc_release(%objc_object* %P)
  RT_ObjCRelease,

  /// %objc_object* objc_retain(%objc_object* %P)
  RT_ObjCRetain,

  /// void swift_unknownRetain(%swift.refcounted* %P)
  RT_UnknownRetain,

  /// void swift_unknownRetain_n(%swift.refcounted* %P)
  RT_UnknownRetainN,

  /// void swift_unknownRelease(%swift.refcounted* %P)
  RT_UnknownRelease,

  /// void swift_unknownRelease_n(%swift.refcounted* %P)
  RT_UnknownReleaseN,

  /// void __swift_fixLifetime(%swift.refcounted* %P)
  RT_FixLifetime,

  /// void swift_bridgeRetain(%swift.refcounted* %P)
  RT_BridgeRetain,

  /// void swift_bridgeRetain_n(%swift.refcounted* %P)
  RT_BridgeRetainN,

  /// void swift_bridgeRelease(%swift.refcounted* %P)
  RT_BridgeRelease,

  /// void swift_bridgeRelease_n(%swift.refcounted* %P)
  RT_BridgeReleaseN,

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
    .Case("swift_retain_n", RT_RetainN)
    .Case("swift_release", RT_Release)
    .Case("swift_release_n", RT_ReleaseN)
    .Case("swift_allocObject", RT_AllocObject)
    .Case("objc_release", RT_ObjCRelease)
    .Case("objc_retain", RT_ObjCRetain)
    .Case("swift_retainUnowned", RT_RetainUnowned)
    .Case("swift_checkUnowned", RT_CheckUnowned)
    .Case("swift_bridgeObjectRetain", RT_BridgeRetain)
    .Case("swift_bridgeObjectRetain_n", RT_BridgeRetainN)
    .Case("swift_bridgeObjectRelease", RT_BridgeRelease)
    .Case("swift_bridgeObjectRelease_n", RT_BridgeReleaseN)
    .Case("swift_unknownRetain", RT_UnknownRetain)
    .Case("swift_unknownRetain_n", RT_UnknownRetainN)
    .Case("swift_unknownRelease", RT_UnknownRelease)
    .Case("swift_unknownRelease_n", RT_UnknownReleaseN)
    .Case("__swift_fixLifetime", RT_FixLifetime)
#if defined(SWIFT_WRAPPER_PREFIX)
    .Case(SWIFT_WRAPPER_NAME("swift_retain"), RT_Retain)
    .Case(SWIFT_WRAPPER_NAME("swift_retain_n"), RT_RetainN)
    .Case(SWIFT_WRAPPER_NAME("swift_release"), RT_Release)
    .Case(SWIFT_WRAPPER_NAME("swift_release_n"), RT_ReleaseN)
    .Case(SWIFT_WRAPPER_NAME("swift_allocObject"), RT_AllocObject)
    .Case(SWIFT_WRAPPER_NAME("objc_release"), RT_ObjCRelease)
    .Case(SWIFT_WRAPPER_NAME("objc_retain"), RT_ObjCRetain)
    .Case(SWIFT_WRAPPER_NAME("swift_retainUnowned"), RT_RetainUnowned)
    .Case(SWIFT_WRAPPER_NAME("swift_checkUnowned"), RT_CheckUnowned)
    .Case(SWIFT_WRAPPER_NAME("swift_bridgeObjectRetain"), RT_BridgeRetain)
    .Case(SWIFT_WRAPPER_NAME("swift_bridgeObjectRetain_n"), RT_BridgeRetainN)
    .Case(SWIFT_WRAPPER_NAME("swift_bridgeObjectRelease"), RT_BridgeRelease)
    .Case(SWIFT_WRAPPER_NAME("swift_bridgeObjectRelease_n"), RT_BridgeReleaseN)
    .Case(SWIFT_WRAPPER_NAME("swift_unknownRetain"), RT_UnknownRetain)
    .Case(SWIFT_WRAPPER_NAME("swift_unknownRetain_n"), RT_UnknownRetainN)
    .Case(SWIFT_WRAPPER_NAME("swift_unknownRelease"), RT_UnknownRelease)
    .Case(SWIFT_WRAPPER_NAME("swift_unknownRelease_n"), RT_UnknownReleaseN)
#endif
    // Support non-atomic versions of reference counting entry points.
    .Case("swift_nonatomic_retain", RT_Retain)
    .Case("swift_nonatomic_retain_n", RT_RetainN)
    .Case("swift_nonatomic_release", RT_Release)
    .Case("swift_nonatomic_release_n", RT_ReleaseN)
    .Case("objc_nonatomic_release", RT_ObjCRelease)
    .Case("objc_nonatomic_retain", RT_ObjCRetain)
    .Case("swift_nonatomic_retainUnowned", RT_RetainUnowned)
    .Case("swift_nonatomic_checkUnowned", RT_CheckUnowned)
    .Case("swift_nonatomic_bridgeObjectRetain", RT_BridgeRetain)
    .Case("swift_nonatomic_bridgeObjectRetain_n", RT_BridgeRetainN)
    .Case("swift_nonatomic_bridgeObjectRelease", RT_BridgeRelease)
    .Case("swift_nonatomic_bridgeObjectRelease_n", RT_BridgeReleaseN)
    .Case("swift_nonatomic_unknownRetain", RT_UnknownRetain)
    .Case("swift_nonatomic_unknownRetain_n", RT_UnknownRetainN)
    .Case("swift_nonatomic_unknownRelease", RT_UnknownRelease)
    .Case("swift_nonatomic_unknownRelease_n", RT_UnknownReleaseN)
#if defined(SWIFT_WRAPPER_PREFIX)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_retain"), RT_Retain)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_retain_n"), RT_RetainN)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_release"), RT_Release)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_release_n"), RT_ReleaseN)
    .Case(SWIFT_WRAPPER_NAME("objc_nonatomic_release"), RT_ObjCRelease)
    .Case(SWIFT_WRAPPER_NAME("objc_nonatomic_retain"), RT_ObjCRetain)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_retainUnowned"), RT_RetainUnowned)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_checkUnowned"), RT_CheckUnowned)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_bridgeObjectRetain"), RT_BridgeRetain)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_bridgeObjectRetain_n"), RT_BridgeRetainN)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_bridgeObjectRelease"), RT_BridgeRelease)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_bridgeObjectRelease_n"), RT_BridgeReleaseN)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_unknownRetain"), RT_UnknownRetain)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_unknownRetain_n"), RT_UnknownRetainN)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_unknownRelease"), RT_UnknownRelease)
    .Case(SWIFT_WRAPPER_NAME("swift_nonatomic_unknownRelease_n"), RT_UnknownReleaseN)
#endif
    .Default(RT_Unknown);
}

} // end namespace swift
#endif
