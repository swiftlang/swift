//===--- LLVMSwiftAA.cpp - LLVM Alias Analysis for Swift ------------------===//
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

#include "swift/LLVMPasses/Passes.h"
#include "LLVMARCOpts.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/LegacyPassManager.h" 
#include "llvm/IR/Module.h"

using namespace llvm;
using swift::SwiftAAResult;
using swift::SwiftAAWrapperPass;

//===----------------------------------------------------------------------===//
//                           Alias Analysis Result
//===----------------------------------------------------------------------===//

llvm::ModRefInfo SwiftAAResult::getModRefInfo(llvm::ImmutableCallSite CS,
                                              const llvm::MemoryLocation &Loc) {
  // We know the mod-ref behavior of various runtime functions.
  switch (classifyInstruction(*CS.getInstruction())) {
  case RT_AllocObject:
  case RT_NoMemoryAccessed:
  case RT_Retain:
  case RT_RetainUnowned:
  case RT_CheckUnowned:
  case RT_ObjCRetain:
  case RT_BridgeRetain:
  case RT_UnknownRetain:
  case RT_RetainN:
  case RT_UnknownRetainN:
  case RT_BridgeRetainN:
  case RT_FixLifetime:
    // These entrypoints don't modify any compiler-visible state.
    return MRI_NoModRef;
  case RT_ReleaseN:
  case RT_UnknownReleaseN:
  case RT_BridgeReleaseN:
  case RT_Release:
  case RT_ObjCRelease:
  case RT_BridgeRelease:
  case RT_UnknownRelease:
  case RT_Unknown:
    break;
  }

  return AAResultBase::getModRefInfo(CS, Loc);
}

//===----------------------------------------------------------------------===//
//                        Alias Analysis Wrapper Pass
//===----------------------------------------------------------------------===//

char SwiftAAWrapperPass::ID = 0;
INITIALIZE_PASS_BEGIN(SwiftAAWrapperPass, "swift-aa",
                      "Swift Alias Analysis", false, true)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_END(SwiftAAWrapperPass, "swift-aa",
                    "Swift Alias Analysis", false, true)

SwiftAAWrapperPass::SwiftAAWrapperPass() : ImmutablePass(ID) {
  initializeSwiftAAWrapperPassPass(*PassRegistry::getPassRegistry());
}

bool SwiftAAWrapperPass::doInitialization(Module &M) {
  Result.reset(new SwiftAAResult());
  return false;
}

bool SwiftAAWrapperPass::doFinalization(Module &M) {
  Result.reset();
  return false;
}

void SwiftAAWrapperPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.setPreservesAll();
  AU.addRequired<TargetLibraryInfoWrapperPass>();
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

llvm::ImmutablePass *swift::createSwiftAAWrapperPass() {
  return new SwiftAAWrapperPass();
}
