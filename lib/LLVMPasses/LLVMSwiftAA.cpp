//===--- LLVMSwiftAA.cpp - LLVM Alias Analysis for Swift ------------------===//
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

#include "swift/LLVMPasses/Passes.h"
#include "LLVMARCOpts.h"
#include "llvm/IR/Module.h"

using namespace llvm;
using swift::SwiftAliasAnalysis;

// Register this pass...
char SwiftAliasAnalysis::ID = 0;
INITIALIZE_AG_PASS(SwiftAliasAnalysis, AliasAnalysis, "swift-aa",
                   "Swift Alias Analysis", false, true, false)

bool SwiftAliasAnalysis::doInitialization(Module &M) {
  InitializeAliasAnalysis(this, &M.getDataLayout());
  return true;
}

AliasAnalysis::ModRefResult
SwiftAliasAnalysis::getModRefInfo(ImmutableCallSite CS,
                                  const llvm::MemoryLocation &Loc) {
  // We know the mod-ref behavior of various runtime functions.
  switch (classifyInstruction(*CS.getInstruction())) {
  case RT_AllocObject:
  case RT_NoMemoryAccessed:
  case RT_Retain:
  case RT_RetainNoResult:
  case RT_RetainAndReturnThree:
  case RT_ObjCRetain:
  case RT_BridgeRetain:
  case RT_UnknownRetain:
  case RT_FixLifetime:
    // These entrypoints don't modify any compiler-visible state.
    return NoModRef;
  case RT_Release:
  case RT_ObjCRelease:
  case RT_BridgeRelease:
  case RT_UnknownRelease:
  case RT_Unknown:
    break;
  }

  return AliasAnalysis::getModRefInfo(CS, Loc);
}

llvm::ImmutablePass *swift::createSwiftAliasAnalysisPass() {
  initializeSwiftAliasAnalysisPass(*PassRegistry::getPassRegistry());
  return new SwiftAliasAnalysis();
}
