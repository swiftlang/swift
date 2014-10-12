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

using namespace llvm;
using swift::SwiftAliasAnalysis;

// Register this pass...
char SwiftAliasAnalysis::ID = 0;
INITIALIZE_AG_PASS(SwiftAliasAnalysis, AliasAnalysis, "swift-aa",
                   "Swift Alias Analysis", false, true, false)


AliasAnalysis::ModRefResult
SwiftAliasAnalysis::getModRefInfo(ImmutableCallSite CS, const Location &Loc) {
  // We know the mod-ref behavior of various runtime functions.
  switch (classifyInstruction(*CS.getInstruction())) {
  case RT_AllocObject:
  case RT_NoMemoryAccessed:
  case RT_Retain:
  case RT_RetainNoResult:
  case RT_RetainAndReturnThree:
  case RT_ObjCRetain:
  case RT_UnknownRetain:
    // These entrypoints don't modify any compiler-visible state.
    return NoModRef;
  case RT_Release:
  case RT_ObjCRelease:
  case RT_UnknownRelease:
  case RT_Unknown:
    break;
  }
  
  return AliasAnalysis::getModRefInfo(CS, Loc);
}
