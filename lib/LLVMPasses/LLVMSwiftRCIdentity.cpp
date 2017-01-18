//===--- LLVMSwiftRCIdentity.cpp - LLVM RCIdentity Analysis for Swift -----===//
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

#include "swift/LLVMPasses/Passes.h"
#include "LLVMARCOpts.h"
#include "llvm/IR/Module.h"

using namespace llvm;
using swift::SwiftRCIdentity;

// Register this pass...
char SwiftRCIdentity::ID = 0;
INITIALIZE_PASS(SwiftRCIdentity, "swift-rc-identity",
               "Swift RC Identity Analysis", false, true)

bool SwiftRCIdentity::doInitialization(Module &M) {
  return true;
}

llvm::Value *
SwiftRCIdentity::stripPointerCasts(llvm::Value *Val) {
  return Val->stripPointerCasts();
}

llvm::Value *
SwiftRCIdentity::stripReferenceForwarding(llvm::Value *Val) {
  auto Inst = dyn_cast<Instruction>(Val);
  if (!Inst)
    return Val;
  auto Kind = classifyInstruction(*Inst);
  switch(Kind) {
  case RT_RetainN:
  case RT_UnknownRetainN:
  case RT_BridgeRetainN:
  case RT_ReleaseN:
  case RT_UnknownReleaseN:
  case RT_BridgeReleaseN:
  case RT_FixLifetime:
  case RT_Retain:
  case RT_UnknownRetain:
  case RT_Release:
  case RT_UnknownRelease:
  case RT_Unknown:
  case RT_AllocObject:
  case RT_NoMemoryAccessed:
  case RT_BridgeRelease:
  case RT_BridgeRetain:
  case RT_RetainUnowned:
  case RT_CheckUnowned:
  case RT_ObjCRelease:
  case RT_EndBorrow:
    break;
  // ObjC forwards references.
  case RT_ObjCRetain:
    Val = cast<CallInst>(Inst)->getArgOperand(0);
    break;
  }
  return Val;
}

llvm::Value *
SwiftRCIdentity::getSwiftRCIdentityRoot(llvm::Value *Val) {
  // Only allow this method to go up a fixed number of levels to make sure
  // we don't explode compile time.
  llvm::Value *OldVal = Val;
  unsigned levels = 0;
  do {
    llvm::Value *NewVal = Val;
    // Try to strip off pointer casts and reference forwarding.
    Val = stripPointerCasts(Val);
    Val = stripReferenceForwarding(Val);
    // Nothing was stripped off.
    if (NewVal == Val)
      break;
    // Hit max number of levels.
    if (++levels > MaxRecursionDepth)
      return OldVal;
  } while (true);
  return Val;
}

llvm::ImmutablePass *swift::createSwiftRCIdentityPass() {
  initializeSwiftRCIdentityPass(*PassRegistry::getPassRegistry());
  return new SwiftRCIdentity();
}
