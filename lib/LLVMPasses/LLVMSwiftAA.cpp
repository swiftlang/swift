//===--- LLVMSwiftAA.cpp - LLVM Alias Analysis for Swift ------------------===//
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
#include "swift/Demangling/Demangle.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/LegacyPassManager.h" 
#include "llvm/IR/Module.h"

using namespace llvm;
using namespace swift;

//===----------------------------------------------------------------------===//
//                           Alias Analysis Result
//===----------------------------------------------------------------------===//

static ModRefInfo getConservativeModRefForKind(const llvm::Instruction &I) {
  switch (classifyInstruction(I)) {
#define KIND(Name, MemBehavior) case RT_ ## Name: return ModRefInfo:: MemBehavior;
#include "LLVMSwift.def"
  }

  llvm_unreachable("Not a valid Instruction.");
}

/// Returns true if \p Loc is a non-constant "direct field offset" global of a
/// class with a runtime-computed layout. Such a global is zero until the class
/// metadata is initialized.
static bool isNonConstantFieldOffsetGlobal(const llvm::MemoryLocation &Loc) {
  auto *global = dyn_cast<GlobalVariable>(getUnderlyingObject(Loc.Ptr));
  if (!global || global->isConstant())
    return false;
  StringRef name = global->getName();
  return name.ends_with("Wvd") && Demangle::isSwiftSymbol(name);
}

ModRefInfo SwiftAAResult::getModRefInfo(const llvm::CallBase *Call,
                                        const llvm::MemoryLocation &Loc,
                                        llvm::AAQueryInfo &AAQI) {
  // A field offset global of a class with a runtime-computed layout is only
  // valid once the class metadata is initialized, which is guaranteed if an
  // instance of the class exists. The metadata accessor call is "readnone",
  // therefore treat the allocation as a write of the field offset global.
  // Otherwise a load of the field offset global could be moved above the
  // allocation (and the metadata accessor call), e.g. by GVN's load PRE.
  if (classifyInstruction(*Call) == RT_AllocObject &&
      isNonConstantFieldOffsetGlobal(Loc)) {
    return AAResultBase::getModRefInfo(Call, Loc, AAQI);
  }

  // We know at compile time that certain entry points do not modify any
  // compiler-visible state ever. Quickly check if we have one of those
  // instructions and return if so.
  if (ModRefInfo::NoModRef == getConservativeModRefForKind(*Call))
    return ModRefInfo::NoModRef;

  // Otherwise, delegate to the rest of the AA ModRefInfo machinery.
  return AAResultBase::getModRefInfo(Call, Loc, AAQI);
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

AnalysisKey SwiftAA::Key;

SwiftAAResult SwiftAA::run(llvm::Function &F,
                           llvm::FunctionAnalysisManager &AM) {
  return SwiftAAResult();
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

llvm::ImmutablePass *swift::createSwiftAAWrapperPass() {
  return new SwiftAAWrapperPass();
}
