//===--- Link.cpp - Link in transparent SILFunctions from module ----------===//
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

#define DEBUG_TYPE "link"
#include "swift/SILPasses/Passes.h"
#include "swift/AST/Module.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/SIL/SILExternalSource.h"
#include "swift/SIL/SILModule.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

// To help testing serialization, deserialization, we turn on sil-link-all.
static llvm::cl::opt<bool>
EnableLinkAll("sil-link-all", llvm::cl::Hidden, llvm::cl::init(false));
static llvm::cl::opt<bool>
EnableLinking("enable-sil-linking", llvm::cl::Hidden, llvm::cl::init(true));

STATISTIC(NumFuncLinked, "Number of SIL functions linked");

namespace {
  class Callback : public SerializedSILLoader::Callback {
    void didDeserialize(Module *M, SILFunction *fn) override {
      updateLinkage(fn);
    }

    void didDeserialize(Module *M, SILGlobalVariable *var) override {
      updateLinkage(var);
    }

    void didDeserialize(Module *M, SILVTable *vtable) override {
      // TODO: should vtables get linkage?
      //updateLinkage(vtable);
    }

    template <class T> void updateLinkage(T *decl) {
      switch (decl->getLinkage()) {
      case SILLinkage::Public:
        decl->setLinkage(SILLinkage::PublicExternal);
        return;
      case SILLinkage::Hidden:
        decl->setLinkage(SILLinkage::HiddenExternal);
        return;
      case SILLinkage::Shared:
      case SILLinkage::Private: // ?
      case SILLinkage::PublicExternal:
      case SILLinkage::HiddenExternal:
        return;
      }
    }
  };
}

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILLinking(SILModule *M) {
  if (!EnableLinking && !EnableLinkAll)
    return;

  Callback callback;
  SerializedSILLoader *SILLoader = SerializedSILLoader::create(
                                     M->getASTContext(), M, &callback);

  SILExternalSource *ExternalSource = M->getExternalSource();

  SmallVector<SILFunction*, 128> Worklist;
  for (auto &Fn : *M)
    Worklist.push_back(&Fn);

  while (!Worklist.empty()) {
    auto Fn = Worklist.pop_back_val();

    for (auto &BB : *Fn) {
      for (auto I = BB.begin(), E = BB.end(); I != E; I++) {
        SILFunction *CalleeFunction = nullptr;
        bool TryLinking = false;
        if (ApplyInst *AI = dyn_cast<ApplyInst>(I)) {
          SILValue Callee = AI->getCallee();
          // Handles FunctionRefInst only.
          if (FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(Callee.getDef()))
            CalleeFunction = FRI->getReferencedFunction();

          // When EnableLinkAll is true, we always link the Callee.
          TryLinking = EnableLinkAll ? true : AI->isTransparent();
        }
        else if (PartialApplyInst *PAI = dyn_cast<PartialApplyInst>(I)) {
          SILValue Callee = PAI->getCallee();
          // Handles FunctionRefInst only.
          if (FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(Callee.getDef())) {
            CalleeFunction = FRI->getReferencedFunction();
            // When EnableLinkAll is true, we always link the Callee.
            TryLinking = EnableLinkAll ? true : CalleeFunction->isTransparent();
          } else {
            continue;
          }
        }
        else if (FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(I)) {
          // When EnableLinkAll is true, we link the function referenced by
          // FunctionRefInst.
          CalleeFunction = EnableLinkAll ? FRI->getReferencedFunction() :
                                           nullptr;
          TryLinking = EnableLinkAll;
        }

        if (!CalleeFunction)
          continue;
        
        // The ExternalSource may wish to rewrite non-empty bodies.
        if (ExternalSource) {
          if (auto NewFn = ExternalSource->lookupSILFunction(CalleeFunction)) {
            Worklist.push_back(NewFn);
            ++NumFuncLinked;
            continue;
          }
        }

        CalleeFunction->setBare(IsBare);

        if (CalleeFunction->empty()) {
          // Try to find the definition in a serialized module when callee is
          // currently empty.
          if (TryLinking) {
            if (auto NewFn = SILLoader->lookupSILFunction(CalleeFunction)) {
              Worklist.push_back(NewFn);
              ++NumFuncLinked;
              continue;
            }
          }
        }
      }
    }
  }

  if (EnableLinkAll)
    SILLoader->getAllVTables();
}
