//===--- Passes.h - LLVM optimizer passes for Swift -------------*- C++ -*-===//
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

#ifndef SWIFT_LLVMPASSES_PASSES_H
#define SWIFT_LLVMPASSES_PASSES_H

#include "swift/LLVMPasses/PassesFwd.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"

namespace swift {

  struct SwiftAAResult : llvm::AAResultBase {
    friend llvm::AAResultBase;

    explicit SwiftAAResult() : AAResultBase() {}
    SwiftAAResult(SwiftAAResult &&Arg)
        : AAResultBase(std::move(Arg)) {}

    bool invalidate(llvm::Function &,
                    const llvm::PreservedAnalyses &) { return false; }

    bool invalidate(llvm::Function &, const llvm::PreservedAnalyses &,
                    llvm::FunctionAnalysisManager::Invalidator &) {
      return false;
    }

    using AAResultBase::getModRefInfo;
    llvm::ModRefInfo getModRefInfo(const llvm::CallBase *Call,
                                   const llvm::MemoryLocation &Loc) {
// FIXME: how to construct a SimpleAAQueryInfo without an AAResults?
//      llvm::SimpleAAQueryInfo AAQI;
//      return getModRefInfo(Call, Loc, AAQI);
      return llvm::ModRefInfo::ModRef;
   }
    llvm::ModRefInfo getModRefInfo(const llvm::CallBase *Call,
                                   const llvm::MemoryLocation &Loc,
                                   llvm::AAQueryInfo &AAQI);
  };

  class SwiftAAWrapperPass : public llvm::ImmutablePass {
    std::unique_ptr<SwiftAAResult> Result;

  public:
    static char ID; // Class identification, replacement for typeinfo
    SwiftAAWrapperPass();

    SwiftAAResult &getResult() { return *Result; }
    const SwiftAAResult &getResult() const { return *Result; }

    bool doInitialization(llvm::Module &M) override;
    bool doFinalization(llvm::Module &M) override;
    void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  };

  class SwiftAA : public llvm::AnalysisInfoMixin<SwiftAA> {
    friend llvm::AnalysisInfoMixin<SwiftAA>;

    static llvm::AnalysisKey Key;

  public:
    using Result = SwiftAAResult;

    SwiftAAResult run(llvm::Function &F, llvm::FunctionAnalysisManager &AM);
  };

  class SwiftRCIdentity {
  public:
    SwiftRCIdentity() {}

    /// Returns the root of the RC-equivalent value for the given V.
    llvm::Value *getSwiftRCIdentityRoot(llvm::Value *V);

  private:
    enum { MaxRecursionDepth = 16 };

    llvm::Value *stripPointerCasts(llvm::Value *Val);
    llvm::Value *stripReferenceForwarding(llvm::Value *Val);
  };

  class SwiftARCOpt : public llvm::FunctionPass {
    /// Swift RC Identity analysis.
    SwiftRCIdentity RC;
    virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
    virtual bool runOnFunction(llvm::Function &F) override;
  public:
    static char ID;
    SwiftARCOpt();
  };

  struct SwiftARCOptPass : public llvm::PassInfoMixin<SwiftARCOptPass> {
    SwiftRCIdentity RC;

    llvm::PreservedAnalyses run(llvm::Function &F,
                                llvm::FunctionAnalysisManager &AM);
  };

  class SwiftARCContract : public llvm::FunctionPass {
    /// Swift RC Identity analysis.
    SwiftRCIdentity RC;
    virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
    virtual bool runOnFunction(llvm::Function &F) override;
  public:
    static char ID;
    SwiftARCContract() : llvm::FunctionPass(ID) {}
  };

  struct SwiftARCContractPass
      : public llvm::PassInfoMixin<SwiftARCContractPass> {
    SwiftRCIdentity RC;

    llvm::PreservedAnalyses run(llvm::Function &F,
                                llvm::FunctionAnalysisManager &AM);
  };

  class InlineTreePrinter : public llvm::ModulePass {
    virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
    virtual bool runOnModule(llvm::Module &M) override;
  public:
    static char ID;
    InlineTreePrinter() : llvm::ModulePass(ID) {}
  };

  class SwiftMergeFunctionsPass
      : public llvm::PassInfoMixin<SwiftMergeFunctionsPass> {
    bool ptrAuthEnabled = false;
    unsigned ptrAuthKey = 0;

  public:
    SwiftMergeFunctionsPass(bool ptrAuthEnabled, unsigned ptrAuthKey)
        : ptrAuthEnabled(ptrAuthEnabled), ptrAuthKey(ptrAuthKey) {}
    llvm::PreservedAnalyses run(llvm::Module &M,
                                llvm::ModuleAnalysisManager &AM);
  };

  struct InlineTreePrinterPass
      : public llvm::PassInfoMixin<InlineTreePrinterPass> {
    llvm::PreservedAnalyses run(llvm::Module &M,
                                llvm::ModuleAnalysisManager &AM);
  };

  struct AsyncEntryReturnMetadataPass
      : public llvm::PassInfoMixin<AsyncEntryReturnMetadataPass> {
    llvm::PreservedAnalyses run(llvm::Module &M,
                                llvm::ModuleAnalysisManager &AM);
  };
} // end namespace swift

#endif
