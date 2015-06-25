//===--- Passes.h - LLVM optimizer passes for Swift -------------*- C++ -*-===//
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

#ifndef SWIFT_LLVMPASSES_PASSES_H
#define SWIFT_LLVMPASSES_PASSES_H

#include "swift/LLVMPasses/PassesFwd.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Pass.h"

namespace llvm {
  void initializeSwiftAliasAnalysisPass(PassRegistry&);
} // end namespace llvm

namespace swift {
  class SwiftAliasAnalysis : public llvm::ImmutablePass, public llvm::AliasAnalysis {
  public:
    static char ID; // Class identification, replacement for typeinfo
    SwiftAliasAnalysis() : ImmutablePass(ID) {}
    
  private:
    bool doInitialization(llvm::Module &M) override;

    virtual void *getAdjustedAnalysisPointer(const void *PI) override {
      if (PI == &AliasAnalysis::ID)
        return static_cast<AliasAnalysis *>(this);
      return this;
    }
    
    void getAnalysisUsage(llvm::AnalysisUsage &AU) const override {
      AU.setPreservesAll();
      AliasAnalysis::getAnalysisUsage(AU);
    }
    
    virtual ModRefResult getModRefInfo(llvm::ImmutableCallSite CS,
                                       const llvm::MemoryLocation &Loc) override;
  };

  class SwiftARCOpt : public llvm::FunctionPass {
    virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
    virtual bool runOnFunction(llvm::Function &F) override;

  public:
    static char ID;
    SwiftARCOpt();
  };

  class SwiftARCExpandPass : public llvm::FunctionPass {
    virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override {
      AU.setPreservesCFG();
    }
    virtual bool runOnFunction(llvm::Function &F) override;

  public:
    static char ID;
    SwiftARCExpandPass() : llvm::FunctionPass(ID) {}
  };

} // end namespace swift

#endif
