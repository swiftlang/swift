//===--- OptimizeARC.cpp - Reference Counting Optimizations ---------------===//
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
//
// This file implements optimizations for reference counting, object
// allocation, and other runtime entrypoints.
//
//===----------------------------------------------------------------------===//

#include "IRGen.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Pass.h"
using namespace llvm;

namespace {
  class SwiftARCOpt : public FunctionPass {
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
    }
    virtual bool runOnFunction(Function &F);
    
  public:
    static char ID;
    SwiftARCOpt() : FunctionPass(ID) {
      initializeObjCARCExpandPass(*PassRegistry::getPassRegistry());
    }
  };
}
namespace llvm {
  void initializeSwiftARCOptPass(PassRegistry&);
}

char SwiftARCOpt::ID = 0;
INITIALIZE_PASS(SwiftARCOpt,
                "swift-arc-optimize", "Swift ARC optimization", false, false)

// Optimization passes.
llvm::FunctionPass *swift::irgen::createSwiftARCOptPass() {
  return new SwiftARCOpt();
}


bool SwiftARCOpt::runOnFunction(Function &F) {
  bool Changed = false;
  for (inst_iterator I = inst_begin(&F), E = inst_end(&F); I != E; ++I) {
    Instruction *Inst = &*I;
    (void)Inst);
  }
  
  return Changed;
}
