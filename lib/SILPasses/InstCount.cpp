//===-- InstCount.cpp - Collects the count of all instructions ------------===//
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
// This pass collects the count of all instructions and reports them
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-instcount"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                 Statistics
//===----------------------------------------------------------------------===//

STATISTIC(TotalInsts , "Number of instructions (of all types)");
STATISTIC(TotalBlocks, "Number of basic blocks");
STATISTIC(TotalFuncs , "Number of non-external functions");

#define INST(Id, Parent, MemBehavior) \
  STATISTIC(Num ## Id, "Number of " #Id);
#include "swift/SIL/SILNodes.def"

namespace {

class InstCountVisitor : public SILVisitor<InstCountVisitor> {
public:

  void visitSILBasicBlock(SILBasicBlock *BB) {
    ++TotalBlocks;
    SILVisitor<InstCountVisitor>::visitSILBasicBlock(BB);
  }

  void visitSILFunction(SILFunction *F) {
    ++TotalFuncs;
    SILVisitor<InstCountVisitor>::visitSILFunction(F);
  }

  void visitValueBase(ValueBase *V) { }

#define INST(Id, Parent, MemBehavior)                           \
  void visit##Id(Id *I) {                                       \
    ++Num ## Id;                                                \
    ++TotalInsts;                                               \
  }
#include "swift/SIL/SILNodes.def"

};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

class SILInstCount : public SILFunctionTransform {

  InstCountVisitor V;

  /// The entry point to the transformation.
  void run() {
    V.visitSILFunction(getFunction());
  }
};

SILTransform *swift::createSILInstCount() {
  return new SILInstCount();
}

