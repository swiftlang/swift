//===--- OptRemarkGenerator.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-opt-remark-gen"

#include "swift/SIL/OptimizationRemark.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                        Opt Remark Generator Visitor
//===----------------------------------------------------------------------===//

namespace {

struct OptRemarkGeneratorInstructionVisitor
    : public SILInstructionVisitor<OptRemarkGeneratorInstructionVisitor> {
  SILModule &mod;
  OptRemark::Emitter ORE;

  OptRemarkGeneratorInstructionVisitor(SILModule &mod)
      : mod(mod), ORE(DEBUG_TYPE, mod) {}

  void visitStrongRetainInst(StrongRetainInst *sri);
  void visitStrongReleaseInst(StrongReleaseInst *sri);
  void visitRetainValueInst(RetainValueInst *rvi);
  void visitReleaseValueInst(ReleaseValueInst *rvi);
  void visitSILInstruction(SILInstruction *) {}
};

} // anonymous namespace

void OptRemarkGeneratorInstructionVisitor::visitStrongRetainInst(
    StrongRetainInst *sri) {
  ORE.emit([&]() {
    using namespace OptRemark;
    return RemarkMissed("memory-management", *sri)
           << "Unable to remove ARC operation";
  });
}

void OptRemarkGeneratorInstructionVisitor::visitStrongReleaseInst(
    StrongReleaseInst *sri) {
  ORE.emit([&]() {
    using namespace OptRemark;
    return RemarkMissed("memory-management", *sri)
           << "Unable to remove ARC operation";
  });
}

void OptRemarkGeneratorInstructionVisitor::visitRetainValueInst(
    RetainValueInst *rvi) {
  ORE.emit([&]() {
    using namespace OptRemark;
    return RemarkMissed("memory-management", *rvi)
           << "Unable to remove ARC operation";
  });
}

void OptRemarkGeneratorInstructionVisitor::visitReleaseValueInst(
    ReleaseValueInst *rvi) {
  ORE.emit([&]() {
    using namespace OptRemark;
    return RemarkMissed("memory-management", *rvi)
           << "Unable to remove ARC operation";
  });
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class OptRemarkGenerator : public SILFunctionTransform {
  ~OptRemarkGenerator() override {}

  /// The entry point to the transformation.
  void run() override {
    OptRemarkGeneratorInstructionVisitor visitor(getFunction()->getModule());
    for (auto &block : *getFunction()) {
      for (auto &inst : block) {
        visitor.visit(&inst);
      }
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createOptRemarkGenerator() {
  return new OptRemarkGenerator();
}
