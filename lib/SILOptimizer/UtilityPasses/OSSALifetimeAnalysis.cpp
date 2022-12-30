//===--- OSSALifetimeAnalysis.cpp - OSSA lifetime analysis ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This currently runs SSAPrunedLiveness on SSA values, MultiDefPrunedLiveness
/// on phi webs, and MultiDefPrunedLiveness on 'copy_addr [init]' and 'store
/// [init]'.
///
/// Soon, it will also run OSSALiveness on OSSA values.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ossa-lifetime-analysis"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/ScopedAddressUtils.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

struct OSSALifetimeAnalyzer {
  SILFunction *function;

  OSSALifetimeAnalyzer(SILFunction *function): function(function) {}

  void analyzeSSAValue(SILValue value);

  void analyzeScopedAddress(ScopedAddressValue scopedAddress);

  void analyzeValues(ArrayRef<SILValue> values);
};

void OSSALifetimeAnalyzer::analyzeSSAValue(SILValue value) {
  llvm::outs() << "\nSSA lifetime analysis: " << value;
  SmallVector<SILBasicBlock *, 8> discoveredBlocks;
  SSAPrunedLiveness liveness(&discoveredBlocks);
  liveness.initializeDef(value);
  liveness.computeSimple();
  liveness.print(llvm::outs());

  PrunedLivenessBoundary boundary;
  liveness.computeBoundary(boundary);
  boundary.print(llvm::outs());
}

void OSSALifetimeAnalyzer::analyzeScopedAddress(
    ScopedAddressValue scopedAddress) {
  llvm::outs() << "\nScoped address analysis: " << scopedAddress.value;
  SmallVector<SILBasicBlock *, 8> discoveredBlocks;
  SSAPrunedLiveness liveness(&discoveredBlocks);
  scopedAddress.computeTransitiveLiveness(liveness);
  liveness.print(llvm::outs());

  PrunedLivenessBoundary boundary;
  liveness.computeBoundary(boundary);
  boundary.print(llvm::outs());
}

void OSSALifetimeAnalyzer::analyzeValues(ArrayRef<SILValue> values) {
  llvm::outs() << "\nMultiDef lifetime analysis:\n";
  SmallVector<SILBasicBlock *, 8> discoveredBlocks;
  MultiDefPrunedLiveness liveness(function, &discoveredBlocks);
  for (SILValue value : values) {
    llvm::outs() << "  def: " << value;
    liveness.initializeDef(value);
  }
  liveness.computeSimple();
  liveness.print(llvm::outs());

  PrunedLivenessBoundary boundary;
  liveness.computeBoundary(boundary);
  boundary.print(llvm::outs());  
}

class OSSALifetimeAnalysis : public SILModuleTransform {
  void run() override;
};

// Find and record all debug_value [trace] instructions. Strip them before
// querying liveness so we can test dead values. Alternatively, we could
// override SSAPrunedLiveness::compute above and simply ignore trace
// instructions.
//
// Consider each traced value first as an indivual live range, then consider
// them all together as a single live range.
void OSSALifetimeAnalysis::run() {
  for (auto &function : *getModule()) {
    llvm::outs() << "\n@" << function.getName() << "\n";
    if (function.empty()) {
      llvm::outs() << "<unknown>\n";
      continue;
    }
    SmallVector<SILValue, 8> traceValues;
    InstructionDeleter deleter;
    for (auto &block : function) {
      for (SILInstruction &inst : block.deletableInstructions()) {
        if (auto *debugValue = dyn_cast<DebugValueInst>(&inst)) {
          if (!debugValue->hasTrace())
            continue;
          traceValues.push_back(debugValue->getOperand());
          deleter.forceDelete(debugValue);
        }
      }
    }
    OSSALifetimeAnalyzer analyzer(&function);
    for (auto value : traceValues) {
      if (value->getType().isAddress()) {
        ScopedAddressValue scopedAddress(value);
        if (scopedAddress) {
          analyzer.analyzeScopedAddress(scopedAddress);
          continue;
        }
      }
      analyzer.analyzeSSAValue(value);
    }
    if (traceValues.size() > 1) {
      analyzer.analyzeValues(traceValues);
    }
  }
}

} // end anonymous namespace

SILTransform *swift::createOSSALifetimeAnalysis() {
  return new OSSALifetimeAnalysis();
}
