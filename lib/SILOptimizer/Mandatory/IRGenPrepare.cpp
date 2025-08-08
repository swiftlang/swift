//===--- IRGenPrepare.cpp -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// Cleanup SIL to make it suitable for IRGen.
///
/// We perform the following canonicalizations:
///
/// 1. We remove calls to Builtin.poundAssert() and Builtin.staticReport(),
///    which are not needed post SIL.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-cleanup"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "swift/Strings.h"

using namespace swift;

// Print the message string of encountered `cond_fail` instructions the first
// time the message string is encountered.
static llvm::cl::opt<bool> PrintCondFailMessages(
  "print-cond-fail-messages", llvm::cl::init(false),
  llvm::cl::desc("print cond_fail messages"));
static llvm::cl::opt<bool> IncludeCondFailMessagesFunction(
  "print-cond-fail-messages-include-function-name", llvm::cl::init(false),
  llvm::cl::desc("when printing cond_fail messages include"
                 "the current SIL function name"));

static llvm::DenseSet<StringRef> CondFailMessages;

static bool cleanFunction(SILFunction &fn) {
  bool madeChange = false;

  for (auto &bb : fn) {
    for (auto i = bb.begin(), e = bb.end(); i != e;) {
      // Make sure there is no iterator invalidation if the inspected
      // instruction gets removed from the block.
      SILInstruction *inst = &*i;
      ++i;

      // Print cond_fail messages the first time a specific cond_fail message
      // string is encountered.
      //   Run the swift-frontend in a mode that will generate LLVM IR adding
      //   the option `-print-cond-fail-messages` will dump all cond_fail
      //   message strings encountered in the SIL.
      //   ```
      //     % swift-frontend -Xllvm -print-cond-fail-messages -emit-ir/-c ...
      //     ...
      //     cond_fail message encountered: Range out of bounds
      //     cond_fail message encountered: Array index is out of range
      //     ...
      //   ```
      if (PrintCondFailMessages) {
        if (auto CFI = dyn_cast<CondFailInst>(inst)) {
          auto msg = CFI->getMessage().str();
          if (IncludeCondFailMessagesFunction) {
            msg.append(" in ");
            msg.append(fn.getName().str());
          }
          if (CondFailMessages.insert(msg).second)
            llvm::dbgs() << "cond_fail message encountered: " << msg << "\n";
        }
      }

      // Remove calls to Builtin.poundAssert() and Builtin.staticReport().
      auto *bi = dyn_cast<BuiltinInst>(inst);
      if (!bi) {
        continue;
      }

      switch (bi->getBuiltinInfo().ID) {
        case BuiltinValueKind::CondFailMessage: {
          SILBuilderWithScope Builder(bi);
          Builder.createCondFail(bi->getLoc(), bi->getOperand(0),
            "unknown program error");
          LLVM_FALLTHROUGH;
        }
        case BuiltinValueKind::PoundAssert:
        case BuiltinValueKind::StaticReport: {
          // The call to the builtin should get removed before we reach
          // IRGen.
          InstructionDeleter deleter;
          deleter.forceDelete(bi);
          // StaticReport only takes trivial operands, and therefore doesn't
          // require fixing the lifetime of its operands.
          deleter.cleanupDeadInstructions();
          madeChange = true;
          break;
        }
        default:
          break;
      }
    }
  }

  return madeChange;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class IRGenPrepare : public SILFunctionTransform {
  void run() override {
    SILFunction *F = getFunction();

    if (getOptions().EmbeddedSwift) {
      // In embedded swift all the code is generated in the top-level module.
      // Even de-serialized functions must be code-gen'd. With the new linkage
      // model, we'll use shared linkage for the deserialized functions, so
      // that they can be de-duplicated across modules.
      auto newLinkage =
          F->getASTContext().LangOpts.hasFeature(Feature::EmbeddedLinkageModel)
            ? SILLinkage::Shared
            : SILLinkage::Hidden;
      SILLinkage linkage = F->getLinkage();
      if (isAvailableExternally(linkage)) {
        F->setLinkage(newLinkage);
      }
    }

    bool shouldInvalidate = cleanFunction(*F);

    if (shouldInvalidate)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

} // end anonymous namespace


SILTransform *swift::createIRGenPrepare() {
  return new IRGenPrepare();
}
