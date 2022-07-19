//===--- DbgAddrBlockSplitter.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This simple pass runs late after all LLVM level optimization passes have
/// completed and break blocks at llvm.dbg.addr when we are compiling at
/// -Onone. This helps us avoid bad behavior in SelectionDAG where SelectionDAG
/// in certain cases sink llvm.dbg.addr to the end of blocks.
///
//===----------------------------------------------------------------------===//

#include "swift/LLVMPasses/Passes.h"
#include "swift/LLVMPasses/PassesFwd.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

namespace {

struct SwiftDbgAddrBlockSplitter : FunctionPass {
  static char ID;
  SwiftDbgAddrBlockSplitter() : FunctionPass(ID) {}

  bool runOnFunction(Function &fn) override;
};

} // namespace

static bool split(Function &fn) {
  SmallVector<Instruction *, 32> breakBlockPoints;

  // If we are in the first block,

  for (auto &block : fn) {
    for (auto &inst : block) {
      if (isa<DbgAddrIntrinsic>(&inst)) {
        breakBlockPoints.push_back(&*std::next(inst.getIterator()));
      }
    }
  }

  if (breakBlockPoints.empty())
    return false;

  bool madeChange = false;

  while (!breakBlockPoints.empty()) {
    auto *i = breakBlockPoints.pop_back_val();
    if (i->isTerminator())
      continue;
    SplitBlock(i->getParent(), i);
    madeChange = true;
  }

  return madeChange;
}

bool SwiftDbgAddrBlockSplitter::runOnFunction(Function &fn) {
  return split(fn);
}

char SwiftDbgAddrBlockSplitter::ID = 0;
INITIALIZE_PASS_BEGIN(SwiftDbgAddrBlockSplitter,
                      "swift-dbg-addr-block-splitter",
                      "Swift pass that splits blocks after llvm.dbg.addr",
                      false, false)
INITIALIZE_PASS_END(SwiftDbgAddrBlockSplitter, "swift-dbg-addr-block-splitter",
                    "Swift pass that splits blocks after llvm.dbg.addr", false,
                    false)

llvm::FunctionPass *swift::createSwiftDbgAddrBlockSplitter() {
  initializeSwiftDbgAddrBlockSplitterPass(
      *llvm::PassRegistry::getPassRegistry());
  return new SwiftDbgAddrBlockSplitter();
}

llvm::PreservedAnalyses
swift::SwiftDbgAddrBlockSplitterPass::run(llvm::Function &F,
                                          llvm::FunctionAnalysisManager &AM) {
  bool changed = split(F);
  if (!changed)
    return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}
