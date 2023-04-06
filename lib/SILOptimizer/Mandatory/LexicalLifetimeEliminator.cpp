//===--- LexicalLifetimeEliminator.cpp ------------------------------------===//
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

#define DEBUG_TYPE "sil-lexical-lifetime-eliminator"

#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

namespace {

class LexicalLifetimeEliminatorPass : public SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();

    if (fn->forceEnableLexicalLifetimes())
      return;

    // If we are already canonical, we do not have any diagnostics to emit.
    if (fn->wasDeserializedCanonical())
      return;

    // If we have experimental late lexical lifetimes enabled, we do not want to
    // run this pass since we want lexical lifetimes to exist later in the
    // pipeline.
    if (fn->getModule().getOptions().LexicalLifetimes ==
        LexicalLifetimesOption::On)
      return;

    bool madeChange = false;
    for (auto &block : *fn) {
      for (auto &inst : block) {
        if (auto *bbi = dyn_cast<BeginBorrowInst>(&inst)) {
          if (bbi->isLexical()) {
            bbi->removeIsLexical();
            madeChange = true;
          }
          continue;
        }
        if (auto *mvi = dyn_cast<MoveValueInst>(&inst)) {
          if (mvi->isLexical()) {
            mvi->removeIsLexical();
            madeChange = true;
          }
          continue;
        }
        if (auto *asi = dyn_cast<AllocStackInst>(&inst)) {
          if (asi->isLexical()) {
            asi->removeIsLexical();
            madeChange = true;
          }
          continue;
        }
      }
    }

    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // anonymous namespace

SILTransform *swift::createLexicalLifetimeEliminator() {
  return new LexicalLifetimeEliminatorPass();
}
