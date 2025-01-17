//===--- DiagnosticDeadFunctionElimination.cpp ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Delete functions that early diagnostic specialization passes mark as being
/// able to be DCE-ed if there are no further uses. This prevents later
/// diagnostic passes from emitting diagnostics both on the original function
/// and the diagnostic function.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-diagnostic-dead-function-eliminator"

#include "swift/AST/SemanticAttrs.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                         MARK: Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

struct DiagnosticDeadFunctionEliminator : SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();

    // If an earlier pass asked us to eliminate the function body if it's
    // unused, and the function is in fact unused, do that now.
    if (!fn->hasSemanticsAttr(semantics::DELETE_IF_UNUSED) ||
        fn->getRefCount() != 0 ||
        isPossiblyUsedExternally(fn->getLinkage(),
                                 fn->getModule().isWholeModule())) {
      return;
    }

    LLVM_DEBUG(llvm::dbgs()
               << "===> Stubbifying unused function " << fn->getName()
               << "'s body that was marked for deletion\n");
    // Remove all non-entry blocks.
    auto entryBB = fn->begin();
    auto nextBB = std::next(entryBB);

    while (nextBB != fn->end()) {
      auto thisBB = nextBB;
      ++nextBB;
      thisBB->eraseFromParent();
    }

    // Rewrite the entry block to only contain an unreachable.
    auto loc = entryBB->begin()->getLoc();
    entryBB->eraseAllInstructions(fn->getModule());
    {
      SILBuilder b(&*entryBB);
      b.createUnreachable(loc);
    }

    // If the function has shared linkage, reduce this version to private
    // linkage, because we don't want the deleted-body form to win in any
    // ODR shootouts.
    if (fn->getLinkage() == SILLinkage::Shared) {
      fn->setLinkage(SILLinkage::Private);
      fn->setSerializedKind(IsNotSerialized);
    }

    invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
  }
};

} // namespace

SILTransform *swift::createDiagnosticDeadFunctionElimination() {
  return new DiagnosticDeadFunctionEliminator();
}
