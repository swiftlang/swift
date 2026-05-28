//===--- WasmAsyncT2TLowering.cpp - Rewrite async t2t to partial_apply ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//
//
// Workaround for swiftlang/swift#89320: on wasm32/wasm64 the WebAssembly
// backend's swiftcc parameter padding pass inserts swiftself/swifterror
// at the trailing position to make caller/callee wasm signatures match
// for `call_indirect`. Async typed-throws closures emit a trailing
// `ind_error_ptr` regular parameter AFTER the swiftself slot, so when a
// Thick caller (with swiftself in the middle) calls a Thin callee
// (without swiftself), the wasm padding produces positional misalignment:
// the callee reads `ind_error_ptr` from the slot the caller passes
// swiftself in, and reads swiftself from the slot the caller passes
// `ind_error_ptr` in. The typed error value ends up written to a null
// context pointer; the caller reads zeros from the real error slot.
//
// This pass rewrites the affected `thin_to_thick_function` instructions
// into zero-capture `partial_apply [callee_guaranteed] [on_stack]`. The
// partial_apply path uses an explicit context allocation that produces
// LLVM IR with matching positional layout on both caller and callee,
// avoiding the wasm backend padding mismatch.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "wasm-async-t2t-lowering"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Returns true if `t2t` matches the issue#89320 bug shape and can be
/// safely rewritten to a zero-capture `partial_apply [on_stack]`.
static bool shouldRewriteT2T(ThinToThickFunctionInst *t2t) {
  auto operandType = t2t->getOperand()->getType().castTo<SILFunctionType>();
  auto resultType = t2t->getType().castTo<SILFunctionType>();

  if (!operandType->isAsync())
    return false;
  if (!operandType->hasIndirectErrorResult())
    return false;

  // Restrict to non-polymorphic shapes for the same reason the IRGen
  // wrapper did: arg-walking gets complex with polymorphic metadata.
  if (operandType->isPolymorphic() || resultType->isPolymorphic())
    return false;

  // Don't rewrite if the operand is not a function_ref or one of its
  // subclasses; the operand-kind expansion is orthogonal to this pass.
  if (!isa<FunctionRefBaseInst>(t2t->getOperand()))
    return false;

  return true;
}

/// Rewrite a single `thin_to_thick_function` to a zero-capture
/// `partial_apply [callee_guaranteed] [on_stack]`, then insert
/// `dealloc_stack` after each terminating use.
///
/// Returns true if the rewrite succeeded.
static bool rewriteToPartialApply(ThinToThickFunctionInst *t2t) {
  // We only handle t2t whose result is consumed in a way we can
  // unambiguously bracket with a single dealloc_stack. Specifically,
  // every use must be in the same basic block (or strictly dominated
  // by the t2t). For the issue#89320 repro shapes, SILGen always
  // produces a t2t immediately followed by its consuming apply in
  // the same BB, so this constraint covers the bug.
  SILBasicBlock *parentBB = t2t->getParent();
  SILInstruction *lastUse = t2t;
  for (auto *use : t2t->getUses()) {
    if (use->getUser()->getParent() != parentBB)
      return false;
    if (lastUse == t2t ||
        std::distance(lastUse->getIterator(), use->getUser()->getIterator()) > 0)
      lastUse = use->getUser();
  }

  SILBuilderWithScope builder(t2t);

  auto *pa = builder.createPartialApply(
      t2t->getLoc(),
      t2t->getOperand(),
      /*Subs=*/SubstitutionMap(),
      /*Args=*/{},
      ParameterConvention::Direct_Guaranteed,
      SILFunctionTypeIsolation::forUnknown(),
      PartialApplyInst::OnStackKind::OnStack);

  t2t->replaceAllUsesWith(pa);
  t2t->eraseFromParent();

  SILBuilderWithScope deallocBuilder(std::next(lastUse->getIterator()));
  deallocBuilder.createDeallocStack(lastUse->getLoc(), pa);

  return true;
}

class WasmAsyncT2TLoweringPass : public SILFunctionTransform {
  void run() override {
    SILFunction *F = getFunction();
    if (!F->getModule().getASTContext().LangOpts.Target.isWasm())
      return;

    llvm::SmallVector<ThinToThickFunctionInst *, 4> toRewrite;
    for (auto &BB : *F) {
      for (auto &I : BB) {
        if (auto *t2t = dyn_cast<ThinToThickFunctionInst>(&I)) {
          if (shouldRewriteT2T(t2t))
            toRewrite.push_back(t2t);
        }
      }
    }

    bool changed = false;
    for (auto *t2t : toRewrite) {
      if (rewriteToPartialApply(t2t))
        changed = true;
    }

    if (changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

} // end anonymous namespace

SILTransform *swift::createWasmAsyncT2TLowering() {
  return new WasmAsyncT2TLoweringPass();
}
