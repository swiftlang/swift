//===--- PartialApplyCombiner.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"

using namespace swift;

namespace {

// Helper class performing the apply{partial_apply(x,y)}(z) -> apply(z,x,y)
// peephole.
class PartialApplyCombiner {
  // True if temporaries are not created yet.
  bool isFirstTime = true;

  // partial_apply which is being processed.
  PartialApplyInst *pai;

  // Temporaries created as copies of alloc_stack arguments of
  // the partial_apply.
  SmallVector<SILValue, 8> tmpCopies;

  // Mapping from the original argument of partial_apply to
  // the temporary containing its copy.
  llvm::DenseMap<SILValue, SILValue> argToTmpCopy;

  // Set of lifetime endpoints for this partial_apply.
  //
  // Used to find the last uses of partial_apply, which is need to insert
  // releases/destroys of temporaries as early as possible.
  ValueLifetimeAnalysis::Frontier partialApplyFrontier;

  SILBuilder &builder;

  InstModCallbacks &callbacks;

  bool processSingleApply(FullApplySite ai);
  bool allocateTemporaries();
  void deallocateTemporaries();
  void destroyTemporaries();

public:
  PartialApplyCombiner(PartialApplyInst *pai, SILBuilder &builder,
                       InstModCallbacks &callbacks)
      : isFirstTime(true), pai(pai), builder(builder), callbacks(callbacks) {}
  SILInstruction *combine();
};

} // end anonymous namespace

/// Returns true on success.
bool PartialApplyCombiner::allocateTemporaries() {
  // A partial_apply [stack]'s argument are not owned by the partial_apply and
  // therefore their lifetime must outlive any uses.
  if (pai->isOnStack()) {
    return true;
  }

  // Copy the original arguments of the partial_apply into newly created
  // temporaries and use these temporaries instead of the original arguments
  // afterwards.
  //
  // This is done to "extend" the life-time of original partial_apply arguments,
  // as they may be destroyed/deallocated before the last use by one of the
  // apply instructions.
  //
  // TODO: Copy arguments of the partial_apply into new temporaries only if the
  // lifetime of arguments ends before their uses by apply instructions.
  bool needsDestroys = false;
  CanSILFunctionType paiTy =
      pai->getCallee()->getType().getAs<SILFunctionType>();

  // Emit a destroy value for each captured closure argument.
  ArrayRef<SILParameterInfo> paramList = paiTy->getParameters();
  auto argList = pai->getArguments();
  paramList = paramList.drop_front(paramList.size() - argList.size());

  llvm::SmallVector<std::pair<SILValue, uint16_t>, 8> argsToHandle;
  for (unsigned i : indices(argList)) {
    SILValue arg = argList[i];
    SILParameterInfo param = paramList[i];
    if (param.isIndirectMutating())
      continue;

    // Create a temporary and copy the argument into it, if:
    // - the argument stems from an alloc_stack
    // - the argument is consumed by the callee and is indirect
    //   (e.g. it is an @in argument)
    if (isa<AllocStackInst>(arg) ||
        (param.isConsumed() &&
         pai->getSubstCalleeConv().isSILIndirect(param))) {
      // If the argument has a dependent type, then we can not create a
      // temporary for it at the beginning of the function, so we must bail.
      //
      // TODO: This is because we are inserting alloc_stack at the beginning/end
      // of functions where the dependent type may not exist yet.
      if (arg->getType().hasOpenedExistential())
        return false;

      // If the temporary is non-trivial, we need to destroy it later.
      if (!arg->getType().isTrivial(*pai->getFunction()))
        needsDestroys = true;
      argsToHandle.push_back(std::make_pair(arg, i));
    }
  }

  if (needsDestroys) {
    // Compute the set of endpoints, which will be used to insert destroys of
    // temporaries. This may fail if the frontier is located on a critical edge
    // which we may not split (no CFG changes in SILCombine).
    ValueLifetimeAnalysis vla(pai);
    if (!vla.computeFrontier(partialApplyFrontier,
                             ValueLifetimeAnalysis::DontModifyCFG))
      return false;
  }

  for (auto argWithIdx : argsToHandle) {
    SILValue Arg = argWithIdx.first;
    builder.setInsertionPoint(pai->getFunction()->begin()->begin());
    // Create a new temporary at the beginning of a function.
    SILDebugVariable dbgVar(/*Constant*/ true, argWithIdx.second);
    auto *tmp = builder.createAllocStack(pai->getLoc(), Arg->getType(), dbgVar);
    builder.setInsertionPoint(pai);
    // Copy argument into this temporary.
    builder.createCopyAddr(pai->getLoc(), Arg, tmp, IsTake_t::IsNotTake,
                           IsInitialization_t::IsInitialization);

    tmpCopies.push_back(tmp);
    argToTmpCopy.insert(std::make_pair(Arg, tmp));
  }
  return true;
}

/// Emit dealloc_stack for all temporaries.
void PartialApplyCombiner::deallocateTemporaries() {
  // Insert dealloc_stack instructions at all function exit points.
  for (SILBasicBlock &block : *pai->getFunction()) {
    TermInst *term = block.getTerminator();
    if (!term->isFunctionExiting())
      continue;

    for (auto copy : tmpCopies) {
      builder.setInsertionPoint(term);
      builder.createDeallocStack(pai->getLoc(), copy);
    }
  }
}

/// Emit code to release/destroy temporaries.
void PartialApplyCombiner::destroyTemporaries() {
  // Insert releases and destroy_addrs as early as possible,
  // because we don't want to keep objects alive longer than
  // its really needed.
  for (auto op : tmpCopies) {
    auto tmpType = op->getType().getObjectType();
    if (tmpType.isTrivial(*pai->getFunction()))
      continue;
    for (auto *endPoint : partialApplyFrontier) {
      builder.setInsertionPoint(endPoint);
      if (!tmpType.isAddressOnly(*pai->getFunction())) {
        SILValue load = builder.emitLoadValueOperation(
            pai->getLoc(), op, LoadOwnershipQualifier::Take);
        builder.emitDestroyValueOperation(pai->getLoc(), load);
      } else {
        builder.createDestroyAddr(pai->getLoc(), op);
      }
    }
  }
}

/// Process an apply instruction which uses a partial_apply
/// as its callee.
/// Returns true on success.
bool PartialApplyCombiner::processSingleApply(FullApplySite paiAI) {
  builder.setInsertionPoint(paiAI.getInstruction());
  builder.setCurrentDebugScope(paiAI.getDebugScope());

  // Prepare the args.
  SmallVector<SILValue, 8> argList;
  // First the ApplyInst args.
  for (auto Op : paiAI.getArguments())
    argList.push_back(Op);

  SILInstruction *insertPoint = &*builder.getInsertionPoint();
  // Next, the partial apply args.

  // Pre-process partial_apply arguments only once, lazily.
  if (isFirstTime) {
    isFirstTime = false;
    if (!allocateTemporaries())
      return false;
  }

  // Now, copy over the partial apply args.
  for (auto arg : pai->getArguments()) {
    // If there is new temporary for this argument, use it instead.
    if (argToTmpCopy.count(arg)) {
      arg = argToTmpCopy.lookup(arg);
    }
    argList.push_back(arg);
  }

  builder.setInsertionPoint(insertPoint);
  builder.setCurrentDebugScope(paiAI.getDebugScope());

  // The thunk that implements the partial apply calls the closure function
  // that expects all arguments to be consumed by the function. However, the
  // captured arguments are not arguments of *this* apply, so they are not
  // pre-incremented. When we combine the partial_apply and this apply into
  // a new apply we need to retain all of the closure non-address type
  // arguments.
  auto paramInfo = pai->getSubstCalleeType()->getParameters();
  auto partialApplyArgs = pai->getArguments();
  // Set of arguments that need to be destroyed after each invocation.
  SmallVector<SILValue, 8> toBeDestroyedArgs;
  for (unsigned i : indices(partialApplyArgs)) {
    auto arg = partialApplyArgs[i];

    if (!arg->getType().isAddress()) {
      // Copy the argument as the callee may consume it.
      arg = builder.emitCopyValueOperation(pai->getLoc(), arg);
      // For non consumed parameters (e.g. guaranteed), we also need to
      // insert destroys after each apply instruction that we create.
      if (!paramInfo[paramInfo.size() - partialApplyArgs.size() + i]
               .isConsumed())
        toBeDestroyedArgs.push_back(arg);
    }
  }

  auto callee = pai->getCallee();
  SubstitutionMap subs = pai->getSubstitutionMap();

  // The partial_apply might be substituting in an open existential type.
  builder.addOpenedArchetypeOperands(pai);

  FullApplySite nai;
  if (auto *tai = dyn_cast<TryApplyInst>(paiAI))
    nai = builder.createTryApply(paiAI.getLoc(), callee, subs, argList,
                                 tai->getNormalBB(), tai->getErrorBB());
  else
    nai = builder.createApply(paiAI.getLoc(), callee, subs, argList,
                              cast<ApplyInst>(paiAI)->isNonThrowing());

  // We also need to destroy the partial_apply instruction itself because it is
  // consumed by the apply_instruction.
  auto loc = RegularLocation::getAutoGeneratedLocation();
  paiAI.insertAfterFullEvaluation([&](SILBasicBlock::iterator insertPt) {
    SILBuilderWithScope builder(insertPt);
    for (auto arg : toBeDestroyedArgs) {
      builder.emitDestroyValueOperation(loc, arg);
    }
    if (!pai->hasCalleeGuaranteedContext()) {
      builder.emitDestroyValueOperation(loc, pai);
    }
  });

  if (auto *apply = dyn_cast<ApplyInst>(paiAI)) {
    callbacks.replaceValueUsesWith(SILValue(apply),
                                   cast<ApplyInst>(nai.getInstruction()));
  }
  callbacks.deleteInst(paiAI.getInstruction());
  return true;
}

/// Perform the apply{partial_apply(x,y)}(z) -> apply(z,x,y) peephole
/// by iterating over all uses of the partial_apply and searching
/// for the pattern to transform.
SILInstruction *PartialApplyCombiner::combine() {
  // We need to model @unowned_inner_pointer better before we can do the
  // peephole here.
  if (llvm::any_of(pai->getSubstCalleeType()->getResults(),
                   [](SILResultInfo resultInfo) {
                     return resultInfo.getConvention() ==
                            ResultConvention::UnownedInnerPointer;
                   })) {
    return nullptr;
  }

  // Iterate over all uses of the partial_apply
  // and look for applies that use it as a callee.

  // Worklist of operands.
  SmallVector<Operand *, 8> worklist(pai->getUses());

  while (!worklist.empty()) {
    auto *use = worklist.pop_back_val();
    auto *user = use->getUser();

    // Recurse through conversions.
    if (auto *cfi = dyn_cast<ConvertEscapeToNoEscapeInst>(user)) {
      // TODO: Handle argument conversion. All the code in this file needs to be
      // cleaned up and generalized. The argument conversion handling in
      // optimizeApplyOfConvertFunctionInst should apply to any combine
      // involving an apply, not just a specific pattern.
      //
      // For now, just handle conversion to @noescape, which is irrelevant for
      // direct application of the closure.
      auto convertCalleeTy = cfi->getType().castTo<SILFunctionType>();
      auto escapingCalleeTy = convertCalleeTy->getWithExtInfo(
          convertCalleeTy->getExtInfo().withNoEscape(false));
      assert(use->get()->getType().castTo<SILFunctionType>() ==
             escapingCalleeTy);
      (void)escapingCalleeTy;
      llvm::copy(cfi->getUses(), std::back_inserter(worklist));
      continue;
    }

    // Look through mark_dependence users of partial_apply [stack].
    if (auto *mdi = dyn_cast<MarkDependenceInst>(user)) {
      if (mdi->getValue() == use->get() &&
          mdi->getValue()->getType().is<SILFunctionType>() &&
          mdi->getValue()->getType().castTo<SILFunctionType>()->isNoEscape()) {
        llvm::copy(mdi->getUses(), std::back_inserter(worklist));
      }
      continue;
    }
    // If this use of a partial_apply is not
    // an apply which uses it as a callee, bail.
    auto ai = FullApplySite::isa(user);
    if (!ai)
      continue;

    if (ai.getCallee() != use->get())
      continue;

    // We cannot handle generic apply yet. Bail.
    if (ai.hasSubstitutions())
      continue;

    if (!processSingleApply(ai))
      return nullptr;
  }

  // release/destroy and deallocate introduced temporaries.
  if (!tmpCopies.empty()) {
    destroyTemporaries();
    deallocateTemporaries();
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

SILInstruction *swift::tryOptimizeApplyOfPartialApply(
    PartialApplyInst *pai, SILBuilder &builder, InstModCallbacks callbacks) {
  PartialApplyCombiner combiner(pai, builder, callbacks);
  return combiner.combine();
}
