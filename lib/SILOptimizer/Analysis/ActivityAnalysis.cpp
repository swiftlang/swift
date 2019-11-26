//===--- ActivityAnalysis.h - SIL Activity Analysis -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#define DEBUG_TYPE "differentiation"

#include "swift/SIL/SILArgument.h"
#include "swift/SIL/Projection.h"
#include "swift/SILOptimizer/Analysis/ActivityAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/Utils/Differentiation.h"

using namespace swift;
using namespace swift::autodiff;

//===----------------------------------------------------------------------===//
// Helpers
//===----------------------------------------------------------------------===//

static bool isWithoutDerivative(SILValue v) {
  if (auto *fnRef = dyn_cast<FunctionRefInst>(v))
    return fnRef->getReferencedFunctionOrNull()->hasSemanticsAttr(
        "autodiff.nonvarying");
  return false;
}

std::unique_ptr<DifferentiableActivityCollection>
DifferentiableActivityAnalysis::newFunctionAnalysis(SILFunction *f) {
  assert(dominanceAnalysis && "Expect a valid dominance anaysis");
  assert(postDominanceAnalysis && "Expect a valid post-dominance anaysis");
  return llvm::make_unique<DifferentiableActivityCollection>(
      *f, dominanceAnalysis->get(f), postDominanceAnalysis->get(f));
}

void DifferentiableActivityAnalysis::initialize(SILPassManager *pm) {
  dominanceAnalysis = pm->getAnalysis<DominanceAnalysis>();
  postDominanceAnalysis = pm->getAnalysis<PostDominanceAnalysis>();
}

SILAnalysis *swift::createDifferentiableActivityAnalysis(SILModule *m) {
  return new DifferentiableActivityAnalysis();
}

DifferentiableActivityCollection::DifferentiableActivityCollection(
    SILFunction &f, DominanceInfo *di, PostDominanceInfo *pdi)
    : function(f), domInfo(di), postDomInfo(pdi) {}

DifferentiableActivityInfo::DifferentiableActivityInfo(
    DifferentiableActivityCollection &parent, GenericSignature derivGenSig)
    : parent(parent), derivativeGenericSignature(derivGenSig) {
  analyze(parent.domInfo, parent.postDomInfo);
}

SILFunction &DifferentiableActivityInfo::getFunction() {
  return parent.function;
}

void DifferentiableActivityInfo::analyze(DominanceInfo *di,
                                         PostDominanceInfo *pdi) {
  auto &function = getFunction();
  LLVM_DEBUG(getADDebugStream()
             << "Running activity analysis on @" << function.getName() << '\n');
  // Inputs are just function's arguments, count `n`.
  auto paramArgs = function.getArgumentsWithoutIndirectResults();
  for (auto value : paramArgs)
    inputValues.push_back(value);
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Inputs in @" << function.getName() << ":\n";
    for (auto val : inputValues)
      s << val << '\n';
  });
  // Outputs are indirect result buffers and return values, count `m`.
  collectAllFormalResultsInTypeOrder(function, outputValues);
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Outputs in @" << function.getName() << ":\n";
    for (auto val : outputValues)
      s << val << '\n';
  });

  // Propagate variedness starting from the inputs.
  assert(variedValueSets.empty());
  for (auto inputAndIdx : enumerate(inputValues)) {
    auto input = inputAndIdx.value();
    unsigned i = inputAndIdx.index();
    variedValueSets.push_back({});
    setVariedAndPropagateToUsers(input, i);
  }

  // Mark differentiable outputs as useful.
  assert(usefulValueSets.empty());
  for (auto outputAndIdx : enumerate(outputValues)) {
    auto output = outputAndIdx.value();
    unsigned i = outputAndIdx.index();
    usefulValueSets.push_back({});
    setUsefulAndPropagateToOperands(output, i);
  }
}

void DifferentiableActivityInfo::setVariedAndPropagateToUsers(
    SILValue value, unsigned independentVariableIndex) {
  // Skip already-varied values to prevent infinite recursion.
  if (isVaried(value, independentVariableIndex))
    return;
  // Set the value as varied.
  variedValueSets[independentVariableIndex].insert(value);
  // Propagate variedness to users.
  for (auto *use : value->getUses())
    propagateVaried(use, independentVariableIndex);
}

void DifferentiableActivityInfo::propagateVaried(
    Operand *operand, unsigned independentVariableIndex) {
  auto *inst = operand->getUser();
  // Propagate variedness for the given operand.
  // General rule: mark results as varied and recursively propagate variedness
  // to users of results.
  auto i = independentVariableIndex;
  // Handle `apply`.
  if (auto *ai = dyn_cast<ApplyInst>(inst)) {
    // If callee is non-varying, skip.
    if (isWithoutDerivative(ai->getCallee()))
      return;
    // If operand is varied, set all direct/indirect results and inout arguments
    // as varied.
    if (isVaried(operand->get(), i)) {
      for (auto indRes : ai->getIndirectSILResults())
        propagateVariedInwardsThroughProjections(indRes, i);
      for (auto inoutArg : ai->getInoutArguments())
        propagateVariedInwardsThroughProjections(inoutArg, i);
      forEachApplyDirectResult(ai, [&](SILValue directResult) {
        setVariedAndPropagateToUsers(directResult, i);
      });
    }
  }
  // Handle store-like instructions:
  //   `store`, `store_borrow`, `copy_addr`, `unconditional_checked_cast`
#define PROPAGATE_VARIED_THROUGH_STORE(INST) \
  else if (auto *si = dyn_cast<INST##Inst>(inst)) { \
    if (isVaried(si->getSrc(), i)) \
      propagateVariedInwardsThroughProjections(si->getDest(), i); \
  }
  PROPAGATE_VARIED_THROUGH_STORE(Store)
  PROPAGATE_VARIED_THROUGH_STORE(StoreBorrow)
  PROPAGATE_VARIED_THROUGH_STORE(CopyAddr)
  PROPAGATE_VARIED_THROUGH_STORE(UnconditionalCheckedCastAddr)
#undef PROPAGATE_VARIED_THROUGH_STORE
  // Handle `tuple_element_addr`.
  else if (auto *teai = dyn_cast<TupleElementAddrInst>(inst)) {
    if (isVaried(teai->getOperand(), i)) {
      // Propagate variedness only if the `tuple_element_addr` result has a
      // tangent space. Otherwise, the result does not need a derivative.
      if (hasTangentSpace(teai))
        setVariedAndPropagateToUsers(teai, i);
    }
  }
  // Handle `struct_extract` and `struct_element_addr` instructions.
  // - If the field is marked `@noDerivative`, do not set the result as
  //   varied because it does not need a derivative.
  // - Otherwise, propagate variedness from operand to result as usual.
#define PROPAGATE_VARIED_FOR_STRUCT_EXTRACTION(INST) \
  else if (auto *sei = dyn_cast<INST##Inst>(inst)) { \
    if (isVaried(sei->getOperand(), i) && \
        !sei->getField()->getAttrs().hasAttribute<NoDerivativeAttr>()) \
      setVariedAndPropagateToUsers(sei, i); \
  }
  PROPAGATE_VARIED_FOR_STRUCT_EXTRACTION(StructExtract)
  PROPAGATE_VARIED_FOR_STRUCT_EXTRACTION(StructElementAddr)
#undef PROPAGATE_VARIED_FOR_STRUCT_EXTRACTION
  // Handle `br`.
  else if (auto *bi = dyn_cast<BranchInst>(inst)) {
    if (isVaried(operand->get(), i))
      setVariedAndPropagateToUsers(bi->getArgForOperand(operand), i);
  }
  // Handle `cond_br`.
  else if (auto *cbi = dyn_cast<CondBranchInst>(inst)) {
    if (isVaried(operand->get(), i))
      if (auto *destBBArg = cbi->getArgForOperand(operand))
        setVariedAndPropagateToUsers(destBBArg, i);
  }
  // Handle `switch_enum`.
  else if (auto *sei = dyn_cast<SwitchEnumInst>(inst)) {
    if (isVaried(sei->getOperand(), i))
      for (auto *succBB : sei->getSuccessorBlocks())
        for (auto *arg : succBB->getArguments())
          setVariedAndPropagateToUsers(arg, i);
  }
  // Handle everything else.
  else {
    for (auto &op : inst->getAllOperands()) {
      if (isVaried(op.get(), i))
        for (auto result : inst->getResults())
          setVariedAndPropagateToUsers(result, i);
    }
  }
}

void DifferentiableActivityInfo::propagateVariedInwardsThroughProjections(
    SILValue value, unsigned independentVariableIndex) {
  // Skip `@noDerivative` struct projections.
#define SKIP_NODERIVATIVE(INST) \
  if (auto *sei = dyn_cast<INST##Inst>(value)) \
    if (sei->getField()->getAttrs().hasAttribute<NoDerivativeAttr>()) \
      return;
  SKIP_NODERIVATIVE(StructExtract)
  SKIP_NODERIVATIVE(StructElementAddr)
#undef SKIP_NODERIVATIVE
  // Set value as varied and propagate to users.
  setVariedAndPropagateToUsers(value, independentVariableIndex);
  auto *inst = value->getDefiningInstruction();
  if (!inst || isa<ApplyInst>(inst))
    return;
  // Standard propagation.
  for (auto &op : inst->getAllOperands())
    propagateVariedInwardsThroughProjections(
        op.get(), independentVariableIndex);
}

void DifferentiableActivityInfo::setUseful(SILValue value,
                                           unsigned dependentVariableIndex) {
  usefulValueSets[dependentVariableIndex].insert(value);
  setUsefulThroughArrayInitialization(value, dependentVariableIndex);
}

void DifferentiableActivityInfo::setUsefulAndPropagateToOperands(
    SILValue value, unsigned dependentVariableIndex) {
  // Skip already-useful values to prevent infinite recursion.
  if (isUseful(value, dependentVariableIndex))
    return;
  if (value->getType().isAddress()) {
    propagateUsefulThroughAddress(value, dependentVariableIndex);
    return;
  }
  setUseful(value, dependentVariableIndex);
  // If the given value is a basic block argument, propagate usefulness to
  // incoming values.
  if (auto *bbArg = dyn_cast<SILPhiArgument>(value)) {
    SmallVector<SILValue, 4> incomingValues;
    bbArg->getSingleTerminatorOperands(incomingValues);
    for (auto incomingValue : incomingValues)
      setUsefulAndPropagateToOperands(incomingValue, dependentVariableIndex);
    return;
  }
  auto *inst = value->getDefiningInstruction();
  if (!inst)
    return;
  propagateUseful(inst, dependentVariableIndex);
}

void DifferentiableActivityInfo::propagateUseful(
    SILInstruction *inst, unsigned dependentVariableIndex) {
  // Propagate usefulness for the given instruction: mark operands as useful and
  // recursively propagate usefulness to defining instructions of operands.
  auto i = dependentVariableIndex;
  // Handle indirect results in `apply`.
  if (auto *ai = dyn_cast<ApplyInst>(inst)) {
    if (isWithoutDerivative(ai->getCallee()))
      return;
    for (auto arg : ai->getArgumentsWithoutIndirectResults())
      setUsefulAndPropagateToOperands(arg, i);
  }
  // Handle store-like instructions:
  //   `store`, `store_borrow`, `copy_addr`, `unconditional_checked_cast`
#define PROPAGATE_USEFUL_THROUGH_STORE(INST) \
  else if (auto *si = dyn_cast<INST##Inst>(inst)) { \
    setUsefulAndPropagateToOperands(si->getSrc(), i); \
  }
  PROPAGATE_USEFUL_THROUGH_STORE(Store)
  PROPAGATE_USEFUL_THROUGH_STORE(StoreBorrow)
  PROPAGATE_USEFUL_THROUGH_STORE(CopyAddr)
  PROPAGATE_USEFUL_THROUGH_STORE(UnconditionalCheckedCastAddr)
#undef PROPAGATE_USEFUL_THROUGH_STORE
  // Handle struct element extraction, skipping `@noDerivative` fields:
  //   `struct_extract`, `struct_element_addr`.
#define PROPAGATE_USEFUL_THROUGH_STRUCT_EXTRACTION(INST) \
  else if (auto *sei = dyn_cast<INST##Inst>(inst)) { \
    if (!sei->getField()->getAttrs().hasAttribute<NoDerivativeAttr>()) \
      setUsefulAndPropagateToOperands(sei->getOperand(), i); \
  }
  PROPAGATE_USEFUL_THROUGH_STRUCT_EXTRACTION(StructExtract)
  PROPAGATE_USEFUL_THROUGH_STRUCT_EXTRACTION(StructElementAddr)
#undef PROPAGATE_USEFUL_THROUGH_STRUCT_EXTRACTION
  // Handle everything else.
  else {
    for (auto &op : inst->getAllOperands())
      setUsefulAndPropagateToOperands(op.get(), i);
  }
}

void DifferentiableActivityInfo::propagateUsefulThroughAddress(
    SILValue value, unsigned dependentVariableIndex) {
  assert(value->getType().isAddress());
  // Skip already-useful values to prevent infinite recursion.
  if (isUseful(value, dependentVariableIndex))
    return;
  setUseful(value, dependentVariableIndex);
  if (auto *inst = value->getDefiningInstruction())
    propagateUseful(inst, dependentVariableIndex);
  // Recursively propagate usefulness through users that are projections or
  // `begin_access` instructions.
  for (auto use : value->getUses()) {
    // Propagate usefulness through user's operands.
    propagateUseful(use->getUser(), dependentVariableIndex);
    for (auto res : use->getUser()->getResults()) {
#define SKIP_NODERIVATIVE(INST)                                                \
      if (auto *sei = dyn_cast<INST##Inst>(res))                               \
        if (sei->getField()->getAttrs().hasAttribute<NoDerivativeAttr>())      \
          continue;
      SKIP_NODERIVATIVE(StructExtract)
      SKIP_NODERIVATIVE(StructElementAddr)
#undef SKIP_NODERIVATIVE
      if (Projection::isAddressProjection(res) || isa<BeginAccessInst>(res))
        propagateUsefulThroughAddress(res, dependentVariableIndex);
    }
  }
}

void DifferentiableActivityInfo::setUsefulThroughArrayInitialization(
    SILValue value, unsigned dependentVariableIndex) {
  // Array initializer syntax is lowered to an intrinsic and one or more
  // stores to a `RawPointer` returned by the intrinsic.
  auto *uai = getAllocateUninitializedArrayIntrinsic(value);
  if (!uai) return;
  for (auto use : value->getUses()) {
    auto *dti = dyn_cast<DestructureTupleInst>(use->getUser());
    if (!dti) continue;
    // The second tuple field of the return value is the `RawPointer`.
    for (auto use : dti->getResult(1)->getUses()) {
      // The `RawPointer` passes through a `pointer_to_address`. That
      // instruction's first use is a `store` whose source is useful; its
      // subsequent uses are `index_addr`s whose only use is a useful `store`.
      auto *ptai = dyn_cast<PointerToAddressInst>(use->getUser());
      assert(ptai && "Expected `pointer_to_address` user for uninitialized "
                     "array intrinsic");
      // Propagate usefulness through array element addresses.
      // - Find `store` and `copy_addr` instructions with array element
      //   address destinations.
      // - For each instruction, set destination (array element address) as
      //   useful and propagate usefulness through source.
      //
      // Note: `propagateUseful(use->getUser(), ...)` is intentionally not used
      // because it marks more values than necessary as useful, including:
      // - The `RawPointer` result of the intrinsic.
      // - The `pointer_to_address` user of the `RawPointer`.
      // - `index_addr` and `integer_literal` instructions for indexing the
      //   `RawPointer`.
      for (auto use : ptai->getUses()) {
        auto *user = use->getUser();
        if (auto *si = dyn_cast<StoreInst>(user)) {
          setUseful(si->getDest(), dependentVariableIndex);
          setUsefulAndPropagateToOperands(si->getSrc(), dependentVariableIndex);
        } else if (auto *cai = dyn_cast<CopyAddrInst>(user)) {
          setUseful(cai->getDest(), dependentVariableIndex);
          setUsefulAndPropagateToOperands(cai->getSrc(),
                                          dependentVariableIndex);
        } else if (auto *iai = dyn_cast<IndexAddrInst>(user)) {
          for (auto use : iai->getUses()) {
            auto *user = use->getUser();
            if (auto si = dyn_cast<StoreInst>(user)) {
              setUseful(si->getDest(), dependentVariableIndex);
              setUsefulAndPropagateToOperands(si->getSrc(),
                                              dependentVariableIndex);
            } else if (auto *cai = dyn_cast<CopyAddrInst>(user)) {
              setUseful(cai->getDest(), dependentVariableIndex);
              setUsefulAndPropagateToOperands(cai->getSrc(),
                                              dependentVariableIndex);
            }
          }
        }
      }
    }
  }
}

bool DifferentiableActivityInfo::isVaried(
    SILValue value, unsigned independentVariableIndex) const {
  assert(independentVariableIndex < variedValueSets.size() &&
         "Independent variable index out of range");
  auto &set = variedValueSets[independentVariableIndex];
  return set.count(value);
}

bool DifferentiableActivityInfo::isVaried(
    SILValue value, IndexSubset *independentVariableIndices) const {
  for (auto i : independentVariableIndices->getIndices())
    if (isVaried(value, i))
      return true;
  return false;
}

bool DifferentiableActivityInfo::isUseful(
    SILValue value, unsigned dependentVariableIndex) const {
  assert(dependentVariableIndex < usefulValueSets.size() &&
         "Dependent variable index out of range");
  auto &set = usefulValueSets[dependentVariableIndex];
  return set.count(value);
}

bool DifferentiableActivityInfo::isActive(
    SILValue value, const SILAutoDiffIndices &indices) const {
  return isVaried(value, indices.parameters) && isUseful(value, indices.source);
}

Activity DifferentiableActivityInfo::getActivity(
    SILValue value, const SILAutoDiffIndices &indices) const {
  Activity activity;
  if (isVaried(value, indices.parameters))
    activity |= ActivityFlags::Varied;
  if (isUseful(value, indices.source))
    activity |= ActivityFlags::Useful;
  return activity;
}
