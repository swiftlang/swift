//===--- PullbackEmitter.cpp - Pullback in differentiation ----*- C++ -*---===//
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
//
// SWIFT_ENABLE_TENSORFLOW
//
// This file defines a helper class for generating pullbacks in automatic
// differentiation.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/SIL/Projection.h"
#include "swift/SILOptimizer/PassManager/PrettyStackTrace.h"
#include "swift/SILOptimizer/Utils/Differentiation/ADContext.h"
#include "swift/SILOptimizer/Utils/Differentiation/PullbackEmitter.h"
#include "swift/SILOptimizer/Utils/Differentiation/VJPEmitter.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"

namespace swift {

class SILDifferentiabilityWitness;
class SILBasicBlock;
class SILFunction;
class SILInstruction;

namespace autodiff {

class ADContext;
class VJPEmitter;

PullbackEmitter::PullbackEmitter(VJPEmitter &vjpEmitter)
    : vjpEmitter(vjpEmitter), builder(getPullback()),
      localAllocBuilder(getPullback()) {
  // Get dominance and post-order info for the original function.
  auto &passManager = getContext().getPassManager();
  auto *domAnalysis = passManager.getAnalysis<DominanceAnalysis>();
  auto *postDomAnalysis = passManager.getAnalysis<PostDominanceAnalysis>();
  auto *postOrderAnalysis = passManager.getAnalysis<PostOrderAnalysis>();
  domInfo = domAnalysis->get(vjpEmitter.original);
  postDomInfo = postDomAnalysis->get(vjpEmitter.original);
  postOrderInfo = postOrderAnalysis->get(vjpEmitter.original);
}

ADContext &PullbackEmitter::getContext() const { return vjpEmitter.context; }

SILModule &PullbackEmitter::getModule() const {
  return getContext().getModule();
}

ASTContext &PullbackEmitter::getASTContext() const {
  return getPullback().getASTContext();
}

SILFunction &PullbackEmitter::getOriginal() const {
  return *vjpEmitter.original;
}

SILFunction &PullbackEmitter::getPullback() const {
  return *vjpEmitter.pullback;
}

SILDifferentiabilityWitness *PullbackEmitter::getWitness() const {
  return vjpEmitter.witness;
}

DifferentiationInvoker PullbackEmitter::getInvoker() const {
  return vjpEmitter.invoker;
}

LinearMapInfo &PullbackEmitter::getPullbackInfo() {
  return vjpEmitter.pullbackInfo;
}

const SILAutoDiffIndices PullbackEmitter::getIndices() const {
  return vjpEmitter.getIndices();
}

const DifferentiableActivityInfo &PullbackEmitter::getActivityInfo() const {
  return vjpEmitter.activityInfo;
}

//--------------------------------------------------------------------------//
// Pullback struct mapping
//--------------------------------------------------------------------------//

void PullbackEmitter::initializePullbackStructElements(
    SILBasicBlock *origBB, SILInstructionResultArray values) {
  auto *pbStructDecl = getPullbackInfo().getLinearMapStruct(origBB);
  assert(pbStructDecl->getStoredProperties().size() == values.size() &&
         "The number of pullback struct fields must equal the number of "
         "pullback struct element values");
  for (auto pair : llvm::zip(pbStructDecl->getStoredProperties(), values)) {
    assert(std::get<1>(pair).getOwnershipKind() !=
               ValueOwnershipKind::Guaranteed &&
           "Pullback struct elements must be @owned");
    auto insertion =
        pullbackStructElements.insert({std::get<0>(pair), std::get<1>(pair)});
    (void)insertion;
    assert(insertion.second && "A pullback struct element already exists!");
  }
}

SILValue PullbackEmitter::getPullbackStructElement(SILBasicBlock *origBB,
                                                   VarDecl *field) {
  assert(getPullbackInfo().getLinearMapStruct(origBB) ==
         cast<StructDecl>(field->getDeclContext()));
  assert(pullbackStructElements.count(field) &&
         "Pullback struct element for this field does not exist!");
  return pullbackStructElements.lookup(field);
}


//--------------------------------------------------------------------------//
// Temporary value management
//--------------------------------------------------------------------------//

SILValue PullbackEmitter::recordTemporary(SILValue value) {
  assert(value->getType().isObject());
    assert(value->getFunction() == &getPullback());
    auto inserted = blockTemporaries[value->getParentBlock()].insert(value);
    (void)inserted;
    LLVM_DEBUG(getADDebugStream() << "Recorded temporary " << value);
    assert(inserted && "Temporary already recorded?");
    return value;
}

void PullbackEmitter::cleanUpTemporariesForBlock(SILBasicBlock *bb,
                                                 SILLocation loc) {
  assert(bb->getParent() == &getPullback());
  LLVM_DEBUG(getADDebugStream() << "Cleaning up temporaries for pullback bb"
                                << bb->getDebugID() << '\n');
  for (auto temp : blockTemporaries[bb])
    builder.emitDestroyValueOperation(loc, temp);
  blockTemporaries[bb].clear();
}

//--------------------------------------------------------------------------//
// Type transformer
//--------------------------------------------------------------------------//

/// Remap any archetypes into the current function's context.
SILType PullbackEmitter::remapType(SILType ty) {
  if (ty.hasArchetype())
    return getPullback().mapTypeIntoContext(ty.mapTypeOutOfContext());
  return getPullback().mapTypeIntoContext(ty);
}

Optional<VectorSpace> PullbackEmitter::getTangentSpace(CanType type) {
  return type->getAutoDiffAssociatedTangentSpace(
      LookUpConformanceInModule(getModule().getSwiftModule()));
}

SILType PullbackEmitter::getRemappedTangentType(SILType type) {
  return SILType::getPrimitiveType(
      getTangentSpace(remapType(type).getASTType())->getCanonicalType(),
      type.getCategory());
}

SubstitutionMap
PullbackEmitter::remapSubstitutionMap(SubstitutionMap substMap) {
  return substMap.subst(getPullback().getForwardingSubstitutionMap());
}

//--------------------------------------------------------------------------//
// Managed value mapping
//--------------------------------------------------------------------------//

bool PullbackEmitter::hasAdjointValue(SILBasicBlock *origBB,
                                      SILValue originalValue) const {
  assert(origBB->getParent() == &getOriginal());
  assert(originalValue->getType().isObject());
  return valueMap.count({origBB, originalValue});
}

void PullbackEmitter::setAdjointValue(SILBasicBlock *origBB,
                                      SILValue originalValue,
                                      AdjointValue adjointValue) {
  LLVM_DEBUG(getADDebugStream()
             << "Setting adjoint value for " << originalValue);
  assert(origBB->getParent() == &getOriginal());
  assert(originalValue->getType().isObject());
  assert(adjointValue.getType().isObject());
  assert(originalValue->getFunction() == &getOriginal());
  // The adjoint value must be in the tangent space.
  assert(adjointValue.getType() ==
         getRemappedTangentType(originalValue->getType()));
  auto insertion = valueMap.try_emplace({origBB, originalValue}, adjointValue);
  LLVM_DEBUG(getADDebugStream()
             << "The new adjoint value, replacing the existing one, is: "
             << insertion.first->getSecond());
  if (!insertion.second)
    insertion.first->getSecond() = adjointValue;
}

AdjointValue PullbackEmitter::getAdjointValue(SILBasicBlock *origBB,
                                              SILValue originalValue) {
  assert(origBB->getParent() == &getOriginal());
  assert(originalValue->getType().isObject());
  assert(originalValue->getFunction() == &getOriginal());
  auto insertion = valueMap.try_emplace(
      {origBB, originalValue},
      makeZeroAdjointValue(getRemappedTangentType(originalValue->getType())));
  auto it = insertion.first;
  return it->getSecond();
}

void PullbackEmitter::addAdjointValue(SILBasicBlock *origBB,
                                      SILValue originalValue,
                                      AdjointValue newAdjointValue,
                                      SILLocation loc) {
  assert(origBB->getParent() == &getOriginal());
  assert(originalValue->getType().isObject());
  assert(newAdjointValue.getType().isObject());
  assert(originalValue->getFunction() == &getOriginal());
  LLVM_DEBUG(getADDebugStream()
             << "Adding adjoint value for " << originalValue);
  // The adjoint value must be in the tangent space.
  assert(newAdjointValue.getType() ==
         getRemappedTangentType(originalValue->getType()));
  auto insertion =
      valueMap.try_emplace({origBB, originalValue}, newAdjointValue);
  auto inserted = insertion.second;
  if (inserted)
    return;
  // If adjoint already exists, accumulate the adjoint onto the existing
  // adjoint.
  auto it = insertion.first;
  auto existingValue = it->getSecond();
  valueMap.erase(it);
  auto adjVal = accumulateAdjointsDirect(existingValue, newAdjointValue, loc);
  setAdjointValue(origBB, originalValue, adjVal);
}

SILArgument *
PullbackEmitter::getActiveValuePullbackBlockArgument(SILBasicBlock *origBB,
                                                     SILValue activeValue) {
  assert(origBB->getParent() == &getOriginal());
  auto pullbackBBArg = activeValuePullbackBBArgumentMap[{origBB, activeValue}];
  assert(pullbackBBArg);
  assert(pullbackBBArg->getParent() == getPullbackBlock(origBB));
  return pullbackBBArg;
}

//--------------------------------------------------------------------------//
// Buffer mapping
//--------------------------------------------------------------------------//

void PullbackEmitter::setAdjointBuffer(SILBasicBlock *origBB,
                                       SILValue originalBuffer,
                                       SILValue adjointBuffer) {
  assert(originalBuffer->getType().isAddress());
  auto insertion =
      bufferMap.try_emplace({origBB, originalBuffer}, adjointBuffer);
  assert(insertion.second);
  (void)insertion;
}

SILValue PullbackEmitter::getAdjointProjection(SILBasicBlock *origBB,
                                               SILValue originalProjection) {
  // Handle `struct_element_addr`.
  if (auto *seai = dyn_cast<StructElementAddrInst>(originalProjection)) {
    assert(!seai->getField()->getAttrs().hasAttribute<NoDerivativeAttr>() &&
           "`@noDerivative` struct projections should never be active");
    auto adjSource = getAdjointBuffer(origBB, seai->getOperand());
    auto *tangentVectorDecl =
        adjSource->getType().getStructOrBoundGenericStruct();
    auto tanFieldLookup =
        tangentVectorDecl->lookupDirect(seai->getField()->getName());
    assert(tanFieldLookup.size() == 1);
    auto *tanField = cast<VarDecl>(tanFieldLookup.front());
    return builder.createStructElementAddr(seai->getLoc(), adjSource, tanField);
  }
  // Handle `tuple_element_addr`.
  if (auto *teai = dyn_cast<TupleElementAddrInst>(originalProjection)) {
    auto source = teai->getOperand();
    auto adjSource = getAdjointBuffer(origBB, source);
    if (!adjSource->getType().is<TupleType>())
      return adjSource;
    auto origTupleTy = source->getType().castTo<TupleType>();
    unsigned adjIndex = 0;
    for (unsigned i : range(teai->getFieldNo())) {
      if (getTangentSpace(
              origTupleTy->getElement(i).getType()->getCanonicalType()))
        ++adjIndex;
    }
    return builder.createTupleElementAddr(teai->getLoc(), adjSource, adjIndex);
  }
  // Handle `begin_access`.
  if (auto *bai = dyn_cast<BeginAccessInst>(originalProjection)) {
    auto adjBase = getAdjointBuffer(origBB, bai->getOperand());
    if (errorOccurred)
      return (bufferMap[{origBB, originalProjection}] = SILValue());
    // Return the base buffer's adjoint buffer.
    return adjBase;
  }
  return SILValue();
}

SILBasicBlock::iterator
PullbackEmitter::getNextFunctionLocalAllocationInsertionPoint() {
  // If there are no local allocations, insert at the pullback entry start.
  if (functionLocalAllocations.empty())
    return getPullback().getEntryBlock()->begin();
  // Otherwise, insert before the last local allocation. Inserting before
  // rather than after ensures that allocation and zero initialization
  // instructions are grouped together.
  auto lastLocalAlloc = functionLocalAllocations.back();
  return lastLocalAlloc->getDefiningInstruction()->getIterator();
}

SILValue &PullbackEmitter::getAdjointBuffer(SILBasicBlock *origBB,
                                            SILValue originalBuffer) {
  assert(originalBuffer->getType().isAddress());
  assert(originalBuffer->getFunction() == &getOriginal());
  auto insertion = bufferMap.try_emplace({origBB, originalBuffer}, SILValue());
  if (!insertion.second) // not inserted
    return insertion.first->getSecond();

  // If the original buffer is a projection, return a corresponding projection
  // into the adjoint buffer.
  if (auto adjProj = getAdjointProjection(origBB, originalBuffer))
    return (bufferMap[{origBB, originalBuffer}] = adjProj);

  // Set insertion point for local allocation builder: before the last local
  // allocation, or at the start of the pullback function's entry if no local
  // allocations exist yet.
  localAllocBuilder.setInsertionPoint(
      getPullback().getEntryBlock(),
      getNextFunctionLocalAllocationInsertionPoint());
  // Allocate local buffer and initialize to zero.
  auto bufObjectType = getRemappedTangentType(originalBuffer->getType());
  auto *newBuf = localAllocBuilder.createAllocStack(
      RegularLocation::getAutoGeneratedLocation(), bufObjectType);
  // Temporarily change global builder insertion point and emit zero into the
  // local buffer.
  auto insertionPoint = builder.getInsertionBB();
  builder.setInsertionPoint(localAllocBuilder.getInsertionBB(),
                            localAllocBuilder.getInsertionPoint());
  emitZeroIndirect(bufObjectType.getASTType(), newBuf, newBuf->getLoc());
  builder.setInsertionPoint(insertionPoint);
  // Register the local buffer.
  functionLocalAllocations.push_back(newBuf);
  return (insertion.first->getSecond() = newBuf);
}

void PullbackEmitter::addToAdjointBuffer(SILBasicBlock *origBB,
                                         SILValue originalBuffer,
                                         SILValue rhsBufferAccess,
                                         SILLocation loc) {
  assert(originalBuffer->getType().isAddress() &&
         rhsBufferAccess->getType().isAddress());
  assert(originalBuffer->getFunction() == &getOriginal());
  assert(rhsBufferAccess->getFunction() == &getPullback());
  auto adjointBuffer = getAdjointBuffer(origBB, originalBuffer);
  accumulateIndirect(adjointBuffer, rhsBufferAccess, loc);
}

//--------------------------------------------------------------------------//
// Entry point
//--------------------------------------------------------------------------//

bool PullbackEmitter::run() {
  PrettyStackTraceSILFunction trace("generating pullback for", &getOriginal());
  auto &original = getOriginal();
  auto &pullback = getPullback();
  auto pbLoc = getPullback().getLocation();
  LLVM_DEBUG(getADDebugStream() << "Running PullbackEmitter on\n" << original);

  auto origExitIt = original.findReturnBB();
  assert(origExitIt != original.end() &&
         "Functions without returns must have been diagnosed");
  auto *origExit = &*origExitIt;

  SmallVector<SILValue, 8> origFormalResults;
  collectAllFormalResultsInTypeOrder(original, origFormalResults);
  auto origResult = origFormalResults[getIndices().source];

  // If original result is non-varied, it will always have a zero derivative.
  // Skip full pullback generation and simply emit zero derivatives for wrt
  // parameters.
  //
  // NOTE(TF-876): This shortcut is currently necessary for functions
  // returning non-varied result with >1 basic block where some basic blocks
  // have no dominated active values; control flow differentiation does not
  // handle this case. See TF-876 for context.
  if (!getActivityInfo().isVaried(origResult, getIndices().parameters)) {
    emitZeroDerivativesForNonvariedResult(origResult);
    return false;
  }

  // Get dominated active values in original blocks.
  // Adjoint values of dominated active values are passed as pullback block
  // arguments.
  DominanceOrder domOrder(original.getEntryBlock(), domInfo);
  // Keep track of visited values.
  SmallPtrSet<SILValue, 8> visited;
  while (auto *bb = domOrder.getNext()) {
    auto &bbActiveValues = activeValues[bb];
    // If the current block has an immediate dominator, append the immediate
    // dominator block's active values to the current block's active values.
    if (auto *domNode = domInfo->getNode(bb)->getIDom()) {
      auto &domBBActiveValues = activeValues[domNode->getBlock()];
      bbActiveValues.append(domBBActiveValues.begin(), domBBActiveValues.end());
    }
    bool diagnosedActiveEnumValue = false;
    // Mark the activity of a value if it has not yet been visited.
    auto markValueActivity = [&](SILValue v) {
      if (visited.count(v))
        return;
      visited.insert(v);
      // Diagnose active enum values. Differentiation of enum values requires
      // special adjoint value handling and is not yet supported. Diagnose
      // only the first active enum value to prevent too many diagnostics.
      if (!diagnosedActiveEnumValue &&
          v->getType().getEnumOrBoundGenericEnum()) {
        getContext().emitNondifferentiabilityError(
            v, getInvoker(), diag::autodiff_enums_unsupported);
        errorOccurred = true;
        diagnosedActiveEnumValue = true;
      }
      // Skip address projections.
      // Address projections do not need their own adjoint buffers; they
      // become projections into their adjoint base buffer.
      if (Projection::isAddressProjection(v))
        return;
      bbActiveValues.push_back(v);
    };
    // Visit bb arguments and all instruction operands/results.
    for (auto *arg : bb->getArguments())
      if (getActivityInfo().isActive(arg, getIndices()))
        markValueActivity(arg);
    for (auto &inst : *bb) {
      for (auto op : inst.getOperandValues())
        if (getActivityInfo().isActive(op, getIndices()))
          markValueActivity(op);
      for (auto result : inst.getResults())
        if (getActivityInfo().isActive(result, getIndices()))
          markValueActivity(result);
    }
    domOrder.pushChildren(bb);
    if (errorOccurred)
      return true;
  }

  // Create pullback blocks and arguments, visiting original blocks in
  // post-order post-dominance order.
  SmallVector<SILBasicBlock *, 8> postOrderPostDomOrder;
  // Start from the root node, which may have a marker `nullptr` block if
  // there are multiple roots.
  PostOrderPostDominanceOrder postDomOrder(postDomInfo->getRootNode(),
                                           postOrderInfo, original.size());
  while (auto *origNode = postDomOrder.getNext()) {
    auto *origBB = origNode->getBlock();
    postDomOrder.pushChildren(origNode);
    // If node is the `nullptr` marker basic block, do not push it.
    if (!origBB)
      continue;
    postOrderPostDomOrder.push_back(origBB);
  }
  for (auto *origBB : postOrderPostDomOrder) {
    auto *pullbackBB = pullback.createBasicBlock();
    pullbackBBMap.insert({origBB, pullbackBB});
    auto pbStructLoweredType =
        remapType(getPullbackInfo().getLinearMapStructLoweredType(origBB));
    // If the BB is the original exit, then the pullback block that we just
    // created must be the pullback function's entry. For the pullback entry,
    // create entry arguments and continue to the next block.
    if (origBB == origExit) {
      assert(pullbackBB->isEntry());
      createEntryArguments(&pullback);
      auto *mainPullbackStruct = pullbackBB->getArguments().back();
      assert(mainPullbackStruct->getType() == pbStructLoweredType);
      pullbackStructArguments[origBB] = mainPullbackStruct;
      // Destructure the pullback struct to get the elements.
      builder.setInsertionPoint(pullbackBB);
      auto *dsi = builder.createDestructureStruct(pbLoc, mainPullbackStruct);
      initializePullbackStructElements(origBB, dsi->getResults());
      continue;
    }
    // Get all active values in the original block.
    // If the original block has no active values, continue.
    auto &bbActiveValues = activeValues[origBB];
    if (bbActiveValues.empty())
      continue;
    // Otherwise, if the original block has active values:
    // - For each active buffer in the original block, allocate a new local
    //   buffer in the pullback entry. (All adjoint buffers are allocated in
    //   the pullback entry and deallocated in the pullback exit.)
    // - For each active value in the original block, add adjoint value
    //   arguments to the pullback block.
    for (auto activeValue : bbActiveValues) {
      if (activeValue->getType().isAddress()) {
        // Allocate and zero initialize a new local buffer using
        // `getAdjointBuffer`.
        builder.setInsertionPoint(pullback.getEntryBlock());
        getAdjointBuffer(origBB, activeValue);
      } else {
        // Create and register pullback block argument for the active value.
        auto *pullbackArg = pullbackBB->createPhiArgument(
            getRemappedTangentType(activeValue->getType()),
            ValueOwnershipKind::Owned);
        activeValuePullbackBBArgumentMap[{origBB, activeValue}] = pullbackArg;
        recordTemporary(pullbackArg);
      }
    }
    // Add a pullback struct argument.
    auto *pbStructArg = pullbackBB->createPhiArgument(
        pbStructLoweredType, ValueOwnershipKind::Owned);
    pullbackStructArguments[origBB] = pbStructArg;
    // Destructure the pullback struct to get the elements.
    builder.setInsertionPoint(pullbackBB);
    auto *dsi = builder.createDestructureStruct(pbLoc, pbStructArg);
    initializePullbackStructElements(origBB, dsi->getResults());

    // - Create pullback trampoline blocks for each successor block of the
    //   original block. Pullback trampoline blocks only have a pullback
    //   struct argument. They branch from a pullback successor block to the
    //   pullback original block, passing adjoint values of active values.
    for (auto *succBB : origBB->getSuccessorBlocks()) {
      auto *pullbackTrampolineBB = pullback.createBasicBlockBefore(pullbackBB);
      pullbackTrampolineBBMap.insert({{origBB, succBB}, pullbackTrampolineBB});
      // Get the enum element type (i.e. the pullback struct type). The enum
      // element type may be boxed if the enum is indirect.
      auto enumLoweredTy =
          getPullbackInfo().getBranchingTraceEnumLoweredType(succBB);
      auto *enumEltDecl =
          getPullbackInfo().lookUpBranchingTraceEnumElement(origBB, succBB);
      auto enumEltType = remapType(enumLoweredTy.getEnumElementType(
          enumEltDecl, getModule(), TypeExpansionContext::minimal()));
      pullbackTrampolineBB->createPhiArgument(enumEltType,
                                              ValueOwnershipKind::Owned);
    }
  }

  auto *pullbackEntry = pullback.getEntryBlock();
  // The pullback function has type (seed, exit_pbs) -> ([arg0], ..., [argn]).
  auto pbParamArgs = pullback.getArgumentsWithoutIndirectResults();
  assert(pbParamArgs.size() == 2);
  seed = pbParamArgs[0];

  // Assign adjoint for original result.
  builder.setInsertionPoint(pullbackEntry,
                            getNextFunctionLocalAllocationInsertionPoint());
  if (seed->getType().isAddress()) {
    auto *seedBufCopy = builder.createAllocStack(pbLoc, seed->getType());
    builder.createCopyAddr(pbLoc, seed, seedBufCopy, IsNotTake,
                           IsInitialization);
    setAdjointBuffer(origExit, origResult, seedBufCopy);
    functionLocalAllocations.push_back(seedBufCopy);
    LLVM_DEBUG(getADDebugStream()
               << "Assigned seed buffer " << seedBufCopy
               << " as the adjoint of original indirect result " << origResult);
  } else {
    setAdjointValue(origExit, origResult, makeConcreteAdjointValue(seed));
    LLVM_DEBUG(getADDebugStream()
               << "Assigned seed " << *seed
               << " as the adjoint of original result " << origResult);
  }

  // Visit original blocks blocks in post-order and perform differentiation
  // in corresponding pullback blocks. If errors occurred, back out.
  for (auto *bb : postOrderPostDomOrder) {
    visitSILBasicBlock(bb);
    if (errorOccurred)
      return true;
  }

  // Prepare and emit a `return` in the pullback exit block.
  auto *origEntry = getOriginal().getEntryBlock();
  auto *pbExit = getPullbackBlock(origEntry);
  builder.setInsertionPoint(pbExit);

  // This vector will contain all the materialized return elements.
  SmallVector<SILValue, 8> retElts;
  // This vector will contain all indirect parameter adjoint buffers.
  SmallVector<SILValue, 4> indParamAdjoints;

  auto origParams = getOriginal().getArgumentsWithoutIndirectResults();

  // Materializes the return element corresponding to the parameter
  // `parameterIndex` into the `retElts` vector.
  auto addRetElt = [&](unsigned parameterIndex) -> void {
    auto origParam = origParams[parameterIndex];
    if (origParam->getType().isObject()) {
      auto pbVal = getAdjointValue(origEntry, origParam);
      auto val = materializeAdjointDirect(pbVal, pbLoc);
      auto newVal = builder.emitCopyValueOperation(pbLoc, val);
      retElts.push_back(newVal);
    } else {
      auto adjBuf = getAdjointBuffer(origEntry, origParam);
      indParamAdjoints.push_back(adjBuf);
    }
  };
  // Collect differentiation parameter adjoints.
  for (auto i : getIndices().parameters->getIndices())
    addRetElt(i);

  // Copy them to adjoint indirect results.
  assert(indParamAdjoints.size() == getPullback().getIndirectResults().size() &&
         "Indirect parameter adjoint count mismatch");
  for (auto pair : zip(indParamAdjoints, getPullback().getIndirectResults())) {
    auto source = std::get<0>(pair);
    auto *dest = std::get<1>(pair);
    builder.createCopyAddr(pbLoc, source, dest, IsTake, IsInitialization);
    // Prevent source buffer from being deallocated, since the underlying
    // value is moved.
    destroyedLocalAllocations.insert(source);
  }

  // Emit cleanups for all local values.
  cleanUpTemporariesForBlock(pbExit, pbLoc);
  // Deallocate local allocations.
  for (auto alloc : functionLocalAllocations) {
    // Assert that local allocations have at least one use.
    // Buffers should not be allocated needlessly.
    assert(!alloc->use_empty());
    if (!destroyedLocalAllocations.count(alloc)) {
      builder.emitDestroyAddrAndFold(pbLoc, alloc);
      destroyedLocalAllocations.insert(alloc);
    }
    builder.createDeallocStack(pbLoc, alloc);
  }
  builder.createReturn(pbLoc, joinElements(retElts, builder, pbLoc));

#ifndef NDEBUG
    bool leakFound = false;
    // Ensure all temporaries have been cleaned up.
    for (auto &bb : pullback) {
      for (auto temp : blockTemporaries[&bb]) {
        if (blockTemporaries[&bb].count(temp)) {
          leakFound = true;
          getADDebugStream() << "Found leaked temporary:\n" << temp;
        }
      }
    }
    // Ensure all local allocations have been cleaned up.
    for (auto localAlloc : functionLocalAllocations) {
      if (!destroyedLocalAllocations.count(localAlloc)) {
        leakFound = true;
        getADDebugStream() << "Found leaked local buffer:\n" << localAlloc;
      }
    }
    assert(!leakFound && "Leaks found!");
#endif

    LLVM_DEBUG(getADDebugStream() << "Generated pullback for "
                                  << original.getName() << ":\n" << pullback);
    return errorOccurred;
}

void PullbackEmitter::emitZeroDerivativesForNonvariedResult(
    SILValue origNonvariedResult) {
  auto &pullback = getPullback();
  auto pbLoc = getPullback().getLocation();
  /*
  // TODO(TF-788): Re-enable non-varied result warning.
  // Emit fixit if original non-varied result has a valid source location.
  auto startLoc = origNonvariedResult.getLoc().getStartSourceLoc();
  auto endLoc = origNonvariedResult.getLoc().getEndSourceLoc();
  if (startLoc.isValid() && endLoc.isValid()) {
    getContext().diagnose(startLoc, diag::autodiff_nonvaried_result_fixit)
        .fixItInsert(startLoc, "withoutDerivative(at:")
        .fixItInsertAfter(endLoc, ")");
  }
  */
  LLVM_DEBUG(getADDebugStream() << getOriginal().getName()
                                << " has non-varied result, returning zero"
                                   " for all pullback results\n");
  auto *pullbackEntry = pullback.createBasicBlock();
  createEntryArguments(&pullback);
  builder.setInsertionPoint(pullbackEntry);
  // Destroy all owned arguments.
  for (auto *arg : pullbackEntry->getArguments())
    if (arg->getOwnershipKind() == ValueOwnershipKind::Owned)
      builder.emitDestroyOperation(pbLoc, arg);
  // Return zero for each result.
  SmallVector<SILValue, 4> directResults;
  auto indirectResultIt = pullback.getIndirectResults().begin();
  for (auto resultInfo : pullback.getLoweredFunctionType()->getResults()) {
    auto resultType =
        pullback.mapTypeIntoContext(resultInfo.getInterfaceType())
            ->getCanonicalType();
    if (resultInfo.isFormalDirect())
      directResults.push_back(emitZeroDirect(resultType, pbLoc));
    else
      emitZeroIndirect(resultType, *indirectResultIt++, pbLoc);
  }
  builder.createReturn(pbLoc, joinElements(directResults, builder, pbLoc));
  LLVM_DEBUG(getADDebugStream()
             << "Generated pullback for " << getOriginal().getName() << ":\n"
             << pullback);
}

SILBasicBlock *PullbackEmitter::buildPullbackSuccessor(
    SILBasicBlock *origBB, SILBasicBlock *origPredBB,
    SmallDenseMap<SILValue, TrampolineBlockSet> &pullbackTrampolineBlockMap) {
  // Get the pullback block and optional pullback trampoline block of the
  // predecessor block.
  auto *pullbackBB = getPullbackBlock(origPredBB);
  auto *pullbackTrampolineBB = getPullbackTrampolineBlock(origPredBB, origBB);
  // If the predecessor block does not have a corresponding pullback
  // trampoline block, then the pullback successor is the pullback block.
  if (!pullbackTrampolineBB)
    return pullbackBB;

  // Otherwise, the pullback successor is the pullback trampoline block,
  // which branches to the pullback block and propagates adjoint values of
  // active values.
  assert(pullbackTrampolineBB->getNumArguments() == 1);
  auto loc = origBB->getParent()->getLocation();
  SmallVector<SILValue, 8> trampolineArguments;
  // Propagate adjoint values/buffers of active values/buffers to
  // predecessor blocks.
  auto &predBBActiveValues = activeValues[origPredBB];
  for (auto activeValue : predBBActiveValues) {
    LLVM_DEBUG(getADDebugStream()
               << "Propagating adjoint of active value " << activeValue
               << " to predecessors' pullback blocks\n");
    if (activeValue->getType().isObject()) {
      auto activeValueAdj = getAdjointValue(origBB, activeValue);
      auto concreteActiveValueAdj =
          materializeAdjointDirect(activeValueAdj, loc);

      if (!pullbackTrampolineBlockMap.count(concreteActiveValueAdj)) {
        concreteActiveValueAdj =
            builder.emitCopyValueOperation(loc, concreteActiveValueAdj);
        setAdjointValue(origBB, activeValue,
                        makeConcreteAdjointValue(concreteActiveValueAdj));
      }
      auto insertion = pullbackTrampolineBlockMap.try_emplace(
          concreteActiveValueAdj, TrampolineBlockSet());
      auto &blockSet = insertion.first->getSecond();
      blockSet.insert(pullbackTrampolineBB);
      trampolineArguments.push_back(concreteActiveValueAdj);

      // If the pullback block does not yet have a registered adjoint
      // value for the active value, set the adjoint value to the
      // forwarded adjoint value argument.
      // TODO: Hoist this logic out of loop over predecessor blocks to
      // remove the `hasAdjointValue` check.
      if (!hasAdjointValue(origPredBB, activeValue)) {
        auto *pullbackBBArg =
            getActiveValuePullbackBlockArgument(origPredBB, activeValue);
        auto forwardedArgAdj = makeConcreteAdjointValue(pullbackBBArg);
        setAdjointValue(origPredBB, activeValue, forwardedArgAdj);
      }
    } else {
      // Propagate adjoint buffers using `copy_addr`.
      auto adjBuf = getAdjointBuffer(origBB, activeValue);
      auto predAdjBuf = getAdjointBuffer(origPredBB, activeValue);
      builder.createCopyAddr(loc, adjBuf, predAdjBuf, IsNotTake,
                             IsNotInitialization);
    }
  }
  // Propagate pullback struct argument.
  SILBuilder pullbackTrampolineBBBuilder(pullbackTrampolineBB);
  auto *predPBStructVal = pullbackTrampolineBB->getArguments().front();
  auto boxType = dyn_cast<SILBoxType>(predPBStructVal->getType().getASTType());
  if (!boxType) {
    trampolineArguments.push_back(predPBStructVal);
  } else {
    auto *projectBox = pullbackTrampolineBBBuilder.createProjectBox(
        loc, predPBStructVal, /*index*/ 0);
    auto loaded = pullbackTrampolineBBBuilder.emitLoadValueOperation(
        loc, projectBox, LoadOwnershipQualifier::Copy);
    pullbackTrampolineBBBuilder.emitDestroyValueOperation(loc, predPBStructVal);
    trampolineArguments.push_back(loaded);
  }
  // Branch from pullback trampoline block to pullback block.
  pullbackTrampolineBBBuilder.createBranch(loc, pullbackBB,
                                           trampolineArguments);
  return pullbackTrampolineBB;
}

void PullbackEmitter::visitSILBasicBlock(SILBasicBlock *bb) {
  auto pbLoc = getPullback().getLocation();
  // Get the corresponding pullback basic block.
  auto *pbBB = getPullbackBlock(bb);
  builder.setInsertionPoint(pbBB);

  LLVM_DEBUG({
    auto &s = getADDebugStream()
              << "Original bb" + std::to_string(bb->getDebugID())
              << ": To differentiate or not to differentiate?\n";
    for (auto &inst : llvm::reverse(*bb)) {
      s << (getPullbackInfo().shouldDifferentiateInstruction(&inst) ? "[x] "
                                                                    : "[ ] ")
        << inst;
    }
  });

  // Visit each instruction in reverse order.
  for (auto &inst : llvm::reverse(*bb)) {
    if (!getPullbackInfo().shouldDifferentiateInstruction(&inst))
      continue;
    // Differentiate instruction.
    visit(&inst);
    if (errorOccurred)
      return;
  }

  // Emit a branching terminator for the block.
  // If the original block is the original entry, then the pullback block is
  // the pullback exit. This is handled specially in `PullbackEmitter::run()`,
  // so we leave the block non-terminated.
  if (bb->isEntry())
    return;

  // Otherwise, add a `switch_enum` terminator for non-exit
  // pullback blocks.
  // 1. Get the pullback struct pullback block argument.
  // 2. Extract the predecessor enum value from the pullback struct value.
  auto *predEnum = getPullbackInfo().getBranchingTraceDecl(bb);
  auto *predEnumField = getPullbackInfo().lookUpLinearMapStructEnumField(bb);
  auto predEnumVal = getPullbackStructElement(bb, predEnumField);

  // Propagate adjoint values from active basic block arguments to
  // incoming values (predecessor terminator operands).
  for (auto *bbArg : bb->getArguments()) {
    if (!getActivityInfo().isActive(bbArg, getIndices()))
      continue;
    // Get predecessor terminator operands.
    SmallVector<std::pair<SILBasicBlock *, SILValue>, 4> incomingValues;
    bbArg->getSingleTerminatorOperands(incomingValues);
    // Materialize adjoint value of active basic block argument, create a
    // copy, and set copy as adjoint value of incoming values.
    auto bbArgAdj = getAdjointValue(bb, bbArg);
    auto concreteBBArgAdj = materializeAdjoint(bbArgAdj, pbLoc);
    auto concreteBBArgAdjCopy =
        builder.emitCopyValueOperation(pbLoc, concreteBBArgAdj);
    for (auto pair : incomingValues) {
      auto *predBB = std::get<0>(pair);
      auto incomingValue = std::get<1>(pair);
      blockTemporaries[getPullbackBlock(predBB)].insert(concreteBBArgAdjCopy);
      setAdjointValue(predBB, incomingValue,
                      makeConcreteAdjointValue(concreteBBArgAdjCopy));
    }
  }

  // 3. Build the pullback successor cases for the `switch_enum`
  //    instruction. The pullback successors correspond to the predecessors
  //    of the current block.
  SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 4>
      pullbackSuccessorCases;
  // A map from active values' adjoint values to the trampoline blocks that
  // are using them.
  SmallDenseMap<SILValue, TrampolineBlockSet> pullbackTrampolineBlockMap;
  SmallVector<SILBasicBlock *, 8> pullbackSuccBBs;
  for (auto *predBB : bb->getPredecessorBlocks()) {
    auto *pullbackSuccBB =
        buildPullbackSuccessor(bb, predBB, pullbackTrampolineBlockMap);
    pullbackSuccBBs.push_back(pullbackSuccBB);
    auto *enumEltDecl =
        getPullbackInfo().lookUpBranchingTraceEnumElement(predBB, bb);
    pullbackSuccessorCases.push_back({enumEltDecl, pullbackSuccBB});
  }
  // Values are trampolined by only a subset of pullback successor blocks.
  // Other successors blocks should destroy the value to balance the reference
  // count.
  for (auto pair : pullbackTrampolineBlockMap) {
    auto value = pair.getFirst();
    // The set of trampoline BBs that are users of `value`.
    auto &userTrampolineBBSet = pair.getSecond();
    // For each pullback successor block that does not trampoline the value,
    // release the value.
    for (auto *pullbackSuccBB : pullbackSuccBBs) {
      if (userTrampolineBBSet.count(pullbackSuccBB))
        continue;
      SILBuilder builder(pullbackSuccBB->begin());
      builder.emitDestroyValueOperation(pbLoc, value);
    }
  }
  // Emit cleanups for all block-local temporaries.
  cleanUpTemporariesForBlock(pbBB, pbLoc);
  // Branch to pullback successor blocks.
  assert(pullbackSuccessorCases.size() == predEnum->getNumElements());
  builder.createSwitchEnum(pbLoc, predEnumVal, /*DefaultBB*/ nullptr,
                           pullbackSuccessorCases);
}

void PullbackEmitter::visit(SILInstruction *inst) {
  if (errorOccurred)
    return;

  LLVM_DEBUG(getADDebugStream() << "PullbackEmitter visited:\n[ORIG]" << *inst);
#ifndef NDEBUG
    auto beforeInsertion = std::prev(builder.getInsertionPoint());
#endif
    SILInstructionVisitor::visit(inst);
    LLVM_DEBUG({
      auto &s = llvm::dbgs() << "[ADJ] Emitted in pullback:\n";
      auto afterInsertion = builder.getInsertionPoint();
      for (auto it = ++beforeInsertion; it != afterInsertion; ++it)
        s << *it;
    });
}

void PullbackEmitter::visitSILInstruction(SILInstruction *inst) {
  LLVM_DEBUG(getADDebugStream()
             << "Unhandled instruction in PullbackEmitter: " << *inst);
  getContext().emitNondifferentiabilityError(
      inst, getInvoker(), diag::autodiff_expression_not_differentiable_note);
  errorOccurred = true;
}

AllocStackInst *PullbackEmitter::getArrayAdjointElementBuffer(
    SILValue arrayAdjoint, int eltIndex, SILType eltTanType, SILLocation loc) {
  // Get `function_ref` and generic signature of
  // `Array.TangentVector.subscript.getter`.
  auto arrayTanType = arrayAdjoint->getType().getASTType();
  auto *arrayTanStructDecl = arrayTanType->getStructOrBoundGenericStruct();
  auto subscriptLookup =
      arrayTanStructDecl->lookupDirect(DeclBaseName::createSubscript());
  auto *subscriptDecl = cast<SubscriptDecl>(subscriptLookup.front());
  auto *subscriptGetterDecl = subscriptDecl->getAccessor(AccessorKind::Get);
  assert(subscriptGetterDecl && "No `Array.TangentVector.subscript` getter");
  SILOptFunctionBuilder fb(getContext().getTransform());
  auto *subscriptGetterFn = fb.getOrCreateFunction(
      loc, SILDeclRef(subscriptGetterDecl), NotForDefinition);
  // %subscript_fn = function_ref @Array.TangentVector<T>.subscript.getter
  auto *subscriptFnRef = builder.createFunctionRef(loc, subscriptGetterFn);
  auto subscriptFnGenSig =
      subscriptGetterFn->getLoweredFunctionType()->getSubstGenericSignature();
  // Apply `Array.TangentVector.subscript.getter` to get array element adjoint
  // buffer.
  auto &ctx = builder.getASTContext();
  auto eltTanAstType = eltTanType.getASTType();
  // %index_literal = integer_literal $Builtin.Int64, <index>
  auto *eltIndexLiteral = builder.createIntegerLiteral(
      loc, SILType::getBuiltinIntegerType(64, ctx), eltIndex);
  auto intType = SILType::getPrimitiveObjectType(
      ctx.getIntDecl()->getDeclaredType()->getCanonicalType());
  // %index_int = struct $Int (%index_literal)
  auto *eltIndexInt = builder.createStruct(loc, intType, {eltIndexLiteral});
  auto *swiftModule = getModule().getSwiftModule();
  auto *diffProto = ctx.getProtocol(KnownProtocolKind::Differentiable);
  auto diffConf = swiftModule->lookupConformance(eltTanAstType, diffProto);
  assert(!diffConf.isInvalid() && "Missing conformance to `Differentiable`");
  auto *addArithProto = ctx.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto addArithConf =
      swiftModule->lookupConformance(eltTanAstType, addArithProto);
  assert(!addArithConf.isInvalid() &&
         "Missing conformance to `AdditiveArithmetic`");
  auto subMap = SubstitutionMap::get(subscriptFnGenSig, {eltTanAstType},
                                     {addArithConf, diffConf});
  // %elt_adj = alloc_stack $T.TangentVector
  auto *eltAdjBuffer = builder.createAllocStack(loc, eltTanType);
  // apply %subscript_fn<T.TangentVector>(%elt_adj, %index_int, %array_adj)
  builder.createApply(loc, subscriptFnRef, subMap,
                      {eltAdjBuffer, eltIndexInt, arrayAdjoint});
  return eltAdjBuffer;
}

void PullbackEmitter::accumulateArrayElementAdjointDirect(
    StoreInst *si, AllocStackInst *eltAdjBuffer) {
  auto eltAdjValue = builder.emitLoadValueOperation(
      si->getLoc(), eltAdjBuffer, LoadOwnershipQualifier::Take);
  recordTemporary(eltAdjValue);
  addAdjointValue(si->getParent(), si->getSrc(),
                  makeConcreteAdjointValue(eltAdjValue), si->getLoc());
  builder.createDeallocStack(si->getLoc(), eltAdjBuffer);
}

void PullbackEmitter::accumulateArrayElementAdjointIndirect(
    CopyAddrInst *cai, AllocStackInst *eltAdjBuffer) {
  addToAdjointBuffer(cai->getParent(), cai->getSrc(), eltAdjBuffer,
                     cai->getLoc());
  builder.emitDestroyAddrAndFold(cai->getLoc(), eltAdjBuffer);
  builder.createDeallocStack(cai->getLoc(), eltAdjBuffer);
}

void PullbackEmitter::accumulateArrayElementAdjoint(SILInstruction *inst) {
  assert(isa<StoreInst>(inst) ||
         isa<CopyAddrInst>(inst) &&
             "Expected only `store` or `copy_addr` to "
             "`array.uninitialized_intrinsic` result address");
  LLVM_DEBUG(getADDebugStream()
             << "Visiting array initialization element store instruction:\n"
             << *inst);
  // Get the source and destination of the `store` or `copy_addr`.
  SILValue src;
  SILValue dest;
  if (auto *si = dyn_cast<StoreInst>(inst)) {
    src = si->getSrc();
    dest = si->getDest();
  } else if (auto *cai = dyn_cast<CopyAddrInst>(inst)) {
    src = cai->getSrc();
    dest = cai->getDest();
  }
  // Get the array element index of the result address.
  int eltIndex = 0;
  if (auto *iai = dyn_cast<IndexAddrInst>(dest->getDefiningInstruction())) {
    auto *ili = cast<IntegerLiteralInst>(iai->getIndex());
    eltIndex = ili->getValue().getLimitedValue();
  } else {
    assert(isa<PointerToAddressInst>(dest->getDefiningInstruction()));
  }
  // Get the array adjoint value.
  SILValue arrayAdjoint;
  auto *ai = getAllocateUninitializedArrayIntrinsicElementAddress(dest);
  assert(ai && "Expected `array.uninitialized_intrinsic` application");
  for (auto use : ai->getUses()) {
    auto *dti = dyn_cast<DestructureTupleInst>(use->getUser());
    if (!dti)
      continue;
    // The first `destructure_tuple` result is the `Array` value.
    auto arrayValue = dti->getResult(0);
    arrayAdjoint =
        getAdjointValue(ai->getParent(), arrayValue).getConcreteValue();
  }
  assert(arrayAdjoint && "Array does not have adjoint value");
  // Apply `Array.TangentVector.subscript` to get array element adjoint value.
  auto eltTanType = getRemappedTangentType(src->getType());
  auto *eltAdjBuffer = getArrayAdjointElementBuffer(arrayAdjoint, eltIndex,
                                                    eltTanType, ai->getLoc());
  // Accumulate array element adjoint into source's adjoint.
  if (auto *si = dyn_cast<StoreInst>(inst)) {
    accumulateArrayElementAdjointDirect(si, eltAdjBuffer);
  } else if (auto *cai = dyn_cast<CopyAddrInst>(inst)) {
    accumulateArrayElementAdjointIndirect(cai, eltAdjBuffer);
  }
}

void PullbackEmitter::visitApplyInst(ApplyInst *ai) {
  assert(getPullbackInfo().shouldDifferentiateApplyInst(ai));
  // Skip `array.uninitialized_intrinsic` intrinsic applications, which have
  // special `store` and `copy_addr` support.
  if (isArrayLiteralIntrinsic(ai))
    return;
  // Replace a call to a function with a call to its pullback.
  auto &nestedApplyInfo = getContext().getNestedApplyInfo();
  auto applyInfoLookup = nestedApplyInfo.find(ai);
  // If no `NestedApplyInfo` was found, then this task doesn't need to be
  // differentiated.
  if (applyInfoLookup == nestedApplyInfo.end()) {
    // Must not be active.
    assert(!getActivityInfo().isActive(ai, getIndices()));
    return;
  }
  auto applyInfo = applyInfoLookup->getSecond();

  // Get the pullback.
  auto *field = getPullbackInfo().lookUpLinearMapDecl(ai);
  assert(field);
  auto loc = ai->getLoc();
  auto pullback = getPullbackStructElement(ai->getParent(), field);

  // Get the original result of the `apply` instruction.
  SmallVector<SILValue, 8> args;
  SmallVector<SILValue, 8> origDirectResults;
  forEachApplyDirectResult(ai, [&](SILValue directResult) {
    origDirectResults.push_back(directResult);
  });
  SmallVector<SILValue, 8> origAllResults;
  collectAllActualResultsInTypeOrder(ai, origDirectResults, origAllResults);
  assert(applyInfo.indices.source < origAllResults.size());
  auto origResult = origAllResults[applyInfo.indices.source];
  assert(origResult);
  auto origNumIndRes = ai->getNumIndirectResults();

  auto pullbackType = remapType(pullback->getType()).castTo<SILFunctionType>();

  // Get the seed (i.e. adjoint value of the original result).
  SILValue seed;
  auto *bb = ai->getParent();
  if (origResult->getType().isObject()) {
    // Otherwise, materialize adjoint value of `ai`.
    seed = materializeAdjoint(getAdjointValue(bb, origResult), loc);
  } else {
    seed = getAdjointBuffer(bb, origResult);
  }

  // Create allocations for pullback indirect results.
  SmallVector<AllocStackInst *, 4> pullbackIndirectResults;
  auto actualPullbackType = applyInfo.originalPullbackType
                                ? *applyInfo.originalPullbackType
                                : pullbackType;
  for (auto indRes : actualPullbackType->getIndirectFormalResults()) {
    auto *alloc = builder.createAllocStack(
        loc, remapType(indRes.getSILStorageInterfaceType()));
    pullbackIndirectResults.push_back(alloc);
    args.push_back(alloc);
  }

  // If callee pullback was reabstracted in VJP, reabstract callee pullback.
  if (applyInfo.originalPullbackType) {
    SILOptFunctionBuilder fb(getContext().getTransform());
    auto *thunk = getOrCreateReabstractionThunk(
        fb, getContext().getModule(), loc, &getPullback(), pullbackType,
        *applyInfo.originalPullbackType);
    auto *thunkRef = builder.createFunctionRef(loc, thunk);
    pullback = builder.createPartialApply(
        loc, thunkRef,
        remapSubstitutionMap(thunk->getForwardingSubstitutionMap()), {pullback},
        pullbackType->getCalleeConvention());
  }
  args.push_back(seed);

  // Call the callee pullback.
  auto *pullbackCall = builder.createApply(loc, pullback, SubstitutionMap(),
                                           args, /*isNonThrowing*/ false);
  builder.emitDestroyValueOperation(loc, pullback);

  // Extract all results from `pullbackCall`.
  SmallVector<SILValue, 8> dirResults;
  extractAllElements(pullbackCall, builder, dirResults);
  // Get all results in type-defined order.
  SmallVector<SILValue, 8> allResults;
  collectAllActualResultsInTypeOrder(pullbackCall, dirResults, allResults);
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "All results of the nested pullback call:\n";
    llvm::for_each(allResults, [&](SILValue v) { s << v; });
  });

  // Accumulate adjoints for original differentiation parameters.
  auto allResultsIt = allResults.begin();
  for (unsigned i : applyInfo.indices.parameters->getIndices()) {
    auto origArg = ai->getArgument(origNumIndRes + i);
    auto tan = *allResultsIt++;
    if (tan->getType().isAddress()) {
      addToAdjointBuffer(bb, origArg, tan, loc);
    } else {
      if (origArg->getType().isAddress()) {
        auto *tmpBuf = builder.createAllocStack(loc, tan->getType());
        builder.emitStoreValueOperation(loc, tan, tmpBuf,
                                        StoreOwnershipQualifier::Init);
        addToAdjointBuffer(bb, origArg, tmpBuf, loc);
        builder.emitDestroyAddrAndFold(loc, tmpBuf);
        builder.createDeallocStack(loc, tmpBuf);
      } else {
        recordTemporary(tan);
        addAdjointValue(bb, origArg, makeConcreteAdjointValue(tan), loc);
      }
    }
  }
  // Destroy unused pullback direct results. Needed for pullback results from
  // VJPs extracted from `@differentiable` function callees, where the
  // `@differentiable` function's differentiation parameter indices are a
  // superset of the active `apply` parameter indices.
  while (allResultsIt != allResults.end()) {
    auto unusedPullbackDirectResult = *allResultsIt++;
    if (unusedPullbackDirectResult->getType().isAddress())
      continue;
    builder.emitDestroyValueOperation(loc, unusedPullbackDirectResult);
  }
  // Destroy and deallocate pullback indirect results.
  for (auto *alloc : llvm::reverse(pullbackIndirectResults)) {
    builder.emitDestroyAddrAndFold(loc, alloc);
    builder.createDeallocStack(loc, alloc);
  }
}

void PullbackEmitter::visitStructInst(StructInst *si) {
  auto *bb = si->getParent();
  auto loc = si->getLoc();
  auto *structDecl = si->getStructDecl();
  auto av = getAdjointValue(bb, si);
  switch (av.getKind()) {
  case AdjointValueKind::Zero:
    for (auto *field : structDecl->getStoredProperties()) {
      auto fv = si->getFieldValue(field);
      addAdjointValue(
          bb, fv, makeZeroAdjointValue(getRemappedTangentType(fv->getType())),
          loc);
    }
    break;
  case AdjointValueKind::Concrete: {
    auto adjStruct = materializeAdjointDirect(std::move(av), loc);
    // Find the struct `TangentVector` type.
    auto structTy = remapType(si->getType()).getASTType();
    auto tangentVectorTy =
        getTangentSpace(structTy)->getType()->getCanonicalType();
    assert(
        !getModule()
             .Types
             .getTypeLowering(tangentVectorTy, TypeExpansionContext::minimal())
             .isAddressOnly());
    auto *tangentVectorDecl = tangentVectorTy->getStructOrBoundGenericStruct();
    assert(tangentVectorDecl);

    auto *dti = builder.createDestructureStruct(si->getLoc(), adjStruct);
    // Accumulate adjoints for the fields of the `struct` operand.
    unsigned fieldIndex = 0;
    for (auto it = structDecl->getStoredProperties().begin();
         it != structDecl->getStoredProperties().end(); ++it, ++fieldIndex) {
      VarDecl *field = *it;
      if (field->getAttrs().hasAttribute<NoDerivativeAttr>())
        continue;
      // Find the corresponding field in the tangent space.
      VarDecl *tanField = nullptr;
      if (tangentVectorDecl == structDecl)
        tanField = field;
      // Otherwise, look up the field by name.
      else {
        auto tanFieldLookup = tangentVectorDecl->lookupDirect(field->getName());
        if (tanFieldLookup.empty()) {
          getContext().emitNondifferentiabilityError(
              si, getInvoker(),
              diag::autodiff_stored_property_no_corresponding_tangent,
              tangentVectorDecl->getNameStr(), field->getNameStr());
          errorOccurred = true;
          return;
        }
        tanField = cast<VarDecl>(tanFieldLookup.front());
      }
      assert(tanField);
      auto tanElt = dti->getResult(fieldIndex);
      addAdjointValue(bb, si->getFieldValue(field),
                      makeConcreteAdjointValue(tanElt), si->getLoc());
    }
    break;
  }
  case AdjointValueKind::Aggregate: {
    // Note: All user-called initializations go through the calls to the
    // initializer, and synthesized initializers only have one level of struct
    // formation which will not result into any aggregate adjoint valeus.
    llvm_unreachable("Aggregate adjoint values should not occur for `struct` "
                     "instructions");
  }
  }
}

void PullbackEmitter::visitStructExtractInst(StructExtractInst *sei) {
  assert(!sei->getField()->getAttrs().hasAttribute<NoDerivativeAttr>() &&
         "`struct_extract` with `@noDerivative` field should not be "
         "differentiated; activity analysis should not marked as varied");
  auto *bb = sei->getParent();
  auto structTy = remapType(sei->getOperand()->getType()).getASTType();
  auto tangentVectorTy =
      getTangentSpace(structTy)->getType()->getCanonicalType();
  assert(!getModule()
              .Types
              .getTypeLowering(tangentVectorTy, TypeExpansionContext::minimal())
              .isAddressOnly());
  auto tangentVectorSILTy = SILType::getPrimitiveObjectType(tangentVectorTy);
  auto *tangentVectorDecl = tangentVectorTy->getStructOrBoundGenericStruct();
  assert(tangentVectorDecl);
  // Find the corresponding field in the tangent space.
  VarDecl *tanField = nullptr;
  // If the tangent space is the original struct, then field is the same.
  if (tangentVectorDecl == sei->getStructDecl())
    tanField = sei->getField();
  // Otherwise, look up the field by name.
  else {
    auto tanFieldLookup =
        tangentVectorDecl->lookupDirect(sei->getField()->getName());
    if (tanFieldLookup.empty()) {
      getContext().emitNondifferentiabilityError(
          sei, getInvoker(),
          diag::autodiff_stored_property_no_corresponding_tangent,
          sei->getStructDecl()->getNameStr(), sei->getField()->getNameStr());
      errorOccurred = true;
      return;
    }
    tanField = cast<VarDecl>(tanFieldLookup.front());
  }
  // Accumulate adjoint for the `struct_extract` operand.
  auto av = getAdjointValue(bb, sei);
  switch (av.getKind()) {
  case AdjointValueKind::Zero:
    addAdjointValue(bb, sei->getOperand(),
                    makeZeroAdjointValue(tangentVectorSILTy), sei->getLoc());
    break;
  case AdjointValueKind::Concrete:
  case AdjointValueKind::Aggregate: {
    SmallVector<AdjointValue, 8> eltVals;
    for (auto *field : tangentVectorDecl->getStoredProperties()) {
      if (field == tanField) {
        eltVals.push_back(av);
      } else {
        auto substMap = tangentVectorTy->getMemberSubstitutionMap(
            field->getModuleContext(), field);
        auto fieldTy = field->getType().subst(substMap);
        auto fieldSILTy = getContext().getTypeConverter().getLoweredType(
            fieldTy, TypeExpansionContext::minimal());
        assert(fieldSILTy.isObject());
        eltVals.push_back(makeZeroAdjointValue(fieldSILTy));
      }
    }
    addAdjointValue(bb, sei->getOperand(),
                    makeAggregateAdjointValue(tangentVectorSILTy, eltVals),
                    sei->getLoc());
  }
  }
}

void PullbackEmitter::visitTupleInst(TupleInst *ti) {
  auto *bb = ti->getParent();
  auto av = getAdjointValue(bb, ti);
  switch (av.getKind()) {
  case AdjointValueKind::Zero:
    for (auto elt : ti->getElements()) {
      if (!getTangentSpace(elt->getType().getASTType()))
        continue;
      addAdjointValue(
          bb, elt, makeZeroAdjointValue(getRemappedTangentType(elt->getType())),
          ti->getLoc());
    }
    break;
  case AdjointValueKind::Concrete: {
    auto adjVal = av.getConcreteValue();
    unsigned adjIdx = 0;
    auto adjValCopy = builder.emitCopyValueOperation(ti->getLoc(), adjVal);
    SmallVector<SILValue, 4> adjElts;
    if (!adjVal->getType().getAs<TupleType>()) {
      recordTemporary(adjValCopy);
      adjElts.push_back(adjValCopy);
    } else {
      auto *dti = builder.createDestructureTuple(ti->getLoc(), adjValCopy);
      for (auto adjElt : dti->getResults())
        recordTemporary(adjElt);
      adjElts.append(dti->getResults().begin(), dti->getResults().end());
    }
    // Accumulate adjoints for `tuple` operands, skipping the
    // non-differentiable ones.
    for (auto i : range(ti->getNumOperands())) {
      if (!getTangentSpace(ti->getOperand(i)->getType().getASTType()))
        continue;
      auto adjElt = adjElts[adjIdx++];
      addAdjointValue(bb, ti->getOperand(i), makeConcreteAdjointValue(adjElt),
                      ti->getLoc());
    }
    break;
  }
  case AdjointValueKind::Aggregate:
    unsigned adjIdx = 0;
    for (auto i : range(ti->getElements().size())) {
      if (!getTangentSpace(ti->getElement(i)->getType().getASTType()))
        continue;
      addAdjointValue(bb, ti->getElement(i), av.getAggregateElement(adjIdx++),
                      ti->getLoc());
    }
    break;
  }
}

void PullbackEmitter::visitTupleExtractInst(TupleExtractInst *tei) {
  auto *bb = tei->getParent();
  auto tupleTanTy = getRemappedTangentType(tei->getOperand()->getType());
  auto av = getAdjointValue(bb, tei);
  switch (av.getKind()) {
  case AdjointValueKind::Zero:
    addAdjointValue(bb, tei->getOperand(), makeZeroAdjointValue(tupleTanTy),
                    tei->getLoc());
    break;
  case AdjointValueKind::Aggregate:
  case AdjointValueKind::Concrete: {
    auto tupleTy = tei->getTupleType();
    auto tupleTanTupleTy = tupleTanTy.getAs<TupleType>();
    if (!tupleTanTupleTy) {
      addAdjointValue(bb, tei->getOperand(), av, tei->getLoc());
      break;
    }
    SmallVector<AdjointValue, 8> elements;
    unsigned adjIdx = 0;
    for (unsigned i : range(tupleTy->getNumElements())) {
      if (!getTangentSpace(
              tupleTy->getElement(i).getType()->getCanonicalType()))
        continue;
      if (tei->getFieldNo() == i)
        elements.push_back(av);
      else
        elements.push_back(makeZeroAdjointValue(
            getRemappedTangentType(SILType::getPrimitiveObjectType(
                tupleTanTupleTy->getElementType(adjIdx++)
                    ->getCanonicalType()))));
    }
    if (elements.size() == 1) {
      addAdjointValue(bb, tei->getOperand(), elements.front(), tei->getLoc());
      break;
    }
    addAdjointValue(bb, tei->getOperand(),
                    makeAggregateAdjointValue(tupleTanTy, elements),
                    tei->getLoc());
    break;
  }
  }
}

void PullbackEmitter::visitDestructureTupleInst(DestructureTupleInst *dti) {
  auto *bb = dti->getParent();
  auto tupleTanTy = getRemappedTangentType(dti->getOperand()->getType());
  SmallVector<AdjointValue, 8> adjValues;
  for (auto origElt : dti->getResults()) {
    if (!getTangentSpace(remapType(origElt->getType()).getASTType()))
      continue;
    adjValues.push_back(getAdjointValue(bb, origElt));
  }
  // Handle tuple tangent type.
  // Add adjoints for every tuple element that has a tangent space.
  if (tupleTanTy.is<TupleType>()) {
    assert(adjValues.size() > 1);
    addAdjointValue(bb, dti->getOperand(),
                    makeAggregateAdjointValue(tupleTanTy, adjValues),
                    dti->getLoc());
  }
  // Handle non-tuple tangent type.
  // Add adjoint for the single tuple element that has a tangent space.
  else {
    assert(adjValues.size() == 1);
    addAdjointValue(bb, dti->getOperand(), adjValues.front(), dti->getLoc());
  }
}

void PullbackEmitter::visitLoadOperation(SingleValueInstruction *inst) {
  assert(isa<LoadInst>(inst) || isa<LoadBorrowInst>(inst));
  auto *bb = inst->getParent();
  auto adjVal =
      materializeAdjointDirect(getAdjointValue(bb, inst), inst->getLoc());
  // Allocate a local buffer and store the adjoint value. This buffer will be
  // used for accumulation into the adjoint buffer.
  auto *localBuf = builder.createAllocStack(inst->getLoc(), adjVal->getType());
  auto copy = builder.emitCopyValueOperation(inst->getLoc(), adjVal);
  builder.emitStoreValueOperation(inst->getLoc(), copy, localBuf,
                                  StoreOwnershipQualifier::Init);
  // Accumulate the adjoint value in the local buffer into the adjoint buffer.
  addToAdjointBuffer(bb, inst->getOperand(0), localBuf, inst->getLoc());
  builder.emitDestroyAddr(inst->getLoc(), localBuf);
  builder.createDeallocStack(inst->getLoc(), localBuf);
}

void PullbackEmitter::visitStoreOperation(SILBasicBlock *bb, SILLocation loc,
                                          SILValue origSrc, SILValue origDest) {
  auto &adjBuf = getAdjointBuffer(bb, origDest);
  auto bufType = remapType(adjBuf->getType());
  auto adjVal =
      builder.emitLoadValueOperation(loc, adjBuf, LoadOwnershipQualifier::Take);
  recordTemporary(adjVal);
  addAdjointValue(bb, origSrc, makeConcreteAdjointValue(adjVal), loc);
  emitZeroIndirect(bufType.getASTType(), adjBuf, loc);
}

void PullbackEmitter::visitStoreInst(StoreInst *si) {
  // Handle `store` to `array.uninitialized_intrinsic` element address
  // specially.
  if (auto *ai =
          getAllocateUninitializedArrayIntrinsicElementAddress(si->getDest())) {
    accumulateArrayElementAdjoint(si);
    return;
  }
  visitStoreOperation(si->getParent(), si->getLoc(), si->getSrc(),
                      si->getDest());
}

void PullbackEmitter::visitCopyAddrInst(CopyAddrInst *cai) {
  // Handle `copy_addr` to `array.uninitialized_intrinsic` element address
  // specially.
  if (auto *ai = getAllocateUninitializedArrayIntrinsicElementAddress(
          cai->getDest())) {
    accumulateArrayElementAdjoint(cai);
    return;
  }
  auto *bb = cai->getParent();
  auto &adjDest = getAdjointBuffer(bb, cai->getDest());
  auto destType = remapType(adjDest->getType());
  addToAdjointBuffer(bb, cai->getSrc(), adjDest, cai->getLoc());
  builder.emitDestroyAddrAndFold(cai->getLoc(), adjDest);
  emitZeroIndirect(destType.getASTType(), adjDest, cai->getLoc());
}

void PullbackEmitter::visitCopyValueInst(CopyValueInst *cvi) {
  auto *bb = cvi->getParent();
  auto adj = getAdjointValue(bb, cvi);
  addAdjointValue(bb, cvi->getOperand(), adj, cvi->getLoc());
}

void PullbackEmitter::visitBeginBorrowInst(BeginBorrowInst *bbi) {
  auto *bb = bbi->getParent();
  auto adj = getAdjointValue(bb, bbi);
  addAdjointValue(bb, bbi->getOperand(), adj, bbi->getLoc());
}

void PullbackEmitter::visitBeginAccessInst(BeginAccessInst *bai) {
  // Check for non-differentiable writes.
  if (bai->getAccessKind() == SILAccessKind::Modify) {
    if (auto *gai = dyn_cast<GlobalAddrInst>(bai->getSource())) {
      getContext().emitNondifferentiabilityError(
          bai, getInvoker(),
          diag::autodiff_cannot_differentiate_writes_to_global_variables);
      errorOccurred = true;
      return;
    }
    if (auto *pbi = dyn_cast<ProjectBoxInst>(bai->getSource())) {
      getContext().emitNondifferentiabilityError(
          bai, getInvoker(),
          diag::autodiff_cannot_differentiate_writes_to_mutable_captures);
      errorOccurred = true;
      return;
    }
  }
}

void PullbackEmitter::visitUnconditionalCheckedCastAddrInst(
    UnconditionalCheckedCastAddrInst *uccai) {
  auto *bb = uccai->getParent();
  auto &adjDest = getAdjointBuffer(bb, uccai->getDest());
  auto &adjSrc = getAdjointBuffer(bb, uccai->getSrc());
  auto destType = remapType(adjDest->getType());
  auto castBuf = builder.createAllocStack(uccai->getLoc(), adjSrc->getType());
  builder.createUnconditionalCheckedCastAddr(
      uccai->getLoc(), adjDest, adjDest->getType().getASTType(), castBuf,
      adjSrc->getType().getASTType());
  addToAdjointBuffer(bb, uccai->getSrc(), castBuf, uccai->getLoc());
  builder.emitDestroyAddrAndFold(uccai->getLoc(), castBuf);
  builder.createDeallocStack(uccai->getLoc(), castBuf);
  emitZeroIndirect(destType.getASTType(), adjDest, uccai->getLoc());
}

#define NOT_DIFFERENTIABLE(INST, DIAG)                                         \
  void PullbackEmitter::visit##INST##Inst(INST##Inst *inst) {                  \
    getContext().emitNondifferentiabilityError(inst, getInvoker(),             \
                                               diag::DIAG);                    \
    errorOccurred = true;                                                      \
    return;                                                                    \
  }
NOT_DIFFERENTIABLE(RefElementAddr, autodiff_class_property_not_supported)
#undef NOT_DIFFERENTIABLE


AdjointValue PullbackEmitter::makeZeroAdjointValue(SILType type) {
  return AdjointValue::createZero(allocator, remapType(type));
}

AdjointValue
PullbackEmitter::makeConcreteAdjointValue(SILValue value) {
  return AdjointValue::createConcrete(allocator, value);
}

template<typename EltRange>
AdjointValue PullbackEmitter::makeAggregateAdjointValue(
    SILType type, EltRange elements) {
  AdjointValue *buf = reinterpret_cast<AdjointValue *>(allocator.Allocate(
      elements.size() * sizeof(AdjointValue), alignof(AdjointValue)));
  MutableArrayRef<AdjointValue> elementsCopy(buf, elements.size());
  std::uninitialized_copy(elements.begin(), elements.end(),
                          elementsCopy.begin());
  return AdjointValue::createAggregate(allocator, remapType(type),
                                       elementsCopy);
}

SILValue PullbackEmitter::materializeAdjointDirect(
    AdjointValue val, SILLocation loc) {
  assert(val.getType().isObject());
  LLVM_DEBUG(getADDebugStream() <<
             "Materializing adjoints for " << val << '\n');
  switch (val.getKind()) {
  case AdjointValueKind::Zero:
    return recordTemporary(emitZeroDirect(val.getType().getASTType(), loc));
  case AdjointValueKind::Aggregate: {
    SmallVector<SILValue, 8> elements;
    for (auto i : range(val.getNumAggregateElements())) {
      auto eltVal = materializeAdjointDirect(val.getAggregateElement(i), loc);
      elements.push_back(builder.emitCopyValueOperation(loc, eltVal));
    }
    if (val.getType().is<TupleType>())
      return recordTemporary(
          builder.createTuple(loc, val.getType(), elements));
    else
      return recordTemporary(
          builder.createStruct(loc, val.getType(), elements));
  }
  case AdjointValueKind::Concrete:
    return val.getConcreteValue();
  }
}

SILValue PullbackEmitter::materializeAdjoint(AdjointValue val,
                                             SILLocation loc) {
  if (val.isConcrete()) {
    LLVM_DEBUG(getADDebugStream()
        << "Materializing adjoint: Value is concrete.\n");
    return val.getConcreteValue();
  }
  LLVM_DEBUG(getADDebugStream() << "Materializing adjoint: Value is "
                                   "non-concrete. Materializing directly.\n");
  return materializeAdjointDirect(val, loc);
}

void PullbackEmitter::materializeAdjointIndirect(
    AdjointValue val, SILValue destBufferAccess, SILLocation loc) {
  switch (val.getKind()) {
  /// Given a `%buf : *T, emit instructions that produce a zero or an aggregate
  /// of zeros of the expected type. When `T` conforms to
  /// `AdditiveArithmetic`, we emit a call to `AdditiveArithmetic.zero`. When
  /// `T` is a builtin float, we emit a `float_literal` instruction.
  /// Otherwise, we assert that `T` must be an aggregate where each element
  /// conforms to `AdditiveArithmetic` or is a builtin float. We expect to emit
  /// a zero for each element and use the appropriate aggregate constructor
  /// instruction (in this case, `tuple`) to produce a tuple. But currently,
  /// since we need indirect passing for aggregate instruction, we just use
  /// `tuple_element_addr` to get element buffers and write elements to them.
  case AdjointValueKind::Zero:
    emitZeroIndirect(val.getSwiftType(), destBufferAccess, loc);
    break;
  /// Given a `%buf : *(T0, T1, T2, ...)` or `%buf : *Struct` recursively emit
  /// instructions to materialize the symbolic tuple or struct, filling the
  /// buffer.
  case AdjointValueKind::Aggregate: {
    if (auto *tupTy = val.getSwiftType()->getAs<TupleType>()) {
      for (auto idx : range(val.getNumAggregateElements())) {
        auto eltTy = SILType::getPrimitiveAddressType(
            tupTy->getElementType(idx)->getCanonicalType());
        auto *eltBuf =
            builder.createTupleElementAddr(loc, destBufferAccess, idx, eltTy);
        materializeAdjointIndirect(
            val.getAggregateElement(idx), eltBuf, loc);
      }
    } else if (auto *structDecl =
                   val.getSwiftType()->getStructOrBoundGenericStruct()) {
      auto fieldIt = structDecl->getStoredProperties().begin();
      for (unsigned i = 0; fieldIt != structDecl->getStoredProperties().end();
           ++fieldIt, ++i) {
        auto eltBuf =
            builder.createStructElementAddr(loc, destBufferAccess, *fieldIt);
        materializeAdjointIndirect(
            val.getAggregateElement(i), eltBuf, loc);
      }
    } else {
      llvm_unreachable("Not an aggregate type");
    }
    break;
  }
  /// Value is already materialized!
  case AdjointValueKind::Concrete:
    auto concreteVal = val.getConcreteValue();
    builder.emitStoreValueOperation(loc, concreteVal, destBufferAccess,
                                    StoreOwnershipQualifier::Init);
    break;
  }
}

void PullbackEmitter::emitZeroIndirect(CanType type, SILValue bufferAccess,
                                       SILLocation loc) {
  auto tangentSpace = getTangentSpace(type);
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case VectorSpace::Kind::Vector:
    emitZeroIntoBuffer(builder, type, bufferAccess, loc);
    return;
  case VectorSpace::Kind::Tuple: {
    auto tupleType = tangentSpace->getTuple();
    SmallVector<SILValue, 8> zeroElements;
    for (unsigned i : range(tupleType->getNumElements())) {
      auto eltAddr = builder.createTupleElementAddr(loc, bufferAccess, i);
      emitZeroIndirect(tupleType->getElementType(i)->getCanonicalType(),
                       eltAddr, loc);
    }
    return;
  }
  case VectorSpace::Kind::Function: {
    llvm_unreachable(
      "Unimplemented: Emit thunks for abstracting zero initialization");
  }
  }
}

SILValue PullbackEmitter::emitZeroDirect(CanType type, SILLocation loc) {
  auto silType = getModule().Types.getLoweredLoadableType(
      type, TypeExpansionContext::minimal(), getModule());
  auto *buffer = builder.createAllocStack(loc, silType);
  emitZeroIndirect(type, buffer, loc);
  auto loaded = builder.emitLoadValueOperation(
      loc, buffer, LoadOwnershipQualifier::Take);
  builder.createDeallocStack(loc, buffer);
  return loaded;
}

AdjointValue
PullbackEmitter::accumulateAdjointsDirect(AdjointValue lhs, AdjointValue rhs,
                                          SILLocation loc) {
  LLVM_DEBUG(getADDebugStream()
             << "Materializing adjoint directly.\nLHS: " << lhs
             << "\nRHS: " << rhs << '\n');

  switch (lhs.getKind()) {
  // x
  case AdjointValueKind::Concrete: {
    auto lhsVal = lhs.getConcreteValue();
    switch (rhs.getKind()) {
    // x + y
    case AdjointValueKind::Concrete: {
      auto rhsVal = rhs.getConcreteValue();
      auto sum = recordTemporary(accumulateDirect(lhsVal, rhsVal, loc));
      return makeConcreteAdjointValue(sum);
    }
    // x + 0 => x
    case AdjointValueKind::Zero:
      return lhs;
    // x + (y, z) => (x.0 + y, x.1 + z)
    case AdjointValueKind::Aggregate:
      SmallVector<AdjointValue, 8> newElements;
      auto lhsTy = lhsVal->getType().getASTType();
      auto lhsValCopy = builder.emitCopyValueOperation(loc, lhsVal);
      if (auto *tupTy = lhsTy->getAs<TupleType>()) {
        auto elts = builder.createDestructureTuple(loc, lhsValCopy);
        llvm::for_each(elts->getResults(),
                       [this](SILValue result) { recordTemporary(result); });
        for (auto i : indices(elts->getResults())) {
          auto rhsElt = rhs.getAggregateElement(i);
          newElements.push_back(accumulateAdjointsDirect(
              makeConcreteAdjointValue(elts->getResult(i)), rhsElt, loc));
        }
      } else if (auto *structDecl = lhsTy->getStructOrBoundGenericStruct()) {
        auto elts =
            builder.createDestructureStruct(lhsVal.getLoc(), lhsValCopy);
        llvm::for_each(elts->getResults(),
                       [this](SILValue result) { recordTemporary(result); });
        for (unsigned i : indices(elts->getResults())) {
          auto rhsElt = rhs.getAggregateElement(i);
          newElements.push_back(
              accumulateAdjointsDirect(
                  makeConcreteAdjointValue(elts->getResult(i)), rhsElt, loc));
        }
      } else {
        llvm_unreachable("Not an aggregate type");
      }
      return makeAggregateAdjointValue(lhsVal->getType(), newElements);
    }
  }
  // 0
  case AdjointValueKind::Zero:
    // 0 + x => x
    return rhs;
  // (x, y)
  case AdjointValueKind::Aggregate:
    switch (rhs.getKind()) {
    // (x, y) + z => (z.0 + x, z.1 + y)
    case AdjointValueKind::Concrete:
      return accumulateAdjointsDirect(rhs, lhs, loc);
    // x + 0 => x
    case AdjointValueKind::Zero:
      return lhs;
    // (x, y) + (z, w) => (x + z, y + w)
    case AdjointValueKind::Aggregate: {
      SmallVector<AdjointValue, 8> newElements;
      for (auto i : range(lhs.getNumAggregateElements()))
        newElements.push_back(
            accumulateAdjointsDirect(lhs.getAggregateElement(i),
                                     rhs.getAggregateElement(i),
                                     loc));
      return makeAggregateAdjointValue(lhs.getType(), newElements);
    }
    }
  }
}

SILValue PullbackEmitter::accumulateDirect(SILValue lhs, SILValue rhs,
                                           SILLocation loc) {
  // TODO: Optimize for the case when lhs == rhs.
  LLVM_DEBUG(getADDebugStream() <<
             "Emitting adjoint accumulation for lhs: " << lhs <<
             " and rhs: " << rhs);
  assert(lhs->getType() == rhs->getType() && "Adjoints must have equal types!");
  assert(lhs->getType().isObject() && rhs->getType().isObject() &&
         "Adjoint types must be both object types!");
  auto adjointTy = lhs->getType();
  auto adjointASTTy = adjointTy.getASTType();
  auto tangentSpace = getTangentSpace(adjointASTTy);
  auto lhsCopy = builder.emitCopyValueOperation(loc, lhs);
  auto rhsCopy = builder.emitCopyValueOperation(loc, rhs);
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case VectorSpace::Kind::Vector: {
    // Allocate buffers for inputs and output.
    auto *resultBuf = builder.createAllocStack(loc, adjointTy);
    auto *lhsBuf = builder.createAllocStack(loc, adjointTy);
    auto *rhsBuf = builder.createAllocStack(loc, adjointTy);
    // Initialize input buffers.
    builder.emitStoreValueOperation(loc, lhsCopy, lhsBuf,
                                    StoreOwnershipQualifier::Init);
    builder.emitStoreValueOperation(loc, rhsCopy, rhsBuf,
                                    StoreOwnershipQualifier::Init);
    accumulateIndirect(resultBuf, lhsBuf, rhsBuf, loc);
    builder.emitDestroyAddr(loc, lhsBuf);
    builder.emitDestroyAddr(loc, rhsBuf);
    // Deallocate input buffers.
    builder.createDeallocStack(loc, rhsBuf);
    builder.createDeallocStack(loc, lhsBuf);
    auto val = builder.emitLoadValueOperation(
        loc, resultBuf, LoadOwnershipQualifier::Take);
    // Deallocate result buffer.
    builder.createDeallocStack(loc, resultBuf);
    return val;
  }
  case VectorSpace::Kind::Tuple: {
    SmallVector<SILValue, 8> adjElements;
    auto lhsElts = builder.createDestructureTuple(loc, lhsCopy)->getResults();
    auto rhsElts = builder.createDestructureTuple(loc, rhsCopy)->getResults();
    for (auto zipped : llvm::zip(lhsElts, rhsElts))
      adjElements.push_back(
          accumulateDirect(std::get<0>(zipped), std::get<1>(zipped), loc));
    return builder.createTuple(loc, adjointTy, adjElements);
  }
  case VectorSpace::Kind::Function: {
    llvm_unreachable(
        "Unimplemented: Emit thunks for abstracting adjoint accumulation");
  }
  }
}

void PullbackEmitter::accumulateIndirect(
    SILValue resultBufAccess, SILValue lhsBufAccess, SILValue rhsBufAccess,
    SILLocation loc) {
  // TODO: Optimize for the case when lhs == rhs.
  assert(lhsBufAccess->getType() == rhsBufAccess->getType() &&
         "Adjoint values must have same type!");
  assert(lhsBufAccess->getType().isAddress() &&
         rhsBufAccess->getType().isAddress() &&
         "Adjoint values must both have address types!");
  auto adjointTy = lhsBufAccess->getType();
  auto adjointASTTy = adjointTy.getASTType();
  auto *swiftMod = getModule().getSwiftModule();
  auto tangentSpace = adjointASTTy->getAutoDiffAssociatedTangentSpace(
      LookUpConformanceInModule(swiftMod));
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case VectorSpace::Kind::Vector: {
    auto *proto = getContext().getAdditiveArithmeticProtocol();
    auto *combinerFuncDecl = getContext().getPlusDecl();
    // Call the combiner function and return.
    auto adjointParentModule = tangentSpace->getNominal()
        ? tangentSpace->getNominal()->getModuleContext()
        : getModule().getSwiftModule();
    auto confRef = adjointParentModule->lookupConformance(adjointASTTy,
                                                           proto);
    assert(!confRef.isInvalid() &&
           "Missing conformance to `AdditiveArithmetic`");
    SILDeclRef declRef(combinerFuncDecl, SILDeclRef::Kind::Func);
    auto silFnTy = getContext().getTypeConverter().getConstantType(
        TypeExpansionContext::minimal(), declRef);
    // %0 = witness_method @+
    auto witnessMethod = builder.createWitnessMethod(loc, adjointASTTy,
                                                     confRef, declRef,
                                                     silFnTy);
    auto subMap = SubstitutionMap::getProtocolSubstitutions(
        proto, adjointASTTy, confRef);
    // %1 = metatype $T.Type
    auto metatypeType =
        CanMetatypeType::get(adjointASTTy, MetatypeRepresentation::Thick);
    auto metatypeSILType = SILType::getPrimitiveObjectType(metatypeType);
    auto metatype = builder.createMetatype(loc, metatypeSILType);
    // %2 = apply $0(%result, %new, %old, %1)
    builder.createApply(loc, witnessMethod, subMap,
                        {resultBufAccess, rhsBufAccess, lhsBufAccess, metatype},
                        /*isNonThrowing*/ false);
    builder.emitDestroyValueOperation(loc, witnessMethod);
    return;
  }
  case VectorSpace::Kind::Tuple: {
    auto tupleType = tangentSpace->getTuple();
    for (unsigned i : range(tupleType->getNumElements())) {
      auto *destAddr = builder.createTupleElementAddr(loc, resultBufAccess, i);
      auto *eltAddrLHS = builder.createTupleElementAddr(loc, lhsBufAccess, i);
      auto *eltAddrRHS = builder.createTupleElementAddr(loc, rhsBufAccess, i);
      accumulateIndirect(destAddr, eltAddrLHS, eltAddrRHS, loc);
    }
    return;
  }
  case VectorSpace::Kind::Function: {
    llvm_unreachable(
        "Unimplemented: Emit thunks for abstracting adjoint value "
        "accumulation");
  }
  }
}

void PullbackEmitter::accumulateIndirect(SILValue lhsDestAccess,
                                         SILValue rhsAccess, SILLocation loc) {
  assert(lhsDestAccess->getType().isAddress() &&
         rhsAccess->getType().isAddress());
  assert(lhsDestAccess->getFunction() == &getPullback());
  assert(rhsAccess->getFunction() == &getPullback());
  auto type = lhsDestAccess->getType();
  auto astType = type.getASTType();
  auto *swiftMod = getModule().getSwiftModule();
  auto tangentSpace = astType->getAutoDiffAssociatedTangentSpace(
      LookUpConformanceInModule(swiftMod));
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case VectorSpace::Kind::Vector: {
    auto *proto = getContext().getAdditiveArithmeticProtocol();
    auto *accumulatorFuncDecl = getContext().getPlusEqualDecl();
    // Call the combiner function and return.
    auto confRef = swiftMod->lookupConformance(astType, proto);
    assert(!confRef.isInvalid() &&
           "Missing conformance to `AdditiveArithmetic`");
    SILDeclRef declRef(accumulatorFuncDecl, SILDeclRef::Kind::Func);
    auto silFnTy = getContext().getTypeConverter().getConstantType(
        TypeExpansionContext::minimal(), declRef);
    // %0 = witness_method @+=
    auto witnessMethod =
        builder.createWitnessMethod(loc, astType, confRef, declRef, silFnTy);
    auto subMap =
        SubstitutionMap::getProtocolSubstitutions(proto, astType, confRef);
    // %1 = metatype $T.Type
    auto metatypeType =
        CanMetatypeType::get(astType, MetatypeRepresentation::Thick);
    auto metatypeSILType = SILType::getPrimitiveObjectType(metatypeType);
    auto metatype = builder.createMetatype(loc, metatypeSILType);
    // %2 = apply $0(%lhs, %rhs, %1)
    builder.createApply(loc, witnessMethod, subMap,
                        {lhsDestAccess, rhsAccess, metatype},
                        /*isNonThrowing*/ false);
    builder.emitDestroyValueOperation(loc, witnessMethod);
    return;
  }
  case VectorSpace::Kind::Tuple: {
    auto tupleType = tangentSpace->getTuple();
    for (unsigned i : range(tupleType->getNumElements())) {
      auto *destAddr = builder.createTupleElementAddr(loc, lhsDestAccess, i);
      auto *eltAddrRHS = builder.createTupleElementAddr(loc, rhsAccess, i);
      accumulateIndirect(destAddr, eltAddrRHS, loc);
    }
    return;
  }
  case VectorSpace::Kind::Function: {
    llvm_unreachable(
        "Unimplemented: Emit thunks for abstracting adjoint value "
        "accumulation");
  }
  }
}

} // end namespace autodiff
} // end namespace swift
