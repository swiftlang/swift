//===--- PrunedLiveness.cpp - Compute liveness from selected uses ---------===//
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

#include "swift/SIL/PrunedLiveness.h"
#include "swift/AST/TypeExpansionContext.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/ScopedAddressUtils.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;

/// Mark blocks live during a reverse CFG traversal from one specific block
/// containing a user.
void PrunedLiveBlocks::computeUseBlockLiveness(SILBasicBlock *userBB,
                                               unsigned startBitNo,
                                               unsigned endBitNo) {
  // If, we are visiting this block, then it is not already LiveOut. Mark it
  // LiveWithin to indicate a liveness boundary within the block.
  markBlockLive(userBB, startBitNo, endBitNo, LiveWithin);

  SmallVector<IsLive, 8> predLivenessInfo;
  BasicBlockWorklist worklist(userBB->getFunction());
  worklist.push(userBB);

  while (auto *block = worklist.pop()) {
    // The popped `bb` is live; now mark all its predecessors LiveOut.
    //
    // Traversal terminates at any previously visited block, including the
    // blocks initialized as definition blocks.
    for (auto *predBlock : block->getPredecessorBlocks()) {
      SWIFT_DEFER { predLivenessInfo.clear(); };
      getBlockLiveness(predBlock, startBitNo, endBitNo, predLivenessInfo);
      for (unsigned i : indices(predLivenessInfo)) {
        switch (predLivenessInfo[i]) {
        case Dead:
          worklist.pushIfNotVisited(predBlock);
          LLVM_FALLTHROUGH;
        case LiveWithin:
          markBlockLive(predBlock, startBitNo, endBitNo, LiveOut);
          break;
        case LiveOut:
          break;
        }
      }
    }
  }
}

/// Update the current def's liveness based on one specific use instruction.
///
/// Return the updated liveness of the \p use block (LiveOut or LiveWithin).
///
/// Terminators are not live out of the block.
void PrunedLiveBlocks::updateForUse(
    SILInstruction *user, unsigned startBitNo, unsigned endBitNo,
    SmallVectorImpl<IsLive> &resultingLivenessInfo) {
  SWIFT_ASSERT_ONLY(seenUse = true);

  auto *bb = user->getParent();
  getBlockLiveness(bb, startBitNo, endBitNo, resultingLivenessInfo);

  for (auto isLive : resultingLivenessInfo) {
    switch (isLive) {
    case LiveOut:
    case LiveWithin:
      continue;
    case Dead: {
      // This use block has not yet been marked live. Mark it and its
      // predecessor blocks live.
      computeUseBlockLiveness(bb, startBitNo, endBitNo);
      resultingLivenessInfo.clear();
      return getBlockLiveness(bb, startBitNo, endBitNo, resultingLivenessInfo);
    }
    }
    llvm_unreachable("covered switch");
  }
}

//===----------------------------------------------------------------------===//
//                            MARK: PrunedLiveness
//===----------------------------------------------------------------------===//

void PrunedLiveness::updateForUse(SILInstruction *user, bool lifetimeEnding) {
  assert(!empty() && "at least one definition must be initialized");

  liveBlocks.updateForUse(user, 0);
  // Note that a user may use the current value from multiple operands. If any
  // of the uses are non-lifetime-ending, then we must consider the user
  // itself non-lifetime-ending; it cannot be a final destroy point because
  // the value of the non-lifetime-ending operand must be kept alive until the
  // end of the user. Consider a call that takes the same value using
  // different conventions:
  //
  //   apply %f(%val, %val) : $(@guaranteed, @owned) -> ()
  //
  // This call is not considered the end of %val's lifetime. The @owned
  // argument must be copied.
  auto iterAndSuccess = users.insert({user, lifetimeEnding});
  if (!iterAndSuccess.second)
    iterAndSuccess.first->second &= lifetimeEnding;
}

InnerBorrowKind PrunedLiveness::updateForBorrowingOperand(Operand *operand) {
  assert(operand->getOperandOwnership() == OperandOwnership::Borrow);

  // A nested borrow scope is considered a use-point at each scope ending
  // instruction.
  //
  // TODO: Handle reborrowed copies by considering the extended borrow
  // scope. Temporarily bail-out on reborrows because we can't handle uses
  // that aren't dominated by currentDef.
  if (!BorrowingOperand(operand).visitScopeEndingUses([this](Operand *end) {
        if (end->getOperandOwnership() == OperandOwnership::Reborrow) {
          return false;
        }
        updateForUse(end->getUser(), /*lifetimeEnding*/ false);
        return true;
      })) {
    return InnerBorrowKind::Reborrowed;
  }
  return InnerBorrowKind::Contained;
}

AddressUseKind PrunedLiveness::checkAndUpdateInteriorPointer(Operand *operand) {
  assert(operand->getOperandOwnership() == OperandOwnership::InteriorPointer);

  if (auto *svi = dyn_cast<SingleValueInstruction>(operand->getUser())) {
    if (auto scopedAddress = ScopedAddressValue(svi)) {
      scopedAddress.visitScopeEndingUses([this](Operand *end) {
        updateForUse(end->getUser(), /*lifetimeEnding*/ false);
        return true;
      });
      return AddressUseKind::NonEscaping;
    }
  }
  // FIXME: findTransitiveUses should be a visitor so we're not recursively
  // allocating use vectors and potentially merging the use points.
  SmallVector<Operand *, 8> uses;
  auto useKind = InteriorPointerOperand(operand).findTransitiveUses(&uses);
  for (auto *use : uses) {
    updateForUse(use->getUser(), /*lifetimeEnding*/ false);
  }
  return useKind;
}

void PrunedLiveness::extendAcrossLiveness(PrunedLiveness &otherLivesness) {
  // update this liveness for all the interesting users in otherLiveness.
  for (std::pair<SILInstruction *, bool> userAndEnd : otherLivesness.users) {
    updateForUse(userAndEnd.first, userAndEnd.second);
  }
}

llvm::StringRef PrunedLiveBlocks::getStringRef(IsLive isLive) const {
  switch (isLive) {
  case Dead:
    return "Dead";
  case LiveWithin:
    return "LiveWithin";
  case LiveOut:
    return "LiveOut";
  }
}

void PrunedLiveBlocks::print(llvm::raw_ostream &OS) const {
  if (!discoveredBlocks) {
    OS << "No deterministic live block list\n";
  }
  for (auto *block : *discoveredBlocks) {
    block->printAsOperand(OS);
    OS
      << ": " << getStringRef(this->getBlockLiveness(block, 0)) << "\n";
  }
}

void PrunedLiveBlocks::dump() const {
  print(llvm::dbgs());
}

void PrunedLiveness::print(llvm::raw_ostream &OS) const {
  liveBlocks.print(OS);
  for (auto &userAndIsLifetimeEnding : users) {
    if (userAndIsLifetimeEnding.second)
      OS << "lifetime-ending user: ";
    else
      OS << "regular user: ";
    userAndIsLifetimeEnding.first->print(OS);
  }
}

void PrunedLiveness::dump() const {
  print(llvm::dbgs());
}

//===----------------------------------------------------------------------===//
//                           PrunedLivenessBoundary
//===----------------------------------------------------------------------===//

void PrunedLivenessBoundary::print(llvm::raw_ostream &OS) const {
  for (auto *user : lastUsers) {
    OS << "last user: " << *user;
  }
  for (auto *block : boundaryEdges) {
    OS << "boundary edge: ";
    block->printAsOperand(OS);
    OS << "\n";
  }
  if (!deadDefs.empty()) {
    for (auto *deadDef : deadDefs) {
      OS << "dead def: " << *deadDef;
    }
  }
}

void PrunedLivenessBoundary::dump() const {
  print(llvm::dbgs());
}

// TODO: with guaranteed phis, it will be possible to hit this assert:
//   assert(succ->getSinglePredecessorBlock() == predBB);
//
// Once it's possible to test dead guaranteed phis, replace the assert with a
// set to avoid multiple insertions at a merge point:
//   // Control flow merge blocks used as insertion points.
//   BasicBlockSet mergeBlocks;
//
void PrunedLivenessBoundary::visitInsertionPoints(
    llvm::function_ref<void(SILBasicBlock::iterator insertPt)> visitor,
    DeadEndBlocks *deBlocks) {
  for (SILInstruction *user : lastUsers) {
    if (!isa<TermInst>(user)) {
      visitor(std::next(user->getIterator()));
      continue;
    }
    auto *predBB = user->getParent();
    for (SILBasicBlock *succ : predBB->getSuccessors()) {
      if (deBlocks && deBlocks->isDeadEnd(succ))
        continue;

      assert(succ->getSinglePredecessorBlock() == predBB);
      visitor(succ->begin());
    }
  }
  for (SILBasicBlock *edge : boundaryEdges) {
    if (deBlocks && deBlocks->isDeadEnd(edge))
      continue;

    visitor(edge->begin());
  }
  for (SILNode *deadDef : deadDefs) {
    if (auto *arg = dyn_cast<SILArgument>(deadDef))
      visitor(arg->getParent()->begin());
    else
      visitor(std::next(cast<SILInstruction>(deadDef)->getIterator()));
  }
}

//===----------------------------------------------------------------------===//
//                              PrunedLiveRange
//===----------------------------------------------------------------------===//

template <typename LivenessWithDefs>
SimpleLiveRangeSummary PrunedLiveRange<LivenessWithDefs>::updateForDef(SILValue def) {
  SimpleLiveRangeSummary summary;
  // Note: Uses with OperandOwnership::NonUse cannot be considered normal uses
  // for liveness. Otherwise, liveness would need to separately track non-uses
  // everywhere. Non-uses cannot be treated like normal non-lifetime-ending uses
  // because they can occur on both applies, which need to extend liveness to
  // the return point, and on forwarding instructions, like
  // init_existential_ref, which need to consume their use even when
  // type-dependent operands exist.
  for (Operand *use : def->getUses()) {
    switch (use->getOperandOwnership()) {
    case OperandOwnership::NonUse:
      break;
    case OperandOwnership::Borrow:
      summary.meet(updateForBorrowingOperand(use));
      break;
    case OperandOwnership::PointerEscape:
      summary.meet(AddressUseKind::PointerEscape);
      break;
    case OperandOwnership::InteriorPointer:
      summary.meet(checkAndUpdateInteriorPointer(use));
      break;
    case OperandOwnership::ForwardingBorrow: {
      ForwardingOperand(use).visitForwardedValues([&](SILValue result) {
        // Do not include transitive uses with 'none' ownership
        if (result->getOwnershipKind() != OwnershipKind::None) {
          updateForDef(result);
        }
        return true;
      });
      break;
    }
    default:
      updateForUse(use->getUser(), use->isLifetimeEnding());
      break;
    }
  }
  return summary;
}

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::isWithinBoundary(
    SILInstruction *inst) const {
  assert(asImpl().isInitialized());

  auto *block = inst->getParent();
  auto blockLiveness = getBlockLiveness(block);
  if (blockLiveness == PrunedLiveBlocks::Dead)
    return false;

  bool isLive = blockLiveness == PrunedLiveBlocks::LiveOut;

  if (isLive && !asImpl().isDefBlock(block))
    return true;

  // Check if instruction is between a last use and a definition
  for (SILInstruction &it : llvm::reverse(*block)) {
    // the def itself is not within the boundary, so cancel liveness before
    // matching 'inst'.
    if (asImpl().isDef(&it)) {
      isLive = false;
    }
    if (&it == inst) {
      return isLive;
    }
    if (!isLive && isInterestingUser(&it)) {
      isLive = true;
    }
  }
  llvm_unreachable("instruction must be in its parent block");
}

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::areUsesWithinBoundary(
    ArrayRef<Operand *> uses, DeadEndBlocks *deadEndBlocks) const {
  assert(asImpl().isInitialized());

  auto checkDeadEnd = [deadEndBlocks](SILInstruction *inst) {
    return deadEndBlocks && deadEndBlocks->isDeadEnd(inst->getParent());
  };
  for (auto *use : uses) {
    auto *user = use->getUser();
    if (!asImpl().isWithinBoundary(user) && !checkDeadEnd(user))
      return false;
  }
  return true;
}

template <typename LivenessWithDefs>
bool PrunedLiveRange<LivenessWithDefs>::areUsesOutsideBoundary(
    ArrayRef<Operand *> uses, DeadEndBlocks *deadEndBlocks) const {
  assert(asImpl().isInitialized());

  auto checkDeadEnd = [deadEndBlocks](SILInstruction *inst) {
    return deadEndBlocks && deadEndBlocks->isDeadEnd(inst->getParent());
  };
  for (auto *use : uses) {
    auto *user = use->getUser();
    if (asImpl().isWithinBoundary(user) || checkDeadEnd(user))
      return false;
  }
  return true;
}

template <typename LivenessWithDefs>
void PrunedLiveRange<LivenessWithDefs>::computeBoundary(
    PrunedLivenessBoundary &boundary) const {
  assert(asImpl().isInitialized());

  for (SILBasicBlock *block : getDiscoveredBlocks()) {
    // Process each block that has not been visited and is not LiveOut.
    switch (getBlockLiveness(block)) {
    case PrunedLiveBlocks::LiveOut:
      for (SILBasicBlock *succBB : block->getSuccessors()) {
        if (getBlockLiveness(succBB) == PrunedLiveBlocks::Dead) {
          boundary.boundaryEdges.push_back(succBB);
        }
      }
      asImpl().findBoundariesInBlock(block, /*isLiveOut*/ true, boundary);
      break;
    case PrunedLiveBlocks::LiveWithin: {
      asImpl().findBoundariesInBlock(block, /*isLiveOut*/ false, boundary);
      break;
    }
    case PrunedLiveBlocks::Dead:
      llvm_unreachable("All discovered blocks must be live");
    }
  }
}

template <typename LivenessWithDefs>
void PrunedLiveRange<LivenessWithDefs>::computeBoundary(
    PrunedLivenessBoundary &boundary,
    ArrayRef<SILBasicBlock *> postDomBlocks) const {
  assert(asImpl().isInitialized());

  if (postDomBlocks.empty())
    return; // all paths must be dead-ends or infinite loops

  BasicBlockWorklist blockWorklist(postDomBlocks[0]->getParent());

  // Visit each post-dominating block as the starting point for a
  // backward CFG traversal.
  for (auto *block : postDomBlocks) {
    blockWorklist.push(block);
  }
  while (auto *block = blockWorklist.pop()) {
    // Process each block that has not been visited and is not LiveOut.
    switch (getBlockLiveness(block)) {
    case PrunedLiveBlocks::LiveOut:
      asImpl().findBoundariesInBlock(block, /*isLiveOut*/ true, boundary);
      break;
    case PrunedLiveBlocks::LiveWithin: {
      asImpl().findBoundariesInBlock(block, /*isLiveOut*/ false, boundary);
      break;
    }
    case PrunedLiveBlocks::Dead:
      // Continue searching upward to find the pruned liveness boundary.
      for (auto *predBB : block->getPredecessorBlocks()) {
        if (getBlockLiveness(predBB) == PrunedLiveBlocks::LiveOut) {
          boundary.boundaryEdges.push_back(block);
        } else {
          blockWorklist.pushIfNotVisited(predBB);
        }
      }
      break;
    }
  }
}

namespace swift {
template class PrunedLiveRange<SSAPrunedLiveness>;
template class PrunedLiveRange<MultiDefPrunedLiveness>;
} // namespace swift

//===----------------------------------------------------------------------===//
//                             SSAPrunedLiveness
//===----------------------------------------------------------------------===//

/// Given live-within (non-live-out) \p block, find the last user.
void findBoundaryInNonDefBlock(SILBasicBlock *block,
                               PrunedLivenessBoundary &boundary,
                               const PrunedLiveness &liveness) {
  assert(liveness.getBlockLiveness(block) == PrunedLiveBlocks::LiveWithin);

  for (SILInstruction &inst : llvm::reverse(*block)) {
    if (liveness.isInterestingUser(&inst)) {
      boundary.lastUsers.push_back(&inst);
      return;
    }
  }
  llvm_unreachable("live-within block must contain an interesting use");
}

/// Given a live-within \p block that contains an SSA definition, and knowledge
/// that all live uses are dominated by that single definition, find either the
/// last user or a dead def.
///
/// A live range with a single definition cannot have any uses above that
/// definition in the same block. This even holds for unreachable self-loops.
void findBoundaryInSSADefBlock(SILNode *ssaDef,
                               PrunedLivenessBoundary &boundary,
                               const PrunedLiveness &liveness) {
  // defInst is null for argument defs.
  SILInstruction *defInst = dyn_cast<SILInstruction>(ssaDef);
  for (SILInstruction &inst : llvm::reverse(*ssaDef->getParentBlock())) {
    if (&inst == defInst) {
      boundary.deadDefs.push_back(cast<SILNode>(&inst));
      return;
    }
    if (liveness.isInterestingUser(&inst)) {
      boundary.lastUsers.push_back(&inst);
      return;
    }
  }
  auto *deadArg = dyn_cast<SILArgument>(ssaDef);
  assert(deadArg
         && "findBoundariesInBlock must be called on a live block");
  boundary.deadDefs.push_back(deadArg);
}

void SSAPrunedLiveness::findBoundariesInBlock(
    SILBasicBlock *block, bool isLiveOut,
    PrunedLivenessBoundary &boundary) const {
  assert(isInitialized());

  // For SSA, a live-out block cannot have a boundary.
  if (isLiveOut)
    return;

  // Handle live-within block
  if (!isDefBlock(block)) {
    findBoundaryInNonDefBlock(block, boundary, *this);
    return;
  }
  // Find either the last user or a dead def
  auto *defInst = def->getDefiningInstruction();
  SILNode *defNode = defInst ? cast<SILNode>(defInst) : cast<SILArgument>(def);
  findBoundaryInSSADefBlock(defNode, boundary, *this);
}

//===----------------------------------------------------------------------===//
//                           MultiDefPrunedLiveness
//===----------------------------------------------------------------------===//

void MultiDefPrunedLiveness::findBoundariesInBlock(
    SILBasicBlock *block, bool isLiveOut,
    PrunedLivenessBoundary &boundary) const {
  assert(isInitialized());

  if (!isDefBlock(block)) {
    // A live-out block with no defs cannot have a boundary.
    if (!isLiveOut) {
      findBoundaryInNonDefBlock(block, boundary, *this);
    }
    return;
  }
  // Handle def blocks...
  //
  // First, check for an SSA live range
  if (++defs.begin() == defs.end()) {
    // For SSA, a live-out block cannot have a boundary.
    if (!isLiveOut) {
      findBoundaryInSSADefBlock(*defs.begin(), boundary, *this);
    }
    return;
  }
  // Handle a live-out or live-within block with potentially multiple defs
  unsigned prevCount = boundary.deadDefs.size() + boundary.lastUsers.size();
  bool isLive = isLiveOut;
  for (auto &inst : llvm::reverse(*block)) {
    // Check if the instruction is a def before checking whether it is a
    // use. The same instruction can be both a dead def and boundary use.
    if (isDef(&inst)) {
      if (!isLive) {
        boundary.deadDefs.push_back(cast<SILNode>(&inst));
      }
      isLive = false;
    }
    // Note: the same instruction could potentially be both a dead def and last
    // user. The liveness boundary supports this, although it won't happen in
    // any context where we care about inserting code on the boundary.
    if (!isLive && isInterestingUser(&inst)) {
      boundary.lastUsers.push_back(&inst);
      isLive = true;
    }
  }
  if (!isLive) {
    for (SILArgument *deadArg : block->getArguments()) {
      if (defs.contains(deadArg)) {
        boundary.deadDefs.push_back(deadArg);
      }
    }
  }
  assert(prevCount < boundary.deadDefs.size() + boundary.lastUsers.size()
         && "findBoundariesInBlock must be called on a live block");
}

SimpleLiveRangeSummary MultiDefPrunedLiveness::computeSimple() {
  assert(isInitialized() && "defs uninitialized");

  SimpleLiveRangeSummary summary;
  for (SILNode *defNode : defs) {
    if (auto *arg = dyn_cast<SILArgument>(defNode))
      summary.meet(updateForDef(arg));
    else {
      for (auto result : cast<SILInstruction>(defNode)->getResults()) {
        summary.meet(updateForDef(result));
      }
    }
  }
  return summary;
}

//===----------------------------------------------------------------------===//
//                       DiagnosticPrunedLiveness
//===----------------------------------------------------------------------===//

// FIXME: This is wrong. Why is nonLifetimeEndingUsesInLiveOut inside
// PrunedLiveness, and what does it mean? Blocks may transition to LiveOut
// later. Or they may already be LiveOut from a previous use. After computing
// liveness, clients should check uses that are in PrunedLivenessBoundary.
void DiagnosticPrunedLiveness::
updateForUse(SILInstruction *user, bool lifetimeEnding) {
  PrunedLiveness::updateForUse(user, 0);

  auto useBlockLive = getBlockLiveness(user->getParent());
  // Record all uses of blocks on the liveness boundary. For blocks marked
  // LiveWithin, the boundary is considered to be the last use in the block.
  if (!lifetimeEnding && useBlockLive == PrunedLiveBlocks::LiveOut) {
    if (nonLifetimeEndingUsesInLiveOut)
      nonLifetimeEndingUsesInLiveOut->insert(user);
    return;
  }
}

//===----------------------------------------------------------------------===//
//                       Field Sensitive PrunedLiveness
//===----------------------------------------------------------------------===//

// We can only analyze components of structs whose storage is fully accessible
// from Swift.
static StructDecl *getFullyReferenceableStruct(SILType ktypeTy) {
  auto structDecl = ktypeTy.getStructOrBoundGenericStruct();
  if (!structDecl || structDecl->hasUnreferenceableStorage())
    return nullptr;
  return structDecl;
}

TypeSubElementCount::TypeSubElementCount(SILType type, SILModule &mod,
                                         TypeExpansionContext context)
    : number(1) {
  if (auto tupleType = type.getAs<TupleType>()) {
    unsigned numElements = 0;
    for (auto index : indices(tupleType.getElementTypes()))
      numElements +=
          TypeSubElementCount(type.getTupleElementType(index), mod, context);
    number = numElements;
    return;
  }

  if (auto *structDecl = getFullyReferenceableStruct(type)) {
    unsigned numElements = 0;
    for (auto *fieldDecl : structDecl->getStoredProperties())
      numElements += TypeSubElementCount(
          type.getFieldType(fieldDecl, mod, context), mod, context);
    number = numElements;
    return;
  }

  // If we have an enum, we add one for tracking if the base enum is set and use
  // the remaining bits for the max sized payload. This ensures that if we have
  // a smaller sized payload, we still get all of the bits set, allowing for a
  // homogeneous representation.
  if (auto *enumDecl = type.getEnumOrBoundGenericEnum()) {
    unsigned numElements = 0;
    for (auto *eltDecl : enumDecl->getAllElements()) {
      if (!eltDecl->hasAssociatedValues())
        continue;
      numElements = std::max(
          numElements,
          unsigned(TypeSubElementCount(
              type.getEnumElementType(eltDecl, mod, context), mod, context)));
    }
    number = numElements + 1;
    return;
  }

  // If this isn't a tuple, struct, or enum, it is a single element. This was
  // our default value, so we can just return.
}

Optional<SubElementNumber>
SubElementNumber::compute(SILValue projectionDerivedFromRoot,
                          SILValue rootAddress) {
  unsigned finalSubElementNumber = 0;
  SILModule &mod = *rootAddress->getModule();

  while (1) {
    // If we got to the root, we're done.
    if (rootAddress == projectionDerivedFromRoot)
      return {SubElementNumber(finalSubElementNumber)};

    if (auto *pbi = dyn_cast<ProjectBoxInst>(projectionDerivedFromRoot)) {
      projectionDerivedFromRoot = pbi->getOperand();
      continue;
    }

    if (auto *bai = dyn_cast<BeginAccessInst>(projectionDerivedFromRoot)) {
      projectionDerivedFromRoot = bai->getSource();
      continue;
    }

    if (auto *teai =
            dyn_cast<TupleElementAddrInst>(projectionDerivedFromRoot)) {
      SILType tupleType = teai->getOperand()->getType();

      // Keep track of what subelement is being referenced.
      for (unsigned i : range(teai->getFieldIndex())) {
        finalSubElementNumber += TypeSubElementCount(
            tupleType.getTupleElementType(i), mod,
            TypeExpansionContext(*rootAddress->getFunction()));
      }
      projectionDerivedFromRoot = teai->getOperand();
      continue;
    }

    if (auto *seai =
            dyn_cast<StructElementAddrInst>(projectionDerivedFromRoot)) {
      SILType type = seai->getOperand()->getType();

      // Keep track of what subelement is being referenced.
      StructDecl *structDecl = seai->getStructDecl();
      for (auto *fieldDecl : structDecl->getStoredProperties()) {
        if (fieldDecl == seai->getField())
          break;
        auto context = TypeExpansionContext(*rootAddress->getFunction());
        finalSubElementNumber += TypeSubElementCount(
            type.getFieldType(fieldDecl, mod, context), mod, context);
      }

      projectionDerivedFromRoot = seai->getOperand();
      continue;
    }

    // In the case of enums, we note that our representation is:
    //
    //                   ---------|Enum| ---
    //                  /                   \
    //                 /                     \
    //                v                       v
    //  |Bits for Max Sized Payload|    |Discrim Bit|
    //
    // So our payload is always going to start at the current field number since
    // we are the left most child of our parent enum. So we just need to look
    // through to our parent enum.
    if (auto *enumData = dyn_cast<UncheckedTakeEnumDataAddrInst>(
            projectionDerivedFromRoot)) {
      projectionDerivedFromRoot = enumData->getOperand();
      continue;
    }

    // Init enum data addr is treated like unchecked take enum data addr.
    if (auto *initData =
            dyn_cast<InitEnumDataAddrInst>(projectionDerivedFromRoot)) {
      projectionDerivedFromRoot = initData->getOperand();
      continue;
    }

    // If we do not know how to handle this case, just return None.
    //
    // NOTE: We use to assert here, but since this is used for diagnostics, we
    // really do not want to abort. Instead, our caller can choose to abort if
    // they get back a None. This ensures that we do not abort in cases where we
    // just want to emit to the user a "I do not understand" error.
    return None;
  }
}

void FieldSensitiveAddressPrunedLiveness::updateForUse(
    SILInstruction *user, TypeTreeLeafTypeRange range, bool lifetimeEnding) {
  SmallVector<PrunedLiveBlocks::IsLive, 8> resultingLiveness;
  liveBlocks.updateForUse(user, range.startEltOffset, range.endEltOffset,
                          resultingLiveness);

  // Note that a user may use the current value from multiple operands. If any
  // of the uses are non-lifetime-ending, then we must consider the user
  // itself non-lifetime-ending; it cannot be a final destroy point because
  // the value of the non-lifetime-ending operand must be kept alive until the
  // end of the user. Consider a call that takes the same value using
  // different conventions:
  //
  //   apply %f(%val, %val) : $(@guaranteed, @owned) -> ()
  //
  // This call is not considered the end of %val's lifetime. The @owned
  // argument must be copied.
  auto iterAndSuccess =
      users.insert({user, InterestingUser(range, lifetimeEnding)});
  if (!iterAndSuccess.second)
    iterAndSuccess.first->second &= lifetimeEnding;
}

void FieldSensitiveAddressPrunedLiveness::isWithinBoundary(
    SILInstruction *inst, SmallBitVector &outVector) const {
  SILBasicBlock *block = inst->getParent();

  SmallVector<PrunedLiveBlocks::IsLive, 8> fieldLiveness;
  getBlockLiveness(block, fieldLiveness);
  outVector.resize(fieldLiveness.size());

  for (auto pair : llvm::enumerate(fieldLiveness)) {
    auto isLive = pair.value();
    unsigned subEltNumber = pair.index();
    switch (isLive) {
    case PrunedLiveBlocks::Dead:
      outVector[subEltNumber] = false;
      continue;
    case PrunedLiveBlocks::LiveOut:
      outVector[subEltNumber] = true;
      continue;
    case PrunedLiveBlocks::LiveWithin:
      // The boundary is within this block. This instruction is before the
      // boundary iff any interesting uses occur after it.
      bool foundValue = false;
      for (SILInstruction &it :
           make_range(std::next(inst->getIterator()), block->end())) {
        auto interestingUser = isInterestingUser(&it);
        switch (interestingUser.first) {
        case FieldSensitiveAddressPrunedLiveness::NonUser:
          break;
        case FieldSensitiveAddressPrunedLiveness::NonLifetimeEndingUse:
        case FieldSensitiveAddressPrunedLiveness::LifetimeEndingUse:
          // Check the overlap in between the sub element number and
          // interestingUser.second. If we don't overlap, just break. We aren't
          // effected by this.
          //
          // TODO: Hoist this out! We should only be visited blocks like this
          // once!
          if (!interestingUser.second->contains(subEltNumber))
            break;
          outVector[subEltNumber] = true;
          foundValue = true;
          break;
        }
      }
      if (foundValue)
        continue;
      outVector[subEltNumber] = false;
    }
  }
}

// Use \p liveness to find the last use in \p bb and add it to \p
// boundary.lastUsers.
void FieldSensitiveAddressPrunedLivenessBoundary::findLastUserInBlock(
    SILBasicBlock *bb, FieldSensitiveAddressPrunedLivenessBoundary &boundary,
    const FieldSensitiveAddressPrunedLiveness &liveness,
    unsigned subElementNumber) {
  // TODO: We should move this loop into the caller and only visit a block once
  // for each sub-element of a type.
  for (auto &inst : llvm::reverse(*bb)) {
    auto pair = liveness.isInterestingUser(&inst);
    if (pair.first == FieldSensitiveAddressPrunedLiveness::NonUser)
      continue;

    // Do an intersection in between the range associated with this address and
    // the sub-element number we are checking for.
    auto &range = *pair.second;
    if (!range.contains(subElementNumber))
      continue;
    boundary.lastUsers.push_back({&inst, range});
    return;
  }
  llvm_unreachable("No user in LiveWithin block");
}

void FieldSensitiveAddressPrunedLivenessBoundary::compute(
    const FieldSensitiveAddressPrunedLiveness &liveness) {
  using IsLive = PrunedLiveBlocks::IsLive;
  SmallVector<IsLive, 8> perSubElementblockLivenessInfo;
  SmallVector<IsLive, 8> boundaryBlockLiveness;

  for (SILBasicBlock *bb : liveness.getDiscoveredBlocks()) {
    SWIFT_DEFER { perSubElementblockLivenessInfo.clear(); };

    // Process each block that has not been visited and is not LiveOut.
    liveness.getBlockLiveness(bb, perSubElementblockLivenessInfo);

    // TODO: We should do this for all sub-element LiveWithin at the same time
    // so that we can avoid iterating over the block multiple times.
    for (auto pair : llvm::enumerate(perSubElementblockLivenessInfo)) {
      switch (pair.value()) {
      case PrunedLiveBlocks::LiveOut:
        for (SILBasicBlock *succBB : bb->getSuccessors()) {
          liveness.getBlockLiveness(succBB, boundaryBlockLiveness);
          if (llvm::all_of(boundaryBlockLiveness, [](IsLive isDead) {
                return isDead == PrunedLiveBlocks::Dead;
              })) {
            boundaryEdges.push_back(succBB);
          }
        }
        break;
      case PrunedLiveBlocks::LiveWithin: {
        // The liveness boundary is inside this block. Find the last user. This
        // is where we would insert a destroy to end the values lifetime for the
        // specific subelementnumber
        findLastUserInBlock(bb, *this, liveness, pair.index());
        break;
      }
      case PrunedLiveBlocks::Dead:
        break;
      }
    }
  }
}
