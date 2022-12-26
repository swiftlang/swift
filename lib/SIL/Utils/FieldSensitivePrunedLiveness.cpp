//===--- FieldSensitivePrunedLiveness.cpp ---------------------------------===//
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

#include "swift/SIL/FieldSensitivePrunedLiveness.h"
#include "swift/AST/TypeExpansionContext.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/ScopedAddressUtils.h"

using namespace swift;

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
