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
#include "swift/SIL/PrunedLiveness.h"
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

bool FieldSensitiveAddressPrunedLiveRange::isWithinBoundary(
    SILInstruction *inst, TypeTreeLeafTypeRange span) {
  // If we do not have any span, return true since we have no counter examples.
  if (span.empty())
    return true;

  using IsLive = PrunedLiveBlocks::IsLive;

  auto *block = inst->getParent();
  SmallVector<IsLive, 8> outVector;
  getBlockLiveness(block, span, outVector);
  for (auto pair : llvm::enumerate(outVector)) {
    bool isLive = false;
    switch (pair.value()) {
    case PrunedLiveBlocks::Dead:
      // If any of our bits are dead, then we are not in the boundary.
      return false;
    case PrunedLiveBlocks::LiveOut:
      // If this bit is live out and we are not in a def block for this inst, we
      // continue. Otherwise, we fall through to live within.
      if (!isDefBlock(block, pair.index()))
        return false;
      isLive = true;
      [[clang::fallthrough]];
    case PrunedLiveBlocks::LiveWithin:
      // Now check if the instruction is between a last use and a definition.
      for (auto &blockInst : llvm::reverse(*block)) {
        if (isDef(&blockInst, pair.index()))
          isLive = false;

        if (&blockInst == inst) {
          if (isLive) {
            break;
          }

          return false;
        }

        if (!isLive) {
          auto interestingUser = isInterestingUser(&blockInst);
          isLive |= interestingUser.first &&
                    interestingUser.second->contains(pair.index());
        }
      }
      llvm_unreachable("Inst not in parent block?!");
    }
  }

  // We succeeded in proving we are within the boundary for all bits.
  return true;
}
