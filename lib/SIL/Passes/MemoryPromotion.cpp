//===--- MemoryPromotion.cpp - Promote memory to SSA registers ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "memory-promotion"
#include "swift/Subsystems.h"
#include "swift/SIL/SILBuilder.h"
//#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
//#include "swift/SIL/Dominance.h"
using namespace swift;

/// isByRefOrIndirectReturn - Return true if the specified apply/partial_apply
/// call operand is a [byref] or indirect return, indicating that the call
/// doesn't capture the pointer.
static bool isByRefOrIndirectReturn(SILInstruction *Apply,
                                    unsigned ArgumentNumber) {
  SILType FnTy = Apply->getOperand(0).getType();
  SILFunctionTypeInfo *FTI = FnTy.getFunctionTypeInfo(*Apply->getModule());

  // If this is an indirect return slot, it isn't captured.
  if (ArgumentNumber == 0 && FTI->hasIndirectReturn())
    return true;

  // Otherwise, check for [byref].
  Type ArgTy = FTI->getSwiftArgumentType(ArgumentNumber);
  return ArgTy->is<LValueType>();
}


//===----------------------------------------------------------------------===//
// ElementUses Helper Class
//===----------------------------------------------------------------------===//

namespace {
  /// ElementUses - This class keeps track of all of the uses of a single
  /// element (i.e. tuple element or struct field) of a memory object.
  struct ElementUses {
    enum UseKindTy {
      // The instruction is a LoadInst.
      Load,

      // The instruction is a StoreInst.
      Store,

      /// The instruction is an Apply, this is a byref or indirect return.
      ByrefUse,

      /// This instruction is a general escape of the value, e.g. a call to a
      /// closure that captures it.
      Escape
    };

    std::vector<std::pair<SILInstruction*, UseKindTy>> Uses;
  };
} // end anonymous namespace

/// getNumElements - Return the number of elements in the flattened SILType.
/// For tuples and structs, this is the (recursive) count of the fields it
/// contains.
static unsigned getNumElements(CanType T, SILModule *M) {
  if (TupleType *TT = T->getAs<TupleType>()) {
    unsigned NumElements = 0;
    for (auto &Elt : TT->getFields())
      NumElements += getNumElements(Elt.getType()->getCanonicalType(), M);
    return NumElements;
  }

  if (StructType *ST = T->getAs<StructType>()) {
    // If the struct is resilient, we can't get to its fields.
    if (SILType::isAddressOnly(T, *M))
      return 1;

    unsigned NumElements = 0;
    for (auto *D : ST->getDecl()->getMembers())
      if (auto *VD = dyn_cast<VarDecl>(D))
        NumElements += getNumElements(VD->getType()->getCanonicalType(), M);
    return NumElements;
  }

  // If this isn't a tuple or struct, it is a single element.
  return 1;
}


//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

/// addElementUses - An operation (e.g. load, store, byref use, etc) on a value
/// acts on all of the aggregate elements in that value.  For example, a load
/// of $*(Int,Int) is a use of both Int elements of the tuple.  This is a helper
/// to keep the Uses data structure up to date for aggregate uses.
static void addElementUses(SmallVectorImpl<ElementUses> &Uses,
                           unsigned BaseElt, SILType UseTy,
                           SILInstruction *User, ElementUses::UseKindTy Kind) {
  for (unsigned i = 0, e = getNumElements(UseTy.getSwiftRValueType(),
                                          User->getModule());
       i != e; ++i)
    Uses[BaseElt+i].Uses.push_back({User, Kind });
}


static void collectAllocationUses(SILValue Pointer,
                                  SmallVectorImpl<ElementUses> &Uses,
                                  unsigned BaseElt) {
  assert(Pointer.getType().isAddress() &&
         "Walked through the pointer to the value?");
  SILType PointeeType = Pointer.getType().getObjectType();

  for (auto UI : Pointer.getUses()) {
    auto *User = cast<SILInstruction>(UI->getUser());

    // These show up as uses but aren't significant for this analysis.
    if (isa<DeallocStackInst>(User) ||
        isa<RetainInst>(User) ||
        isa<ReleaseInst>(User) ||
        isa<DeallocRefInst>(User))
      continue;

    // Loads are a use of the value.  Note that this could be an aggregate load.
    if (auto *LI = dyn_cast<LoadInst>(User)) {
      addElementUses(Uses, BaseElt, PointeeType, LI, ElementUses::Load);
      continue;
    }

    // Stores *to* the allocation are writes.  Stores *of* them are escapes.
    //  Note that this could be an aggregate store.
    if (isa<StoreInst>(User) && UI->getOperandNumber() == 1) {
      addElementUses(Uses, BaseElt, PointeeType, User, ElementUses::Store);
      continue;
    }

    // FIXME: CopyAddrInst
    // TODO: "Assign".


    // apply and partial_apply instructions do not capture the pointer when
    // it is passed through [byref] arguments or for indirect returns, but we
    // need to treat them as a may-store.
    if (isa<FunctionInst>(User) &&
        isByRefOrIndirectReturn(User, UI->getOperandNumber()-1)) {
      addElementUses(Uses, BaseElt, PointeeType, User, ElementUses::ByrefUse);
      continue;
    }


    // TODO: isa<StructElementAddrInst>(User) || isa<TupleElementAddrInst>(User)

    // TODO: InitializeVarInst.

    // TODO: isa<ProjectExistentialInst>(User)

    // Otherwise, the use is something complicated, it escapes.
    addElementUses(Uses, BaseElt, PointeeType, User, ElementUses::Escape);
  }
}

static void optimizeAllocBox(AllocBoxInst *ABI) {
  // Set up the datastructure used to collect the uses of the alloc_box.  The
  // uses are bucketed up into the elements of the allocation that are being
  // used.  This matters for element-wise tuples and fragile structs.
  SmallVector<ElementUses, 1> Uses;
  Uses.resize(getNumElements(ABI->getElementType().getSwiftRValueType(),
                             ABI->getModule()));

  // Walk the use list of the pointer, collecting them into the Uses array.
  collectAllocationUses(SILValue(ABI, 1), Uses, 0);



}


/// performSILMemoryPromotion - Promote alloc_box uses into SSA registers and
/// perform definitive initialization analysis.
void swift::performSILMemoryPromotion(SILModule *M) {
  
  for (auto &Fn : *M) {
    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        auto *ABI = dyn_cast<AllocBoxInst>(I);
        if (ABI == nullptr) {
          ++I;
          continue;
        }

        optimizeAllocBox(ABI);

        // Carefully move iterator to avoid invalidation problems.
        ++I;
        if (ABI->use_empty())
          ABI->eraseFromParent();
      }
    }
  }
}


