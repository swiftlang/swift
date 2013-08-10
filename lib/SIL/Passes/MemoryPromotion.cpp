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
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
//#include "swift/SIL/Dominance.h"
using namespace swift;

STATISTIC(NumLoadPromoted, "Number of loads promoted");

template<typename ...ArgTypes>
static void diagnose(SILModule *M, SILLocation loc, ArgTypes... args) {
  M->getASTContext().Diags.diagnose(loc.getSourceLoc(), Diagnostic(args...));
}

//===----------------------------------------------------------------------===//
// Per-Element Promotion Logic
//===----------------------------------------------------------------------===//

namespace {
  enum UseKind {
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

  /// ElementUses - This class keeps track of all of the uses of a single
  /// element (i.e. tuple element or struct field) of a memory object.
  typedef std::vector<std::pair<SILInstruction*, UseKind>> ElementUses;

  enum class EscapeKind {
    Unknown,
    Yes,
    No
  };

  /// LiveOutBlockState - Keep track of information about blocks that have
  /// already been analyzed.  Since this is a global analysis, we need this to
  /// cache information about different paths through the CFG.
  struct LiveOutBlockState {
    /// For this block, keep track of whether there is a path from the entry
    /// of the function to the end of the block that crosses an escape site.
    EscapeKind EscapeInfo = EscapeKind::Unknown;

    /// Keep track of whether there is a Store, ByrefUse, or Escape locally in
    /// this block.
    bool HasNonLoadUse = false;

  };
} // end anonymous namespace

namespace {
  /// ElementPromotion - This is the main heavy lifting for processing the uses
  /// of an element of an allocation.
  class ElementPromotion {
    AllocBoxInst *TheAllocBox;
    ElementUses &Uses;
    llvm::SmallDenseMap<SILBasicBlock*, LiveOutBlockState, 32> PerBlockInfo;

    /// This is the set of uses that are not loads (i.e., they are Stores,
    /// ByrefUses, and Escapes).
    llvm::SmallPtrSet<SILInstruction*, 16> NonLoadUses;

    bool HasAnyEscape = false;

    // Keep track of whether we've emitted an error.  We only emit one error per
    // element as a policy decision.
    bool HadError = false;
  public:
    ElementPromotion(AllocBoxInst *TheAllocBox, ElementUses &Uses);

    void doIt();
    void handleLoadUse(SILInstruction *Inst);
    void handleByrefUse(SILInstruction *Inst);
    void handleEscape(SILInstruction *Inst);

    enum DIKind {
      DI_Yes,
      DI_No,
      DI_Partial
    };
    DIKind checkDefinitelyInit(SILInstruction *Inst, SILValue *AV = nullptr);
  };
} // end anonymous namespace

ElementPromotion::ElementPromotion(AllocBoxInst *TheAllocBox, ElementUses &Uses)
  : TheAllocBox(TheAllocBox), Uses(Uses) {

  // The first step of processing an element is to collect information about the
  // element into data structures we use later.
  for (auto Use : Uses) {
    // Keep track of all the uses that aren't loads.
    if (Use.second != UseKind::Load)
      NonLoadUses.insert(Use.first);

    if (Use.second == UseKind::Escape) {
      // Determine which blocks the value can escape from.  We aren't allowed to
      // promote loads in blocks reachable from an escape point.
      HasAnyEscape = true;
      auto &BBInfo = PerBlockInfo[Use.first->getParent()];
      BBInfo.EscapeInfo = EscapeKind::Yes;
      BBInfo.HasNonLoadUse = true;
    } else if (Use.second == UseKind::Store ||
               Use.second == UseKind::ByrefUse) {
      // Keep track of which blocks have local stores.  This makes scanning for
      // assignments cheaper later.
      PerBlockInfo[Use.first->getParent()].HasNonLoadUse = true;
    }
  }
}


void ElementPromotion::doIt() {
  // With any escapes tallied up, we can work through all the uses, checking
  // for definitive initialization, promoting loads, rewriting assigns, and
  // performing other tasks.
  for (auto Use : Uses) {
    switch (Use.second) {
    case UseKind::Load:     handleLoadUse(Use.first); break;
    case UseKind::Store:    break;
    case UseKind::ByrefUse: handleByrefUse(Use.first); break;
    case UseKind::Escape:   handleEscape(Use.first); break;
    }

    if (HadError) break;
  }
}

/// Given a load (i.e., a LoadInst or CopyAddr), determine whether the loaded
/// value is definitely assigned or not.  If not, produce a diagnostic.  If so,
/// attempt to promote the value into SSA form.
void ElementPromotion::handleLoadUse(SILInstruction *Inst) {
  SILValue Result;

  // If this is a Load (not a CopyAddr), we try to compute the loaded value as
  // an SSA register.  Otherwise, we don't ask for an available value to avoid
  // constructing SSA for the value.
  auto DI = checkDefinitelyInit(Inst, isa<LoadInst>(Inst) ? &Result : nullptr);
  if (DI == DI_Yes) {
    // If the value is definitely initialized, check to see if this is a load
    // that we have a value available for.  If so, we can replace the load now.
    if (Result && Inst->getType(0) == Result.getType()) {
      SILValue(Inst, 0).replaceAllUsesWith(Result);
      Inst->eraseFromParent();
      ++NumLoadPromoted;
    }

    return;
  }

  // Otherwise, this is a use of an uninitialized value.  Emit a diagnostic.
  // TODO: The QoI could be improved in many different ways here.  We could give
  // some path information, give the name / access path of the variable, etc.
  diagnose(Inst->getModule(), Inst->getLoc(),
           diag::variable_used_before_initialized);
  diagnose(Inst->getModule(), TheAllocBox->getLoc(),
           diag::variable_defined_here);
  HadError = true;
}

/// Given a byref use (an Apply), determine whether the loaded
/// value is definitely assigned or not.  If not, produce a diagnostic.
void ElementPromotion::handleByrefUse(SILInstruction *Inst) {
  auto DI = checkDefinitelyInit(Inst);
  if (DI == DI_Yes)
    return;

  // Otherwise, this is a use of an uninitialized value.  Emit a diagnostic.
  // TODO: The QoI could be improved in many different ways here.  We could give
  // some path information, give the name / access path of the variable, etc.
  diagnose(Inst->getModule(), Inst->getLoc(),
           diag::variable_byref_before_initialized);
  diagnose(Inst->getModule(), TheAllocBox->getLoc(),
           diag::variable_defined_here);
  HadError = true;
}

void ElementPromotion::handleEscape(SILInstruction *Inst) {
  auto DI = checkDefinitelyInit(Inst);
  if (DI == DI_Yes)
    return;

  // Otherwise, this is a use of an uninitialized value.  Emit a diagnostic.
  // TODO: The QoI could be improved in many different ways here.  We could give
  // some path information, give the name / access path of the variable, etc.
  diagnose(Inst->getModule(), Inst->getLoc(),
           diag::variable_escape_before_initialized);
  diagnose(Inst->getModule(), TheAllocBox->getLoc(),
           diag::variable_defined_here);
  HadError = true;
}




/// getStoredValueFrom - Given a may store to the stack slot we're promoting,
/// return the value being stored.
static SILValue getStoredValueFrom(SILInstruction *I) {
  if (auto *SI = dyn_cast<StoreInst>(I))
    return SI->getOperand(0);
  return SILValue();
}

/// The specified instruction is a use of the element.  Determine whether the
/// element is definitely initialized at this point or not.  If the value is
/// initialized on some paths, but not others, this returns a partial result.
///
/// In addition to computing whether a value is definitely initialized or not,
/// if AV is non-null, this function can return the currently live value in some
/// cases.
ElementPromotion::DIKind
ElementPromotion::checkDefinitelyInit(SILInstruction *Inst, SILValue *AV) {
  // If there is a store in the current block, scan the block to see if the
  // store is before or after the load.  If it is before, it produces the value
  // we are looking for.
  if (PerBlockInfo[Inst->getParent()].HasNonLoadUse) {
    for (SILBasicBlock::iterator BBI = Inst, E = Inst->getParent()->begin();
         BBI != E;) {
      SILInstruction *TheInst = --BBI;
      if (NonLoadUses.count(TheInst)) {
        if (AV) *AV = getStoredValueFrom(TheInst);

        return DI_Yes;
      }

      // If we found the allocation itself, then we are loading something that
      // is not defined at all yet.
      if (TheInst == TheAllocBox)
        return DI_No;
    }
  }

  // FIXME: Need to do cross-block analysis.

  return DI_Partial;
}


//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

namespace {
  /// Recursively computing the number of elements in a type can be exponential,
  /// and even if the number of elements is low, the presence of single-element
  /// struct wrappers (like Int!) means that computing this is non-trivial.
  ///
  /// Cache the results of the NumElements computation in a densemap to make this
  /// be linear in the number of types analyzed.
  class NumElementsCache {
    llvm::DenseMap<CanType, unsigned> ElementCountMap;
    
    NumElementsCache(const NumElementsCache&) = delete;
    void operator=(const NumElementsCache&) = delete;
  public:
    NumElementsCache() {}
    unsigned get(CanType T);
    
  };
} // end anonymous namespace

/// getNumElements - Return the number of elements in the flattened SILType.
/// For tuples and structs, this is the (recursive) count of the fields it
/// contains.
unsigned NumElementsCache::get(CanType T) {
  if (TupleType *TT = T->getAs<TupleType>()) {
    auto It = ElementCountMap.find(T);
    if (It != ElementCountMap.end()) return It->second;
    
    unsigned NumElements = 0;
    for (auto &Elt : TT->getFields())
      NumElements += get(Elt.getType()->getCanonicalType());
    return ElementCountMap[T] = NumElements;
  }
  
  if (T->is<StructType>() || T->is<BoundGenericStructType>()) {
    auto It = ElementCountMap.find(T);
    if (It != ElementCountMap.end()) return It->second;
    
    StructDecl *SD;
    if (auto *ST = T->getAs<StructType>())
      SD = ST->getDecl();
    else
      SD = T->castTo<BoundGenericStructType>()->getDecl();
    
    unsigned NumElements = 0;
    for (auto *D : SD->getMembers())
      if (auto *VD = dyn_cast<VarDecl>(D))
        if (!VD->isProperty())
          NumElements += get(VD->getType()->getCanonicalType());
    return ElementCountMap[T] = NumElements;
  }
  
  // If this isn't a tuple or struct, it is a single element.
  return 1;
}


/// addElementUses - An operation (e.g. load, store, byref use, etc) on a value
/// acts on all of the aggregate elements in that value.  For example, a load
/// of $*(Int,Int) is a use of both Int elements of the tuple.  This is a helper
/// to keep the Uses data structure up to date for aggregate uses.
static void addElementUses(SmallVectorImpl<ElementUses> &Uses,
                           unsigned BaseElt, SILType UseTy,
                           SILInstruction *User, UseKind Kind,
                           NumElementsCache &NumElements) {
  for (unsigned i = 0, e = NumElements.get(UseTy.getSwiftRValueType());
       i != e; ++i)
    Uses[BaseElt+i].push_back({ User, Kind });
}


static void collectAllocationUses(SILValue Pointer,
                                  SmallVectorImpl<ElementUses> &Uses,
                                  unsigned BaseElt,
                                  NumElementsCache &NumElements) {
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

    // FIXME: Canonicalize aggregate loads of entire sub-tuples/sub-structs into
    // loads of their elements + struct_inst / tuple_inst.
    // FIXME: Canonicalize aggregate stores of entire sub-tuples/sub-structs
    // extracts of their elements + individual stores.

    // Loads are a use of the value.  Note that this could be an aggregate load.
    if (auto *LI = dyn_cast<LoadInst>(User)) {
      addElementUses(Uses, BaseElt, PointeeType, LI, UseKind::Load,
                     NumElements);
      continue;
    }

    // Stores *to* the allocation are writes.  Stores *of* them are escapes.
    // Note that this could be an aggregate store.
    if (isa<StoreInst>(User) && UI->getOperandNumber() == 1) {
      addElementUses(Uses, BaseElt, PointeeType, User, UseKind::Store,
                     NumElements);
      continue;
    }

    if (auto *CAI = dyn_cast<CopyAddrInst>(User)) {
      // If this is the source of the copy_addr, then this is a load.  If it is
      // the destination, then this is a store.
      auto Kind = UI->getOperandNumber() == 0 ? UseKind::Load : UseKind::Store;
      addElementUses(Uses, BaseElt, PointeeType, CAI, Kind, NumElements);
      continue;
    }

    // TODO: "Assign".
    // TODO: InitializeVarInst.
    // TODO: isa<ProjectExistentialInst>(User)
    



    // The apply instruction does not capture the pointer when it is passed
    // through [byref] arguments or for indirect returns.  Byref arguments are
    // treated as uses and may-store's, but an indirect return is treated as a
    // full store.
    //
    // Note that partial_apply instructions always close over their argument.
    //
    if (auto *Apply = dyn_cast<ApplyInst>(User)) {
      SILType FnTy = Apply->getOperand(0).getType();
      SILFunctionTypeInfo *FTI = FnTy.getFunctionTypeInfo(*Apply->getModule());
      unsigned ArgumentNumber = UI->getOperandNumber()-1;

      // If this is an indirect return slot, it is a store.
      if (ArgumentNumber == 0 && FTI->hasIndirectReturn()) {
        addElementUses(Uses, BaseElt, PointeeType, User, UseKind::Store,
                       NumElements);
        continue;
      }

      // Otherwise, check for [byref].
      Type ArgTy = FTI->getSwiftArgumentType(ArgumentNumber);
      if (ArgTy->is<LValueType>()) {
        addElementUses(Uses, BaseElt, PointeeType, User, UseKind::ByrefUse,
                       NumElements);
        continue;
      }

      // Otherwise, it is an escape.
    }

    // tuple_element P, 42 indexes into the current element.  Recursively
    // process its uses with the adjusted element number.
    if (auto *TEAI = dyn_cast<TupleElementAddrInst>(User)) {
      unsigned FieldNo = TEAI->getFieldNo();
      auto *TT = Pointer.getType().getSwiftRValueType()->castTo<TupleType>();
      unsigned NewBaseElt = BaseElt;
      for (unsigned i = 0; i != FieldNo; ++i) {
        CanType EltTy = TT->getElementType(i)->getCanonicalType();
        NewBaseElt += NumElements.get(EltTy);
      }

      collectAllocationUses(SILValue(TEAI, 0), Uses, NewBaseElt, NumElements);
      continue;
    }

    // struct_element P, #field indexes into the current element.  Recursively
    // process its uses with the adjusted element number.
    if (auto *SEAI = dyn_cast<StructElementAddrInst>(User)) {
      VarDecl *Field = SEAI->getField();
      CanType StructTy = Pointer.getType().getSwiftRValueType();

      StructDecl *SD;
      if (auto *ST = StructTy->getAs<StructType>())
        SD = ST->getDecl();
      else
        SD = StructTy->castTo<BoundGenericStructType>()->getDecl();

      unsigned NewBaseElt = BaseElt;
      for (auto *D : SD->getMembers()) {
        if (D == Field) break;
        if (auto *VD = dyn_cast<VarDecl>(D))
          if (!VD->isProperty())
            NewBaseElt += NumElements.get(VD->getType()->getCanonicalType());
      }

      collectAllocationUses(SILValue(SEAI, 0), Uses, NewBaseElt, NumElements);
      continue;
    }

     // Otherwise, the use is something complicated, it escapes.
    addElementUses(Uses, BaseElt, PointeeType, User, UseKind::Escape,
                   NumElements);
  }
}


static void optimizeAllocBox(AllocBoxInst *ABI, NumElementsCache &NumElements) {
  // Set up the datastructure used to collect the uses of the alloc_box.  The
  // uses are bucketed up into the elements of the allocation that are being
  // used.  This matters for element-wise tuples and fragile structs.
  SmallVector<ElementUses, 1> Uses;
  Uses.resize(NumElements.get(ABI->getElementType().getSwiftRValueType()));

  // Walk the use list of the pointer, collecting them into the Uses array.
  collectAllocationUses(SILValue(ABI, 1), Uses, 0, NumElements);

  // Process each scalar value in the uses array individually.
  for (auto &Elt : Uses)
    ElementPromotion(ABI, Elt).doIt();
}


/// performSILMemoryPromotion - Promote alloc_box uses into SSA registers and
/// perform definitive initialization analysis.
void swift::performSILMemoryPromotion(SILModule *M) {
  NumElementsCache NumElements;
  for (auto &Fn : *M) {
    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        auto *ABI = dyn_cast<AllocBoxInst>(I);
        if (ABI == nullptr) {
          ++I;
          continue;
        }

        optimizeAllocBox(ABI, NumElements);

        // Carefully move iterator to avoid invalidation problems.
        ++I;
        if (ABI->use_empty())
          ABI->eraseFromParent();
      }
    }
  }
}


