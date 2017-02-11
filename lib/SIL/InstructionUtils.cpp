//===--- InstructionUtils.cpp - Utilities for SIL instructions ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-inst-utils"
#include "swift/SIL/InstructionUtils.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILVisitor.h"

using namespace swift;

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
/// FIXME: Why don't we strip projections after stripping indexes?
SILValue swift::getUnderlyingObject(SILValue V) {
  while (true) {
    SILValue V2 = stripIndexingInsts(stripAddressProjections(stripCasts(V)));
    if (V2 == V)
      return V2;
    V = V2;
  }
}

SILValue swift::getUnderlyingAddressRoot(SILValue V) {
  while (true) {
    SILValue V2 = stripIndexingInsts(stripCasts(V));
    switch (V2->getKind()) {
      case ValueKind::StructElementAddrInst:
      case ValueKind::TupleElementAddrInst:
      case ValueKind::UncheckedTakeEnumDataAddrInst:
        V2 = cast<SILInstruction>(V2)->getOperand(0);
        break;
      default:
        break;
    }
    if (V2 == V)
      return V2;
    V = V2;
  }
}


SILValue swift::getUnderlyingObjectStopAtMarkDependence(SILValue V) {
  while (true) {
    SILValue V2 = stripIndexingInsts(stripAddressProjections(stripCastsWithoutMarkDependence(V)));
    if (V2 == V)
      return V2;
    V = V2;
  }
}

static bool isRCIdentityPreservingCast(ValueKind Kind) {
  switch (Kind) {
    case ValueKind::UpcastInst:
    case ValueKind::UncheckedRefCastInst:
    case ValueKind::UncheckedRefCastAddrInst:
    case ValueKind::UnconditionalCheckedCastInst:
    case ValueKind::RefToBridgeObjectInst:
    case ValueKind::BridgeObjectToRefInst:
      return true;
    default:
      return false;
  }
}

/// Return the underlying SILValue after stripping off identity SILArguments if
/// we belong to a BB with one predecessor.
SILValue swift::stripSinglePredecessorArgs(SILValue V) {
  while (true) {
    auto *A = dyn_cast<SILArgument>(V);
    if (!A)
      return V;
    
    SILBasicBlock *BB = A->getParent();
    
    // First try and grab the single predecessor of our parent BB. If we don't
    // have one, bail.
    SILBasicBlock *Pred = BB->getSinglePredecessorBlock();
    if (!Pred)
      return V;
    
    // Then grab the terminator of Pred...
    TermInst *PredTI = Pred->getTerminator();
    
    // And attempt to find our matching argument.
    //
    // *NOTE* We can only strip things here if we know that there is no semantic
    // change in terms of upcasts/downcasts/enum extraction since this is used
    // by other routines here. This means that we can only look through
    // cond_br/br.
    //
    // For instance, routines that use stripUpcasts() do not want to strip off a
    // downcast that results from checked_cast_br.
    if (auto *BI = dyn_cast<BranchInst>(PredTI)) {
      V = BI->getArg(A->getIndex());
      continue;
    }
    
    if (auto *CBI = dyn_cast<CondBranchInst>(PredTI)) {
      if (SILValue Arg = CBI->getArgForDestBB(BB, A)) {
        V = Arg;
        continue;
      }
    }
    
    return V;
  }
}

SILValue swift::stripCastsWithoutMarkDependence(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);

    auto K = V->getKind();
    if (isRCIdentityPreservingCast(K) ||
        K == ValueKind::UncheckedTrivialBitCastInst) {
      V = cast<SILInstruction>(V)->getOperand(0);
      continue;
    }

    return V;
  }
}

SILValue swift::stripCasts(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);
    
    auto K = V->getKind();
    if (isRCIdentityPreservingCast(K)
        || K == ValueKind::UncheckedTrivialBitCastInst
        || K == ValueKind::MarkDependenceInst) {
      V = cast<SILInstruction>(V)->getOperand(0);
      continue;
    }
    
    return V;
  }
}

SILValue swift::stripUpCasts(SILValue V) {
  assert(V->getType().isClassOrClassMetatype() &&
         "Expected class or class metatype!");
  
  V = stripSinglePredecessorArgs(V);
  
  while (isa<UpcastInst>(V))
    V = stripSinglePredecessorArgs(cast<UpcastInst>(V)->getOperand());
  
  return V;
}

SILValue swift::stripClassCasts(SILValue V) {
  while (true) {
    if (auto *UI = dyn_cast<UpcastInst>(V)) {
      V = UI->getOperand();
      continue;
    }
    
    if (auto *UCCI = dyn_cast<UnconditionalCheckedCastInst>(V)) {
      V = UCCI->getOperand();
      continue;
    }
    
    return V;
  }
}

SILValue swift::stripAddressProjections(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);
    if (!Projection::isAddressProjection(V))
      return V;
    V = cast<SILInstruction>(V)->getOperand(0);
  }
}

SILValue swift::stripUnaryAddressProjections(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);
    if (!Projection::isAddressProjection(V))
      return V;
    auto *Inst = cast<SILInstruction>(V);
    if (Inst->getNumOperands() > 1)
      return V;
    V = Inst->getOperand(0);
  }
}

SILValue swift::stripValueProjections(SILValue V) {
  while (true) {
    V = stripSinglePredecessorArgs(V);
    if (!Projection::isObjectProjection(V))
      return V;
    V = cast<SILInstruction>(V)->getOperand(0);
  }
}

SILValue swift::stripIndexingInsts(SILValue V) {
  while (true) {
    if (!isa<IndexingInst>(V))
      return V;
    V = cast<IndexingInst>(V)->getBase();
  }
}

SILValue swift::stripExpectIntrinsic(SILValue V) {
  auto *BI = dyn_cast<BuiltinInst>(V);
  if (!BI)
    return V;
  if (BI->getIntrinsicInfo().ID != llvm::Intrinsic::expect)
    return V;
  return BI->getArguments()[0];
}

void ConformanceCollector::scanType(Type type) {
  type = type->getCanonicalType();
  if (Visited.count(type.getPointer()) != 0)
    return;

  // Look for all possible metatypes and conformances which are used in type.
  type.visit([this](Type SubType) {
    if (NominalTypeDecl *NT = SubType->getNominalOrBoundGenericNominal()) {
      if (Visited.count(SubType.getPointer()) == 0) {

        if (Visited.count(NT) == 0) {
          EscapingMetaTypes.push_back(NT);
          Visited.insert(NT);
        }

        // Also inserts the type passed to scanType().
        Visited.insert(SubType.getPointer());
        auto substs = SubType->gatherAllSubstitutions(M.getSwiftModule(),
                                                     nullptr);
        scanSubsts(substs);
      }
    }
  });
}

void ConformanceCollector::scanSubsts(SubstitutionList substs) {
  for (const Substitution &subst : substs) {
    scanConformances(subst.getConformances());
    scanType(subst.getReplacement());
  }
}

void ConformanceCollector::scanConformance(ProtocolConformance *C) {
  if (!C || Visited.count(C) != 0)
    return;
  Visited.insert(C);
  Conformances.push_back(C);

  switch (C->getKind()) {
    case ProtocolConformanceKind::Normal:
      break;
    case ProtocolConformanceKind::Inherited:
      scanConformance(cast<InheritedProtocolConformance>(C)->
                        getInheritedConformance());
      break;
    case ProtocolConformanceKind::Specialized: {

      auto *SpecC = cast<SpecializedProtocolConformance>(C);
      scanConformance(SpecC->getGenericConformance());
      scanSubsts(SpecC->getGenericSubstitutions());
      break;
    }
  }

  SILWitnessTable *WT = M.lookUpWitnessTable(C, /*deserializeLazily*/ false);
  if (!WT)
    return;

  for (const SILWitnessTable::Entry &entry : WT->getEntries()) {
    switch (entry.getKind()) {
      case SILWitnessTable::AssociatedTypeProtocol:
        scanConformance(entry.getAssociatedTypeProtocolWitness().Witness);
        break;
        
      case SILWitnessTable::BaseProtocol:
        scanConformance(entry.getBaseProtocolWitness().Witness);
        break;
        
      case SILWitnessTable::AssociatedType:
        scanType(entry.getAssociatedTypeWitness().Witness);
        break;

      case SILWitnessTable::Method:
      case SILWitnessTable::Invalid:
      case SILWitnessTable::MissingOptional:
        break;
    }
  }
}

void ConformanceCollector::scanConformances(
                                     ArrayRef<ProtocolConformanceRef> CRefs) {
  for (ProtocolConformanceRef CRef : CRefs) {
    scanConformance(CRef);
  }
}

void ConformanceCollector::collect(swift::SILInstruction *I) {
  switch (I->getKind()) {
    case ValueKind::InitExistentialAddrInst: {
      auto *IEI = cast<InitExistentialAddrInst>(I);
      for (ProtocolConformanceRef CRef : IEI->getConformances()) {
        if (CRef.isConcrete()) {
          scanConformance(CRef.getConcrete()->getRootNormalConformance());
        }
      }
      scanType(IEI->getFormalConcreteType());
      break;
    }
    case ValueKind::InitExistentialRefInst:
      scanConformances(cast<InitExistentialRefInst>(I)->getConformances());
      break;
    case ValueKind::InitExistentialMetatypeInst:
      scanConformances(cast<InitExistentialMetatypeInst>(I)->getConformances());
      break;
    case ValueKind::WitnessMethodInst:
      scanConformance(cast<WitnessMethodInst>(I)->getConformance());
      break;
    case ValueKind::SuperMethodInst: {
      SILType InstanceTy = cast<SuperMethodInst>(I)->getOperand()->getType();
      if (auto MTy = dyn_cast<MetatypeType>(InstanceTy.getSwiftRValueType()))
        InstanceTy = SILType::getPrimitiveObjectType(MTy.getInstanceType());
      if (SILType SuperTy = InstanceTy.getSuperclass(/*resolver=*/nullptr))
        scanType(SuperTy.getSwiftRValueType());
      break;
    }
    case ValueKind::AllocBoxInst: {
      CanSILBoxType BTy = cast<AllocBoxInst>(I)->getBoxType();
      size_t NumFields = BTy->getLayout()->getFields().size();
      for (size_t Idx = 0; Idx < NumFields; ++Idx) {
        scanType(BTy->getFieldLoweredType(M, Idx));
      }
      scanType(I->getType().getSwiftRValueType());
      break;
    }
    case ValueKind::AllocExistentialBoxInst: {
      auto *AEBI = cast<AllocExistentialBoxInst>(I);
      scanType(AEBI->getFormalConcreteType());
      scanConformances(AEBI->getConformances());
      break;
    }
    case ValueKind::DeallocExistentialBoxInst:
      scanType(cast<DeallocExistentialBoxInst>(I)->getConcreteType());
      break;
    case ValueKind::AllocRefInst:
    case ValueKind::AllocRefDynamicInst:
    case ValueKind::MetatypeInst:
    case ValueKind::UnconditionalCheckedCastInst:
      scanType(I->getType().getSwiftRValueType());
      break;
    case ValueKind::AllocStackInst: {
      Type Ty = I->getType().getSwiftRValueType();
      if (Ty->hasArchetype())
        scanType(Ty);
      break;
    }
    case ValueKind::CheckedCastAddrBranchInst: {
      auto *CCABI = cast<CheckedCastAddrBranchInst>(I);
      scanType(CCABI->getSourceType());
      scanType(CCABI->getTargetType());
      break;
    }
    case ValueKind::UnconditionalCheckedCastAddrInst: {
      auto *UCCAI = cast<UnconditionalCheckedCastAddrInst>(I);
      scanType(UCCAI->getSourceType());
      scanType(UCCAI->getTargetType());
      break;
    }
    case ValueKind::CheckedCastBranchInst:
      scanType(cast<CheckedCastBranchInst>(I)->getCastType().
                 getSwiftRValueType());
      break;
    case ValueKind::ValueMetatypeInst: {
      auto *VMTI = cast<ValueMetatypeInst>(I);
      scanType(VMTI->getOperand()->getType().getSwiftRValueType());
      break;
    }
    case ValueKind::DestroyAddrInst:
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::InjectEnumAddrInst:
    case ValueKind::SwitchEnumAddrInst:
    case ValueKind::SelectEnumAddrInst:
    case ValueKind::IndexAddrInst:
    case ValueKind::RefElementAddrInst:
    case ValueKind::CopyAddrInst: {
      Type Ty = I->getOperand(0)->getType().getSwiftRValueType();
      if (Ty->hasArchetype())
        scanType(Ty);
      break;
    }
    default:
      if (ApplySite AS = ApplySite::isa(I)) {
        auto substs = AS.getSubstitutions();
        scanSubsts(substs);

        CanSILFunctionType OrigFnTy = AS.getOrigCalleeType();
        CanSILFunctionType SubstFnTy = AS.getSubstCalleeType();

        scanFuncParams(OrigFnTy->getParameters(), SubstFnTy->getParameters());
        scanFuncParams(OrigFnTy->getResults(), SubstFnTy->getResults());

        if (OrigFnTy->getRepresentation() ==
            SILFunctionType::Representation::WitnessMethod) {
          // The self parameter of a witness method is always generic.
          scanType(OrigFnTy->getSelfInstanceType());
        }
      }
      break;
  }
}

void ConformanceCollector::collect(SILWitnessTable *WT) {
  scanConformance(WT->getConformance());
}

void ConformanceCollector::dump() {
  llvm::errs() << "ConformanceCollector:\n";
  for (const ProtocolConformance *C : Conformances) {
    C->dump();
  }
}

namespace {

enum class OwnershipQualifiedKind {
  NotApplicable,
  Qualified,
  Unqualified,
};

struct OwnershipQualifiedKindVisitor : SILInstructionVisitor<OwnershipQualifiedKindVisitor, OwnershipQualifiedKind> {

  OwnershipQualifiedKind visitValueBase(ValueBase *V) {
    return OwnershipQualifiedKind::NotApplicable;
  }

#define QUALIFIED_INST(CLASS) \
  OwnershipQualifiedKind visit ## CLASS(CLASS *I) { \
    return OwnershipQualifiedKind::Qualified;             \
  }
  QUALIFIED_INST(EndBorrowInst)
  QUALIFIED_INST(LoadBorrowInst)
  QUALIFIED_INST(CopyValueInst)
  QUALIFIED_INST(CopyUnownedValueInst)
  QUALIFIED_INST(DestroyValueInst)
#undef QUALIFIED_INST

  OwnershipQualifiedKind visitLoadInst(LoadInst *LI) {
    if (LI->getOwnershipQualifier() == LoadOwnershipQualifier::Unqualified)
      return OwnershipQualifiedKind::Unqualified;
    return OwnershipQualifiedKind::Qualified;
  }

  OwnershipQualifiedKind visitStoreInst(StoreInst *SI) {
    if (SI->getOwnershipQualifier() == StoreOwnershipQualifier::Unqualified)
      return OwnershipQualifiedKind::Unqualified;
    return OwnershipQualifiedKind::Qualified;
  }
};

} // end anonymous namespace

bool FunctionOwnershipEvaluator::evaluate(SILInstruction *I) {
  assert(I->getFunction() == F.get() && "Can not evaluate function ownership "
         "implications of an instruction that "
         "does not belong to the instruction "
         "that we are evaluating");

  switch (OwnershipQualifiedKindVisitor().visit(I)) {
  case OwnershipQualifiedKind::Unqualified: {
    // If we already know that the function has unqualified ownership, just
    // return early.
    if (!F.get()->hasQualifiedOwnership())
      return true;

    // Ok, so we know at this point that we have qualified ownership. If we have
    // seen any instructions with qualified ownership, we have an error since
    // the function mixes qualified and unqualified instructions.
    if (HasOwnershipQualifiedInstruction)
      return false;

    // Otherwise, set the function to have unqualified ownership. This will
    // ensure that no more Qualified instructions can be added to the given
    // function.
    F.get()->setUnqualifiedOwnership();
    return true;
  }
  case OwnershipQualifiedKind::Qualified: {
    // First check if our function has unqualified ownership. If we already do
    // have unqualified ownership, then we know that we have already seen an
    // unqualified ownership instruction. This means the function has both
    // qualified and unqualified instructions. =><=.
    if (!F.get()->hasQualifiedOwnership())
      return false;

    // Ok, at this point we know that we are still qualified. Since functions
    // start as qualified, we need to set the HasOwnershipQualifiedInstructions
    // so we do not need to look back through the function if we see an
    // unqualified instruction later on.
    HasOwnershipQualifiedInstruction = true;
    return true;
  }
  case OwnershipQualifiedKind::NotApplicable: {
    // Not Applicable instr
    return true;
  }
  }

  llvm_unreachable("Unhandled OwnershipQualifiedKind in switch.");
}
