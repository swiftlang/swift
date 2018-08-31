//===--- SILCombinerCastVisitors.cpp --------------------------------------===//
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

#define DEBUG_TYPE "sil-combine"
#include "SILCombiner.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/CFG.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"

using namespace swift;
using namespace swift::PatternMatch;

SILInstruction *
SILCombiner::visitRefToRawPointerInst(RefToRawPointerInst *RRPI) {
  // Ref to raw pointer consumption of other ref casts.
  if (auto *URCI = dyn_cast<UncheckedRefCastInst>(RRPI->getOperand())) {
    // (ref_to_raw_pointer (unchecked_ref_cast x))
    //    -> (ref_to_raw_pointer x)
    if (URCI->getOperand()->getType().isAnyClassReferenceType()) {
      RRPI->setOperand(URCI->getOperand());
      return URCI->use_empty() ? eraseInstFromFunction(*URCI) : nullptr;
    }
    // (ref_to_raw_pointer (unchecked_ref_cast x))
    //    -> (unchecked_trivial_bit_cast x)
    return Builder.createUncheckedTrivialBitCast(RRPI->getLoc(),
                                                 URCI->getOperand(),
                                                 RRPI->getType());
  }

  // (ref_to_raw_pointer (open_existential_ref (init_existential_ref x))) ->
  // (ref_to_raw_pointer x)
  if (auto *OER = dyn_cast<OpenExistentialRefInst>(RRPI->getOperand()))
    if (auto *IER = dyn_cast<InitExistentialRefInst>(OER->getOperand()))
      return Builder.createRefToRawPointer(RRPI->getLoc(), IER->getOperand(),
                                           RRPI->getType());

  return nullptr;
}

SILInstruction *SILCombiner::visitUpcastInst(UpcastInst *UCI) {
  // Ref to raw pointer consumption of other ref casts.
  //
  // (upcast (upcast x)) -> (upcast x)
  if (auto *Op = dyn_cast<UpcastInst>(UCI->getOperand())) {
    UCI->setOperand(Op->getOperand());
    return Op->use_empty() ? eraseInstFromFunction(*Op) : nullptr;
  }

  return nullptr;
}

SILInstruction *
SILCombiner::
visitPointerToAddressInst(PointerToAddressInst *PTAI) {
  Builder.setCurrentDebugScope(PTAI->getDebugScope());

  // If we reach this point, we know that the types must be different since
  // otherwise simplifyInstruction would have handled the identity case. This is
  // always legal to do since address-to-pointer pointer-to-address implies
  // layout compatibility.
  //
  // (pointer-to-address strict (address-to-pointer %x))
  // -> (unchecked_addr_cast %x)
  if (PTAI->isStrict()) {
    if (auto *ATPI = dyn_cast<AddressToPointerInst>(PTAI->getOperand())) {
      return Builder.createUncheckedAddrCast(PTAI->getLoc(), ATPI->getOperand(),
                                             PTAI->getType());
    }
  }
  // Turn this also into an index_addr. We generate this pattern after switching
  // the Word type to an explicit Int32 or Int64 in the stdlib.
  //
  // %101 = builtin "strideof"<Int>(%84 : $@thick Int.Type) :
  //         $Builtin.Word
  // %102 = builtin "zextOrBitCast_Word_Int64"(%101 : $Builtin.Word) :
  //         $Builtin.Int64
  // %111 = builtin "smul_with_overflow_Int64"(%108 : $Builtin.Int64,
  //                               %102 : $Builtin.Int64, %20 : $Builtin.Int1) :
  //         $(Builtin.Int64, Builtin.Int1)
  // %112 = tuple_extract %111 : $(Builtin.Int64, Builtin.Int1), 0
  // %113 = builtin "truncOrBitCast_Int64_Word"(%112 : $Builtin.Int64) :
  //         $Builtin.Word
  // %114 = index_raw_pointer %100 : $Builtin.RawPointer, %113 : $Builtin.Word
  // %115 = pointer_to_address %114 : $Builtin.RawPointer to [strict] $*Int
  SILValue Distance;
  SILValue TruncOrBitCast;
  MetatypeInst *Metatype;
  IndexRawPointerInst *IndexRawPtr;
  BuiltinInst *StrideMul;
  if (match(
          PTAI->getOperand(),
          m_IndexRawPointerInst(IndexRawPtr))) {
    SILValue Ptr = IndexRawPtr->getOperand(0);
    SILValue TruncOrBitCast = IndexRawPtr->getOperand(1);
    if (match(TruncOrBitCast,
              m_ApplyInst(BuiltinValueKind::TruncOrBitCast,
                          m_TupleExtractInst(m_BuiltinInst(StrideMul), 0)))) {
      if (match(StrideMul,
                m_ApplyInst(
                    BuiltinValueKind::SMulOver, m_SILValue(Distance),
                    m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                                m_ApplyInst(BuiltinValueKind::Strideof,
                                            m_MetatypeInst(Metatype))))) ||
          match(StrideMul,
                m_ApplyInst(
                    BuiltinValueKind::SMulOver,
                    m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                                m_ApplyInst(BuiltinValueKind::Strideof,
                                            m_MetatypeInst(Metatype))),
                    m_SILValue(Distance)))) {
        SILType InstanceType =
            Metatype->getType().getMetatypeInstanceType(PTAI->getModule());
        auto *Trunc = cast<BuiltinInst>(TruncOrBitCast);

        // Make sure that the type of the metatype matches the type that we are
        // casting to so we stride by the correct amount.
        if (InstanceType.getAddressType() != PTAI->getType()) {
          return nullptr;
        }

        auto *NewPTAI = Builder.createPointerToAddress(PTAI->getLoc(), Ptr,
                                                       PTAI->getType(),
                                                       PTAI->isStrict(),
                                                       PTAI->isInvariant());
        auto DistanceAsWord = Builder.createBuiltin(
            PTAI->getLoc(), Trunc->getName(), Trunc->getType(), {}, Distance);

        return Builder.createIndexAddr(PTAI->getLoc(), NewPTAI, DistanceAsWord);
      }
    }
  }
  // Turn:
  //
  //   %stride = Builtin.strideof(T) * %distance
  //   %ptr' = index_raw_pointer %ptr, %stride
  //   %result = pointer_to_address %ptr, [strict] $T'
  //
  // To:
  //
  //   %addr = pointer_to_address %ptr, [strict] $T
  //   %result = index_addr %addr, %distance
  //
  BuiltinInst *Bytes;
  if (match(PTAI->getOperand(),
            m_IndexRawPointerInst(m_ValueBase(),
                                  m_TupleExtractInst(m_BuiltinInst(Bytes),
                                                     0)))) {
    if (match(Bytes, m_ApplyInst(BuiltinValueKind::SMulOver, m_ValueBase(),
                                 m_ApplyInst(BuiltinValueKind::Strideof,
                                             m_MetatypeInst(Metatype)),
                                 m_ValueBase()))) {
      SILType InstanceType =
        Metatype->getType().getMetatypeInstanceType(PTAI->getModule());

      // Make sure that the type of the metatype matches the type that we are
      // casting to so we stride by the correct amount.
      if (InstanceType.getAddressType() != PTAI->getType())
        return nullptr;

      auto IRPI = cast<IndexRawPointerInst>(PTAI->getOperand());
      SILValue Ptr = IRPI->getOperand(0);
      SILValue Distance = Bytes->getArguments()[0];
      auto *NewPTAI =
        Builder.createPointerToAddress(PTAI->getLoc(), Ptr, PTAI->getType(),
                                       PTAI->isStrict(), PTAI->isInvariant());
      return Builder.createIndexAddr(PTAI->getLoc(), NewPTAI, Distance);
    }
  }

  return nullptr;
}

SILInstruction *
SILCombiner::visitUncheckedAddrCastInst(UncheckedAddrCastInst *UADCI) {
  Builder.setCurrentDebugScope(UADCI->getDebugScope());

  // (unchecked-addr-cast (unchecked-addr-cast x X->Y) Y->Z)
  //   ->
  // (unchecked-addr-cast x X->Z)
  if (auto *OtherUADCI = dyn_cast<UncheckedAddrCastInst>(UADCI->getOperand()))
    return Builder.createUncheckedAddrCast(UADCI->getLoc(),
                                           OtherUADCI->getOperand(),
                                           UADCI->getType());

  // (unchecked-addr-cast cls->superclass) -> (upcast cls->superclass)
  if (UADCI->getType() != UADCI->getOperand()->getType() &&
      UADCI->getType().isExactSuperclassOf(UADCI->getOperand()->getType()))
    return Builder.createUpcast(UADCI->getLoc(), UADCI->getOperand(),
                                UADCI->getType());

  return nullptr;
}

SILInstruction *
SILCombiner::visitUncheckedRefCastInst(UncheckedRefCastInst *URCI) {
  // (unchecked-ref-cast (unchecked-ref-cast x X->Y) Y->Z)
  //   ->
  // (unchecked-ref-cast x X->Z)
  if (auto *OtherURCI = dyn_cast<UncheckedRefCastInst>(URCI->getOperand()))
    return Builder.createUncheckedRefCast(URCI->getLoc(),
                                          OtherURCI->getOperand(),
                                          URCI->getType());

  // (unchecked_ref_cast (upcast x X->Y) Y->Z) -> (unchecked_ref_cast x X->Z)
  if (auto *UI = dyn_cast<UpcastInst>(URCI->getOperand()))
    return Builder.createUncheckedRefCast(URCI->getLoc(),
                                              UI->getOperand(),
                                              URCI->getType());

  if (URCI->getType() != URCI->getOperand()->getType() &&
      URCI->getType().isExactSuperclassOf(URCI->getOperand()->getType()))
    return Builder.createUpcast(URCI->getLoc(), URCI->getOperand(),
                                URCI->getType());

  // (unchecked_ref_cast (open_existential_ref (init_existential_ref X))) ->
  // (unchecked_ref_cast X)
  if (auto *OER = dyn_cast<OpenExistentialRefInst>(URCI->getOperand()))
    if (auto *IER = dyn_cast<InitExistentialRefInst>(OER->getOperand()))
      return Builder.createUncheckedRefCast(URCI->getLoc(), IER->getOperand(),
                                            URCI->getType());

  return nullptr;
}


SILInstruction *
SILCombiner::visitBridgeObjectToRefInst(BridgeObjectToRefInst *BORI) {
  // Fold noop casts through Builtin.BridgeObject.
  // (bridge_object_to_ref (unchecked-ref-cast x BridgeObject) y)
  //  -> (unchecked-ref-cast x y)
  if (auto URC = dyn_cast<UncheckedRefCastInst>(BORI->getOperand()))
    return Builder.createUncheckedRefCast(BORI->getLoc(), URC->getOperand(),
                                          BORI->getType());
  return nullptr;
}



SILInstruction *
SILCombiner::visitUncheckedRefCastAddrInst(UncheckedRefCastAddrInst *URCI) {
  SILType SrcTy = URCI->getSrc()->getType();
  if (!SrcTy.isLoadable(URCI->getModule()))
    return nullptr;

  SILType DestTy = URCI->getDest()->getType();
  if (!DestTy.isLoadable(URCI->getModule()))
    return nullptr;

  // After promoting unchecked_ref_cast_addr to unchecked_ref_cast, the SIL
  // verifier will assert that the loadable source and dest type of reference
  // castable. If the static types are invalid, simply avoid promotion, that way
  // the runtime will then report a failure if this cast is ever executed.
  if (!SILType::canRefCast(SrcTy.getObjectType(), DestTy.getObjectType(),
                           URCI->getModule()))
    return nullptr;
 
  SILLocation Loc = URCI->getLoc();
  Builder.setCurrentDebugScope(URCI->getDebugScope());
  LoadInst *load = Builder.createLoad(Loc, URCI->getSrc(),
                                      LoadOwnershipQualifier::Unqualified);
  auto *cast = Builder.tryCreateUncheckedRefCast(Loc, load,
                                                 DestTy.getObjectType());
  assert(cast && "SILBuilder cannot handle reference-castable types");
  Builder.createStore(Loc, cast, URCI->getDest(),
                      StoreOwnershipQualifier::Unqualified);

  return eraseInstFromFunction(*URCI);
}

SILInstruction *
SILCombiner::
visitUnconditionalCheckedCastAddrInst(UnconditionalCheckedCastAddrInst *UCCAI) {
  CastOpt.optimizeUnconditionalCheckedCastAddrInst(UCCAI);
  return nullptr;
}

SILInstruction *
SILCombiner::
visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *UCCI) {
  if (CastOpt.optimizeUnconditionalCheckedCastInst(UCCI))
    return nullptr;

  // FIXME: rename from RemoveCondFails to RemoveRuntimeAsserts.
  if (RemoveCondFails) {
    auto LoweredTargetType = UCCI->getType();
    auto Loc = UCCI->getLoc();
    auto Op = UCCI->getOperand();
    if (LoweredTargetType.isAddress()) {
      // unconditional_checked_cast -> unchecked_addr_cast
      return Builder.createUncheckedAddrCast(Loc, Op, LoweredTargetType);
    } else if (LoweredTargetType.isHeapObjectReferenceType()) {
      if (!(Op->getType().isHeapObjectReferenceType() ||
            Op->getType().isClassExistentialType())) {
        return nullptr;
      }
      // unconditional_checked_cast -> unchecked_ref_cast
      return Builder.createUncheckedRefCast(Loc, Op, LoweredTargetType);
    }
  }

  return nullptr;
}

SILInstruction *
SILCombiner::
visitRawPointerToRefInst(RawPointerToRefInst *RawToRef) {
  // (raw_pointer_to_ref (ref_to_raw_pointer x X->Y) Y->Z)
  //   ->
  // (unchecked_ref_cast X->Z)
  if (auto *RefToRaw = dyn_cast<RefToRawPointerInst>(RawToRef->getOperand())) {
    return Builder.createUncheckedRefCast(RawToRef->getLoc(),
                                          RefToRaw->getOperand(),
                                          RawToRef->getType());
  }

  return nullptr;
}

SILInstruction *
SILCombiner::
visitUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *UTBCI) {
  // (unchecked_trivial_bit_cast Y->Z
  //                                 (unchecked_trivial_bit_cast X->Y x))
  //   ->
  // (unchecked_trivial_bit_cast X->Z x)
  SILValue Op = UTBCI->getOperand();
  if (auto *OtherUTBCI = dyn_cast<UncheckedTrivialBitCastInst>(Op)) {
    return Builder.createUncheckedTrivialBitCast(UTBCI->getLoc(),
                                                 OtherUTBCI->getOperand(),
                                                 UTBCI->getType());
  }

  // (unchecked_trivial_bit_cast Y->Z
  //                                 (unchecked_ref_cast X->Y x))
  //   ->
  // (unchecked_trivial_bit_cast X->Z x)
  if (auto *URBCI = dyn_cast<UncheckedRefCastInst>(Op)) {
    return Builder.createUncheckedTrivialBitCast(UTBCI->getLoc(),
                                                 URBCI->getOperand(),
                                                 UTBCI->getType());
  }

  return nullptr;
}

SILInstruction *
SILCombiner::
visitUncheckedBitwiseCastInst(UncheckedBitwiseCastInst *UBCI) {
  // (unchecked_bitwise_cast Y->Z (unchecked_bitwise_cast X->Y x))
  // OR (unchecked_trivial_cast Y->Z (unchecked_bitwise_cast X->Y x))
  //   ->
  // (unchecked_bitwise_cast X->Z x)
  SILValue Oper;
  if (match(UBCI->getOperand(),
            m_CombineOr(m_UncheckedBitwiseCastInst(m_SILValue(Oper)),
                        m_UncheckedTrivialBitCastInst(m_SILValue(Oper))))) {
    return Builder.createUncheckedBitwiseCast(UBCI->getLoc(), Oper,
                                              UBCI->getType());
  }
  if (UBCI->getType().isTrivial(UBCI->getModule()))
    return Builder.createUncheckedTrivialBitCast(UBCI->getLoc(),
                                                 UBCI->getOperand(),
                                                 UBCI->getType());

  if (auto refCast = Builder.tryCreateUncheckedRefCast(
        UBCI->getLoc(), UBCI->getOperand(), UBCI->getType()))
    return refCast;

  return nullptr;
}

/// Helper function for simplifying conversions between
/// thick and objc metatypes.
static SILInstruction *
visitMetatypeConversionInst(SILBuilder &Builder, ConversionInst *MCI,
                            MetatypeRepresentation Representation) {
  SILValue Op = MCI->getOperand(0);
  // Instruction has a proper target type already.
  SILType Ty = MCI->getType();
  auto MetatypeTy = Op->getType().getAs<AnyMetatypeType>();

  if (MetatypeTy->getRepresentation() != Representation)
    return nullptr;

  if (isa<MetatypeInst>(Op))
    return Builder.createMetatype(MCI->getLoc(), Ty);

  if (auto *VMI = dyn_cast<ValueMetatypeInst>(Op))
    return Builder.createValueMetatype(MCI->getLoc(), Ty, VMI->getOperand());

  if (auto *EMI = dyn_cast<ExistentialMetatypeInst>(Op))
    return Builder.createExistentialMetatype(MCI->getLoc(), Ty,
                                             EMI->getOperand());

  return nullptr;
}

SILInstruction *
SILCombiner::visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *TTOCMI) {
  // Perform the following transformations:
  // (thick_to_objc_metatype (metatype @thick)) ->
  // (metatype @objc_metatype)
  //
  // (thick_to_objc_metatype (value_metatype @thick)) ->
  // (value_metatype @objc_metatype)
  //
  // (thick_to_objc_metatype (existential_metatype @thick)) ->
  // (existential_metatype @objc_metatype)
  return visitMetatypeConversionInst(Builder, TTOCMI,
                                     MetatypeRepresentation::Thick);
}

SILInstruction *
SILCombiner::visitObjCToThickMetatypeInst(ObjCToThickMetatypeInst *OCTTMI) {
  // Perform the following transformations:
  // (objc_to_thick_metatype (metatype @objc_metatype)) ->
  // (metatype @thick)
  //
  // (objc_to_thick_metatype (value_metatype @objc_metatype)) ->
  // (value_metatype @thick)
  //
  // (objc_to_thick_metatype (existential_metatype @objc_metatype)) ->
  // (existential_metatype @thick)
  return visitMetatypeConversionInst(Builder, OCTTMI,
                                     MetatypeRepresentation::ObjC);
}

SILInstruction *
SILCombiner::visitCheckedCastBranchInst(CheckedCastBranchInst *CBI) {
  CastOpt.optimizeCheckedCastBranchInst(CBI);
  return nullptr;
}

SILInstruction *
SILCombiner::
visitCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *CCABI) {
  CastOpt.optimizeCheckedCastAddrBranchInst(CCABI);
  return nullptr;
}

SILInstruction *SILCombiner::visitConvertEscapeToNoEscapeInst(
    ConvertEscapeToNoEscapeInst *Cvt) {
  auto *OrigThinToThick =
      dyn_cast<ThinToThickFunctionInst>(Cvt->getConverted());
  if (!OrigThinToThick)
    return nullptr;
  auto origFunType = OrigThinToThick->getType().getAs<SILFunctionType>();
  auto NewTy = origFunType->getWithExtInfo(origFunType->getExtInfo().withNoEscape(true));

  return Builder.createThinToThickFunction(
      OrigThinToThick->getLoc(), OrigThinToThick->getOperand(),
      SILType::getPrimitiveObjectType(NewTy));
}
/// Replace a convert_function that only has refcounting uses with its
/// operand.
SILInstruction *SILCombiner::visitConvertFunctionInst(ConvertFunctionInst *CFI) {
  auto anyNonRefCountUse =
    std::any_of(CFI->use_begin(),
                CFI->use_end(),
                [](Operand *Use) {
                  return !isa<RefCountingInst>(Use->getUser());
                });

  if (anyNonRefCountUse)
    return nullptr;

  // Replace all retain/releases on convert_function by retain/releases on
  // its argument. This is required to preserve the lifetime of its argument,
  // which could be e.g. a partial_apply instruction capturing some further
  // arguments.
  auto Converted = CFI->getConverted();
  while (!CFI->use_empty()) {
    auto *Use = *(CFI->use_begin());
    assert(Use->getUser()->getResults().empty() &&
           "Did not expect user with a result!");
    Use->set(Converted);
  }

  eraseInstFromFunction(*CFI);
  return nullptr;
}
