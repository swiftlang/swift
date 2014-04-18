//===--- SimplifyInstruction.cpp - Fold instructions ----------------------===//
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

#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
using namespace swift;

namespace swift {
  class ASTContext;
}

namespace {
  class InstSimplifier : public SILInstructionVisitor<InstSimplifier, SILValue>{
  public:
    SILValue visitSILInstruction(SILInstruction *I) { return SILValue(); }

    SILValue visitTupleExtractInst(TupleExtractInst *TEI);
    SILValue visitStructExtractInst(StructExtractInst *SEI);
    SILValue visitEnumInst(EnumInst *EI);
    SILValue visitAddressToPointerInst(AddressToPointerInst *ATPI);
    SILValue visitPointerToAddressInst(PointerToAddressInst *PTAI);
    SILValue visitRefToRawPointerInst(RefToRawPointerInst *RRPI);
    SILValue
    visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *UCCI);
    SILValue visitObjectPointerToRefInst(ObjectPointerToRefInst *OPRI);
    SILValue visitRefToObjectPointerInst(RefToObjectPointerInst *ROPI);
    SILValue visitStructInst(StructInst *SI);
    SILValue visitTupleInst(TupleInst *SI);
    SILValue visitApplyInst(ApplyInst *AI);

  };
} // end anonymous namespace

SILValue InstSimplifier::visitStructInst(StructInst *SI) {
  // Ignore empty structs.
  if (SI->getNumOperands() < 1)
    return SILValue();

  // Optimize structs that are generated from struct_extract instructions
  // from the same struct.
  if (auto *Ex0 = dyn_cast<StructExtractInst>(SI->getOperand(0))) {
    // Check that the constructed struct and the extracted struct are of the
    // same type.
    if (SI->getType() != Ex0->getOperand().getType())
      return SILValue();

    // Check that all of the operands are extracts of the correct kind.
    for (unsigned i = 0, e = SI->getNumOperands(); i < e; i++) {
      auto *Ex = dyn_cast<StructExtractInst>(SI->getOperand(i));
      // Must be an extract.
      if (!Ex)
        return SILValue();

      // Extract from the same struct as the first extract_inst.
      if (Ex0->getOperand() != Ex->getOperand())
        return SILValue();

      // And the order of the field must be identical to the construction order.
      if (Ex->getFieldNo() != i)
        return SILValue();
    }

    return Ex0->getOperand();
  }
  
  return SILValue();
}

SILValue InstSimplifier::visitTupleInst(TupleInst *TI) {
  // Ignore empty tuples.
  if (TI->getNumOperands() < 1)
    return SILValue();

  // Optimize tuples that are generated from tuple_extract instructions
  // from the same tuple.
  if (auto *Ex0 = dyn_cast<TupleExtractInst>(TI->getOperand(0))) {
    // Check that the constructed tuple and the extracted tuple are of the
    // same type.
    if (TI->getType() != Ex0->getOperand().getType())
      return SILValue();

    // Check that all of the operands are extracts of the correct kind.
    for (unsigned i = 0, e = TI->getNumOperands(); i < e; i++) {
      auto *Ex = dyn_cast<TupleExtractInst>(TI->getOperand(i));
      // Must be an extract.
      if (!Ex)
        return SILValue();

      // Extract from the same struct as the first extract_inst.
      if (Ex0->getOperand() != Ex->getOperand())
        return SILValue();

      // And the order of the field must be identical to the construction order.
      if (Ex->getFieldNo() != i)
        return SILValue();
    }

    return Ex0->getOperand();
  }

  return SILValue();
}

SILValue InstSimplifier::visitTupleExtractInst(TupleExtractInst *TEI) {
  // tuple_extract(tuple(x, y), 0) -> x
  if (TupleInst *TheTuple = dyn_cast<TupleInst>(TEI->getOperand()))
    return TheTuple->getElements()[TEI->getFieldNo()];

  return SILValue();
}

SILValue InstSimplifier::visitStructExtractInst(StructExtractInst *SEI) {
  // struct_extract(struct(x, y), x) -> x
  if (StructInst *Struct = dyn_cast<StructInst>(SEI->getOperand()))
    return Struct->getFieldValue(SEI->getField());
  
  return SILValue();
}

SILValue InstSimplifier::visitEnumInst(EnumInst *EI) {
  // Simplify enum insts to the value from a switch_enum when possible, e.g.
  // for
  //   switch_enum %0 : $Bool, case #Bool.true!enumelt: bb1
  // bb1:
  //   %1 = enum $Bool, #Bool.true!enumelt
  //
  // we'll return %0
  if (EI->hasOperand())
    return SILValue();

  auto *BB = EI->getParent();
  auto *Pred = BB->getSinglePredecessor();
  if (!Pred)
    return SILValue();

  if (auto *SEI = dyn_cast<SwitchEnumInst>(Pred->getTerminator())) {
    if (EI->getType() != SEI->getOperand().getType())
      return SILValue();

    if (BB == SEI->getCaseDestination(EI->getElement()))
      return SEI->getOperand();
  }

  return SILValue();
}

SILValue InstSimplifier::visitAddressToPointerInst(AddressToPointerInst *ATPI) {
  // (address_to_pointer (pointer_to_address x)) -> x
  if (auto *PTAI = dyn_cast<PointerToAddressInst>(ATPI->getOperand()))
    if (PTAI->getType() == ATPI->getOperand().getType())
      return PTAI->getOperand();

  return SILValue();
}

SILValue InstSimplifier::visitPointerToAddressInst(PointerToAddressInst *PTAI) {
  // (pointer_to_address (address_to_pointer x)) -> x
  if (auto *ATPI = dyn_cast<AddressToPointerInst>(PTAI->getOperand()))
    if (ATPI->getOperand().getType() == PTAI->getType())
      return ATPI->getOperand();

  return SILValue();
}

SILValue InstSimplifier::visitRefToRawPointerInst(RefToRawPointerInst *RefToRaw) {
  // Perform the following simplification:
  //
  // (ref_to_raw_pointer (raw_pointer_to_ref x)) -> x
  //
  // *NOTE* We don't need to check types here.
  if (auto *RawToRef = dyn_cast<RawPointerToRefInst>(&*RefToRaw->getOperand()))
    return RawToRef->getOperand();

  return SILValue();
}

SILValue
InstSimplifier::
visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *UCCI) {
  // (UCCI downcast (upcast x #type1 to #type2) #type2 to #type1) -> x
  if (UCCI->getCastKind() == CheckedCastKind::Downcast)
    if (auto *Upcast = dyn_cast<UpcastInst>(UCCI->getOperand()))
      if (UCCI->getOperand().getType() == Upcast->getType() &&
          UCCI->getType() == Upcast->getOperand().getType())
      return Upcast->getOperand();

  return SILValue();
}

SILValue
InstSimplifier::
visitObjectPointerToRefInst(ObjectPointerToRefInst *OPRI) {
  // (object-pointer-to-ref-inst (ref-to-object-pointer-inst x) typeof(x)) -> x
  if (auto *RTOPI = dyn_cast<RefToObjectPointerInst>(&*OPRI->getOperand()))
    if (RTOPI->getOperand().getType() == OPRI->getType())
      return RTOPI->getOperand();
SILValue
InstSimplifier::
visitRefToObjectPointerInst(RefToObjectPointerInst *ROPI) {
  // (ref-to-object-pointer-inst (object-pointer-to-ref-inst x) typeof(x)) -> x
  if (auto *OPRI = dyn_cast<ObjectPointerToRefInst>(&*ROPI->getOperand()))
    if (OPRI->getOperand().getType() == ROPI->getType())
      return OPRI->getOperand();

  return SILValue();
}

static SILValue optimizeBuiltinCanBeObjCClass(ApplyInst *AI) {
  assert(AI->hasSubstitutions() && "Expected substitutions for canBeObjCClass");

  auto const &Subs = AI->getSubstitutions();
  assert((Subs.size() == 1) &&
         "Expected one substitution in call to canBeObjCClass");

  SILBuilder Builder(AI);

  auto Ty = Subs[0].Replacement->getCanonicalType();
  switch (Ty->canBeObjCClass()) {
  case TypeTraitResult::IsNot:
    return Builder.createIntegerLiteral(AI->getLoc(), AI->getType(), 0);
  case TypeTraitResult::Is:
    return Builder.createIntegerLiteral(AI->getLoc(), AI->getType(), 1);
  case TypeTraitResult::CanBe:
    return SILValue();
  }
}

/// Simplify an apply of the builtin canBeObjCClass to either 0 or 1
/// when we can statically determine the result.
SILValue InstSimplifier::visitApplyInst(ApplyInst *AI) {
  auto *BFRI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee());
  if (!BFRI)
    return SILValue();

  if (BFRI->getBuiltinInfo().ID == BuiltinValueKind::CanBeObjCClass)
    return optimizeBuiltinCanBeObjCClass(AI);

  // If we have an expect optimizer hint with a constant value input, there is
  // nothing left to expect so propagate the input, i.e.,
  //
  // apply(expect, constant, _) -> constant.
  if (BFRI->getIntrinsicInfo().ID == llvm::Intrinsic::expect)
    if (auto *Literal = dyn_cast<IntegerLiteralInst>(AI->getArgument(0)))
      return Literal;

  return SILValue();
}

/// \brief Try to simplify the specified instruction, performing local
/// analysis of the operands of the instruction, without looking at its uses
/// (e.g. constant folding).  If a simpler result can be found, it is
/// returned, otherwise a null SILValue is returned.
///
SILValue swift::simplifyInstruction(SILInstruction *I) {
  return InstSimplifier().visit(I);
}
