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
    SILValue visitIntegerLiteralInst(IntegerLiteralInst *ILI);
    SILValue visitEnumInst(EnumInst *EI);
    SILValue visitAddressToPointerInst(AddressToPointerInst *ATPI);
    SILValue visitPointerToAddressInst(PointerToAddressInst *PTAI);
    SILValue visitRefToRawPointerInst(RefToRawPointerInst *RRPI);
  };
} // end anonymous namespace


SILValue InstSimplifier::visitTupleExtractInst(TupleExtractInst *TEI) {
  // tuple_extract(tuple(x, y), 0) -> x
  if (TupleInst *TheTuple = dyn_cast<TupleInst>(TEI->getOperand()))
    return TheTuple->getElements()[TEI->getFieldNo()];

  return SILValue();
}

SILValue InstSimplifier::visitStructExtractInst(StructExtractInst *SEI) {
  // struct_extract(struct(x, y), x) -> x
  if (StructInst *Struct = dyn_cast<StructInst>(SEI->getOperand()))
    return Struct->getOperandForField(SEI->getField())->get();
  
  return SILValue();
}

SILValue InstSimplifier::visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
  // Simplify bool integer_literal insts to the condition that
  // generates them when possible, e.g. an Int1 integer_literal 1 in
  // the TrueBB branch target of a conditional branch.
  auto *BB = ILI->getParent();
  auto &Context = BB->getParent()->getASTContext();
  if (ILI->getType() != SILType::getBuiltinIntegerType(1, Context))
    return SILValue();

  auto *Pred = BB->getSinglePredecessor();
  if (!Pred)
    return SILValue();

  if (auto *CBI = dyn_cast<CondBranchInst>(Pred->getTerminator())) {
    auto Value = ILI->getValue().getBoolValue();
    auto *OtherBB = Value ? CBI->getTrueBB() : CBI->getFalseBB();

    if (BB == OtherBB)
      return CBI->getCondition();
  }

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
  if (auto *PTAI = dyn_cast<PointerToAddressInst>(ATPI->getOperand().getDef()))
    if (PTAI->getType() == ATPI->getOperand().getType())
      return PTAI->getOperand();

  return SILValue();
}

SILValue InstSimplifier::visitPointerToAddressInst(PointerToAddressInst *PTAI) {
  // (pointer_to_address (address_to_pointer x)) -> x
  if (auto *ATPI = dyn_cast<AddressToPointerInst>(PTAI->getOperand().getDef()))
    if (ATPI->getType(0) == PTAI->getOperand().getType())
      return ATPI->getOperand();

  return SILValue();
}

SILValue InstSimplifier::visitRefToRawPointerInst(RefToRawPointerInst *RRPI) {
  // (ref_to_raw_pointer (ref_to_object_pointer x))
  //    -> (ref_to_raw_pointer x)
  if (auto *ROPI = dyn_cast<RefToObjectPointerInst>(&*RRPI->getOperand())) {
    RRPI->setOperand(ROPI->getOperand());
    return SILValue();
  }

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
