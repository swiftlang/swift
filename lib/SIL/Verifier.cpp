//===--- Verifier.cpp - Verification of Swift SIL Code --------------------===//
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

#include "swift/SIL/Function.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
using namespace swift;

namespace {
/// SILVerifier class - This class implements the SIL verifier, which walks over
/// SIL, checking and enforcing its invariants.
class SILVerifier : public SILVisitor<SILVerifier> {
public:
  SILVerifier() {}
  
  void visit(Instruction *I) {
    
    const BasicBlock *BB = I->getParent();
    (void)BB;
    // Check that non-terminators look ok.
    if (!isa<TermInst>(I)) {
      assert(!BB->empty() &&
             "Can't be in a parent block if it is empty");
      assert(&*BB->getInsts().rbegin() != I &&
             "Non-terminators cannot be the last in a block");
    } else {
      assert(&*BB->getInsts().rbegin() == I &&
             "Terminator must be the last in block");
    }

    
    // Dispatch to our more-specialized instances below.
    ((SILVisitor<SILVerifier>*)this)->visit(I);
  }

  void visitAllocInst(AllocInst *AI) {
    assert(AI->getType()->is<LValueType>() &&"Allocation should return lvalue");
  }

  void visitAllocVarInst(AllocVarInst *AI) {
    visitAllocInst(AI);
  }

  void visitApplyInst(ApplyInst *AI) {
    assert(AI->getCallee().getType()->is<FunctionType>() &&
           "Callee of ApplyInst should have function type");
    FunctionType *FT = AI->getCallee().getType()->castTo<FunctionType>();
    assert(AI->getType()->isEqual(FT->getResult()) &&
           "ApplyInst result type mismatch");

    // If there is a single argument to the apply, it might be a scalar or the
    // whole tuple being presented all at once.
    if (AI->getArguments().size() != 1 ||
        !AI->getArguments()[0].getType()->isEqual(FT->getInput())) {
      // Otherwise, we must have a decomposed tuple.  Verify the arguments match
      // up.
      const TupleType *TT = FT->getInput()->castTo<TupleType>();
      (void)TT;
      assert(AI->getArguments().size() == TT->getFields().size() &&
             "ApplyInst contains unexpected argument count for function");
      for (unsigned i = 0, e = AI->getArguments().size(); i != e; ++i)
        assert(AI->getArguments()[i].getType()
                 ->isEqual(TT->getFields()[i].getType()) &&
               "ApplyInst argument type mismatch");
    }
  }

  void visitConstantRefInst(ConstantRefInst *CRI) {
    assert(CRI->getType()->is<AnyFunctionType>() &&
           "constant_ref should have a function result");
  }

  void visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
    assert(ILI->getType()->is<BuiltinIntegerType>() &&
           "invalid integer literal type");
  }
  void visitLoadInst(LoadInst *LI) {
    assert(!LI->getType()->is<LValueType>() && "Load should produce rvalue");
    assert(LI->getLValue().getType()->is<LValueType>() &&
           "Load op should be lvalue");
    assert(LI->getLValue().getType()->getRValueType()->isEqual(LI->getType()) &&
           "Load operand type and result type mismatch");
  }

  void visitStoreInst(StoreInst *SI) {
    assert(!SI->getSrc().getType()->is<LValueType>() &&
           "Src value should be rvalue");
    assert(SI->getDest().getType()->is<LValueType>() &&
           "Dest address should be lvalue");
    assert(SI->getDest().getType()->getRValueType()->
              isEqual(SI->getSrc().getType()) &&
           "Store operand type and dest type mismatch");
  }

  void visitCopyAddrInst(CopyAddrInst *SI) {
    assert(SI->getSrc().getType()->is<LValueType>() &&
           "Src value should be lvalue");
    assert(SI->getDest().getType()->is<LValueType>() &&
           "Dest address should be lvalue");
    assert(SI->getDest().getType()->getRValueType()->
           isEqual(SI->getSrc().getType()->getRValueType()) &&
           "Store operand type and dest type mismatch");
  }
  
  void visitSpecializeInst(SpecializeInst *SI) {
    assert(SI->getType()->is<FunctionType>() &&
           "Specialize dest should be a function type");
    assert(SI->getOperand().getType()->is<PolymorphicFunctionType>() &&
           "Specialize source should be a polymorphic function type");
  }

  void visitTupleInst(TupleInst *TI) {
    assert(TI->getType()->is<TupleType>() && "TupleInst should return a tuple");
    TupleType *ResTy = TI->getType()->castTo<TupleType>(); (void)ResTy;

    assert(TI->getElements().size() == ResTy->getFields().size() &&
           "Tuple field count mismatch!");
  }
  void visitMetatypeInst(MetatypeInst *MI) {
    assert(MI->getType(0)->is<MetaTypeType>() &&
           "Metatype instruction must be of metatype type");
  }
  
  void visitRetainInst(RetainInst *RI) {
    assert(!RI->getOperand().getType()->is<LValueType>() &&
           "Operand of retain must not be lvalue");
  }
  void visitReleaseInst(ReleaseInst *RI) {
    assert(!RI->getOperand().getType()->is<LValueType>() &&
           "Operand of release must not be lvalue");
  }
  void visitDeallocVarInst(DeallocVarInst *DI) {
    assert(DI->getOperand().getType()->is<LValueType>() &&
           "Operand of dealloc must be lvalue");
  }
  void visitDestroyAddrInst(DestroyAddrInst *DI) {
    assert(DI->getOperand().getType()->is<LValueType>() &&
           "Operand of destroy must be lvalue");
  }

  void visitIndexAddrInst(IndexAddrInst *IAI) {
    assert(IAI->getType()->is<LValueType>() &&
           IAI->getType()->isEqual(IAI->getOperand().getType()) &&
           "invalid IndexAddrInst");
  }
  
  void visitExtractInst(ExtractInst *EI) {
#ifndef NDEBUG
    Type operandTy = EI->getOperand().getType();
    assert(!operandTy->is<LValueType>() &&
           "cannot extract from address");
    assert(!operandTy->hasReferenceSemantics() &&
           "cannot extract from reference type");
    assert(!EI->getType(0)->is<LValueType>() &&
           "result of extract cannot be address");
#endif
  }

  void visitElementAddrInst(ElementAddrInst *EI) {
#ifndef NDEBUG
    Type operandTy = EI->getOperand().getType();
    assert(operandTy->is<LValueType>() &&
           "must derive element_addr from address");
    assert(!operandTy->hasReferenceSemantics() &&
           "cannot derive element_addr from reference type");
    assert(EI->getType(0)->is<LValueType>() &&
           "result of element_addr must be lvalue");
#endif
  }
  
  void visitRefElementAddrInst(RefElementAddrInst *EI) {
#ifndef NDEBUG
    Type operandTy = EI->getOperand().getType();
    assert(operandTy->hasReferenceSemantics() &&
           "must derive ref_element_addr from reference type");
    assert(EI->getType(0)->is<LValueType>() &&
           "result of element_addr must be lvalue");
#endif
  }
  
  void visitArchetypeMethodInst(ArchetypeMethodInst *AMI) {
#ifndef NDEBUG
    FunctionType *methodType = AMI->getType(0)->getAs<FunctionType>();
    assert(methodType &&
           "result method must be of a concrete function type");
    Type operandType = AMI->getOperand().getType();
    assert(methodType->getInput()->isEqual(operandType) &&
           "result must be a method of the operand");
    assert(methodType->getResult()->is<FunctionType>() &&
           "result must be a method");
    if (LValueType *lvt = operandType->getAs<LValueType>()) {
      assert(lvt->getObjectType()->is<ArchetypeType>() &&
             "archetype_method must apply to an archetype address");
    } else if (MetaTypeType *mt = operandType->getAs<MetaTypeType>()) {
      assert(mt->getInstanceType()->is<ArchetypeType>() &&
             "archetype_method must apply to an archetype metatype");
    } else
      llvm_unreachable("method must apply to an address or metatype");
#endif
  }
  
  void visitExistentialMethodInst(ExistentialMethodInst *EMI) {
#ifndef NDEBUG
    FunctionType *methodType = EMI->getType(0)->getAs<FunctionType>();
    assert(methodType &&
           "result method must be of a concrete function type");
    Type operandType = EMI->getOperand().getType();
    assert(methodType->getInput()->isEqual(
                            operandType->getASTContext().TheRawPointerType) &&
           "result must be a method of the operand");
    assert(methodType->getResult()->is<FunctionType>() &&
           "result must be a method");
    LValueType *operandLVT = operandType->getAs<LValueType>();
    assert(operandLVT &&
           "existential_method must apply to an existential address");
    assert(operandLVT->getObjectType()->isExistentialType() &&
           "existential_method must apply to an existential address");    
#endif
  }
  
  void visitProjectExistentialInst(ProjectExistentialInst *PEI) {
#ifndef NDEBUG
    LValueType *operandLVT = PEI->getOperand().getType()->getAs<LValueType>();
    assert(operandLVT && "project_existential must be applied to address");
    assert(operandLVT->getObjectType()->isExistentialType() &&
           "project_existential must be applied to address of existential");
#endif
  }
  
  void visitAllocExistentialInst(AllocExistentialInst *AEI) {
#ifndef NDEBUG
    LValueType *exLVT = AEI->getExistential().getType()->getAs<LValueType>();
    assert(exLVT && "alloc_existential must be applied to an address");
    assert(exLVT->getObjectType()->isExistentialType() &&
           "alloc_existential must be applied to address of existential");
#endif
  }
  
  void visitArchetypeToSuperInst(ArchetypeToSuperInst *ASI) {
    // FIXME: archetypes should be address-only
    assert(ASI->getOperand().getType()->is<ArchetypeType>() &&
           "archetype_to_super operand must be archetype");
    assert(ASI->getType()->hasReferenceSemantics() &&
           "archetype_to_super must convert to a reference type");
  }
  
  void visitDowncastInst(DowncastInst *DI) {
    assert(DI->getOperand().getType()->hasReferenceSemantics() &&
           "downcast operand must be a reference type");
    assert(DI->getType()->hasReferenceSemantics() &&
           "downcast must convert to a reference type");
  }
  
  void visitIntegerValueInst(IntegerValueInst *IVI) {
    assert(IVI->getType()->is<BuiltinIntegerType>());
  }

  void visitReturnInst(ReturnInst *RI) {
    assert(RI->getReturnValue() && "Return of null value is invalid");
  }
  
  void visitBranchInst(BranchInst *BI) {
  }
  
  void visitCondBranchInst(CondBranchInst *CBI) {
    assert(CBI->getCondition() &&
           "Condition of conditional branch can't be missing");
  }
};
} // end anonymous namespace


/// verify - Run the IR verifier to make sure that the Function follows
/// invariants.
void Function::verify() const {
  SILVerifier().visitFunction(const_cast<Function*>(this));
}
