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
    assert(AI->getType().isAddress() &&"Allocation should return address");
  }

  void visitAllocVarInst(AllocVarInst *AI) {
    visitAllocInst(AI);
  }
  
  bool argumentTypeMatches(SILType argValueType, Type declaredArgType) {
    if (argValueType.isAddressOnly()) {
      return argValueType.getSwiftRValueType()->isEqual(declaredArgType);
    } else {
      return argValueType.getSwiftType()->isEqual(declaredArgType);
    }
  }

  void visitApplyInst(ApplyInst *AI) {
    FunctionType *FT = AI->getCallee().getType().getAs<FunctionType>();
    assert(FT && "Callee of ApplyInst must have concrete function type");
    ASTContext &C = FT->getASTContext();
    bool implicitReturn = false;
    
    if (AI->getType() == SILType::getEmptyTupleType(C) &&
        !FT->getResult()->isEqual(C.TheEmptyTupleType)) {
      // Address-only returns are passed as an implicit extra argument.
      // The result of the instruction should be the empty tuple.
      implicitReturn = true;
    } else {
      assert(FT->getResult()->isEqual(AI->getType().getSwiftType()) &&
             "Return type does not match function type");
      
      // If there is a single argument to the apply, it could either be a scalar
      // or the whole argument tuple being presented all at once.
      if (AI->getArguments().size() == 1 &&
          argumentTypeMatches(AI->getArguments()[0].getType(),
                              FT->getInput())) {
        // The single argument type matches the function argument type, so the
        // arguments are OK.
        return;
      }
    }
    
    // Check that the arguments match the decomposed tuple.
    const TupleType *TT = FT->getInput()->castTo<TupleType>();
    (void)TT;
    for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i)
      assert(argumentTypeMatches(AI->getArguments()[i].getType(),
                                 TT->getFields()[i].getType()) &&
             "ApplyInst argument type mismatch");
    
    if (implicitReturn) {
      // Check that the indirect return argument exists and is of the right
      // type.
      assert(AI->getArguments().size() == TT->getFields().size() + 1 &&
             "ApplyInst doesn't have enough arguments for function "
             "and indirect return");
      SILType indirectReturn = AI->getArguments().back().getType();
      assert(indirectReturn.isAddressOnly() &&
             "indirect return argument is not address-only");
      assert(indirectReturn.getSwiftRValueType()->isEqual(FT->getResult()) &&
             "indirect return argument does not match function return type");
    } else {
      assert(AI->getArguments().size() == TT->getFields().size() &&
             "ApplyInst doesn't have enough arguments for function");
    }
  }

  void visitConstantRefInst(ConstantRefInst *CRI) {
    assert(CRI->getType().is<AnyFunctionType>() &&
           "constant_ref should have a function result");
  }

  void visitIntegerLiteralInst(IntegerLiteralInst *ILI) {
    assert(ILI->getType().is<BuiltinIntegerType>() &&
           "invalid integer literal type");
  }
  void visitLoadInst(LoadInst *LI) {
    assert(!LI->getType().isAddress() && "Can't load an address");
    assert(LI->getLValue().getType().isAddress() &&
           "Load operand must be an address");
    assert(LI->getLValue().getType().getObjectType() == LI->getType() &&
           "Load operand type and result type mismatch");
  }

  void visitStoreInst(StoreInst *SI) {
    assert(!SI->getSrc().getType().isAddress() &&
           "Can't store from an address source");
    assert(SI->getDest().getType().isAddress() &&
           "Must store to an address dest");
    assert(SI->getDest().getType().getObjectType() == SI->getSrc().getType() &&
           "Store operand type and dest type mismatch");
  }

  void visitCopyAddrInst(CopyAddrInst *SI) {
    assert(SI->getSrc().getType().isAddress() &&
           "Src value should be lvalue");
    assert(SI->getDest().getType().isAddress() &&
           "Dest address should be lvalue");
    assert(SI->getDest().getType() == SI->getSrc().getType() &&
           "Store operand type and dest type mismatch");
  }
  
  void visitZeroAddrInst(ZeroAddrInst *ZI) {
    assert(ZI->getDest().getType().isAddress() &&
           "Dest address should be lvalue");
  }
  
  void visitZeroValueInst(ZeroValueInst *ZVI) {
    assert(!ZVI->getType().isAddress() &&
           "zero_value cannot create an address");
  }
  
  void visitSpecializeInst(SpecializeInst *SI) {
    assert(SI->getType().is<FunctionType>() &&
           "Specialize dest should be a function type");
    assert(SI->getOperand().getType().is<PolymorphicFunctionType>() &&
           "Specialize source should be a polymorphic function type");
  }

  void visitTupleInst(TupleInst *TI) {
    assert(TI->getType().is<TupleType>() && "TupleInst should return a tuple");
    TupleType *ResTy = TI->getType().castTo<TupleType>(); (void)ResTy;

    assert(TI->getElements().size() == ResTy->getFields().size() &&
           "Tuple field count mismatch!");
  }
  void visitMetatypeInst(MetatypeInst *MI) {
    assert(MI->getType(0).is<MetaTypeType>() &&
           "Metatype instruction must be of metatype type");
  }
  
  void visitRetainInst(RetainInst *RI) {
    assert(!RI->getOperand().getType().isAddress() &&
           "Operand of retain must not be lvalue");
  }
  void visitReleaseInst(ReleaseInst *RI) {
    assert(!RI->getOperand().getType().isAddress() &&
           "Operand of release must not be lvalue");
  }
  void visitDeallocVarInst(DeallocVarInst *DI) {
    assert(DI->getOperand().getType().isAddress() &&
           "Operand of dealloc must be lvalue");
  }
  void visitDestroyAddrInst(DestroyAddrInst *DI) {
    assert(DI->getOperand().getType().isAddress() &&
           "Operand of destroy must be lvalue");
  }

  void visitIndexAddrInst(IndexAddrInst *IAI) {
    assert(IAI->getType().isAddress() &&
           IAI->getType() == IAI->getOperand().getType() &&
           "invalid IndexAddrInst");
  }
  
  void visitExtractInst(ExtractInst *EI) {
#ifndef NDEBUG
    SILType operandTy = EI->getOperand().getType();
    assert(!operandTy.isAddress() &&
           "cannot extract from address");
    assert(!operandTy.hasReferenceSemantics() &&
           "cannot extract from reference type");
    assert(!EI->getType(0).isAddress() &&
           "result of extract cannot be address");
#endif
  }

  void visitElementAddrInst(ElementAddrInst *EI) {
#ifndef NDEBUG
    SILType operandTy = EI->getOperand().getType();
    assert(operandTy.isAddress() &&
           "must derive element_addr from address");
    assert(!operandTy.hasReferenceSemantics() &&
           "cannot derive element_addr from reference type");
    assert(EI->getType(0).isAddress() &&
           "result of element_addr must be lvalue");
#endif
  }
  
  void visitRefElementAddrInst(RefElementAddrInst *EI) {
#ifndef NDEBUG
    SILType operandTy = EI->getOperand().getType();
    assert(operandTy.hasReferenceSemantics() &&
           "must derive ref_element_addr from reference type");
    assert(EI->getType(0).isAddress() &&
           "result of element_addr must be lvalue");
#endif
  }
  
  void visitArchetypeMethodInst(ArchetypeMethodInst *AMI) {
#ifndef NDEBUG
    FunctionType *methodType = AMI->getType(0).getAs<FunctionType>();
    assert(methodType &&
           "result method must be of a concrete function type");
    SILType operandType = AMI->getOperand().getType();
    assert(methodType->getInput()->isEqual(operandType.getSwiftType()) &&
           "result must be a method of the operand");
    assert(methodType->getResult()->is<FunctionType>() &&
           "result must be a method");
    if (LValueType *lvt = operandType.getAs<LValueType>()) {
      assert(lvt->getObjectType()->is<ArchetypeType>() &&
             "archetype_method must apply to an archetype address");
    } else if (MetaTypeType *mt = operandType.getAs<MetaTypeType>()) {
      assert(mt->getInstanceType()->is<ArchetypeType>() &&
             "archetype_method must apply to an archetype metatype");
    } else
      llvm_unreachable("method must apply to an address or metatype");
#endif
  }
  
  void visitExistentialMethodInst(ExistentialMethodInst *EMI) {
#ifndef NDEBUG
    FunctionType *methodType = EMI->getType(0).getAs<FunctionType>();
    assert(methodType &&
           "result method must be of a concrete function type");
    SILType operandType = EMI->getOperand().getType();
    assert(methodType->getInput()->isEqual(
                            operandType.getASTContext().TheRawPointerType) &&
           "result must be a method of the operand");
    assert(methodType->getResult()->is<FunctionType>() &&
           "result must be a method");
    assert(operandType.isAddress() &&
           "existential_method must apply to an existential address");
    assert(operandType.isExistentialType() &&
           "existential_method must apply to an existential address");    
#endif
  }
  
  void visitProjectExistentialInst(ProjectExistentialInst *PEI) {
#ifndef NDEBUG
    SILType operandType = PEI->getOperand().getType();
    assert(operandType.isAddress() && "project_existential must be applied to address");
    assert(operandType.isExistentialType() &&
           "project_existential must be applied to address of existential");
#endif
  }
  
  void visitAllocExistentialInst(AllocExistentialInst *AEI) {
#ifndef NDEBUG
    SILType exType = AEI->getExistential().getType();
    assert(exType.isAddress() &&
           "alloc_existential must be applied to an address");
    assert(exType.isExistentialType() &&
           "alloc_existential must be applied to address of existential");
#endif
  }
  
  void visitDeallocExistentialInst(DeallocExistentialInst *DEI) {
#ifndef NDEBUG
    SILType exType = DEI->getExistential().getType();
    assert(exType.isAddress() &&
           "dealloc_existential must be applied to an address");
    assert(exType.isExistentialType() &&
           "dealloc_existential must be applied to address of existential");
#endif
  }
  
  void visitArchetypeToSuperInst(ArchetypeToSuperInst *ASI) {
    // FIXME: archetypes should be address-only
    assert(ASI->getOperand().getType().is<ArchetypeType>() &&
           "archetype_to_super operand must be archetype");
    assert(ASI->getType().hasReferenceSemantics() &&
           "archetype_to_super must convert to a reference type");
  }
  
  void visitDowncastInst(DowncastInst *DI) {
    assert(DI->getOperand().getType().hasReferenceSemantics() &&
           "downcast operand must be a reference type");
    assert(DI->getType().hasReferenceSemantics() &&
           "downcast must convert to a reference type");
  }
  
  void visitIntegerValueInst(IntegerValueInst *IVI) {
    assert(IVI->getType().is<BuiltinIntegerType>() &&
           "invalid integer value type");
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
