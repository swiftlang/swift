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
#include "llvm/Support/Debug.h"

using namespace swift;

// The verifier is basically all assertions, so don't compile it with NDEBUG to
// prevent release builds from triggering spurious unused variable warnings.
#ifndef NDEBUG

namespace {
/// SILVerifier class - This class implements the SIL verifier, which walks over
/// SIL, checking and enforcing its invariants.
class SILVerifier : public SILVisitor<SILVerifier> {
  Function const &F;
public:
  SILVerifier(Function const &F) : F(F) {}
  
  void visit(Instruction *I) {
    
    const BasicBlock *BB = I->getParent();
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

  void visitAllocVarInst(AllocVarInst *AI) {
    assert(AI->getType().isAddress() && "alloc_var must return address");
  }
  
  void visitAllocRefInst(AllocRefInst *AI) {
    assert(AI->getType().hasReferenceSemantics() && !AI->getType().isAddress()
           && "alloc_ref must return reference type value");
  }
  
  void visitApplyInst(ApplyInst *AI) {
    DEBUG(llvm::dbgs() << "verifying";
          AI->print(llvm::dbgs()));
    SILType calleeTy = AI->getCallee().getType();
    DEBUG(llvm::dbgs() << "callee type: ";
          AI->getCallee().getType().print(llvm::dbgs());
          llvm::dbgs() << '\n');
    assert(!calleeTy.isAddress() && "callee of apply cannot be an address");
    assert(calleeTy.is<FunctionType>() &&
           "callee of apply must have concrete function type");
    SILFunctionTypeInfo *ti = F.getModule().getFunctionTypeInfo(calleeTy);
    
    DEBUG(llvm::dbgs() << "function input types:\n";
          for (SILType t : ti->getInputTypes()) {
            llvm::dbgs() << "  ";
            t.print(llvm::dbgs());
            llvm::dbgs() << '\n';
          });
    DEBUG(llvm::dbgs() << "function result type ";
          ti->getResultType().print(llvm::dbgs());
          llvm::dbgs() << '\n');

    DEBUG(llvm::dbgs() << "argument types:\n";
          for (Value arg : AI->getArguments()) {
            llvm::dbgs() << "  ";
            arg.getType().print(llvm::dbgs());
            llvm::dbgs() << '\n';
          });
    
    // Check that the arguments and result match.
    assert(AI->getArguments().size() == ti->getInputTypes().size() &&
           "apply doesn't have right number of arguments for function");
    for (size_t i = 0, size = AI->getArguments().size(); i < size; ++i) {
      DEBUG(llvm::dbgs() << "  argument type ";
            AI->getArguments()[i].getType().print(llvm::dbgs());
            llvm::dbgs() << " for input type ";
            ti->getInputTypes()[i].print(llvm::dbgs());
            llvm::dbgs() << '\n');
      assert(AI->getArguments()[i].getType() == ti->getInputTypes()[i] &&
             "input types to apply don't match function input types");
    }
    DEBUG(llvm::dbgs() << "result type ";
          AI->getType().print(llvm::dbgs());
          llvm::dbgs() << '\n');
    assert(AI->getType() == ti->getResultType() &&
           "type of apply instruction doesn't match function result type");
  }
  
  void visitPartialApplyInst(PartialApplyInst *PAI) {
    SILType calleeTy = PAI->getCallee().getType();
    assert(!calleeTy.isAddress() && "callee of closure cannot be an address");
    assert(calleeTy.is<FunctionType>() &&
           "callee of closure must have concrete function type");
    assert(calleeTy.castTo<FunctionType>()->isThin() &&
           "callee of closure must have a thin function type");
    SILType appliedTy = PAI->getType();
    assert(!appliedTy.isAddress() && "result of closure cannot be an address");
    assert(appliedTy.is<FunctionType>() &&
           "result of closure must have concrete function type");
    // FIXME: A "curry" with no arguments could remain thin.
    assert(!appliedTy.castTo<FunctionType>()->isThin() &&
           "result of closure cannot have a thin function type");

    SILFunctionTypeInfo *ti = F.getModule().getFunctionTypeInfo(calleeTy);
    
    // Check that the arguments match the curry levels.
    assert(PAI->getArguments().size() == ti->getCurryInputTypes().size() &&
           "closure doesn't have right number of curry arguments for function");
    for (size_t i = 0, size = PAI->getArguments().size(); i < size; ++i) {
      DEBUG(llvm::dbgs() << "  argument type ";
            PAI->getArguments()[i].getType().print(llvm::dbgs());
            llvm::dbgs() << " for input type ";
            ti->getCurryInputTypes()[i].print(llvm::dbgs());
            llvm::dbgs() << '\n');
      assert(PAI->getArguments()[i].getType() == ti->getCurryInputTypes()[i] &&
             "input types to closure don't match function input types");
    }
    DEBUG(llvm::dbgs() << "result type ";
          PAI->getType().print(llvm::dbgs());
          llvm::dbgs() << '\n');
    
    // The result type should match the uncurried type.
    FunctionType *ft = calleeTy.castTo<FunctionType>();
    for (unsigned i = 0; i < calleeTy.getUncurryLevel(); ++i)
      ft = ft->getResult()->castTo<FunctionType>();
    
    assert(PAI->getType().getSwiftType()->isEqual(ft)
           && PAI->getType().getUncurryLevel() == 0
           && "type of apply instruction doesn't match function result type");
    
  }

  void visitConstantRefInst(ConstantRefInst *CRI) {
    assert(CRI->getType().is<AnyFunctionType>() &&
           "constant_ref should have a function result");
    assert(CRI->getType().castTo<AnyFunctionType>()->isThin() &&
           "constant_ref should have a thin function result");
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
  
  void visitInitializeVarInst(InitializeVarInst *ZI) {
    assert(ZI->getDest().getType().isAddress() &&
           "Dest address should be lvalue");
  }
  
  void visitSpecializeInst(SpecializeInst *SI) {
    assert(SI->getType().is<FunctionType>() &&
           "Specialize result should have a function type");
    assert(SI->getType().castTo<FunctionType>()->isThin() &&
           "Specialize result should have a thin function type");
    
    SILType operandTy = SI->getOperand().getType();
    assert((operandTy.is<PolymorphicFunctionType>()
            || (operandTy.is<FunctionType>()
                && operandTy.castTo<FunctionType>()->getResult()
                  ->is<PolymorphicFunctionType>()))
           && "Specialize source should have a polymorphic function type");
    assert(operandTy.castTo<AnyFunctionType>()->isThin()
           && "Specialize source should have a thin function type");
    assert(SI->getType().getUncurryLevel()
             == SI->getOperand().getType().getUncurryLevel()
           && "Specialize source and dest uncurry levels must match");
  }

  void visitTupleInst(TupleInst *TI) {
    assert(TI->getType().is<TupleType>() && "TupleInst should return a tuple");
    TupleType *ResTy = TI->getType().castTo<TupleType>();

    assert(TI->getElements().size() == ResTy->getFields().size() &&
           "Tuple field count mismatch!");
  }
  void visitMetatypeInst(MetatypeInst *MI) {
    assert(MI->getType(0).is<MetaTypeType>() &&
           "metatype instruction must be of metatype type");
  }
  void visitClassMetatypeInst(ClassMetatypeInst *MI) {
    assert(MI->getType().is<MetaTypeType>() &&
           "class_metatype instruction must be of metatype type");
    assert(MI->getBase().getType().getSwiftType()->getClassOrBoundGenericClass() &&
           "class_metatype base must be of class type");
    assert(MI->getBase().getType().getSwiftType() ==
             CanType(MI->getType().castTo<MetaTypeType>()->getInstanceType()) &&
           "class_metatype result must be metatype of base class type");
  }
  void visitModuleInst(ModuleInst *MI) {
    assert(MI->getType(0).is<ModuleType>() &&
           "module instruction must be of module type");
  }
  void visitAssociatedMetatypeInst(AssociatedMetatypeInst *MI) {
    assert(MI->getType(0).is<MetaTypeType>() &&
           "associated_metatype instruction must be of metatype type");
    assert(MI->getSourceMetatype().getType().is<MetaTypeType>() &&
           "associated_metatype operand must be of metatype type");
  }
  
  void visitRetainInst(RetainInst *RI) {
    assert(!RI->getOperand().getType().isAddress() &&
           "Operand of retain must not be address");
    assert(RI->getOperand().getType().hasReferenceSemantics() &&
           "Operand of dealloc_ref must be reference type");
  }
  void visitReleaseInst(ReleaseInst *RI) {
    assert(!RI->getOperand().getType().isAddress() &&
           "Operand of release must not be address");
    assert(RI->getOperand().getType().hasReferenceSemantics() &&
           "Operand of dealloc_ref must be reference type");
  }
  void visitDeallocVarInst(DeallocVarInst *DI) {
    assert(DI->getOperand().getType().isAddress() &&
           "Operand of dealloc_var must be address");
  }
  void visitDeallocRefInst(DeallocRefInst *DI) {
    assert(!DI->getOperand().getType().isAddress() &&
           "Operand of dealloc_ref must not be address");
    assert(DI->getOperand().getType().hasReferenceSemantics() &&
           "Operand of dealloc_ref must be reference type");
  }
  void visitDestroyAddrInst(DestroyAddrInst *DI) {
    assert(DI->getOperand().getType().isAddressOnly() &&
           "Operand of destroy_addr must be address-only");
  }

  void visitIndexAddrInst(IndexAddrInst *IAI) {
    assert(IAI->getType().isAddress() &&
           IAI->getType() == IAI->getOperand().getType() &&
           "invalid IndexAddrInst");
  }
  
  void visitExtractInst(ExtractInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    assert(!operandTy.isAddress() &&
           "cannot extract from address");
    assert(!operandTy.hasReferenceSemantics() &&
           "cannot extract from reference type");
    assert(!EI->getType(0).isAddress() &&
           "result of extract cannot be address");
  }

  void visitElementAddrInst(ElementAddrInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    assert(operandTy.isAddress() &&
           "must derive element_addr from address");
    assert(!operandTy.hasReferenceSemantics() &&
           "cannot derive element_addr from reference type");
    assert(EI->getType(0).isAddress() &&
           "result of element_addr must be lvalue");
  }
  
  void visitRefElementAddrInst(RefElementAddrInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    assert(!operandTy.isAddress() &&
           "must derive ref_element_addr from non-address");
    assert(operandTy.hasReferenceSemantics() &&
           "must derive ref_element_addr from reference type");
    assert(EI->getType(0).isAddress() &&
           "result of ref_element_addr must be lvalue");
  }
  
  void visitArchetypeMethodInst(ArchetypeMethodInst *AMI) {
    DEBUG(llvm::dbgs() << "verifying";
          AMI->print(llvm::dbgs()));
    assert(AMI->getType(0).getUncurryLevel() == AMI->getMember().uncurryLevel &&
           "result method must be at natural uncurry level of method");
    FunctionType *methodType = AMI->getType(0).getAs<FunctionType>();
    DEBUG(llvm::dbgs() << "method type ";
          methodType->print(llvm::dbgs());
          llvm::dbgs() << "\n");
    assert(methodType &&
           "result method must be of a concrete function type");
    assert(methodType->isThin() &&
           "result method must be of a thin function type");
    SILType operandType = AMI->getOperand().getType();
    DEBUG(llvm::dbgs() << "operand type ";
          operandType.print(llvm::dbgs());
          llvm::dbgs() << "\n");
    assert(methodType->getInput()->isEqual(operandType.getSwiftType()) &&
           "result must be a method of the operand");
    assert(methodType->getResult()->is<FunctionType>() &&
           "result must be a method");
    if (operandType.isAddress()) {
      assert(operandType.is<ArchetypeType>() &&
             "archetype_method must apply to an archetype address");
    } else if (MetaTypeType *mt = operandType.getAs<MetaTypeType>()) {
      assert(mt->getInstanceType()->is<ArchetypeType>() &&
             "archetype_method must apply to an archetype metatype");
    } else
      llvm_unreachable("method must apply to an address or metatype");
  }
  
  void visitProtocolMethodInst(ProtocolMethodInst *EMI) {
    assert(EMI->getType(0).getUncurryLevel() == EMI->getMember().uncurryLevel &&
           "result method must be at natural uncurry level of method");
    assert(EMI->getType(0).getUncurryLevel() == 1 &&
           "protocol method result must be at uncurry level 1");
    FunctionType *methodType = EMI->getType(0).getAs<FunctionType>();
    assert(methodType &&
           "result method must be of a concrete function type");
    assert(!methodType->isThin() &&
           "result method must not be of a thin function type");
    SILType operandType = EMI->getOperand().getType();
    assert(methodType->getInput()->isEqual(
                            operandType.getASTContext().TheOpaquePointerType) &&
           "result must be a method of opaque pointer");
    assert(methodType->getResult()->is<FunctionType>() &&
           "result must be a method");
    assert(operandType.isAddress() &&
           "protocol_method must apply to an existential address");
    assert(operandType.isExistentialType() &&
           "protocol_method must apply to an existential address");
  }
  
  static bool isClassOrClassMetatype(Type t) {
    if (auto *meta = t->getAs<MetaTypeType>()) {
      return bool(meta->getInstanceType()->getClassOrBoundGenericClass());
    } else {
      return bool(t->getClassOrBoundGenericClass());
    }
  }
  
  void visitClassMethodInst(ClassMethodInst *CMI) {
    assert(CMI->getType(0).getUncurryLevel() == CMI->getMember().uncurryLevel &&
           "result method must be at natural uncurry level of method");
    auto *methodType = CMI->getType(0).getAs<AnyFunctionType>();
    assert(methodType &&
           "result method must be of a function type");
    assert(methodType->isThin() &&
           "result method must be of a thin function type");
    SILType operandType = CMI->getOperand().getType();
    assert(isClassOrClassMetatype(operandType.getSwiftType()) &&
           "operand must be of a class type");
    assert(isClassOrClassMetatype(methodType->getInput()) &&
           "result must be a method of a class");
    assert(methodType->getResult()->is<AnyFunctionType>() &&
           "result must be a method");
  }
  
  void visitSuperMethodInst(SuperMethodInst *CMI) {
    assert(CMI->getType(0).getUncurryLevel() == CMI->getMember().uncurryLevel &&
           "result method must be at natural uncurry level of method");
    auto *methodType = CMI->getType(0).getAs<AnyFunctionType>();
    assert(methodType &&
           "result method must be of a function type");
    assert(methodType->isThin() &&
           "result method must be of a thin function type");
    SILType operandType = CMI->getOperand().getType();
    assert(isClassOrClassMetatype(operandType.getSwiftType()) &&
           "operand must be of a class type");
    assert(isClassOrClassMetatype(methodType->getInput()) &&
           "result must be a method of a class");
    assert(methodType->getResult()->is<AnyFunctionType>() &&
           "result must be a method");
  }
  
  void visitProjectExistentialInst(ProjectExistentialInst *PEI) {
    SILType operandType = PEI->getOperand().getType();
    assert(operandType.isAddress() && "project_existential must be applied to address");
    assert(operandType.isExistentialType() &&
           "project_existential must be applied to address of existential");
  }
  
  void visitInitExistentialInst(InitExistentialInst *AEI) {
    SILType exType = AEI->getExistential().getType();
    assert(exType.isAddress() &&
           "init_existential must be applied to an address");
    assert(exType.isExistentialType() &&
           "init_existential must be applied to address of existential");
    assert(!AEI->getConcreteType()->isExistentialType() &&
           "init_existential cannot put an existential container inside "
           "an existential container");
  }
  
  void visitUpcastExistentialInst(UpcastExistentialInst *UEI) {
    SILType srcType = UEI->getSrcExistential().getType();
    SILType destType = UEI->getDestExistential().getType();
    assert(srcType.isAddress() &&
           "upcast_existential source must be an address");
    assert(srcType.isExistentialType() &&
           "upcast_existential source must be address of existential");
    assert(destType.isAddress() &&
           "upcast_existential dest must be an address");
    assert(destType.isExistentialType() &&
           "upcast_existential dest must be address of existential");
  }
  
  void visitDeinitExistentialInst(DeinitExistentialInst *DEI) {
    SILType exType = DEI->getExistential().getType();
    assert(exType.isAddress() &&
           "deinit_existential must be applied to an address");
    assert(exType.isExistentialType() &&
           "deinit_existential must be applied to address of existential");
  }
  
  void visitArchetypeToSuperInst(ArchetypeToSuperInst *ASI) {
    assert(ASI->getOperand().getType().isAddressOnly() &&
           "archetype_to_super operand must be an address");
    assert(ASI->getOperand().getType().is<ArchetypeType>() &&
           "archetype_to_super operand must be archetype");
    assert(ASI->getType().hasReferenceSemantics() &&
           "archetype_to_super must convert to a reference type");
  }
  
  void visitThinToThickFunctionInst(ThinToThickFunctionInst *TTFI) {
    assert(!TTFI->getOperand().getType().isAddress() &&
           "thin_to_thick_function operand cannot be an address");
    assert(!TTFI->getType().isAddress() &&
           "thin_to_thick_function result cannot be an address");
    assert(TTFI->getOperand().getType().is<AnyFunctionType>() &&
           "thin_to_thick_function operand must be a function");
    assert(TTFI->getType().is<AnyFunctionType>() &&
           "thin_to_thick_function result must be a function");
    if (auto *opFTy = dyn_cast<FunctionType>(
                                 TTFI->getOperand().getType().getSwiftType())) {
      auto *resFTy = dyn_cast<FunctionType>(TTFI->getType().getSwiftType());
      assert(resFTy &&
             opFTy->getInput()->isEqual(resFTy->getInput()) &&
             opFTy->getResult()->isEqual(resFTy->getResult()) &&
             opFTy->isAutoClosure() == resFTy->isAutoClosure() &&
             opFTy->isBlock() == resFTy->isBlock() &&
             "thin_to_thick_function operand and result type must differ only "
             " in thinness");
      assert(!resFTy->isThin() &&
             "thin_to_thick_function result must not be thin");
      assert(opFTy->isThin() &&
             "thin_to_thick_function operand must be thin");
    } else if (auto *opPTy = dyn_cast<PolymorphicFunctionType>(
                                 TTFI->getOperand().getType().getSwiftType())) {
      auto *resPTy = dyn_cast<PolymorphicFunctionType>(
                                               TTFI->getType().getSwiftType());
      assert(resPTy &&
             opPTy->getInput()->isEqual(resPTy->getInput()) &&
             opPTy->getResult()->isEqual(resPTy->getResult()) &&
             "thin_to_thick_function operand and result type must differ only "
             " in thinness");
      assert(!resPTy->isThin() &&
             "thin_to_thick_function result must not be thin");
      assert(opPTy->isThin() &&
             "thin_to_thick_function operand must be thin");
    } else {
      llvm_unreachable("invalid AnyFunctionType?!");
    }
  }
  
  void visitSuperToArchetypeInst(SuperToArchetypeInst *SAI) {
    assert(SAI->getSrcBase().getType().hasReferenceSemantics() &&
           "super_to_archetype source must be a reference type");
    assert(SAI->getDestArchetypeAddress().getType().is<ArchetypeType>() &&
           "super_to_archetype dest must be an archetype address");
  }

  void visitUpcastInst(UpcastInst *UI) {
    if (UI->getType().is<MetaTypeType>()) {
      CanType instTy(UI->getType().castTo<MetaTypeType>()->getInstanceType());
      assert(UI->getOperand().getType().is<MetaTypeType>()
             && "upcast operand must be a class or class metatype instance");
      CanType opInstTy(UI->getOperand().getType().castTo<MetaTypeType>()
                         ->getInstanceType());
      assert(opInstTy->getClassOrBoundGenericClass()
             && "upcast operand must be a class or class metatype instance");
      assert(instTy->getClassOrBoundGenericClass()
             && "upcast must convert a class metatype to a class metatype");
    } else {
      assert(UI->getOperand().getType().getSwiftType()
              ->getClassOrBoundGenericClass() &&
             "upcast operand must be a class or class metatype instance");
      assert(UI->getType().getSwiftType()->getClassOrBoundGenericClass() &&
             "upcast must convert a class instance to a class type");
    }
  }
  
  void visitDowncastInst(DowncastInst *DI) {
    assert(DI->getOperand().getType().hasReferenceSemantics() &&
           "downcast operand must be a reference type");
    assert(DI->getType().hasReferenceSemantics() &&
           "downcast must convert to a reference type");
  }
  
  void visitAddressToPointerInst(AddressToPointerInst *AI) {
    assert(AI->getOperand().getType().isAddress() &&
           "address-to-pointer operand must be an address");
    assert(AI->getType().getSwiftType()->isEqual(
                              AI->getType().getASTContext().TheRawPointerType)
           && "address-to-pointer result type must be RawPointer");
  }
  
  void visitImplicitConvertInst(ImplicitConvertInst *ICI) {
    assert(!ICI->getOperand().getType().isAddress() &&
           "conversion operand cannot be an address");
    assert(!ICI->getType().isAddress() &&
           "conversion result cannot be an address");
    
    if (auto *opFTy = ICI->getOperand().getType().getAs<FunctionType>()) {
      auto *resFTy = ICI->getType().getAs<FunctionType>();
      assert(resFTy && "function must be converted to another function");
      assert(opFTy->isThin() == resFTy->isThin() &&
             "implicit_convert cannot change function thinness");
    }
  }
  
  void visitIntegerValueInst(IntegerValueInst *IVI) {
    assert(IVI->getType().is<BuiltinIntegerType>() &&
           "invalid integer value type");
  }

  void visitReturnInst(ReturnInst *RI) {
    DEBUG(RI->print(llvm::dbgs()));
    assert(RI->getReturnValue() && "Return of null value is invalid");
    
    // FIXME: when curry entry points are typed properly, verify return type
    // here.
    /*
    SILFunctionTypeInfo *ti =
      F.getModule().getFunctionTypeInfo(F.getLoweredType());
    DEBUG(llvm::dbgs() << "  operand type ";
          RI->getReturnValue().getType().print(llvm::dbgs());
          llvm::dbgs() << " for return type ";
          ti->getResultType().print(llvm::dbgs());
          llvm::dbgs() << '\n');
    assert(RI->getReturnValue().getType() == ti->getResultType() &&
           "return value type does not match return type of function");
     */
  }
  
  void visitBranchInst(BranchInst *BI) {
  }
  
  void visitCondBranchInst(CondBranchInst *CBI) {
    assert(CBI->getCondition() &&
           "Condition of conditional branch can't be missing");
  }
  
  void verify() {
    visitFunction(const_cast<Function*>(&F));
  }
};
} // end anonymous namespace

#endif //NDEBUG

/// verify - Run the IR verifier to make sure that the Function follows
/// invariants.
void Function::verify() const {
#ifndef NDEBUG
  SILVerifier(*this).verify();
#endif
}
