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

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/Dominance.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/Basic/Range.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// The verifier is basically all assertions, so don't compile it with NDEBUG to
// prevent release builds from triggering spurious unused variable warnings.
#ifndef NDEBUG

namespace {
/// Metaprogramming-friendly base class.
template <class Impl>
class SILVerifierBase : public SILInstructionVisitor<Impl> {
public:
  // visitCLASS calls visitPARENT and checkCLASS.
  // checkCLASS does nothing by default.
#define VALUE(CLASS, PARENT)                                    \
  void visit##CLASS(CLASS *I) {                                 \
    static_cast<Impl*>(this)->visit##PARENT(I);                 \
    static_cast<Impl*>(this)->check##CLASS(I);                  \
  }                                                             \
  void check##CLASS(CLASS *I) {}
#include "swift/SIL/SILNodes.def"

  void visitValueBase(ValueBase *V) {
    static_cast<Impl*>(this)->checkValueBase(V);
  }
  void checkValueBase(ValueBase *V) {}
};

/// The SIL verifier walks over a SIL function / basic block / instruction,
/// checking and enforcing its invariants.
class SILVerifier : public SILVerifierBase<SILVerifier> {
  const SILFunction &F;
  const SILInstruction *CurInstruction = nullptr;
  DominanceInfo Dominance;
public:
  SILVerifier(SILFunction const &F)
    : F(F), Dominance(const_cast<SILFunction*>(&F)) {}
  
  void _require(bool condition, const char *complaint) {
    if (condition) return;

    llvm::dbgs() << "SIL verification failed: " << complaint << "\n";

    if (CurInstruction) {
      llvm::dbgs() << "Verifying instruction:\n";
      CurInstruction->print(llvm::dbgs());
      llvm::dbgs() << "In basic block:\n";
      CurInstruction->getParent()->print(llvm::dbgs());
    }

    assert(0 && "triggering standard assertion failure routine");
  }
#define require(condition, complaint) \
  _require(condition, complaint ": " #condition)

  void visitSILInstruction(SILInstruction *I) {
    CurInstruction = I;
    checkSILInstruction(I);
  }

  void checkSILInstruction(SILInstruction *I) {
    const SILBasicBlock *BB = I->getParent();
    // Check that non-terminators look ok.
    if (!isa<TermInst>(I)) {
      require(!BB->empty(), "Can't be in a parent block if it is empty");
      require(&*BB->getInsts().rbegin() != I,
              "Non-terminators cannot be the last in a block");
    } else {
      require(&*BB->getInsts().rbegin() == I,
              "Terminator must be the last in block");
    }

    // Verify that all of our uses are in this function.
    for (Operand *use : I->getUses()) {
      auto user = use->getUser();
      require(user, "instruction user is null?");
      require(isa<SILInstruction>(user),
              "instruction used by non-instruction");
      auto userI = cast<SILInstruction>(user);
      require(userI->getParent(),
              "instruction used by unparented instruction");
      require(userI->getParent()->getParent() == &F,
              "instruction used by instruction in different function");

      auto operands = userI->getAllOperands();
      require(operands.begin() <= use && use <= operands.end(),
              "use doesn't actually belong to instruction it claims to");
    }

    // Verify some basis structural stuff about an instruction's operands.
    for (auto &operand : I->getAllOperands()) {
      require(operand.get().isValid(), "instruction has null operand");

      if (auto *valueI = dyn_cast<SILInstruction>(operand.get())) {
        require(valueI->getParent(),
                "instruction uses value of unparented instruction");
        require(valueI->getParent()->getParent() == &F,
                "instruction uses value of instruction from different function");
        require(Dominance.properlyDominates(valueI, I),
                "instruction doesn't dominate its operand");
      }

      require(operand.getUser() == I,
              "instruction's operand's owner isn't the instruction");
      require(isInValueUses(&operand), "operand value isn't used by operand");
    }
  }

  /// Check that this operand appears in the use-chain of the value it uses.
  static bool isInValueUses(const Operand *operand) {
    for (auto use : operand->get()->getUses())
      if (use == operand)
        return true;
    return false;
  }

  void checkAllocVarInst(AllocVarInst *AI) {
    require(AI->getType().isAddress(), "alloc_var must return address");
  }
  
  void checkAllocRefInst(AllocRefInst *AI) {
    require(AI->getType().hasReferenceSemantics() && !AI->getType().isAddress(),
            "alloc_ref must return reference type value");
  }
  
  void checkApplyInst(ApplyInst *AI) {
    DEBUG(llvm::dbgs() << "verifying";
          AI->print(llvm::dbgs()));
    SILType calleeTy = AI->getCallee().getType();
    DEBUG(llvm::dbgs() << "callee type: ";
          AI->getCallee().getType().print(llvm::dbgs());
          llvm::dbgs() << '\n');
    require(!calleeTy.isAddress(), "callee of apply cannot be an address");
    require(calleeTy.is<FunctionType>(),
            "callee of apply must have concrete function type");
    SILFunctionTypeInfo *ti = calleeTy.getFunctionTypeInfo(F.getModule());
    
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
          for (SILValue arg : AI->getArguments()) {
            llvm::dbgs() << "  ";
            arg.getType().print(llvm::dbgs());
            llvm::dbgs() << '\n';
          });
    
    // Check that the arguments and result match.
    require(AI->getArguments().size() == ti->getInputTypes().size(),
            "apply doesn't have right number of arguments for function");
    for (size_t i = 0, size = AI->getArguments().size(); i < size; ++i) {
      DEBUG(llvm::dbgs() << "  argument type ";
            AI->getArguments()[i].getType().print(llvm::dbgs());
            llvm::dbgs() << " for input type ";
            ti->getInputTypes()[i].print(llvm::dbgs());
            llvm::dbgs() << '\n');
      require(AI->getArguments()[i].getType() == ti->getInputTypes()[i],
              "input types to apply don't match function input types");
    }
    DEBUG(llvm::dbgs() << "result type ";
          AI->getType().print(llvm::dbgs());
          llvm::dbgs() << '\n');
    require(AI->getType() == ti->getResultType(),
            "type of apply instruction doesn't match function result type");
  }
  
  void checkPartialApplyInst(PartialApplyInst *PAI) {
    SILType calleeTy = PAI->getCallee().getType();
    require(!calleeTy.isAddress(), "callee of closure cannot be an address");
    require(calleeTy.is<FunctionType>(),
            "callee of closure must have concrete function type");
    SILType appliedTy = PAI->getType();
    require(!appliedTy.isAddress(), "result of closure cannot be an address");
    require(appliedTy.is<FunctionType>(),
            "result of closure must have concrete function type");
    // FIXME: A "curry" with no arguments could remain thin.
    require(!appliedTy.castTo<FunctionType>()->isThin(),
            "result of closure cannot have a thin function type");

    SILFunctionTypeInfo *info
      = calleeTy.getFunctionTypeInfo(F.getModule());
    SILFunctionTypeInfo *resultInfo
      = appliedTy.getFunctionTypeInfo(F.getModule());
    
    // The arguments must match the suffix of the original function's input
    // types.
    require(PAI->getArguments().size() + resultInfo->getInputTypes().size()
              == info->getInputTypes().size(),
            "result of partial_apply should take as many inputs as were not "
            "applied by the instruction");
    
    unsigned offset = info->getInputTypes().size() - PAI->getArguments().size();
    
    for (unsigned i = 0, size = PAI->getArguments().size(); i < size; ++i) {
      require(PAI->getArguments()[i].getType()
                == info->getInputTypes()[i + offset],
              "applied argument types do not match suffix of function type's "
              "inputs");
    }
    
    // The arguments to the result function type must match the prefix of the
    // original function's input types.
    for (unsigned i = 0, size = resultInfo->getInputTypes().size();
         i < size; ++i) {
      require(resultInfo->getInputTypes()[i] == info->getInputTypes()[i],
              "inputs to result function type do not match unapplied inputs "
              "of original function");
    }
    require(resultInfo->getResultType() == info->getResultType(),
            "result type of result function type does not match original "
            "function");
  }

  void checkBuiltinFunctionRefInst(BuiltinFunctionRefInst *BFI) {
    require(isa<BuiltinModule>(BFI->getFunction()->getDeclContext()),
         "builtin_function_ref must refer to a function in the Builtin module");
    require(BFI->getType().is<AnyFunctionType>(),
            "builtin_function_ref should have a function result");
    require(BFI->getType().castTo<AnyFunctionType>()->isThin(),
            "builtin_function_ref should have a thin function result");
  }
  
  void checkFunctionRefInst(FunctionRefInst *CRI) {
    require(CRI->getType().is<AnyFunctionType>(),
            "function_ref should have a function result");
    require(CRI->getType().castTo<AnyFunctionType>()->isThin(),
            "function_ref should have a thin function result");
  }
  
  void checkGlobalAddrInst(GlobalAddrInst *GAI) {
    require(GAI->getType().isAddress(),
            "GlobalAddr must have an address result type");
    require(!GAI->getGlobal()->isProperty(),
            "GlobalAddr cannot take the address of a property decl");
    require(!GAI->getGlobal()->getDeclContext()->isLocalContext(),
            "GlobalAddr cannot take the address of a local var");
  }

  void checkIntegerLiteralInst(IntegerLiteralInst *ILI) {
    require(ILI->getType().is<BuiltinIntegerType>(),
            "invalid integer literal type");
  }
  void checkLoadInst(LoadInst *LI) {
    require(!LI->getType().isAddress(), "Can't load an address");
    require(LI->getOperand().getType().isAddress(),
            "Load operand must be an address");
    require(LI->getOperand().getType().getObjectType() == LI->getType(),
            "Load operand type and result type mismatch");
  }

  void checkStoreInst(StoreInst *SI) {
    require(!SI->getSrc().getType().isAddress(),
            "Can't store from an address source");
    require(SI->getDest().getType().isAddress(),
            "Must store to an address dest");
    require(SI->getDest().getType().getObjectType() == SI->getSrc().getType(),
            "Store operand type and dest type mismatch");
  }

  void checkCopyAddrInst(CopyAddrInst *SI) {
    require(SI->getSrc().getType().isAddress(),
            "Src value should be lvalue");
    require(SI->getDest().getType().isAddress(),
            "Dest address should be lvalue");
    require(SI->getDest().getType() == SI->getSrc().getType(),
            "Store operand type and dest type mismatch");
  }
  
  void checkInitializeVarInst(InitializeVarInst *ZI) {
    require(ZI->getOperand().getType().isAddress(),
            "Dest address should be lvalue");
  }
  
  void checkSpecializeInst(SpecializeInst *SI) {
    require(SI->getType().is<FunctionType>(),
            "Specialize result should have a function type");
    require(SI->getType().castTo<FunctionType>()->isThin(),
            "Specialize result should have a thin function type");
    
    SILType operandTy = SI->getOperand().getType();
    require((operandTy.is<PolymorphicFunctionType>()
            || (operandTy.is<FunctionType>()
                && operandTy.castTo<FunctionType>()->getResult()
                  ->is<PolymorphicFunctionType>())),
            "Specialize source should have a polymorphic function type");
    require(operandTy.castTo<AnyFunctionType>()->isThin(),
            "Specialize source should have a thin function type");
  }

  void checkStructInst(StructInst *SI) {
    require(SI->getType().is<StructType>()
            || SI->getType().is<BoundGenericStructType>(),
            "StructInst should return a struct");
    require(!SI->getType().isAddress(),
            "StructInst cannot produce an address");
    
    // FIXME: Verify element count and types.
  }

  void checkTupleInst(TupleInst *TI) {
    require(TI->getType().is<TupleType>(), "TupleInst should return a tuple");
    require(!TI->getType().isAddress(),
            "TupleInst cannot produce an address");
    TupleType *ResTy = TI->getType().castTo<TupleType>();

    require(TI->getElements().size() == ResTy->getFields().size(),
            "Tuple field count mismatch!");
    
    for (size_t i = 0, size = TI->getElements().size(); i < size; ++i) {
      require(TI->getElements()[i].getType().getSwiftType()
               ->isEqual(ResTy->getFields()[i].getType()),
              "Tuple element arguments do not match tuple type!");
    }
  }
  
  void checkBuiltinZeroInst(BuiltinZeroInst *ZI) {
    // FIXME: We don't want reference types to be nullable.
    require(ZI->getType().is<BuiltinType>()
            || ZI->getType().hasReferenceSemantics(),
            "builtin_zero result must be a builtin or reference type");
  }
  
  void checkMetatypeInst(MetatypeInst *MI) {
    require(MI->getType(0).is<MetaTypeType>(),
            "metatype instruction must be of metatype type");
  }
  void checkClassMetatypeInst(ClassMetatypeInst *MI) {
    require(MI->getType().is<MetaTypeType>(),
            "class_metatype instruction must be of metatype type");
    require(MI->getOperand().getType().getSwiftType()
            ->getClassOrBoundGenericClass(),
            "class_metatype base must be of class type");
    require(MI->getOperand().getType().getSwiftType() ==
             CanType(MI->getType().castTo<MetaTypeType>()->getInstanceType()),
            "class_metatype result must be metatype of base class type");
  }
  void checkArchetypeMetatypeInst(ArchetypeMetatypeInst *MI) {
    require(MI->getType().is<MetaTypeType>(),
            "archetype_metatype instruction must be of metatype type");
    require(MI->getOperand().getType().getSwiftRValueType()->is<ArchetypeType>(),
            "archetype_metatype operand must be of archetype type");
    require(MI->getOperand().getType().getSwiftRValueType() ==
            CanType(MI->getType().castTo<MetaTypeType>()->getInstanceType()),
            "archetype_metatype result must be metatype of operand type");
  }
  void checkProtocolMetatypeInst(ProtocolMetatypeInst *MI) {
    require(MI->getType().is<MetaTypeType>(),
            "protocol_metatype instruction must be of metatype type");
    require(MI->getOperand().getType().getSwiftRValueType()->isExistentialType(),
            "protocol_metatype operand must be of protocol type");
    require(MI->getOperand().getType().getSwiftRValueType() ==
            CanType(MI->getType().castTo<MetaTypeType>()->getInstanceType()),
            "protocol_metatype result must be metatype of operand type");
  }
  void checkModuleInst(ModuleInst *MI) {
    require(MI->getType(0).is<ModuleType>(),
            "module instruction must be of module type");
  }
  void checkAssociatedMetatypeInst(AssociatedMetatypeInst *MI) {
    require(MI->getType(0).is<MetaTypeType>(),
            "associated_metatype instruction must be of metatype type");
    require(MI->getOperand().getType().is<MetaTypeType>(),
            "associated_metatype operand must be of metatype type");
  }
  
  void checkRetainInst(RetainInst *RI) {
    require(!RI->getOperand().getType().isAddress(),
            "Operand of retain must not be address");
    require(RI->getOperand().getType().hasReferenceSemantics(),
            "Operand of retain must be reference type");
  }
  void checkRetainAutoreleasedInst(RetainAutoreleasedInst *RI) {
    require(!RI->getOperand().getType().isAddress(),
            "Operand of retain_autoreleased must not be address");
    require(RI->getOperand().getType().hasReferenceSemantics(),
            "Operand of retain_autoreleased must be reference type");
    require(isa<ApplyInst>(RI->getOperand()),
            "Operand of retain_autoreleased must be the return value of "
            "an apply instruction");
  }
  void checkReleaseInst(ReleaseInst *RI) {
    require(!RI->getOperand().getType().isAddress(),
            "Operand of release must not be address");
    require(RI->getOperand().getType().hasReferenceSemantics(),
            "Operand of dealloc_ref must be reference type");
  }
  void checkDeallocVarInst(DeallocVarInst *DI) {
    require(DI->getOperand().getType().isAddress(),
            "Operand of dealloc_var must be address");
  }
  void checkDeallocRefInst(DeallocRefInst *DI) {
    require(!DI->getOperand().getType().isAddress(),
            "Operand of dealloc_ref must not be address");
    require(DI->getOperand().getType().hasReferenceSemantics(),
            "Operand of dealloc_ref must be reference type");
  }
  void checkDestroyAddrInst(DestroyAddrInst *DI) {
    require(DI->getOperand().getType().isAddressOnly(F.getModule()),
            "Operand of destroy_addr must be address-only");
  }

  void checkIndexAddrInst(IndexAddrInst *IAI) {
    require(IAI->getType().isAddress(), "index_addr must produce an address");
    require(IAI->getType() == IAI->getBase().getType(),
            "index_addr must produce an address of the same type as its base");
    require(IAI->getIndex().getType().is<BuiltinIntegerType>(),
            "index_addr index must be of a builtin integer type");
  }
  
  void checkIndexRawPointerInst(IndexRawPointerInst *IAI) {
    require(IAI->getType().is<BuiltinRawPointerType>(),
            "index_raw_pointer must produce a RawPointer");
    require(IAI->getBase().getType().is<BuiltinRawPointerType>(),
            "index_raw_pointer base must be a RawPointer");
    require(IAI->getIndex().getType().is<BuiltinIntegerType>(),
            "index_raw_pointer index must be of a builtin integer type");
  }
  
  void checkTupleExtractInst(TupleExtractInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    require(!operandTy.isAddress(),
            "cannot tuple_extract from address");
    require(!EI->getType(0).isAddress(),
            "result of tuple_extract cannot be address");
    require(operandTy.is<TupleType>(),
            "must tuple_extract from tuple");
    
    ArrayRef<TupleTypeElt> fields = operandTy.castTo<TupleType>()->getFields();
    require(EI->getFieldNo() < fields.size(),
            "invalid field index for element_addr instruction");
    require(EI->getType().getSwiftRValueType()
            == CanType(fields[EI->getFieldNo()].getType()),
            "type of tuple_element_addr does not match type of element");
  }

  void checkStructExtractInst(StructExtractInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    require(!operandTy.isAddress(),
            "cannot struct_extract from address");
    require(!EI->getType(0).isAddress(),
            "result of struct_extract cannot be address");
    require(operandTy.is<StructType>()
            || operandTy.is<BoundGenericStructType>(),
            "must struct_extract from struct");
    require(!EI->getField()->isProperty(),
            "cannot load logical property with struct_extract");

    // FIXME: Verify type of instruction. This requires type substitution for
    // generic types.
  }
  
  void checkTupleElementAddrInst(TupleElementAddrInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    require(operandTy.isAddress(),
            "must derive element_addr from address");
    require(!operandTy.hasReferenceSemantics(),
            "cannot derive tuple_element_addr from reference type");
    require(EI->getType(0).isAddress(),
            "result of tuple_element_addr must be address");
    require(operandTy.is<TupleType>(),
            "must derive tuple_element_addr from tuple");

    ArrayRef<TupleTypeElt> fields = operandTy.castTo<TupleType>()->getFields();
    require(EI->getFieldNo() < fields.size(),
            "invalid field index for element_addr instruction");
    require(EI->getType().getSwiftRValueType()
              == CanType(fields[EI->getFieldNo()].getType()),
            "type of tuple_element_addr does not match type of element");
  }

  void checkStructElementAddrInst(StructElementAddrInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    require(operandTy.isAddress(),
            "must derive struct_element_addr from address");
    require(operandTy.is<StructType>()
            || operandTy.is<BoundGenericStructType>(),
            "must derive struct_element_addr from struct address");
    require(EI->getType(0).isAddress(),
            "result of struct_element_addr must be address");
    require(!EI->getField()->isProperty(),
            "cannot get address of logical property with struct_element_addr");
    
    // FIXME: Verify type of instruction. This requires type substitution for
    // generic types.
  }

  void checkRefElementAddrInst(RefElementAddrInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    require(!operandTy.isAddress(),
            "must derive ref_element_addr from non-address");
    require(operandTy.hasReferenceSemantics(),
            "must derive ref_element_addr from reference type");
    require(EI->getType(0).isAddress(),
            "result of ref_element_addr must be lvalue");
    require(!EI->getField()->isProperty(),
            "cannot get address of logical property with ref_element_addr");

    // FIXME: Verify type of instruction. This requires type substitution for
    // generic types.
  }
  
  CanType getMethodThisType(AnyFunctionType *ft) {
    Lowering::UncurryDirection direction
      = F.getModule().Types.getUncurryDirection(ft->getAbstractCC());
    
    auto *inputTuple = ft->getInput()->getAs<TupleType>();
    if (!inputTuple)
      return ft->getInput()->getCanonicalType();
    
    switch (direction) {
    case Lowering::UncurryDirection::LeftToRight:
      return inputTuple->getFields()[0].getType()->getCanonicalType();
    case Lowering::UncurryDirection::RightToLeft:
      return inputTuple->getFields().back().getType()->getCanonicalType();
    }
  }
  
  void checkArchetypeMethodInst(ArchetypeMethodInst *AMI) {
    DEBUG(llvm::dbgs() << "verifying";
          AMI->print(llvm::dbgs()));
    FunctionType *methodType = AMI->getType(0).getAs<FunctionType>();
    DEBUG(llvm::dbgs() << "method type ";
          methodType->print(llvm::dbgs());
          llvm::dbgs() << "\n");
    require(methodType,
            "result method must be of a concrete function type");
    require(methodType->isThin()
              == AMI->getLookupArchetype().castTo<ArchetypeType>()
                  ->isClassBounded(),
            "result method must not be thin function type if class-bounded, "
            "thick if not class-bounded");
    SILType operandType = AMI->getLookupArchetype();
    DEBUG(llvm::dbgs() << "operand type ";
          operandType.print(llvm::dbgs());
          llvm::dbgs() << "\n");
    require(operandType.is<ArchetypeType>(),
            "operand type must be an archetype");
    
    CanType thisType = getMethodThisType(methodType);
    require(thisType == operandType.getSwiftType()
            || thisType->isEqual(
                            MetaTypeType::get(operandType.getSwiftRValueType(),
                                              operandType.getASTContext())),
            "result must be method of operand type");
    if (MetaTypeType *mt = operandType.getAs<MetaTypeType>()) {
      require(mt->getInstanceType()->is<ArchetypeType>(),
              "archetype_method must apply to an archetype metatype");
    } else {
      require(operandType.is<ArchetypeType>(),
              "archetype_method must apply to an archetype or archetype metatype");
    }
  }
  
  void checkProtocolMethodInst(ProtocolMethodInst *EMI) {
    FunctionType *methodType = EMI->getType(0).getAs<FunctionType>();
    require(methodType,
            "result method must be of a concrete function type");
    SILType operandType = EMI->getOperand().getType();
    require(methodType->isThin()
              == operandType.isClassBoundedExistentialType(),
            "result method must be thin function type if class-bounded, or "
            "thick if not class-bounded");
    
    if (EMI->getMember().getDecl()->isInstanceMember()) {
      require(operandType.isExistentialType(),
              "instance protocol_method must apply to an existential address");
      if (operandType.isClassBoundedExistentialType()) {
        require(getMethodThisType(methodType)->isEqual(
                               operandType.getASTContext().TheObjCPointerType),
                "result must be a method of objc pointer");
        
      } else {
        require(getMethodThisType(methodType)->isEqual(
                               operandType.getASTContext().TheOpaquePointerType),
                "result must be a method of opaque pointer");
      }
    } else {
      require(!operandType.isAddress(),
              "static protocol_method cannot apply to an address");
      require(operandType.is<MetaTypeType>(),
              "static protocol_method must apply to an existential metatype");
      require(operandType.castTo<MetaTypeType>()
                ->getInstanceType()->isExistentialType(),
              "static protocol_method must apply to an existential metatype");
      require(getMethodThisType(methodType) ==
                                  EMI->getOperand().getType().getSwiftType(),
              "result must be a method of the existential metatype");
    }
  }
  
  static bool isClassOrClassMetatype(Type t) {
    if (auto *meta = t->getAs<MetaTypeType>()) {
      return bool(meta->getInstanceType()->getClassOrBoundGenericClass());
    } else {
      return bool(t->getClassOrBoundGenericClass());
    }
  }
  
  void checkClassMethodInst(ClassMethodInst *CMI) {
    auto *methodType = CMI->getType(0).getAs<AnyFunctionType>();
    require(methodType,
            "result method must be of a function type");
    require(methodType->isThin(),
            "result method must be of a thin function type");
    SILType operandType = CMI->getOperand().getType();
    require(isClassOrClassMetatype(operandType.getSwiftType()),
            "operand must be of a class type");
    require(isClassOrClassMetatype(getMethodThisType(methodType)),
            "result must be a method of a class");
  }
  
  void checkSuperMethodInst(SuperMethodInst *CMI) {
    auto *methodType = CMI->getType(0).getAs<AnyFunctionType>();
    require(methodType,
            "result method must be of a function type");
    require(methodType->isThin(),
            "result method must be of a thin function type");
    SILType operandType = CMI->getOperand().getType();
    require(isClassOrClassMetatype(operandType.getSwiftType()),
            "operand must be of a class type");
    require(isClassOrClassMetatype(getMethodThisType(methodType)),
            "result must be a method of a class");
  }
  
  void checkProjectExistentialInst(ProjectExistentialInst *PEI) {
    SILType operandType = PEI->getOperand().getType();
    require(operandType.isAddress(),
            "project_existential must be applied to address");
    require(operandType.isExistentialType(),
            "project_existential must be applied to address of existential");
    require(PEI->getType() == SILType::getOpaquePointerType(F.getASTContext()),
            "project_existential_ref result must be an OpaquePointer");
  }
  
  void checkProjectExistentialRefInst(ProjectExistentialRefInst *PEI) {
    require(!PEI->getOperand().getType().isAddress(),
            "project_existential_ref operand must not be address");
    require(PEI->getOperand().getType().isClassBoundedExistentialType(),
            "project_existential_ref operand must be class-bounded existential");
    require(PEI->getType() == SILType::getObjCPointerType(F.getASTContext()),
            "project_existential_ref result must be an ObjCPointer");
  }
  
  void checkInitExistentialInst(InitExistentialInst *AEI) {
    SILType exType = AEI->getOperand().getType();
    require(exType.isAddress(),
            "init_existential must be applied to an address");
    require(exType.isExistentialType(),
            "init_existential must be applied to address of existential");
    require(!exType.isClassBoundedExistentialType(),
            "init_existential must be applied to non-class-bounded existential");
    require(!AEI->getConcreteType().isExistentialType(),
            "init_existential cannot put an existential container inside "
            "an existential container");
  }
  
  void checkInitExistentialRefInst(InitExistentialRefInst *IEI) {
    SILType concreteType = IEI->getOperand().getType();
    require(concreteType.getSwiftType()->mayHaveSuperclass(),
            "init_existential_ref operand must be a class instance");
    require(IEI->getType().isClassBoundedExistentialType(),
            "init_existential_ref result must be a class-bounded existential type");
    require(!IEI->getType().isAddress(),
            "init_existential_ref result must not be an address");
  }
  
  void checkUpcastExistentialInst(UpcastExistentialInst *UEI) {
    SILType srcType = UEI->getSrcExistential().getType();
    SILType destType = UEI->getDestExistential().getType();
    require(srcType.isExistentialType(),
            "upcast_existential source must be existential");
    require(destType.isAddress(),
            "upcast_existential dest must be an address");
    require(destType.isExistentialType(),
            "upcast_existential dest must be address of existential");
    require(!destType.isClassBoundedExistentialType(),
            "upcast_existential dest must be non-class-bounded existential");
  }
  
  void checkUpcastExistentialRefInst(UpcastExistentialRefInst *UEI) {
    require(!UEI->getOperand().getType().isAddress(),
            "upcast_existential_ref operand must not be an address");
    require(UEI->getOperand().getType().isClassBoundedExistentialType(),
            "upcast_existential_ref operand must be class-bounded existential");
    require(!UEI->getType().isAddress(),
            "upcast_existential_ref result must not be an address");
    require(UEI->getType().isClassBoundedExistentialType(),
            "upcast_existential_ref result must be class-bounded existential");
  }
  
  void checkDeinitExistentialInst(DeinitExistentialInst *DEI) {
    SILType exType = DEI->getOperand().getType();
    require(exType.isAddress(),
            "deinit_existential must be applied to an address");
    require(exType.isExistentialType(),
            "deinit_existential must be applied to address of existential");
    require(!exType.isClassBoundedExistentialType(),
            "deinit_existential must be applied to non-class-bounded existential");
  }
  
  void checkArchetypeRefToSuperInst(ArchetypeRefToSuperInst *ASI) {
    ArchetypeType *archetype
      = ASI->getOperand().getType().getAs<ArchetypeType>();
    require(archetype, "archetype_ref_to_super operand must be archetype");
    require(archetype->isClassBounded(),
            "archetype_ref_to_super operand must be class-bounded archetype");
    require(ASI->getType().getSwiftType()->getClassOrBoundGenericClass(),
            "archetype_ref_to_super must convert to a class type");
  }
  
  void checkSuperToArchetypeRefInst(SuperToArchetypeRefInst *SAI) {
    require(SAI->getOperand().getType()
              .getSwiftType()->getClassOrBoundGenericClass(),
            "super_to_archetype_ref operand must be a class instance");
    ArchetypeType *archetype
      = SAI->getType().getAs<ArchetypeType>();
    require(archetype, "super_to_archetype_ref must convert to archetype type");
    require(archetype->isClassBounded(),
            "super_to_archetype_ref must convert to class-bounded archetypet type");
  }
  
  void checkBridgeToBlockInst(BridgeToBlockInst *BBI) {
    SILType operandTy = BBI->getOperand().getType();
    SILType resultTy = BBI->getType();
    
    require(!operandTy.isAddress(),
            "bridge_to_block operand cannot be an address");
    require(!resultTy.isAddress(),
            "bridge_to_block result cannot be an address");
    require(operandTy.is<FunctionType>(),
            "bridge_to_block operand must be a function type");
    require(resultTy.is<FunctionType>(),
            "bridge_to_block result must be a function type");
    
    auto *operandFTy = BBI->getOperand().getType().castTo<FunctionType>();
    auto *resultFTy = BBI->getType().castTo<FunctionType>();
    
    require(CanType(operandFTy->getInput()) == CanType(resultFTy->getInput()),
            "bridge_to_block operand and result types must differ only in "
            "[objc_block]-ness");
    require(CanType(operandFTy->getResult()) == CanType(resultFTy->getResult()),
            "bridge_to_block operand and result types must differ only in "
            "[objc_block]-ness");
    require(operandFTy->isAutoClosure() == resultFTy->isAutoClosure(),
            "bridge_to_block operand and result types must differ only in "
            "[objc_block]-ness");
    require(!operandFTy->isThin(), "bridge_to_block operand cannot be [thin]");
    require(!resultFTy->isThin(), "bridge_to_block result cannot be [thin]");
    require(!operandFTy->isBlock(),
            "bridge_to_block operand cannot be [objc_block]");
    require(resultFTy->isBlock(),
            "bridge_to_block result must be [objc_block]");
  }
  
  void checkThinToThickFunctionInst(ThinToThickFunctionInst *TTFI) {
    require(!TTFI->getOperand().getType().isAddress(),
            "thin_to_thick_function operand cannot be an address");
    require(!TTFI->getType().isAddress(),
            "thin_to_thick_function result cannot be an address");
    require(TTFI->getOperand().getType().is<AnyFunctionType>(),
            "thin_to_thick_function operand must be a function");
    require(TTFI->getType().is<AnyFunctionType>(),
            "thin_to_thick_function result must be a function");
    if (auto *opFTy = dyn_cast<FunctionType>(
                                 TTFI->getOperand().getType().getSwiftType())) {
      auto *resFTy = dyn_cast<FunctionType>(TTFI->getType().getSwiftType());
      require(resFTy &&
              opFTy->getInput()->isEqual(resFTy->getInput()) &&
              opFTy->getResult()->isEqual(resFTy->getResult()) &&
              opFTy->isAutoClosure() == resFTy->isAutoClosure() &&
              opFTy->isBlock() == resFTy->isBlock(),
              "thin_to_thick_function operand and result type must differ only "
              " in thinness");
      require(!resFTy->isThin(),
              "thin_to_thick_function result must not be thin");
      require(opFTy->isThin(),
              "thin_to_thick_function operand must be thin");
    } else if (auto *opPTy = dyn_cast<PolymorphicFunctionType>(
                                 TTFI->getOperand().getType().getSwiftType())) {
      auto *resPTy = dyn_cast<PolymorphicFunctionType>(
                                               TTFI->getType().getSwiftType());
      require(resPTy &&
              opPTy->getInput()->isEqual(resPTy->getInput()) &&
              opPTy->getResult()->isEqual(resPTy->getResult()),
              "thin_to_thick_function operand and result type must differ only "
              " in thinness");
      require(!resPTy->isThin(),
              "thin_to_thick_function result must not be thin");
      require(opPTy->isThin(),
              "thin_to_thick_function operand must be thin");
    } else {
      llvm_unreachable("invalid AnyFunctionType?!");
    }
  }
  
  void checkConvertCCInst(ConvertCCInst *CCI) {
    require(!CCI->getOperand().getType().isAddress(),
            "convert_cc operand cannot be an address");
    require(!CCI->getType().isAddress(),
            "convert_cc result cannot be an address");
    require(CCI->getOperand().getType().is<AnyFunctionType>(),
            "convert_cc operand must be a function");
    require(CCI->getType().is<AnyFunctionType>(),
            "convert_cc result must be a function");
    if (auto *opFTy = dyn_cast<FunctionType>(
                                 CCI->getOperand().getType().getSwiftType())) {
      auto *resFTy = dyn_cast<FunctionType>(CCI->getType().getSwiftType());
      require(resFTy &&
              opFTy->getInput()->isEqual(resFTy->getInput()) &&
              opFTy->getResult()->isEqual(resFTy->getResult()) &&
              opFTy->isAutoClosure() == resFTy->isAutoClosure() &&
              opFTy->isBlock() == resFTy->isBlock(),
              "convert_cc operand and result type must differ only "
              " in calling convention");
      require(!resFTy->isThin(),
              "convert_cc result must be thin");
      require(opFTy->isThin(),
              "convert_cc operand must be thin");
    } else if (auto *opPTy = dyn_cast<PolymorphicFunctionType>(
                                 CCI->getOperand().getType().getSwiftType())) {
      auto *resPTy = dyn_cast<PolymorphicFunctionType>(
                                               CCI->getType().getSwiftType());
      require(resPTy &&
              opPTy->getInput()->isEqual(resPTy->getInput()) &&
              opPTy->getResult()->isEqual(resPTy->getResult()),
              "convert_cc operand and result type must differ only "
              " in calling convention");
      require(!resPTy->isThin(),
              "convert_cc result must be thin");
      require(opPTy->isThin(),
              "convert_cc operand must be thin");
    } else {
      llvm_unreachable("invalid AnyFunctionType?!");
    }    
  }
  
  void checkUpcastInst(UpcastInst *UI) {
    if (UI->getType().is<MetaTypeType>()) {
      CanType instTy(UI->getType().castTo<MetaTypeType>()->getInstanceType());
      require(UI->getOperand().getType().is<MetaTypeType>(),
              "upcast operand must be a class or class metatype instance");
      CanType opInstTy(UI->getOperand().getType().castTo<MetaTypeType>()
                         ->getInstanceType());
      require(opInstTy->getClassOrBoundGenericClass(),
              "upcast operand must be a class or class metatype instance");
      require(instTy->getClassOrBoundGenericClass(),
              "upcast must convert a class metatype to a class metatype");
    } else {
      require(UI->getOperand().getType().getSwiftType()
                ->getClassOrBoundGenericClass(),
              "upcast operand must be a class or class metatype instance");
      require(UI->getType().getSwiftType()->getClassOrBoundGenericClass(),
              "upcast must convert a class instance to a class type");
    }
  }
  
  void checkDowncastInst(DowncastInst *DI) {
    require(DI->getOperand().getType().getSwiftType()
              ->getClassOrBoundGenericClass(),
            "downcast operand must be a class type");
    require(DI->getType().getSwiftType()->getClassOrBoundGenericClass(),
            "downcast must convert to a class type");
  }
  
  void checkIsaInst(IsaInst *II) {
    require(II->getOperand().getType().getSwiftType()
              ->getClassOrBoundGenericClass(),
            "isa operand must be a class type");
    CanType testTy = II->getTestType().getSwiftRValueType();
    if (auto *archetype = dyn_cast<ArchetypeType>(testTy))
      require((bool)archetype->getSuperclass(),
              "isa must test against a class type or base-class-constrained "
              "archetype");
    else
      require(testTy->getClassOrBoundGenericClass(),
              "isa must test against a class type or base-class-constrained "
              "archetype");
  }
  
  void checkAddressToPointerInst(AddressToPointerInst *AI) {
    require(AI->getOperand().getType().isAddress(),
            "address-to-pointer operand must be an address");
    require(AI->getType().getSwiftType()->isEqual(
                              AI->getType().getASTContext().TheRawPointerType),
            "address-to-pointer result type must be RawPointer");
  }

  void checkDowncastArchetypeAddrInst(DowncastArchetypeAddrInst *DAAI) {
    require(DAAI->getOperand().getType().isAddress(),
            "downcast_archetype_addr operand must be an address");
    ArchetypeType *archetype = DAAI->getOperand().getType().getAs<ArchetypeType>();
    require(archetype, "downcast_archetype_addr operand must be an archetype");
    require(!archetype->isClassBounded(),
            "downcast_archetype_addr operand must be a non-class-bounded archetype");
    
    require(DAAI->getType().isAddress(),
            "downcast_archetype_addr result must be an address");
  }
  
  void checkDowncastArchetypeRefInst(DowncastArchetypeRefInst *DARI) {
    require(!DARI->getOperand().getType().isAddress(),
            "downcast_archetype_ref operand must not be an address");
    ArchetypeType *archetype = DARI->getOperand().getType().getAs<ArchetypeType>();
    require(archetype, "downcast_archetype_ref operand must be an archetype");
    require(archetype->isClassBounded(),
            "downcast_archetype_ref operand must be a class-bounded archetype");
    
    require(!DARI->getType().isAddress(),
            "downcast_archetype_ref result must not be an address");
    require(DARI->getType().getSwiftType()->mayHaveSuperclass(),
            "downcast_archetype_ref result must be a class type "
            "or class-bounded archetype");
  }
  
  void checkProjectDowncastExistentialAddrInst(ProjectDowncastExistentialAddrInst *DEAI) {
    require(DEAI->getOperand().getType().isAddress(),
            "project_downcast_existential_addr operand must be an address");
    require(DEAI->getOperand().getType().isExistentialType(),
            "project_downcast_existential_addr operand must be an existential");
    require(!DEAI->getOperand().getType().isClassBoundedExistentialType(),
            "project_downcast_existential_addr operand must be a non-class-bounded "
            "existential");
    
    require(DEAI->getType().isAddress(),
            "project_downcast_existential_addr result must be an address");
  }
  
  void checkDowncastExistentialRefInst(DowncastExistentialRefInst *DERI) {
    require(!DERI->getOperand().getType().isAddress(),
            "downcast_existential_ref operand must not be an address");
    require(DERI->getOperand().getType().isClassBoundedExistentialType(),
            "downcast_existential_ref operand must be a class-bounded existential");
    
    require(!DERI->getType().isAddress(),
            "downcast_existential_ref result must not be an address");
    require(DERI->getType().getSwiftType()->mayHaveSuperclass(),
            "downcast_existential_ref result must be a class type "
            "or class-bounded archetype");
  }
  
  void checkRefToObjectPointerInst(RefToObjectPointerInst *AI) {
    require(AI->getOperand().getType()
              .getSwiftType()->mayHaveSuperclass(),
            "ref-to-object-pointer operand must be a class reference");
    require(AI->getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheObjectPointerType),
            "ref-to-object-pointer result must be ObjectPointer");
  }
  
  void checkObjectPointerToRefInst(ObjectPointerToRefInst *AI) {
    require(AI->getType()
              .getSwiftType()->mayHaveSuperclass(),
            "object-pointer-to-ref result must be a class reference");
    require(AI->getOperand().getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheObjectPointerType),
            "object-pointer-to-ref operand must be ObjectPointer");
  }
  
  void checkRefToRawPointerInst(RefToRawPointerInst *AI) {
    require(AI->getOperand().getType()
              .getSwiftType()->mayHaveSuperclass() ||
            AI->getOperand().getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheObjectPointerType),
            "ref-to-raw-pointer operand must be a class reference or"
            " ObjectPointer");
    require(AI->getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheRawPointerType),
            "ref-to-raw-pointer result must be RawPointer");
  }
  
  void checkRawPointerToRefInst(RawPointerToRefInst *AI) {
    require(AI->getType()
              .getSwiftType()->mayHaveSuperclass() ||
            AI->getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheObjectPointerType),
        "raw-pointer-to-ref result must be a class reference or ObjectPointer");
    require(AI->getOperand().getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheRawPointerType),
            "raw-pointer-to-ref operand must be ObjectPointer");
  }
  
  void checkConvertFunctionInst(ConvertFunctionInst *ICI) {
    require(!ICI->getOperand().getType().isAddress(),
            "conversion operand cannot be an address");
    require(!ICI->getType().isAddress(),
            "conversion result cannot be an address");
    
    auto *opFTy = ICI->getOperand().getType().getAs<AnyFunctionType>();
    auto *resFTy = ICI->getType().getAs<AnyFunctionType>();

    require(opFTy, "convert_function operand must be a function");
    require(resFTy, "convert_function result must be a function");
    
    require(opFTy->isThin() == resFTy->isThin(),
            "convert_function cannot change function thinness");
    
    SILFunctionTypeInfo *opTI
      = ICI->getOperand().getType().getFunctionTypeInfo(F.getModule());
    SILFunctionTypeInfo *resTI
      = ICI->getType().getFunctionTypeInfo(F.getModule());

    require(opTI->getResultType() == resTI->getResultType(),
            "result types of convert_function operand and result do no match");
    require(opTI->getInputTypes().size() == resTI->getInputTypes().size(),
            "input types of convert_function operand and result do not match");
    require(std::equal(opTI->getInputTypes().begin(),
                       opTI->getInputTypes().end(),
                      resTI->getInputTypes().begin()),
            "input types of convert_function operand and result do not match");
  }
  
  void checkReturnInst(ReturnInst *RI) {
    DEBUG(RI->print(llvm::dbgs()));
    
    SILFunctionTypeInfo *ti =
      F.getLoweredType().getFunctionTypeInfo(F.getModule());
    SILType functionResultType = ti->getResultType();
    SILType instResultType = RI->getOperand().getType();
    DEBUG(llvm::dbgs() << "function return type: ";
          functionResultType.dump();
          llvm::dbgs() << "return inst type: ";
          instResultType.dump(););
    require(functionResultType == instResultType,
            "return value type does not match return type of function");
  }
  
  void checkAutoreleaseReturnInst(AutoreleaseReturnInst *RI) {
    DEBUG(RI->print(llvm::dbgs()));
    
    SILFunctionTypeInfo *ti =
    F.getLoweredType().getFunctionTypeInfo(F.getModule());
    SILType functionResultType = ti->getResultType();
    SILType instResultType = RI->getOperand().getType();
    DEBUG(llvm::dbgs() << "function return type: ";
          functionResultType.dump();
          llvm::dbgs() << "return inst type: ";
          instResultType.dump(););
    require(functionResultType == instResultType,
            "return value type does not match return type of function");
    require(!instResultType.isAddress(),
            "autoreleased return value cannot be an address");
    require(instResultType.hasReferenceSemantics(),
            "autoreleased return value must be a reference type");
  }
  
  void checkBranchInst(BranchInst *BI) {
    require(BI->getArgs().size() == BI->getDestBB()->bbarg_size(),
            "branch has wrong number of arguments for dest bb");
    require(std::equal(BI->getArgs().begin(), BI->getArgs().end(),
                      BI->getDestBB()->bbarg_begin(),
                      [](SILValue branchArg, SILArgument *bbArg) {
                        return branchArg.getType() == bbArg->getType();
                      }),
            "branch argument types do not match arguments for dest bb");
  }
  
  void checkCondBranchInst(CondBranchInst *CBI) {
    require(CBI->getCondition().getType() ==
             SILType::getBuiltinIntegerType(1,
                                 CBI->getCondition().getType().getASTContext()),
            "condition of conditional branch must have Int1 type");
    
    require(CBI->getTrueArgs().size() == CBI->getTrueBB()->bbarg_size(),
            "true branch has wrong number of arguments for dest bb");
    require(std::equal(CBI->getTrueArgs().begin(), CBI->getTrueArgs().end(),
                      CBI->getTrueBB()->bbarg_begin(),
                      [](SILValue branchArg, SILArgument *bbArg) {
                        return branchArg.getType() == bbArg->getType();
                      }),
            "true branch argument types do not match arguments for dest bb");

    require(CBI->getFalseArgs().size() == CBI->getFalseBB()->bbarg_size(),
            "false branch has wrong number of arguments for dest bb");
    require(std::equal(CBI->getFalseArgs().begin(), CBI->getFalseArgs().end(),
                      CBI->getFalseBB()->bbarg_begin(),
                      [](SILValue branchArg, SILArgument *bbArg) {
                        return branchArg.getType() == bbArg->getType();
                      }),
            "false branch argument types do not match arguments for dest bb");
  }
  
  void verifyEntryPointArguments(SILBasicBlock *entry) {
    SILType ty = F.getLoweredType();
    SILFunctionTypeInfo *ti = ty.getFunctionTypeInfo(F.getModule());
    
    DEBUG(llvm::dbgs() << "Argument types for entry point BB:\n";
          for (auto *arg : make_range(entry->bbarg_begin(), entry->bbarg_end()))
            arg->getType().dump();
          llvm::dbgs() << "Input types for SIL function type ";
          ty.print(llvm::dbgs());
          llvm::dbgs() << ":\n";
          for (auto input : ti->getInputTypes())
            input.dump(););
    
    require(entry->bbarg_size() == ti->getInputTypes().size(),
            "entry point has wrong number of arguments");
    
    
    require(std::equal(entry->bbarg_begin(), entry->bbarg_end(),
                      ti->getInputTypes().begin(),
                      [](SILArgument *bbarg, SILType ty) {
                        return bbarg->getType() == ty;
                      }),
            "entry point argument types do not match function type");
  }
  
  void visitSILFunction(SILFunction *F) {
    
    verifyEntryPointArguments(F->getBlocks().begin());
    SILVisitor::visitSILFunction(F);
  }
  
  void verify() {

    visitSILFunction(const_cast<SILFunction*>(&F));
  }
};
} // end anonymous namespace

#endif //NDEBUG

/// verify - Run the SIL verifier to make sure that the SILFunction follows
/// invariants.
void SILFunction::verify() const {
#ifndef NDEBUG
  if (isExternalDeclaration()) {
    assert(getLinkage() != SILLinkage::Internal &&
           "external declaration of internal SILFunction not allowed");
    return;
  }
  SILVerifier(*this).verify();
#endif
}

