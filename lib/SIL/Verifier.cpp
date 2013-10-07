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
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/Dominance.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/Basic/Range.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringSet.h"
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
} // end anonymous namespace

namespace {

/// The SIL verifier walks over a SIL function / basic block / instruction,
/// checking and enforcing its invariants.
class SILVerifier : public SILVerifierBase<SILVerifier> {
  Module *M;
  const SILFunction &F;
  const SILInstruction *CurInstruction = nullptr;
  DominanceInfo *Dominance;

  SILVerifier(const SILVerifier&) = delete;
  void operator=(const SILVerifier&) = delete;
public:
  void _require(bool condition, const Twine &complaint,
                const std::function<void()> &extraContext = nullptr) {
    if (condition) return;

    llvm::dbgs() << "SIL verification failed: " << complaint << "\n";

    if (extraContext) extraContext();

    if (CurInstruction) {
      llvm::dbgs() << "Verifying instruction:\n";
      CurInstruction->printInContext(llvm::dbgs());
      llvm::dbgs() << "In function @" << F.getName() <<" basic block:\n";
      CurInstruction->getParent()->print(llvm::dbgs());
    }

    abort();
  }
#define require(condition, complaint) \
  _require(condition, complaint ": " #condition)

  template <class T> typename CanTypeWrapperTraits<T>::type
  _requireObjectType(SILValue value, const Twine &valueDescription,
                     const char *typeName) {
    _require(value.getType().isObject(),
             valueDescription + " must be an object");
    auto result = dyn_cast<T>(value.getType().getSwiftRValueType());
    _require(result, valueDescription + " must have type " + typeName);
    return result;
  }
#define requireObjectType(type, value, valueDescription) \
  _requireObjectType<type>(value, valueDescription, #type)

  void requireReferenceValue(SILValue value, const Twine &valueDescription) {
    require(value.getType().isObject(), valueDescription +" must be an object");
    require(value.getType().hasReferenceSemantics(),
            valueDescription + " must have reference semantics");
  }

  /// Assert that two types are equal.
  void requireSameType(SILType type1, SILType type2, const Twine &complaint) {
    _require(type1 == type2, complaint, [&] {
      llvm::dbgs() << "  " << type1 << "\n  " << type2 << '\n';
    });
  }

  SILVerifier(Module *M, const SILFunction &F) : M(M), F(F) {
    // Check to make sure that all blocks are well formed.  If not, the
    // SILVerifier object will explode trying to compute dominance info.
    for (auto &BB : F) {
      require(!BB.empty(), "Basic blocks cannot be empty");
      require(isa<TermInst>(BB.getInstList().back()),
              "Basic blocks must end with a terminator instruction");
    }

    Dominance = new DominanceInfo(const_cast<SILFunction*>(&F));
  }

  ~SILVerifier() {
    delete Dominance;
  }

  void visitSILInstruction(SILInstruction *I) {
    CurInstruction = I;
    checkSILInstruction(I);

    // Check the SILLLocation attached to the instruction.
    checkInstructionsSILLocation(I);
  }

  void checkSILInstruction(SILInstruction *I) {
    const SILBasicBlock *BB = I->getParent();
    // Check that non-terminators look ok.
    if (!isa<TermInst>(I)) {
      require(!BB->empty(), "Can't be in a parent block if it is empty");
      require(&*BB->getInstList().rbegin() != I,
              "Non-terminators cannot be the last in a block");
    } else {
      require(&*BB->getInstList().rbegin() == I,
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
                "instruction uses value of instruction from another function");
        require(Dominance->properlyDominates(valueI, I),
                "instruction doesn't dominate its operand");
      }

      require(operand.getUser() == I,
              "instruction's operand's owner isn't the instruction");
      require(isInValueUses(&operand), "operand value isn't used by operand");
    }
  }

  void checkInstructionsSILLocation(SILInstruction *I) {
    SILLocation L = I->getLoc();
    SILLocation::LocationKind LocKind = L.getKind();
    ValueKind InstKind = I->getKind();

    // Regular locations and SIL file locations are allowed on all instructions.
    if (LocKind == SILLocation::RegularKind ||
        LocKind == SILLocation::SILFileKind)
      return;

    if (LocKind == SILLocation::CleanupKind ||
        LocKind == SILLocation::InlinedKind)
      require(InstKind != ValueKind::ReturnInst ||
              InstKind != ValueKind::AutoreleaseReturnInst,
        "cleanup and inlined locations are not allowed on return instructions");

    if (LocKind == SILLocation::ReturnKind ||
        LocKind == SILLocation::ImplicitReturnKind)
      require(InstKind == ValueKind::BranchInst ||
              InstKind == ValueKind::ReturnInst ||
              InstKind == ValueKind::AutoreleaseReturnInst ||
              InstKind == ValueKind::UnreachableInst,
        "return locations are only allowed on branch and return instructions");

    if (LocKind == SILLocation::ArtificialUnreachableKind)
      require(InstKind == ValueKind::UnreachableInst,
        "artificial locations are only allowed on Unreachable instructions");
  }

  /// Check that this operand appears in the use-chain of the value it uses.
  static bool isInValueUses(const Operand *operand) {
    for (auto use : operand->get()->getUses())
      if (use == operand)
        return true;
    return false;
  }

  void checkAllocStackInst(AllocStackInst *AI) {
    require(AI->getContainerResult().getType().isLocalStorage(),
            "first result of alloc_stack must be local storage");
    require(AI->getAddressResult().getType().isAddress(),
            "second result of alloc_stack must be an address type");
    require(AI->getContainerResult().getType().getSwiftRValueType()
              == AI->getElementType().getSwiftRValueType(),
            "container storage must be for allocated type");
  }
  
  void checkAllocRefInst(AllocRefInst *AI) {
    requireReferenceValue(AI, "Result of alloc_ref");
  }
  
  /// Check the substitutions passed to an apply or partial_apply.
  SILType checkApplySubstitutions(ArrayRef<Substitution> subs,
                                  SILType calleeTy) {
    // If there are substitutions, verify them and apply them to the callee.
    auto polyTy = calleeTy.getAs<PolymorphicFunctionType>();
    if (subs.empty()) {
      require(!polyTy,
              "callee of apply without substitutions must not be polymorphic");
      return calleeTy;
    }
    require(polyTy,
            "callee of apply with substitutions must be polymorphic");
    require(polyTy->getAllArchetypes().size() == subs.size(),
            "number of apply substitutions must match number of archetypes "
            "in the function type");
    
    // Apply the substitutions.
    // FIXME: Eventually we want this substitution to apply 1:1 to the
    // SILFunctionTypeInfo-level calling convention of the type, instead of
    // hiding the abstraction difference behind the specialization.
    auto substTy = polyTy->substGenericArgs(M, subs)->getCanonicalType();
    
    return SILType::getPrimitiveObjectType(substTy);
  }
  
  void checkApplyInst(ApplyInst *AI) {
    SILType calleeTy = AI->getCallee().getType();
    require(calleeTy.isObject(), "callee of apply must be an object");
    require(calleeTy.is<AnyFunctionType>(),
            "callee of apply must have any function type");
    
    calleeTy = checkApplySubstitutions(AI->getSubstitutions(), calleeTy);
    
    require(calleeTy == AI->getSubstCalleeType(),
            "substituted callee type does not match substitutions");
    
    SILFunctionTypeInfo *ti = calleeTy.getFunctionTypeInfo(F.getModule());
    
    // Check that the arguments and result match.
    require(AI->getArguments().size() == ti->getInputTypes().size(),
            "apply doesn't have right number of arguments for function");
    for (size_t i = 0, size = AI->getArguments().size(); i < size; ++i) {
      requireSameType(AI->getArguments()[i].getType(), ti->getInputTypes()[i],
                      "operand of 'apply' doesn't match function input type");
    }
    require(AI->getType() == ti->getResultType(),
            "type of apply instruction doesn't match function result type");
  }
  
  void checkPartialApplyInst(PartialApplyInst *PAI) {
    SILType calleeTy = PAI->getCallee().getType();
    require(calleeTy.isObject(), "callee of closure must be an object");
    require(calleeTy.is<AnyFunctionType>(),
            "callee of closure must have Any function type");

    SILType appliedTy = PAI->getType();
    require(appliedTy.isObject(), "result of closure must be an object");
    require(appliedTy.is<FunctionType>(),
            "result of closure must have concrete function type");
    // FIXME: A "curry" with no arguments could remain thin.
    require(!appliedTy.castTo<FunctionType>()->isThin(),
            "result of closure cannot have a thin function type");

    calleeTy = checkApplySubstitutions(PAI->getSubstitutions(), calleeTy);

    require(calleeTy == PAI->getSubstCalleeType(),
            "substituted callee type does not match substitutions");

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
    require(isa<BuiltinModule>(BFI->getReferencedFunction()->getDeclContext()),
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
    require(!GAI->getGlobal()->isComputed(),
            "GlobalAddr cannot take the address of a computed variable");
    require(!GAI->getGlobal()->getDeclContext()->isLocalContext(),
            "GlobalAddr cannot take the address of a local var");
  }

  void checkIntegerLiteralInst(IntegerLiteralInst *ILI) {
    require(ILI->getType().is<BuiltinIntegerType>(),
            "invalid integer literal type");
  }
  void checkLoadInst(LoadInst *LI) {
    require(LI->getType().isObject(), "Result of load must be an object");
    require(LI->getOperand().getType().isAddress(),
            "Load operand must be an address");
    require(LI->getOperand().getType().getObjectType() == LI->getType(),
            "Load operand type and result type mismatch");
  }

  void checkStoreInst(StoreInst *SI) {
    require(SI->getSrc().getType().isObject(),
            "Can't store from an address source");
    require(SI->getDest().getType().isAddress(),
            "Must store to an address dest");
    require(SI->getDest().getType().getObjectType() == SI->getSrc().getType(),
            "Store operand type and dest type mismatch");
  }

  void checkAssignInst(AssignInst *AI) {
    SILValue Src = AI->getSrc(), Dest = AI->getDest();
    require(AI->getModule().getStage() == SILStage::Raw,
            "assign instruction can only exist in raw SIL");
    require(Src.getType().isObject(), "Can't assign from an address source");
    require(Dest.getType().isAddress(), "Must store to an address dest");
    require(Dest.getType().getObjectType() == Src.getType(),
            "Store operand type and dest type mismatch");
  }

  void checkMarkUninitializedInst(MarkUninitializedInst *MU) {
    SILValue Src = MU->getOperand();
    require(MU->getModule().getStage() == SILStage::Raw,
            "mark_uninitialized instruction can only exist in raw SIL");
    require(Src.getType().isAddress(), "Must store to an address dest");
    require(Src.getType() == MU->getType(0),"operand and result type mismatch");
  }
  void checkMarkFunctionEscapeInst(MarkFunctionEscapeInst *MFE) {
    require(MFE->getModule().getStage() == SILStage::Raw,
            "mark_function_escape instruction can only exist in raw SIL");
    for (auto Elt : MFE->getElements())
      require(Elt.getType().isAddress(), "MFE must refer to variable addrs");
  }

  void checkCopyAddrInst(CopyAddrInst *SI) {
    require(SI->getSrc().getType().isAddress(),
            "Src value should be lvalue");
    require(SI->getDest().getType().isAddress(),
            "Dest address should be lvalue");
    require(SI->getDest().getType() == SI->getSrc().getType(),
            "Store operand type and dest type mismatch");
  }

  void checkCopyValueInst(CopyValueInst *I) {
    require(I->getOperand().getType().isObject(),
            "Source value should be an object value");
    require(I->getOperand().getType() == I->getType(),
            "Result type does not match input type");
  }

  void checkDestroyValueInst(DestroyValueInst *I) {
    require(I->getOperand().getType().isObject(),
            "Source value should be an object value");
  }

  void checkInitializeVarInst(InitializeVarInst *ZI) {
    require(ZI->getOperand().getType().isAddress(),
            "Dest address should be lvalue");
  }
  
  void checkStructInst(StructInst *SI) {
    auto *structDecl = SI->getType().getStructOrBoundGenericStruct();
    require(structDecl, "StructInst must return a struct");
    require(SI->getType().isObject(),
            "StructInst must produce an object");

    CanType structTy = SI->getType().getSwiftType();
    auto opi = SI->getElements().begin(), opEnd = SI->getElements().end();
    for (VarDecl *field : structDecl->getStoredProperties()) {
      require(opi != opEnd,
              "number of struct operands does not match number of stored "
              "member variables of struct");
      
      Type fieldTy = structTy->getTypeOfMember(M, field, nullptr);
      require(fieldTy->isEqual((*opi).getType().getSwiftType()),
              "struct operand type does not match field type");
      ++opi;
    }
  }
  
  void checkEnumInst(EnumInst *UI) {
    EnumDecl *ud = UI->getType().getEnumOrBoundGenericEnum();
    require(ud, "EnumInst must return an enum");
    require(UI->getElement()->getParentEnum() == ud,
            "EnumInst case must be a case of the result enum type");
    require(UI->getType().isObject(),
            "EnumInst must produce an object");
    require(UI->hasOperand() == UI->getElement()->hasArgumentType(),
            "EnumInst must take an argument iff the element does");
    
    if (UI->getElement()->hasArgumentType()) {
      require(UI->getOperand().getType().isObject(),
              "EnumInst operand must be an object");
      Type caseTy = UI->getType().getSwiftRValueType()
        ->getTypeOfMember(M, UI->getElement(), nullptr,
                          UI->getElement()->getArgumentType());
      require(caseTy->isEqual(UI->getOperand().getType().getSwiftRValueType()),
              "EnumInst operand type does not match type of case");
    }
  }

  void checkEnumDataAddrInst(EnumDataAddrInst *UI) {
    EnumDecl *ud = UI->getOperand().getType().getEnumOrBoundGenericEnum();
    require(ud, "EnumDataAddrInst must take an enum operand");
    require(UI->getElement()->getParentEnum() == ud,
            "EnumDataAddrInst case must be a case of the enum operand type");
    require(UI->getElement()->hasArgumentType(),
            "EnumDataAddrInst case must have a data type");
    require(UI->getOperand().getType().isAddress(),
            "EnumDataAddrInst must take an address operand");
    require(UI->getType().isAddress(),
            "EnumDataAddrInst must produce an address");
    
    Type caseTy = UI->getOperand().getType().getSwiftRValueType()
      ->getTypeOfMember(M, UI->getElement(), nullptr,
                        UI->getElement()->getArgumentType());
    
    require(caseTy->isEqual(UI->getType().getSwiftRValueType()),
            "EnumDataAddrInst result does not match type of enum case");
  }
  
  void checkInjectEnumAddrInst(InjectEnumAddrInst *IUAI) {
    require(IUAI->getOperand().getType().is<EnumType>()
              || IUAI->getOperand().getType().is<BoundGenericEnumType>(),
            "InjectEnumAddrInst must take an enum operand");
    require(IUAI->getElement()->getParentEnum()
              == IUAI->getOperand().getType().getEnumOrBoundGenericEnum(),
            "InjectEnumAddrInst case must be a case of the enum operand type");
    require(IUAI->getOperand().getType().isAddress(),
            "InjectEnumAddrInst must take an address operand");
  }
  
  void checkTupleInst(TupleInst *TI) {
    CanTupleType ResTy = requireObjectType(TupleType, TI, "Result of tuple");

    require(TI->getElements().size() == ResTy->getFields().size(),
            "Tuple field count mismatch!");
    
    for (size_t i = 0, size = TI->getElements().size(); i < size; ++i) {
      require(TI->getElements()[i].getType().getSwiftType()
               ->isEqual(ResTy.getElementType(i)),
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
  
  void checkStrongRetainInst(StrongRetainInst *RI) {
    requireReferenceValue(RI->getOperand(), "Operand of strong_retain");
  }
  void checkStrongRetainAutoreleasedInst(StrongRetainAutoreleasedInst *RI) {
    requireReferenceValue(RI->getOperand(), "Operand of "
                          "strong_retain_autoreleased");
    require(isa<ApplyInst>(RI->getOperand()),
            "Operand of strong_retain_autoreleased must be the return value of "
            "an apply instruction");
  }
  void checkStrongReleaseInst(StrongReleaseInst *RI) {
    requireReferenceValue(RI->getOperand(), "Operand of release");
  }
  void checkStrongRetainUnownedInst(StrongRetainUnownedInst *RI) {
    requireObjectType(UnownedStorageType, RI->getOperand(),
                      "Operand of retain_unowned");
  }
  void checkUnownedRetainInst(UnownedRetainInst *RI) {
    requireObjectType(UnownedStorageType, RI->getOperand(),
                      "Operand of unowned_retain");
  }
  void checkUnownedReleaseInst(UnownedReleaseInst *RI) {
    requireObjectType(UnownedStorageType, RI->getOperand(),
                      "Operand of unowned_release");
  }
  void checkDeallocStackInst(DeallocStackInst *DI) {
    require(DI->getOperand().getType().isLocalStorage(),
            "Operand of dealloc_stack must be local storage");
  }
  void checkDeallocRefInst(DeallocRefInst *DI) {
    require(DI->getOperand().getType().isObject(),
            "Operand of dealloc_ref must be object");
    require(DI->getOperand().getType().getClassOrBoundGenericClass(),
            "Operand of dealloc_ref must be of class type");
  }
  void checkDeallocBoxInst(DeallocBoxInst *DI) {
    require(DI->getElementType().isObject(),
            "Element type of dealloc_box must be an object type");
    requireObjectType(BuiltinObjectPointerType, DI->getOperand(),
                      "Operand of dealloc_box");
  }
  void checkDestroyAddrInst(DestroyAddrInst *DI) {
    require(DI->getOperand().getType().isAddress(),
            "Operand of destroy_addr must be address");
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
    CanTupleType operandTy = requireObjectType(TupleType, EI->getOperand(),
                                               "Operand of tuple_extract");
    require(EI->getType().isObject(),
            "result of tuple_extract must be object");
    
    require(EI->getFieldNo() < operandTy->getNumElements(),
            "invalid field index for element_addr instruction");
    require(EI->getType().getSwiftRValueType()
            == operandTy.getElementType(EI->getFieldNo()),
            "type of tuple_element_addr does not match type of element");
  }

  void checkStructExtractInst(StructExtractInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    require(operandTy.isObject(),
            "cannot struct_extract from address");
    require(EI->getType().isObject(),
            "result of struct_extract cannot be address");
    StructDecl *sd = operandTy.getStructOrBoundGenericStruct();
    require(sd, "must struct_extract from struct");
    require(!EI->getField()->isComputed(),
            "cannot load computed property with struct_extract");

    require(EI->getField()->getDeclContext() == sd,
            "struct_extract field is not a member of the struct");
    
    Type fieldTy = operandTy.getSwiftRValueType()
      ->getTypeOfMember(M, EI->getField(), nullptr);
    require(fieldTy->isEqual(EI->getType().getSwiftRValueType()),
            "result of struct_extract does not match type of field");
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
    StructDecl *sd = operandTy.getStructOrBoundGenericStruct();
    require(sd, "struct_element_addr operand must be struct address");
    require(EI->getType(0).isAddress(),
            "result of struct_element_addr must be address");
    require(!EI->getField()->isComputed(),
            "cannot get address of computed property with struct_element_addr");

    require(EI->getField()->getDeclContext() == sd,
            "struct_element_addr field is not a member of the struct");
    
    Type fieldTy = operandTy.getSwiftRValueType()
      ->getTypeOfMember(M, EI->getField(), nullptr);
    require(fieldTy->isEqual(EI->getType().getSwiftRValueType()),
            "result of struct_element_addr does not match type of field");
  }

  void checkRefElementAddrInst(RefElementAddrInst *EI) {
    requireReferenceValue(EI->getOperand(), "Operand of ref_element_addr");
    require(EI->getType(0).isAddress(),
            "result of ref_element_addr must be lvalue");
    require(!EI->getField()->isComputed(),
            "cannot get address of computed property with ref_element_addr");
    SILType operandTy = EI->getOperand().getType();
    ClassDecl *cd = operandTy.getClassOrBoundGenericClass();
    require(cd, "ref_element_addr operand must be a class instance");
    
    require(EI->getField()->getDeclContext() == cd,
            "ref_element_addr field must be a member of the class");
    
    Type fieldTy = operandTy.getSwiftRValueType()
      ->getTypeOfMember(M, EI->getField(), nullptr);
    require(fieldTy->isEqual(EI->getType().getSwiftRValueType()),
            "result of ref_element_addr does not match type of field");
  }
  
  CanType getMethodSelfType(AnyFunctionType *ft) {
    auto *inputTuple = ft->getInput()->getAs<TupleType>();
    if (!inputTuple)
      return ft->getInput()->getCanonicalType();    
    return inputTuple->getFields().back().getType()->getCanonicalType();
  }
  
  void checkArchetypeMethodInst(ArchetypeMethodInst *AMI) {
    DEBUG(llvm::dbgs() << "verifying";
          AMI->print(llvm::dbgs()));
    AnyFunctionType *methodType = AMI->getType(0).getAs<AnyFunctionType>();
    DEBUG(llvm::dbgs() << "method type ";
          methodType->print(llvm::dbgs());
          llvm::dbgs() << "\n");
    require(methodType,
            "result method must be a function type");
    require(methodType->isThin()
              == AMI->getLookupArchetype().castTo<ArchetypeType>()
                  ->requiresClass(),
            "result method must not be thin function type if class archetype, "
            "thick if not class");
    SILType operandType = AMI->getLookupArchetype();
    DEBUG(llvm::dbgs() << "operand type ";
          operandType.print(llvm::dbgs());
          llvm::dbgs() << "\n");
    require(operandType.is<ArchetypeType>(),
            "operand type must be an archetype");
    
    CanType selfType = getMethodSelfType(methodType);
    require(selfType == operandType.getSwiftType()
            || selfType->isEqual(
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
  
  bool isSelfArchetype(CanType t, ArrayRef<ProtocolDecl*> protocols) {
    ArchetypeType *archetype = dyn_cast<ArchetypeType>(t);
    if (!archetype)
      return false;
    if (!archetype->getAssocType())
      return false;
    if (!archetype->getAssocType()->isSelf())
      return false;
    
    auto *selfProto = archetype->getAssocType()->getProtocol();
    for (auto checkProto : protocols) {
      if (checkProto == selfProto || checkProto->inheritsFrom(selfProto))
        return true;
    }
    
    return false;
  }

  void checkProtocolMethodInst(ProtocolMethodInst *EMI) {
    AnyFunctionType *methodType = EMI->getType(0).getAs<AnyFunctionType>();
    require(methodType,
            "result method must be a function type");
    SILType operandType = EMI->getOperand().getType();
    require(methodType->isThin()
              == operandType.isClassExistentialType(),
            "result method must be thin function type if class protocol, or "
            "thick if not class");
    
    auto proto = dyn_cast<ProtocolDecl>(EMI->getMember().getDecl()
                                          ->getDeclContext());
    require(proto, "protocol_method must take a method of a protocol");
    
    if (EMI->getMember().getDecl()->isInstanceMember()) {
      require(operandType.isExistentialType(),
              "instance protocol_method must apply to an existential address");
      CanType selfType = getMethodSelfType(methodType);
      if (!operandType.isClassExistentialType()) {
        require(isa<LValueType>(selfType),
                "protocol_method result must take its self parameter "
                "by address");
      }
      CanType selfObjType = selfType->getRValueType()->getCanonicalType();
      require(isSelfArchetype(selfObjType, proto),
              "result must be a method of protocol's Self archetype");
    } else {
      require(operandType.isObject(),
              "static protocol_method cannot apply to an address");
      require(operandType.is<MetaTypeType>(),
              "static protocol_method must apply to an existential metatype");
      require(operandType.castTo<MetaTypeType>()
                ->getInstanceType()->isExistentialType(),
              "static protocol_method must apply to an existential metatype");
      require(getMethodSelfType(methodType) ==
                                  EMI->getOperand().getType().getSwiftType(),
              "result must be a method of the existential metatype");
    }
  }

  void checkDynamicMethodInst(DynamicMethodInst *EMI) {
    AnyFunctionType *methodType = EMI->getType(0).getAs<AnyFunctionType>();
    require(methodType,
            "result method must be a function type");
    SILType operandType = EMI->getOperand().getType();

    require(EMI->getMember().getDecl()->isObjC(), "method must be [objc]");
    if (EMI->getMember().getDecl()->isInstanceMember()) {
      require(operandType.getSwiftType()->is<BuiltinObjCPointerType>(),
              "operand must have Builtin.ObjCPointer type");
    } else {
      require(operandType.getSwiftType()->is<MetaTypeType>(),
              "operand must have metatype type");
      require(operandType.getSwiftType()->castTo<MetaTypeType>()
                ->getInstanceType()->is<ProtocolType>(),
              "operand must have metatype of protocol type");
      require(operandType.getSwiftType()->castTo<MetaTypeType>()
                ->getInstanceType()->castTo<ProtocolType>()->getDecl()
                ->isSpecificProtocol(KnownProtocolKind::DynamicLookup),
              "operand must have metatype of DynamicLookup type");
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
    require(CMI->getType()
              == F.getModule().Types.getConstantType(CMI->getMember()),
            "result type of class_method must match type of method");
    auto methodType = CMI->getType().getAs<AnyFunctionType>();
    require(methodType,
            "result method must be of a function type");
    require(methodType->isThin(),
            "result method must be of a thin function type");
    SILType operandType = CMI->getOperand().getType();
    require(isClassOrClassMetatype(operandType.getSwiftType()),
            "operand must be of a class type");
    require(isClassOrClassMetatype(getMethodSelfType(methodType)),
            "result must be a method of a class");
  }
  
  void checkSuperMethodInst(SuperMethodInst *CMI) {
    require(CMI->getType()
              == F.getModule().Types.getConstantType(CMI->getMember()),
            "result type of class_method must match type of method");
    auto methodType = CMI->getType(0).getAs<AnyFunctionType>();
    require(methodType,
            "result method must be of a function type");
    require(methodType->isThin(),
            "result method must be of a thin function type");
    SILType operandType = CMI->getOperand().getType();
    require(isClassOrClassMetatype(operandType.getSwiftType()),
            "operand must be of a class type");
    require(isClassOrClassMetatype(getMethodSelfType(methodType)),
            "result must be a method of a class");
    
    Type methodClass = CMI->getMember().getDecl()->getDeclContext()
      ->getDeclaredTypeInContext();
    
    require(methodClass->getClassOrBoundGenericClass(),
            "super_method must look up a class method");
    require(!methodClass->isEqual(operandType.getSwiftType()),
            "super_method operand should be a subtype of the "
            "lookup class type");
  }
  
  void checkProjectExistentialInst(ProjectExistentialInst *PEI) {
    SILType operandType = PEI->getOperand().getType();
    require(operandType.isAddress(),
            "project_existential must be applied to address");
    
    SmallVector<ProtocolDecl*, 4> protocols;
    require(operandType.getSwiftRValueType()->isExistentialType(protocols),
            "project_existential must be applied to address of existential");
    require(PEI->getType().isAddress(),
            "project_existential result must be an address");
    
    require(isSelfArchetype(PEI->getType().getSwiftRValueType(), protocols),
            "project_existential result must be Self archetype of protocol");
  }
  
  void checkProjectExistentialRefInst(ProjectExistentialRefInst *PEI) {
    SILType operandType = PEI->getOperand().getType();
    require(operandType.isObject(),
            "project_existential_ref operand must not be address");
    SmallVector<ProtocolDecl*, 4> protocols;
    require(operandType.getSwiftType()->isExistentialType(protocols),
            "project_existential must be applied to existential");
    require(operandType.isClassExistentialType(),
            "project_existential_ref operand must be class existential");
    
    require(isSelfArchetype(PEI->getType().getSwiftRValueType(), protocols),
            "project_existential_ref result must be Self archetype of protocol");
  }
  
  void checkInitExistentialInst(InitExistentialInst *AEI) {
    SILType exType = AEI->getOperand().getType();
    require(exType.isAddress(),
            "init_existential must be applied to an address");
    require(exType.isExistentialType(),
            "init_existential must be applied to address of existential");
    require(!exType.isClassExistentialType(),
            "init_existential must be applied to non-class existential");
    require(!AEI->getConcreteType().isExistentialType(),
            "init_existential cannot put an existential container inside "
            "an existential container");
  }
  
  void checkInitExistentialRefInst(InitExistentialRefInst *IEI) {
    SILType concreteType = IEI->getOperand().getType();
    require(concreteType.getSwiftType()->mayHaveSuperclass(),
            "init_existential_ref operand must be a class instance");
    require(IEI->getType().isClassExistentialType(),
            "init_existential_ref result must be a class existential type");
    require(IEI->getType().isObject(),
            "init_existential_ref result must not be an address");
  }
  
  void checkUpcastExistentialInst(UpcastExistentialInst *UEI) {
    SILType srcType = UEI->getSrcExistential().getType();
    SILType destType = UEI->getDestExistential().getType();
    require(srcType != destType,
            "can't upcast_existential to same type");
    require(srcType.isExistentialType(),
            "upcast_existential source must be existential");
    require(destType.isAddress(),
            "upcast_existential dest must be an address");
    require(destType.isExistentialType(),
            "upcast_existential dest must be address of existential");
    require(!destType.isClassExistentialType(),
            "upcast_existential dest must be non-class existential");
  }
  
  void checkUpcastExistentialRefInst(UpcastExistentialRefInst *UEI) {
    require(UEI->getOperand().getType() != UEI->getType(),
            "can't upcast_existential_ref to same type");
    require(UEI->getOperand().getType().isObject(),
            "upcast_existential_ref operand must not be an address");
    require(UEI->getOperand().getType().isClassExistentialType(),
            "upcast_existential_ref operand must be class existential");
    require(UEI->getType().isObject(),
            "upcast_existential_ref result must not be an address");
    require(UEI->getType().isClassExistentialType(),
            "upcast_existential_ref result must be class existential");
  }
  
  void checkDeinitExistentialInst(DeinitExistentialInst *DEI) {
    SILType exType = DEI->getOperand().getType();
    require(exType.isAddress(),
            "deinit_existential must be applied to an address");
    require(exType.isExistentialType(),
            "deinit_existential must be applied to address of existential");
    require(!exType.isClassExistentialType(),
            "deinit_existential must be applied to non-class existential");
  }
  
  void checkArchetypeRefToSuperInst(ArchetypeRefToSuperInst *ASI) {
    ArchetypeType *archetype
      = ASI->getOperand().getType().getAs<ArchetypeType>();
    require(archetype, "archetype_ref_to_super operand must be archetype");
    require(archetype->requiresClass(),
            "archetype_ref_to_super operand must be class archetype");
    require(ASI->getType().getClassOrBoundGenericClass(),
            "archetype_ref_to_super must convert to a class type");
  }
  
  void verifyCheckedCast(CheckedCastKind kind, SILType fromTy, SILType toTy) {
    // Verify common invariants.
    require(fromTy != toTy, "can't checked cast to same type");
    require(fromTy.isAddress() == toTy.isAddress(),
            "address-ness of checked cast src and dest must match");
    
    switch (kind) {
    case CheckedCastKind::Unresolved:
    case CheckedCastKind::InvalidCoercible:
      llvm_unreachable("invalid for SIL");
    case CheckedCastKind::Downcast:
      require(fromTy.isObject(),
              "downcast operand must be an object");
      require(fromTy.getClassOrBoundGenericClass(),
              "downcast operand must be a class type");
      require(toTy.getClassOrBoundGenericClass(),
              "downcast must convert to a class type");
      require(fromTy.getSwiftType()->isSuperclassOf(toTy.getSwiftType(),
                                                    nullptr),
              "downcast must convert to a subclass");
      return;
    case CheckedCastKind::SuperToArchetype: {
      require(fromTy.isObject(),
              "super_to_archetype operand must be an object");
      require(fromTy.getClassOrBoundGenericClass(),
              "super_to_archetype operand must be a class instance");
      auto archetype = toTy.getAs<ArchetypeType>();
      require(archetype, "super_to_archetype must convert to archetype type");
      require(archetype->requiresClass(),
              "super_to_archetype must convert to class archetype type");
      return;
    }
    case CheckedCastKind::ArchetypeToConcrete: {
      require(fromTy.getAs<ArchetypeType>(),
              "archetype_to_concrete must convert from archetype type");
      return;
    }
    case CheckedCastKind::ArchetypeToArchetype: {
      require(fromTy.getAs<ArchetypeType>(),
              "archetype_to_archetype must convert from archetype type");
      require(toTy.getAs<ArchetypeType>(),
              "archetype_to_archetype must convert to archetype type");
      return;
    }
    case CheckedCastKind::ExistentialToArchetype: {
      require(fromTy.isExistentialType(),
              "existential_to_archetype must convert from protocol type");
      require(toTy.getAs<ArchetypeType>(),
              "existential_to_archetype must convert to archetype type");
      return;
    }
    case CheckedCastKind::ExistentialToConcrete: {
      require(fromTy.isExistentialType(),
              "existential_to_concrete must convert from protocol type");
      return;
    }
    }
  }
  
  void checkUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *CI) {
    verifyCheckedCast(CI->getCastKind(),
                      CI->getOperand().getType(),
                      CI->getType());
  }
  
  void checkCheckedCastBranchInst(CheckedCastBranchInst *CBI) {
    verifyCheckedCast(CBI->getCastKind(),
                      CBI->getOperand().getType(),
                      CBI->getCastType());
    
    require(CBI->getSuccessBB()->bbarg_size() == 1,
            "success dest of checked_cast_br must take one argument");
    require(CBI->getSuccessBB()->bbarg_begin()[0]->getType()
              == CBI->getCastType(),
            "success dest block argument of checked_cast_br must match type of cast");
    require(CBI->getFailureBB()->bbarg_empty(),
            "failure dest of checked_cast_br must take no arguments");
  }
  
  void checkBridgeToBlockInst(BridgeToBlockInst *BBI) {
    SILType operandTy = BBI->getOperand().getType();
    SILType resultTy = BBI->getType();
    
    require(operandTy.isObject(),
            "bridge_to_block operand cannot be an address");
    require(resultTy.isObject(),
            "bridge_to_block result cannot be an address");
    require(operandTy.is<FunctionType>(),
            "bridge_to_block operand must be a function type");
    require(resultTy.is<FunctionType>(),
            "bridge_to_block result must be a function type");
    
    auto operandFTy = BBI->getOperand().getType().castTo<FunctionType>();
    auto resultFTy = BBI->getType().castTo<FunctionType>();
    
    require(operandFTy.getInput() == resultFTy.getInput(),
            "bridge_to_block operand and result types must differ only in "
            "[objc_block]-ness");
    require(operandFTy.getResult() == resultFTy.getResult(),
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
    requireObjectType(AnyFunctionType, TTFI->getOperand(),
                      "thin_to_thick_function operand");
    requireObjectType(AnyFunctionType, TTFI,
                      "thin_to_thick_function result");
    if (auto opFTy = dyn_cast<FunctionType>(
                                 TTFI->getOperand().getType().getSwiftType())) {
      auto resFTy = dyn_cast<FunctionType>(TTFI->getType().getSwiftType());
      require(resFTy &&
              opFTy->getInput()->isEqual(resFTy->getInput()) &&
              opFTy->getResult()->isEqual(resFTy->getResult()) &&
              opFTy->isAutoClosure() == resFTy->isAutoClosure() &&
              opFTy->isBlock() == resFTy->isBlock() &&
              opFTy->getAbstractCC() == resFTy->getAbstractCC(),
              "thin_to_thick_function operand and result type must differ only "
              " in thinness");
      require(!resFTy->isThin(),
              "thin_to_thick_function result must not be thin");
      require(opFTy->isThin(),
              "thin_to_thick_function operand must be thin");
    } else if (auto opPTy = dyn_cast<PolymorphicFunctionType>(
                                 TTFI->getOperand().getType().getSwiftType())) {
      auto resPTy = dyn_cast<PolymorphicFunctionType>(
                                               TTFI->getType().getSwiftType());
      require(resPTy &&
              opPTy->getInput()->isEqual(resPTy->getInput()) &&
              opPTy->getResult()->isEqual(resPTy->getResult()) &&
              opPTy->getAbstractCC() == opPTy->getAbstractCC(),
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
    requireObjectType(AnyFunctionType, CCI->getOperand(), "convert_cc operand");
    requireObjectType(AnyFunctionType, CCI, "convert_cc result");
    if (auto opFTy = dyn_cast<FunctionType>(
                                 CCI->getOperand().getType().getSwiftType())) {
      auto resFTy = dyn_cast<FunctionType>(CCI->getType().getSwiftType());
      require(resFTy &&
              opFTy->getInput()->isEqual(resFTy->getInput()) &&
              opFTy->getResult()->isEqual(resFTy->getResult()) &&
              opFTy->isAutoClosure() == resFTy->isAutoClosure() &&
              opFTy->isBlock() == resFTy->isBlock(),
              "convert_cc operand and result type must differ only "
              " in calling convention");
      // FIXME: Non-virtual CC thunks can be thin, as can ABI-compatible thunks.
      require(!resFTy->isThin(),
              "convert_cc result must be thick");
      require(opFTy->isThin(),
              "convert_cc operand must be thin");
    } else if (auto opPTy = dyn_cast<PolymorphicFunctionType>(
                                 CCI->getOperand().getType().getSwiftType())) {
      auto resPTy = dyn_cast<PolymorphicFunctionType>(
                                               CCI->getType().getSwiftType());
      require(resPTy &&
              opPTy->getInput()->isEqual(resPTy->getInput()) &&
              opPTy->getResult()->isEqual(resPTy->getResult()),
              "convert_cc operand and result type must differ only "
              " in calling convention");
      // FIXME: Non-virtual CC thunks can be thin, as can ABI-compatible thunks.
      require(!resPTy->isThin(),
              "convert_cc result must be thick");
      require(opPTy->isThin(),
              "convert_cc operand must be thin");
    } else {
      llvm_unreachable("invalid AnyFunctionType?!");
    }    
  }

  void checkRefToUnownedInst(RefToUnownedInst *I) {
    requireReferenceValue(I->getOperand(), "Operand of ref_to_unowned");
    auto operandType = I->getOperand().getType().getSwiftRValueType();
    auto resultType = requireObjectType(UnownedStorageType, I,
                                        "Result of ref_to_unowned");
    require(resultType.getReferentType() == operandType,
            "Result of ref_to_unowned does not have the "
            "operand's type as its referent type");
  }

  void checkUnownedToRefInst(UnownedToRefInst *I) {
    auto operandType = requireObjectType(UnownedStorageType,
                                         I->getOperand(),
                                         "Operand of unowned_to_ref");
    requireReferenceValue(I, "Result of unowned_to_ref");
    auto resultType = I->getType().getSwiftRValueType();
    require(operandType.getReferentType() == resultType,
            "Operand of unowned_to_ref does not have the "
            "operand's type as its referent type");
  }
  
  void checkUpcastInst(UpcastInst *UI) {
    require(UI->getType() != UI->getOperand().getType(),
            "can't upcast to same type");
    
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
      require(instTy->isSuperclassOf(opInstTy, nullptr),
              "upcast must cast to a superclass");
    } else {
      require(UI->getOperand().getType().getSwiftType()
                ->getClassOrBoundGenericClass(),
              "upcast operand must be a class or class metatype instance");
      require(UI->getType().getSwiftType()->getClassOrBoundGenericClass(),
              "upcast must convert a class instance to a class type");
      require(UI->getType().getSwiftType()
                ->isSuperclassOf(UI->getOperand().getType().getSwiftType(),
                                 nullptr),
              "upcast must cast to a superclass");
    }
  }
  
  void checkIsNonnullInst(IsNonnullInst *II) {
    require(II->getOperand().getType().getSwiftType()
              ->mayHaveSuperclass(),
            "isa operand must be a class type");
  }

  void checkAddressToPointerInst(AddressToPointerInst *AI) {
    require(AI->getOperand().getType().isAddress(),
            "address-to-pointer operand must be an address");
    require(AI->getType().getSwiftType()->isEqual(
                              AI->getType().getASTContext().TheRawPointerType),
            "address-to-pointer result type must be RawPointer");
  }

  void checkRefToObjectPointerInst(RefToObjectPointerInst *AI) {
    require(AI->getOperand().getType()
              .getSwiftType()->mayHaveSuperclass(),
            "ref-to-object-pointer operand must be a class reference");
    auto destType = AI->getType().getSwiftType();
    auto &C = AI->getType().getASTContext();
    require(destType->isEqual(C.TheObjectPointerType)
            || destType->isEqual(C.TheObjCPointerType),
          "ref-to-object-pointer result must be ObjectPointer or ObjCPointer");
  }
  
  void checkObjectPointerToRefInst(ObjectPointerToRefInst *AI) {
    require(AI->getType()
              .getSwiftType()->mayHaveSuperclass(),
            "object-pointer-to-ref result must be a class reference");
    auto srcType = AI->getOperand().getType().getSwiftType();
    auto &C = AI->getOperand().getType().getASTContext();
    require(srcType->isEqual(C.TheObjectPointerType)
            || srcType->isEqual(C.TheObjCPointerType),
          "object-pointer-to-ref operand must be ObjectPointer or ObjCPointer");
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
    require(ICI->getOperand().getType().isObject(),
            "conversion operand cannot be an address");
    require(ICI->getType().isObject(),
            "conversion result cannot be an address");
    
    auto opFTy = ICI->getOperand().getType().getAs<AnyFunctionType>();
    auto resFTy = ICI->getType().getAs<AnyFunctionType>();

    require(opFTy, "convert_function operand must be a function");
    require(resFTy, "convert_function result must be a function");
    require(opFTy->getAbstractCC() == resFTy->getAbstractCC(),
            "convert_function cannot change function cc");
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
    require(instResultType.isObject(),
            "autoreleased return value cannot be an address");
    require(instResultType.hasReferenceSemantics(),
            "autoreleased return value must be a reference type");
  }
  
  void checkSwitchIntInst(SwitchIntInst *SII) {
    require(SII->getOperand().getType().is<BuiltinIntegerType>(),
            "switch_int operand is not a builtin int type");
    
    auto ult = [](const APInt &a, const APInt &b) { return a.ult(b); };
    std::set<APInt, decltype(ult)> cases(ult);
    
    for (unsigned i = 0, e = SII->getNumCases(); i < e; ++i) {
      APInt value;
      SILBasicBlock *dest;
      std::tie(value, dest) = SII->getCase(i);
      
      require(!cases.count(value),
              "multiple switch_int cases for same value");
      cases.insert(value);
      
      require(dest->bbarg_empty(),
              "switch_int case destination cannot take arguments");
    }
    if (SII->hasDefault())
      require(SII->getDefaultBB()->bbarg_empty(),
              "switch_int default destination cannot take arguments");
  }

  void checkSwitchEnumInst(SwitchEnumInst *SOI) {
    require(SOI->getOperand().getType().isObject(),
            "switch_enum operand must be an object");
    
    CanType uTy = SOI->getOperand().getType().getSwiftRValueType();
    EnumDecl *uDecl = uTy->getEnumOrBoundGenericEnum();
    require(uDecl, "switch_enum operand is not an enum");
    
    // Find the set of enum elements for the type so we can verify
    // exhaustiveness.
    // FIXME: We also need to consider if the enum is resilient, in which case
    // we're never guaranteed to be exhaustive.
    llvm::DenseSet<EnumElementDecl*> unswitchedElts;
    uDecl->getAllElements(unswitchedElts);
    
    // Verify the set of enum cases we dispatch on.
    for (unsigned i = 0, e = SOI->getNumCases(); i < e; ++i) {
      EnumElementDecl *elt;
      SILBasicBlock *dest;
      std::tie(elt, dest) = SOI->getCase(i);
      
      require(elt->getDeclContext() == uDecl,
              "switch_enum dispatches on enum element that is not part of "
              "its type");
      require(unswitchedElts.count(elt),
              "switch_enum dispatches on same enum element more than once");
      unswitchedElts.erase(elt);
      
      // The destination BB can take the argument payload, if any, as a BB
      // arguments, or it can ignore it and take no arguments.
      if (elt->hasArgumentType()) {
        require(dest->getBBArgs().size() == 0
                  || dest->getBBArgs().size() == 1,
                "switch_enum destination for case w/ args must take 0 or 1 "
                "arguments");

        if (dest->getBBArgs().size() == 1) {
          Type eltArgTy = uTy->getTypeOfMember(M, elt, nullptr,
                                               elt->getArgumentType());
          CanType bbArgTy = dest->getBBArgs()[0]->getType().getSwiftRValueType();
          require(eltArgTy->isEqual(bbArgTy),
                  "switch_enum destination bbarg must match case arg type");
          require(!dest->getBBArgs()[0]->getType().isAddress(),
                  "switch_enum destination bbarg type must not be an address");
        }
        
      } else {
        require(dest->getBBArgs().size() == 0,
                "switch_enum destination for no-argument case must take no "
                "arguments");
      }
    }
    
    // If the switch is non-exhaustive, we require a default.
    require(unswitchedElts.empty() || SOI->hasDefault(),
            "nonexhaustive switch_enum must have a default destination");
    if (SOI->hasDefault())
      require(SOI->getDefaultBB()->bbarg_empty(),
              "switch_enum default destination must take no arguments");
  }
  
  void checkDestructiveSwitchEnumAddrInst(DestructiveSwitchEnumAddrInst *SOI){
    require(SOI->getOperand().getType().isAddress(),
            "destructive_switch_enum_addr operand must be an object");
    
    CanType uTy = SOI->getOperand().getType().getSwiftRValueType();
    EnumDecl *uDecl = uTy->getEnumOrBoundGenericEnum();
    require(uDecl, "destructive_switch_enum_addr operand must be an enum");
    
    // Find the set of enum elements for the type so we can verify
    // exhaustiveness.
    // FIXME: We also need to consider if the enum is resilient, in which case
    // we're never guaranteed to be exhaustive.
    llvm::DenseSet<EnumElementDecl*> unswitchedElts;
    uDecl->getAllElements(unswitchedElts);
    
    // Verify the set of enum cases we dispatch on.
    for (unsigned i = 0, e = SOI->getNumCases(); i < e; ++i) {
      EnumElementDecl *elt;
      SILBasicBlock *dest;
      std::tie(elt, dest) = SOI->getCase(i);
      
      require(elt->getDeclContext() == uDecl,
              "destructive_switch_enum_addr dispatches on enum element that "
              "is not part of its type");
      require(unswitchedElts.count(elt),
              "destructive_switch_enum_addr dispatches on same enum element "
              "more than once");
      unswitchedElts.erase(elt);
      
      // The destination BB must take the argument payload, if any, as a BB
      // argument.
      if (elt->hasArgumentType()) {
        require(dest->getBBArgs().size() == 1,
                "destructive_switch_enum_addr destination for case w/ args "
                "must take an argument");
        
        Type eltArgTy = uTy->getTypeOfMember(M, elt, nullptr,
                                             elt->getArgumentType());
        CanType bbArgTy = dest->getBBArgs()[0]->getType().getSwiftRValueType();
        require(eltArgTy->isEqual(bbArgTy),
                "destructive_switch_enum_addr destination bbarg must match "
                "case arg type");
        require(dest->getBBArgs()[0]->getType().isAddress(),
                "destructive_switch_enum_addr destination bbarg type must "
                "be an address");
      } else {
        require(dest->getBBArgs().size() == 0,
                "destructive_switch_enum_addr destination for no-argument "
                "case must take no arguments");
      }
    }
    
    // If the switch is non-exhaustive, we require a default.
    require(unswitchedElts.empty() || SOI->hasDefault(),
            "nonexhaustive destructive_switch_enum_addr must have a default "
            "destination");
    if (SOI->hasDefault())
      require(SOI->getDefaultBB()->bbarg_empty(),
              "destructive_switch_enum_addr default destination must take "
              "no arguments");
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

  void checkDynamicMethodBranchInst(DynamicMethodBranchInst *DMBI) {
    SILType operandType = DMBI->getOperand().getType();

    require(DMBI->getMember().getDecl()->isObjC(), "method must be [objc]");
    if (DMBI->getMember().getDecl()->isInstanceMember()) {
      require(operandType.getSwiftType()->is<BuiltinObjCPointerType>(),
              "operand must have Builtin.ObjCPointer type");
    } else {
      require(operandType.getSwiftType()->is<MetaTypeType>(),
              "operand must have metatype type");
      require(operandType.getSwiftType()->castTo<MetaTypeType>()
              ->getInstanceType()->is<ProtocolType>(),
              "operand must have metatype of protocol type");
      require(operandType.getSwiftType()->castTo<MetaTypeType>()
              ->getInstanceType()->castTo<ProtocolType>()->getDecl()
              ->isSpecificProtocol(KnownProtocolKind::DynamicLookup),
              "operand must have metatype of DynamicLookup type");
    }

    // FIXME: Check branch arguments.
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

  void verifyEpilogBlock(SILFunction *F) {
    bool FoundEpilogBlock = false;
    for (auto &BB : *F) {
      if (isa<ReturnInst>(BB.getTerminator())) {
        require(FoundEpilogBlock == false,
                "more than one function epilog block");
        FoundEpilogBlock = true;
      }
    }
  }
  
  void visitSILFunction(SILFunction *F) {
    
    verifyEntryPointArguments(F->getBlocks().begin());
    verifyEpilogBlock(F);

    SILVisitor::visitSILFunction(F);
  }
  
  void verify() {

    visitSILFunction(const_cast<SILFunction*>(&F));
  }
};
} // end anonymous namespace

#undef require
#undef requireObjectType
#endif //NDEBUG

/// verify - Run the SIL verifier to make sure that the SILFunction follows
/// invariants.
void SILFunction::verify(Module *M) const {
#ifndef NDEBUG
  if (isExternalDeclaration()) {
    assert(getLinkage() != SILLinkage::Internal &&
           "external declaration of internal SILFunction not allowed");
    return;
  }
  SILVerifier(M, *this).verify();
#endif
}

/// Verify that a vtable follows invariants.
void SILVTable::verify(const SILModule &M) const {
#ifndef NDEBUG
  for (auto &entry : getEntries()) {
    // All vtable entries must be decls in a class context.
    assert(entry.first.hasDecl() && "vtable entry is not a decl");
    ValueDecl *decl = entry.first.getDecl();
    auto theClass = dyn_cast_or_null<ClassDecl>(decl->getDeclContext());
    assert(theClass && "vtable entry must refer to a class member");
    
    // The class context must be the vtable's class, or a superclass thereof.
    auto c = getClass();
    do {
      if (c == theClass)
        break;
      if (auto ty = c->getSuperclass())
        c = ty->getClassOrBoundGenericClass();
      else
        c = nullptr;
    } while (c);
    assert(c && "vtable entry must refer to a member of the vtable's class");
    
    // All function vtable entries must be at their natural uncurry level.
    // FIXME: We should change this to uncurry level 1.
    assert(!entry.first.isCurried && "vtable entry must not be curried");
    
    // Foreign entry points shouldn't appear in vtables.
    assert(!entry.first.isForeign && "vtable entry must not be foreign");
    
    // TODO: Verify that property entries are dynamically dispatched under our
    // finalized property dynamic dispatch rules.
  }
#endif
}

/// Verify the module.
void SILModule::verify(Module *M) const {
#ifndef NDEBUG
  // Check all functions.
  llvm::StringSet<> functionNames;
  for (const SILFunction &f : *this) {
    if (!functionNames.insert(f.getName())) {
      llvm::errs() << "Function redefined: " << f.getName() << "!\n";
      assert(false && "triggering standard assertion failure routine");
    }
    f.verify(M);
  }
  
  // Check all vtables.
  llvm::DenseSet<ClassDecl*> vtableClasses;
  for (const SILVTable &vt : getVTables()) {
    if (!vtableClasses.insert(vt.getClass()).second) {
      llvm::errs() << "Vtable redefined: " << vt.getClass()->getName() << "!\n";
      assert(false && "triggering standard assertion failure routine");
    }
    vt.verify(*this);
  }
#endif
}
