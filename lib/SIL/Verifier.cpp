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

#define DEBUG_TYPE "silverifier"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/Dominance.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Basic/Range.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/CommandLine.h"
using namespace swift;

using Lowering::AbstractionPattern;

// This flag is used only to check that sil-combine can properly
// remove any code after unreachable, thus bringing SIL into
// its canonical form which may get temporarily broken during
// intermediate transformations.
static llvm::cl::opt<bool> SkipUnreachableMustBeLastErrors(
                              "verify-skip-unreachable-must-be-last",
                              llvm::cl::init(false));

// The verifier is basically all assertions, so don't compile it with NDEBUG to
// prevent release builds from triggering spurious unused variable warnings.
#ifndef NDEBUG

/// Returns true if A is an opened existential type, Self, or is equal to an
/// archetype in F's nested archetype list.
///
/// FIXME: Once Self has been removed in favor of opened existential types
/// everywhere, remove support for self.
static bool isArchetypeValidInFunction(ArchetypeType *A, SILFunction *F) {
  // The only two cases where an archetype is always legal in a function is if
  // it is self or if it is from an opened existential type. Currently, Self is
  // being migrated away from in favor of opened existential types, so we should
  // remove the special case here for Self when that process is completed.
  //
  // *NOTE* Associated types of self are not valid here.
  if (!A->getOpenedExistentialType().isNull() || A->getSelfProtocol())
    return true;

  // Ok, we have an archetype, make sure it is in the nested archetypes of our
  // caller.
  for (auto Iter : F->getContextGenericParams()->getAllNestedArchetypes())
    if (A->isEqual(&*Iter))
      return true;
  return A->getIsRecursive();
}

namespace {
/// Metaprogramming-friendly base class.
template <class Impl>
class SILVerifierBase : public SILVisitor<Impl> {
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
  Lowering::TypeConverter &TC;
  const SILInstruction *CurInstruction = nullptr;
  DominanceInfo *Dominance = nullptr;

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
      llvm::dbgs() << "In function:\n";
      F.print(llvm::dbgs());
    } else {
      llvm::dbgs() << "In function:\n";
      F.print(llvm::dbgs());
    }

    abort();
  }
#define require(condition, complaint) \
  _require(bool(condition), complaint ": " #condition)

  template <class T> typename CanTypeWrapperTraits<T>::type
  _requireObjectType(SILType type, const Twine &valueDescription,
                     const char *typeName) {
    _require(type.isObject(), valueDescription + " must be an object");
    auto result = type.getAs<T>();
    _require(bool(result), valueDescription + " must have type " + typeName);
    return result;
  }
  template <class T> typename CanTypeWrapperTraits<T>::type
  _requireObjectType(SILValue value, const Twine &valueDescription,
                     const char *typeName) {
    return _requireObjectType<T>(value.getType(), valueDescription, typeName);
  }
#define requireObjectType(type, value, valueDescription) \
  _requireObjectType<type>(value, valueDescription, #type)

  // Require that the operand is a non-optional reference-counted type.
  void requireReferenceValue(SILValue value, const Twine &valueDescription) {
    require(value.getType().isObject(), valueDescription +" must be an object");
    require(value.getType().isReferenceCounted(F.getModule()),
            valueDescription + " must have reference semantics");
  }

  // Require that the operand is a reference-counted type, or an Optional
  // thereof.
  void requireReferenceOrOptionalReferenceValue(SILValue value,
                                                const Twine &valueDescription) {
    require(value.getType().isObject(), valueDescription +" must be an object");
    
    auto objectTy = value.getType();
    OptionalTypeKind otk;
    if (auto optObjTy = objectTy.getAnyOptionalObjectType(F.getModule(), otk)) {
      objectTy = optObjTy;
    }
    
    require(objectTy.isReferenceCounted(F.getModule()),
            valueDescription + " must have reference semantics");
  }
  
  // Require that the operand is a type that supports reference storage
  // modifiers.
  void requireReferenceStorageCapableValue(SILValue value,
                                           const Twine &valueDescription) {
    requireReferenceOrOptionalReferenceValue(value, valueDescription);
    require(!value.getType().is<SILFunctionType>(),
            valueDescription + " cannot apply to a function type");
  }
  
  // Require that the operand is a reference-counted type, or potentially an
  // optional thereof.
  void requireRetainablePointerValue(SILValue value,
                                     const Twine &valueDescription) {
    require(value.getType().isObject(), valueDescription +" must be an object");
    require(value.getType().hasRetainablePointerRepresentation(),
            valueDescription + " must have retainable pointer representation");
  }

  /// Assert that two types are equal.
  void requireSameType(SILType type1, SILType type2, const Twine &complaint) {
    _require(type1 == type2, complaint, [&] {
      llvm::dbgs() << "  " << type1 << "\n  " << type2 << '\n';
    });
  }
  
  /// Require two function types to be ABI-compatible.
  void requireABICompatibleFunctionTypes(CanSILFunctionType type1,
                                         CanSILFunctionType type2,
                                         const Twine &what) {
    
    auto complain = [=](const char *msg) -> std::function<void()> {
      return [=]{
        llvm::dbgs() << "  " << msg << '\n'
                     << "  " << type1 << "\n  " << type2 << '\n';
      };
    };
    auto complainBy = [=](std::function<void()> msg) -> std::function<void()> {
      return [=]{
        msg();
        llvm::dbgs() << '\n';
        llvm::dbgs() << "  " << type1 << "\n  " << type2 << '\n';
      };
    };
    
    // The calling convention and function representation can't be changed.
    _require(type1->getRepresentation() == type2->getRepresentation(), what,
             complain("Different function representations"));

    // TODO: We should compare generic signatures. Class and witness methods
    // allow variance in "self"-fulfilled parameters; other functions must
    // match exactly.
    auto signature1 = type1->getGenericSignature();
    auto signature2 = type2->getGenericSignature();

    auto getAnyOptionalObjectTypeInContext = [&](CanGenericSignature sig,
                                                 SILType type) {
      Lowering::GenericContextScope context(F.getModule().Types, sig);
      OptionalTypeKind _;
      return type.getAnyOptionalObjectType(F.getModule(), _);
    };

    // TODO: More sophisticated param and return ABI compatibility rules could
    // diverge.
    std::function<bool (SILType, SILType)>
    areABICompatibleParamsOrReturns = [&](SILType a, SILType b) -> bool {
      // Address parameters are all ABI-compatible, though the referenced
      // values may not be. Assume whoever's doing this knows what they're
      // doing.
      if (a.isAddress() && b.isAddress())
        return true;
      // Addresses aren't compatible with values.
      // TODO: An exception for pointerish types?
      else if (a.isAddress() || b.isAddress())
        return false;
      
      // Tuples are ABI compatible if their elements are.
      // TODO: Should destructure recursively.
      SmallVector<CanType, 1> aElements, bElements;
      if (auto tup = a.getAs<TupleType>()) {
        auto types = tup.getElementTypes();
        aElements.append(types.begin(), types.end());
      } else {
        aElements.push_back(a.getSwiftRValueType());
      }
      if (auto tup = b.getAs<TupleType>()) {
        auto types = tup.getElementTypes();
        bElements.append(types.begin(), types.end());
      } else {
        bElements.push_back(b.getSwiftRValueType());
      }
      
      if (aElements.size() != bElements.size())
        return false;

      for (unsigned i : indices(aElements)) {
        auto aa = SILType::getPrimitiveObjectType(aElements[i]),
             bb = SILType::getPrimitiveObjectType(bElements[i]);
        // Equivalent types are always ABI-compatible.
        if (aa == bb)
          continue;
        
        // Bridgeable object types are interchangeable.
        if (aa.isBridgeableObjectType() && bb.isBridgeableObjectType())
          continue;
        
        // Optional and IUO are interchangeable if their elements are.
        auto aObject = getAnyOptionalObjectTypeInContext(signature1, aa);
        auto bObject = getAnyOptionalObjectTypeInContext(signature2, bb);
        if (aObject && bObject
            && areABICompatibleParamsOrReturns(aObject, bObject))
          continue;
        // Optional objects are ABI-interchangeable with non-optionals;
        // None is represented by a null pointer.
        if (aObject && aObject.isBridgeableObjectType()
            && bb.isBridgeableObjectType())
          continue;
        if (bObject && bObject.isBridgeableObjectType()
            && aa.isBridgeableObjectType())
          continue;
        
        // Function types are interchangeable if they're also ABI-compatible.
        if (auto aFunc = aa.getAs<SILFunctionType>())
          if (auto bFunc = bb.getAs<SILFunctionType>()) {
            // FIXME
            requireABICompatibleFunctionTypes(aFunc, bFunc, what);
            return true;
          }
        
        // Metatypes are interchangeable with metatypes with the same
        // representation.
        if (auto aMeta = aa.getAs<MetatypeType>())
          if (auto bMeta = bb.getAs<MetatypeType>())
            if (aMeta->getRepresentation() == bMeta->getRepresentation())
              continue;
        
        // Other types must match exactly.
        return false;
      }
      return true;
    };
    
    // Check the return value.
    
    auto result1 = type1->getResult();
    auto result2 = type2->getResult();
    _require(result1.getConvention() == result2.getConvention(), what,
             complain("Different return value conventions"));
    _require(areABICompatibleParamsOrReturns(result1.getSILType(),
                                             result2.getSILType()), what,
             complain("ABI-incompatible return values"));

    // Our error result conventions are designed to be ABI compatible
    // with functions lacking error results.  Just make sure that the
    // actual conventions match up.
    if (type1->hasErrorResult() && type2->hasErrorResult()) {
      auto error1 = type1->getErrorResult();
      auto error2 = type2->getErrorResult();
      _require(error1.getConvention() == error2.getConvention(), what,
               complain("Different error result conventions"));
      _require(areABICompatibleParamsOrReturns(error1.getSILType(),
                                               error2.getSILType()), what,
               complain("ABI-incompatible error results"));
    }

    // Check the parameters.
    // TODO: Could allow known-empty types to be inserted or removed, but SIL
    // doesn't know what empty types are yet.
    
    _require(type1->getParameters().size() == type2->getParameters().size(),
             what, complain("different number of parameters"));
    for (unsigned i : indices(type1->getParameters())) {
      auto param1 = type1->getParameters()[i];
      auto param2 = type2->getParameters()[i];
      
      _require(param1.getConvention() == param2.getConvention(), what,
               complainBy([=] {
                 llvm::dbgs() << "Different conventions for parameter " << i;
               }));
      _require(areABICompatibleParamsOrReturns(param1.getSILType(),
                                               param2.getSILType()), what,
               complainBy([=] {
                 llvm::dbgs() << "ABI-incompatible types for parameter " << i;
               }));
    }
  }

  void requireSameFunctionComponents(CanSILFunctionType type1,
                                     CanSILFunctionType type2,
                                     const Twine &what) {
    require(type1->getResult() == type2->getResult(),
            "result types of " + what + " do not match");
    require(type1->getParameters().size() ==
            type2->getParameters().size(),
            "inputs of " + what + " do not match in count");
    for (auto i : indices(type1->getParameters())) {
      require(type1->getParameters()[i] ==
              type2->getParameters()[i],
              "input " + Twine(i) + " of " + what + " do not match");
    }
  }

  SILVerifier(const SILFunction &F)
    : M(F.getModule().getSwiftModule()), F(F), TC(F.getModule().Types),
      Dominance(nullptr) {
    if (F.isExternalDeclaration())
      return;
      
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
    if (Dominance)
      delete Dominance;
  }

  void visitSILArgument(SILArgument *arg) {
    checkLegalTypes(arg->getFunction(), arg);
  }

  void visitSILInstruction(SILInstruction *I) {
    CurInstruction = I;
    checkSILInstruction(I);

    // Check the SILLLocation attached to the instruction.
    checkInstructionsSILLocation(I);

    checkLegalTypes(I->getFunction(), I);
  }

  void checkSILInstruction(SILInstruction *I) {
    const SILBasicBlock *BB = I->getParent();
    // Check that non-terminators look ok.
    if (!isa<TermInst>(I)) {
      require(!BB->empty(), "Can't be in a parent block if it is empty");
      require(&*BB->getInstList().rbegin() != I,
              "Non-terminators cannot be the last in a block");
    } else {
      // Skip the check for UnreachableInst, if explicitly asked to do so.
      if (!isa<UnreachableInst>(I) || !SkipUnreachableMustBeLastErrors)
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
      require(userI->getFunction() == &F,
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
        require(valueI->getFunction() == &F,
                "instruction uses value of instruction from another function");
        require(Dominance->properlyDominates(valueI, I),
                "instruction isn't dominated by its operand");
      }
      
      if (auto *valueBBA = dyn_cast<SILArgument>(operand.get())) {
        require(valueBBA->getParent(),
                "instruction uses value of unparented instruction");
        require(valueBBA->getFunction() == &F,
                "bb argument value from another function");
        require(Dominance->dominates(valueBBA->getParent(), I->getParent()),
                "instruction isn't dominated by its bb argument operand");
      }

      require(operand.getUser() == I,
              "instruction's operand's owner isn't the instruction");
      require(isInValueUses(&operand), "operand value isn't used by operand");

      // Make sure that if operand is generic that its primary archetypes match
      // the function context.
      checkLegalTypes(I->getFunction(), operand.get().getDef());
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

    if (!maybeScopeless(*I))
      require(I->getDebugScope(), "instruction has a location, but no scope");
  }

  /// Check that the types of this value producer are all legal in the function
  /// context in which it exists.
  void checkLegalTypes(SILFunction *F, ValueBase *value) {
    for (auto type : value->getTypes()) {
      checkLegalType(F, type);
    }
  }

  /// Check that the given type is a legal SIL value.
  void checkLegalType(SILFunction *F, SILType type) {
    auto rvalueType = type.getSwiftRValueType();
    require(!isa<LValueType>(rvalueType),
            "l-value types are not legal in SIL");
    require(!isa<AnyFunctionType>(rvalueType),
            "AST function types are not legal in SIL");

    rvalueType.visit([&](Type t) {
      auto *A = dyn_cast<ArchetypeType>(t.getPointer());
      if (!A)
        return;
      require(isArchetypeValidInFunction(A, F),
              "Operand is of an ArchetypeType that does not exist in the "
              "Caller's generic param list.");
    });
  }

  /// Check that this operand appears in the use-chain of the value it uses.
  static bool isInValueUses(const Operand *operand) {
    for (auto use : operand->get()->getUses())
      if (use == operand)
        return true;
    return false;
  }

  /// \return True if all of the users of the AllocStack instruction \p ASI are
  /// inside the same basic block.
  static bool isSingleBlockUsage(AllocStackInst *ASI, DominanceInfo *Dominance){
    SILBasicBlock *BB = ASI->getParent();
    for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI)
      if (UI->getUser()->getParent() != BB &&
          Dominance->isReachableFromEntry(UI->getUser()->getParent()))
        return false;

    return true;
  }

  void checkAllocStackInst(AllocStackInst *AI) {
    require(AI->getContainerResult().getType().isLocalStorage(),
            "first result of alloc_stack must be local storage");
    require(AI->getAddressResult().getType().isAddress(),
            "second result of alloc_stack must be an address type");
    require(AI->getContainerResult().getType().getSwiftRValueType()
              == AI->getElementType().getSwiftRValueType(),
            "container storage must be for allocated type");

    // Scan the parent block of AI and check that the users of AI inside this
    // block are inside the lifetime of the allocated memory.
    SILBasicBlock *SBB = AI->getParent();
    bool Allocated = true;
    for (SILBasicBlock::iterator Inst = AI, E = SBB->end(); Inst != E; ++Inst) {
      if (LoadInst *LI = dyn_cast<LoadInst>(Inst))
        if (LI->getOperand().getDef() == AI)
          require(Allocated, "AllocStack used by Load outside its lifetime");

      if (StoreInst *SI = dyn_cast<StoreInst>(Inst))
        if (SI->getDest().getDef() == AI)
          require(Allocated, "AllocStack used by Store outside its lifetime");

      if (DeallocStackInst *DSI = dyn_cast<DeallocStackInst>(Inst))
        if (DSI->getOperand() == AI)
          Allocated = false;
    }

    // If the AllocStackInst is also deallocated inside the allocation block
    // then make sure that all the users are inside that block.
    if (!Allocated) {
      require(isSingleBlockUsage(AI, Dominance),
              "AllocStack has users in other basic blocks after allocation");
    }
  }

  void checkAllocRefInst(AllocRefInst *AI) {
    requireReferenceValue(AI, "Result of alloc_ref");
  }

  void checkAllocRefDynamicInst(AllocRefDynamicInst *ARDI) {
    requireReferenceValue(ARDI, "Result of alloc_ref_dynamic");
    require(ARDI->getOperand().getType().is<AnyMetatypeType>(),
            "operand of alloc_ref_dynamic must be of metatype type");
    auto metaTy = ARDI->getOperand().getType().castTo<AnyMetatypeType>();
    require(metaTy->hasRepresentation(),
            "operand of alloc_ref_dynamic must have a metatype representation");
    if (ARDI->isObjC()) {
      require(metaTy->getRepresentation() == MetatypeRepresentation::ObjC,
              "alloc_ref_dynamic @objc requires operand of ObjC metatype");
    } else {
      require(metaTy->getRepresentation() == MetatypeRepresentation::Thick,
              "alloc_ref_dynamic requires operand of thick metatype");
    }
  }

  /// Check the substitutions passed to an apply or partial_apply.
  CanSILFunctionType checkApplySubstitutions(ArrayRef<Substitution> subs,
                                             SILType calleeTy) {
    auto fnTy = requireObjectType(SILFunctionType, calleeTy, "callee operand");

    // If there are substitutions, verify them and apply them to the callee.
    if (subs.empty()) {
      require(!fnTy->isPolymorphic(),
              "callee of apply without substitutions must not be polymorphic");
      return fnTy;
    }
    require(fnTy->isPolymorphic(),
            "callee of apply with substitutions must be polymorphic");

    // Apply the substitutions.
    return fnTy->substGenericArgs(F.getModule(), M, subs);
  }

  void checkFullApplySite(FullApplySite site) {
    // If we have a substitution whose replacement type is an archetype, make
    // sure that the replacement archetype is in the context generic params of
    // the caller function.
    // For each substitution Sub in AI...
    for (auto &Sub : site.getSubstitutions()) {
      // If Sub's replacement is not an archetype type or is from an opened
      // existential type, skip it...
      auto *A = Sub.getReplacement()->getAs<ArchetypeType>();
      if (!A)
        continue;
      require(isArchetypeValidInFunction(A, site.getInstruction()->getFunction()),
              "Archetype to be substituted must be valid in function.");
    }

    // Then make sure that we have a type that can be substituted for the
    // callee.
    auto substTy = checkApplySubstitutions(site.getSubstitutions(),
                                           site.getCallee().getType());
    require(site.getOrigCalleeType()->getRepresentation() ==
            site.getSubstCalleeType()->getRepresentation(),
            "calling convention difference between types");

    require(!site.getSubstCalleeType()->isPolymorphic(),
            "substituted callee type should not be generic");

    requireSameType(SILType::getPrimitiveObjectType(substTy),
                    SILType::getPrimitiveObjectType(site.getSubstCalleeType()),
            "substituted callee type does not match substitutions");

    // Check that the arguments and result match.
    require(site.getArguments().size() ==
            substTy->getParameters().size(),
            "apply doesn't have right number of arguments for function");
    for (size_t i = 0, size = site.getArguments().size(); i < size; ++i) {
      requireSameType(site.getArguments()[i].getType(),
                      substTy->getParameters()[i].getSILType(),
                      "operand of 'apply' doesn't match function input type");
    }
  }

  void checkApplyInst(ApplyInst *AI) {
    checkFullApplySite(AI);

    auto substTy = AI->getSubstCalleeType();
    require(AI->getType() == substTy->getResult().getSILType(),
            "type of apply instruction doesn't match function result type");
    require(!substTy->hasErrorResult(),
            "apply instruction cannot call function with error result");

    // Check that if the apply is of a noreturn callee, make sure that an
    // unreachable is the next instruction.
    if (AI->getModule().getStage() == SILStage::Raw ||
        !AI->getCallee().getType().getAs<SILFunctionType>()->isNoReturn())
      return;
    require(isa<UnreachableInst>(std::next(SILBasicBlock::iterator(AI))),
            "No return apply without an unreachable as a next instruction.");
  }

  void checkTryApplyInst(TryApplyInst *AI) {
    checkFullApplySite(AI);

    auto substTy = AI->getSubstCalleeType();

    auto normalBB = AI->getNormalBB();
    require(normalBB->bbarg_size() == 1,
            "normal destination of try_apply must take one argument");
    requireSameType((*normalBB->bbarg_begin())->getType(),
                    substTy->getResult().getSILType(),
                    "normal destination of try_apply must take argument "
                    "of normal result type");

    auto errorBB = AI->getErrorBB();
    require(substTy->hasErrorResult(),
            "try_apply must call function with error result");
    require(errorBB->bbarg_size() == 1,
            "error destination of try_apply must take one argument");
    requireSameType((*errorBB->bbarg_begin())->getType(),
                    substTy->getErrorResult().getSILType(),
                    "error destination of try_apply must take argument "
                    "of error result type");
  }

  void verifyLLVMIntrinsic(BuiltinInst *BI, llvm::Intrinsic::ID ID) {
    // Certain llvm instrinsic require constant values as their operands.
    // Consequently, these must not be phi nodes (aka. basic block arguments).
    switch (ID) {
    default:
      break;
    case llvm::Intrinsic::ctlz: // llvm.ctlz
    case llvm::Intrinsic::cttz: // llvm.cttz
      require(!isa<SILArgument>(BI->getArguments()[1]),
              "is_zero_undef argument of bit counting intrinsics must be an "
              "integer literal");
      break;
    case llvm::Intrinsic::memcpy:
    case llvm::Intrinsic::memmove:
    case llvm::Intrinsic::memset:
      require(!isa<SILArgument>(BI->getArguments()[3]),
              "alignment argument of memory intrinsics must be an integer "
              "literal");
      require(!isa<SILArgument>(BI->getArguments()[4]),
              "isvolatile argument of memory intrinsics must be an integer "
              "literal");
      break;
    case llvm::Intrinsic::lifetime_start:
    case llvm::Intrinsic::lifetime_end:
    case llvm::Intrinsic::invariant_start:
      require(!isa<SILArgument>(BI->getArguments()[0]),
              "size argument of memory use markers must be an integer literal");
      break;
    case llvm::Intrinsic::invariant_end:
      require(!isa<SILArgument>(BI->getArguments()[1]),
              "llvm.invariant.end parameter #2 must be an integer literal");
      break;
    }
  }

  void checkPartialApplyInst(PartialApplyInst *PAI) {
    auto resultInfo = requireObjectType(SILFunctionType, PAI,
                                        "result of partial_apply");
    verifySILFunctionType(resultInfo);
    require(resultInfo->getExtInfo().hasContext(),
            "result of closure cannot have a thin function type");

    // If we have a substitution whose replacement type is an archetype, make
    // sure that the replacement archetype is in the context generic params of
    // the caller function.
    // For each substitution Sub in AI...
    for (auto &Sub : PAI->getSubstitutions()) {
      // If Sub's replacement is not an archetype type or is from an opened
      // existential type, skip it...
      Sub.getReplacement().visit([&](Type t) {
        auto *A = t->getAs<ArchetypeType>();
        if (!A)
          return;
        require(isArchetypeValidInFunction(A, PAI->getFunction()),
                "Archetype to be substituted must be valid in function.");
      });
    }

    auto substTy = checkApplySubstitutions(PAI->getSubstitutions(),
                                        PAI->getCallee().getType());

    require(!PAI->getSubstCalleeType()->isPolymorphic(),
            "substituted callee type should not be generic");

    requireSameType(SILType::getPrimitiveObjectType(substTy),
                    SILType::getPrimitiveObjectType(PAI->getSubstCalleeType()),
            "substituted callee type does not match substitutions");

    // The arguments must match the suffix of the original function's input
    // types.
    require(PAI->getArguments().size() +
              resultInfo->getParameters().size()
              == substTy->getParameters().size(),
            "result of partial_apply should take as many inputs as were not "
            "applied by the instruction");

    unsigned offset =
      substTy->getParameters().size() - PAI->getArguments().size();

    for (unsigned i = 0, size = PAI->getArguments().size(); i < size; ++i) {
      require(PAI->getArguments()[i].getType()
                == substTy->getParameters()[i + offset].getSILType(),
              "applied argument types do not match suffix of function type's "
              "inputs");
    }

    // The arguments to the result function type must match the prefix of the
    // original function's input types.
    for (unsigned i = 0, size = resultInfo->getParameters().size();
         i < size; ++i) {
      require(resultInfo->getParameters()[i] ==
              substTy->getParameters()[i],
              "inputs to result function type do not match unapplied inputs "
              "of original function");
    }
    
    // The "returns inner pointer" convention doesn't survive through a partial
    // application, since the thunk takes responsibility for lifetime-extending
    // 'self'.
    auto expectedResult = substTy->getResult();
    if (expectedResult.getConvention() == ResultConvention::UnownedInnerPointer)
    {
      expectedResult = SILResultInfo(expectedResult.getType(),
                                     ResultConvention::Unowned);
      require(resultInfo->getResult() == expectedResult,
              "result type of result function type for partially applied "
              "@unowned_inner_pointer function should have @unowned convention");
    } else {
      require(resultInfo->getResult() == expectedResult,
              "result type of result function type does not match original "
              "function");
    }
  }

  void checkBuiltinInst(BuiltinInst *BI) {
    // Check for special constraints on llvm intrinsics.
    if (BI->getIntrinsicInfo().ID != llvm::Intrinsic::not_intrinsic)
      verifyLLVMIntrinsic(BI, BI->getIntrinsicInfo().ID);
  }
  
  bool isValidLinkageForFragileRef(SILLinkage linkage) {
    switch (linkage) {
    case SILLinkage::Private:
    case SILLinkage::PrivateExternal:
    case SILLinkage::Hidden:
    case SILLinkage::HiddenExternal:
      return false;

    case SILLinkage::Shared:
    case SILLinkage::SharedExternal:
        // This handles some kind of generated functions, like constructors
        // of clang imported types.
        // TODO: check why those functions are not fragile anyway and make
        // a less conservative check here.
        return true;

    case SILLinkage::Public:
    case SILLinkage::PublicExternal:
      return true;
    }
  }

  void checkFunctionRefInst(FunctionRefInst *FRI) {
    auto fnType = requireObjectType(SILFunctionType, FRI,
                                    "result of function_ref");
    require(!fnType->getExtInfo().hasContext(),
            "function_ref should have a context-free function result");
    if (F.isFragile()) {
      SILFunction *RefF = FRI->getReferencedFunction();
      require(RefF->isFragile()
                || isValidLinkageForFragileRef(RefF->getLinkage())
                || RefF->isExternalDeclaration(),
              "function_ref inside fragile function cannot "
              "reference a private or hidden symbol");
    }
    verifySILFunctionType(fnType);
  }

  void checkGlobalAddrInst(GlobalAddrInst *GAI) {
    require(GAI->getType().isAddress(),
            "GlobalAddr must have an address result type");
    require(GAI->getType().getObjectType() ==
              GAI->getReferencedGlobal()->getLoweredType(),
            "GlobalAddr must be the address type of the variable it "
            "references");
    if (F.isFragile()) {
      SILGlobalVariable *RefG = GAI->getReferencedGlobal();
      require(RefG->isFragile()
                || isValidLinkageForFragileRef(RefG->getLinkage()),
              "function_ref inside fragile function cannot "
              "reference a private or hidden symbol");
    }
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
  void checkLoadWeakInst(LoadWeakInst *LWI) {
    require(LWI->getType().isObject(), "Result of load must be an object");
    require(LWI->getType().getSwiftType()->getAnyOptionalObjectType(),
            "Result of weak load must be an optional");
    auto PointerType = LWI->getOperand().getType();
    auto PointerRVType = PointerType.getSwiftRValueType();
    require(PointerType.isAddress() &&
            PointerRVType->is<WeakStorageType>(),
            "load_weak operand must be an weak address");
    require(PointerRVType->getReferenceStorageReferent()->getCanonicalType() ==
            LWI->getType().getSwiftType(),
            "Load operand type and result type mismatch");
  }

  void checkStoreWeakInst(StoreWeakInst *SWI) {
    require(SWI->getSrc().getType().isObject(),
            "Can't store from an address source");
    require(SWI->getSrc().getType().getSwiftType()->getAnyOptionalObjectType(),
            "store_weak must be of an optional value");
    auto PointerType = SWI->getDest().getType();
    auto PointerRVType = PointerType.getSwiftRValueType();
    require(PointerType.isAddress() &&
            PointerRVType->is<WeakStorageType>(),
            "store_weak address operand must be an weak address");
    require(PointerRVType->getReferenceStorageReferent()->getCanonicalType() ==
            SWI->getSrc().getType().getSwiftType(),
            "Store operand type and dest type mismatch");
  }

  void checkMarkUninitializedInst(MarkUninitializedInst *MU) {
    SILValue Src = MU->getOperand();
    require(MU->getModule().getStage() == SILStage::Raw,
            "mark_uninitialized instruction can only exist in raw SIL");
    require(Src.getType().isAddress() ||
            Src.getType().getSwiftRValueType()->getClassOrBoundGenericClass(),
            "mark_uninitialized must be an address or class");
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

  void checkRetainValueInst(RetainValueInst *I) {
    require(I->getOperand().getType().isObject(),
            "Source value should be an object value");
  }

  void checkReleaseValueInst(ReleaseValueInst *I) {
    require(I->getOperand().getType().isObject(),
            "Source value should be an object value");
  }
  
  void checkAutoreleaseValueInst(AutoreleaseValueInst *I) {
    require(I->getOperand().getType().isObject(),
            "Source value should be an object value");
    // TODO: This instruction could in principle be generalized.
    require(I->getOperand().getType().hasRetainablePointerRepresentation(),
            "Source value must be a reference type or optional thereof");
  }
  
  void checkCopyBlockInst(CopyBlockInst *I) {
    require(I->getOperand().getType().isBlockPointerCompatible(),
            "operand of copy_block should be a block");
    require(I->getOperand().getType() == I->getType(),
            "result of copy_block should be same type as operand");
  }

  void checkAllocValueBufferInst(AllocValueBufferInst *I) {
    require(I->getOperand().getType().isAddress(),
            "Operand value should be an address");
    require(I->getOperand().getType().is<BuiltinUnsafeValueBufferType>(),
            "Operand value should be a Builtin.UnsafeValueBuffer");
  }

  void checkProjectValueBufferInst(ProjectValueBufferInst *I) {
    require(I->getOperand().getType().isAddress(),
            "Operand value should be an address");
    require(I->getOperand().getType().is<BuiltinUnsafeValueBufferType>(),
            "Operand value should be a Builtin.UnsafeValueBuffer");
  }

  void checkDeallocValueBufferInst(DeallocValueBufferInst *I) {
    require(I->getOperand().getType().isAddress(),
            "Operand value should be an address");
    require(I->getOperand().getType().is<BuiltinUnsafeValueBufferType>(),
            "Operand value should be a Builtin.UnsafeValueBuffer");
  }
  
  void checkStructInst(StructInst *SI) {
    auto *structDecl = SI->getType().getStructOrBoundGenericStruct();
    require(structDecl, "StructInst must return a struct");
    require(!structDecl->hasUnreferenceableStorage(),
            "Cannot build a struct with unreferenceable storage from elements "
            "using StructInst");
    require(SI->getType().isObject(),
            "StructInst must produce an object");

    SILType structTy = SI->getType();
    auto opi = SI->getElements().begin(), opEnd = SI->getElements().end();
    for (VarDecl *field : structDecl->getStoredProperties()) {
      require(opi != opEnd,
              "number of struct operands does not match number of stored "
              "member variables of struct");

      SILType loweredType = structTy.getFieldType(field, F.getModule());
      require((*opi).getType() == loweredType,
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
      SILType caseTy = UI->getType().getEnumElementType(UI->getElement(),
                                                        F.getModule());
      require(caseTy == UI->getOperand().getType(),
              "EnumInst operand type does not match type of case");
    }
  }

  void checkInitEnumDataAddrInst(InitEnumDataAddrInst *UI) {
    EnumDecl *ud = UI->getOperand().getType().getEnumOrBoundGenericEnum();
    require(ud, "InitEnumDataAddrInst must take an enum operand");
    require(UI->getElement()->getParentEnum() == ud,
            "InitEnumDataAddrInst case must be a case of the enum operand type");
    require(UI->getElement()->hasArgumentType(),
            "InitEnumDataAddrInst case must have a data type");
    require(UI->getOperand().getType().isAddress(),
            "InitEnumDataAddrInst must take an address operand");
    require(UI->getType().isAddress(),
            "InitEnumDataAddrInst must produce an address");

    SILType caseTy =
      UI->getOperand().getType().getEnumElementType(UI->getElement(),
                                                    F.getModule());
    requireSameType(caseTy, UI->getType(),
            "InitEnumDataAddrInst result does not match type of enum case");
  }

  void checkUncheckedEnumDataInst(UncheckedEnumDataInst *UI) {
    EnumDecl *ud = UI->getOperand().getType().getEnumOrBoundGenericEnum();
    require(ud, "UncheckedEnumData must take an enum operand");
    require(UI->getElement()->getParentEnum() == ud,
            "UncheckedEnumData case must be a case of the enum operand type");
    require(UI->getElement()->hasArgumentType(),
            "UncheckedEnumData case must have a data type");
    require(UI->getOperand().getType().isObject(),
            "UncheckedEnumData must take an address operand");
    require(UI->getType().isObject(),
            "UncheckedEnumData must produce an address");

    SILType caseTy =
      UI->getOperand().getType().getEnumElementType(UI->getElement(),
                                                    F.getModule());
    require(caseTy == UI->getType(),
            "UncheckedEnumData result does not match type of enum case");
  }

  void checkUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *UI) {
    EnumDecl *ud = UI->getOperand().getType().getEnumOrBoundGenericEnum();
    require(ud, "UncheckedTakeEnumDataAddrInst must take an enum operand");
    require(UI->getElement()->getParentEnum() == ud,
            "UncheckedTakeEnumDataAddrInst case must be a case of the enum operand type");
    require(UI->getElement()->hasArgumentType(),
            "UncheckedTakeEnumDataAddrInst case must have a data type");
    require(UI->getOperand().getType().isAddress(),
            "UncheckedTakeEnumDataAddrInst must take an address operand");
    require(UI->getType().isAddress(),
            "UncheckedTakeEnumDataAddrInst must produce an address");

    SILType caseTy =
      UI->getOperand().getType().getEnumElementType(UI->getElement(),
                                                    F.getModule());
    require(caseTy == UI->getType(),
            "UncheckedTakeEnumDataAddrInst result does not match type of enum case");
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

    require(TI->getElements().size() == ResTy->getNumElements(),
            "Tuple field count mismatch!");

    for (size_t i = 0, size = TI->getElements().size(); i < size; ++i) {
      require(TI->getElement(i).getType().getSwiftType()
               ->isEqual(ResTy.getElementType(i)),
              "Tuple element arguments do not match tuple type!");
    }
  }

  // Is a SIL type a potential lowering of a formal type?
  static bool isLoweringOf(SILType loweredType,
                           CanType formalType) {
    // Metatypes preserve their instance type through lowering.
    if (auto loweredMT = loweredType.getAs<MetatypeType>()) {
      if (auto formalMT = dyn_cast<MetatypeType>(formalType)) {
        return loweredMT.getInstanceType() == formalMT.getInstanceType();
      }
    }
    if (auto loweredEMT = loweredType.getAs<ExistentialMetatypeType>()) {
      if (auto formalEMT = dyn_cast<ExistentialMetatypeType>(formalType)) {
        return loweredEMT.getInstanceType() == formalEMT.getInstanceType();
      }
    }
    
    // TODO: Function types go through a more elaborate lowering.
    // For now, just check that a SIL function type came from some AST function
    // type.
    if (loweredType.is<SILFunctionType>())
      return isa<AnyFunctionType>(formalType);
    
    // Tuples are lowered elementwise.
    // TODO: Will this always be the case?
    if (auto loweredTT = loweredType.getAs<TupleType>())
      if (auto formalTT = dyn_cast<TupleType>(formalType)) {
        if (loweredTT->getNumElements() != formalTT->getNumElements())
          return false;
        for (unsigned i = 0, e = loweredTT->getNumElements(); i < e; ++i) {
          if (!isLoweringOf(SILType::getPrimitiveAddressType(
                                                   loweredTT.getElementType(i)),
                            formalTT.getElementType(i)))
            return false;
        }
        return true;
      }
    
    // Other types are preserved through lowering.
    return loweredType.getSwiftRValueType() == formalType;
  }
  
  void checkMetatypeInst(MetatypeInst *MI) {
    require(MI->getType(0).is<MetatypeType>(),
            "metatype instruction must be of metatype type");
    require(MI->getType(0).castTo<MetatypeType>()->hasRepresentation(),
            "metatype instruction must have a metatype representation");
  }
  void checkValueMetatypeInst(ValueMetatypeInst *MI) {
    require(MI->getType().is<MetatypeType>(),
            "value_metatype instruction must be of metatype type");
    require(MI->getType().castTo<MetatypeType>()->hasRepresentation(),
            "value_metatype instruction must have a metatype representation");
    auto formalInstanceTy
      = MI->getType().castTo<MetatypeType>().getInstanceType();
    require(isLoweringOf(MI->getOperand().getType(), formalInstanceTy),
            "value_metatype result must be formal metatype of "
            "lowered operand type");
  }
  void checkExistentialMetatypeInst(ExistentialMetatypeInst *MI) {
    require(MI->getType().is<ExistentialMetatypeType>(),
            "existential_metatype instruction must be of metatype type");
    require(MI->getType().castTo<ExistentialMetatypeType>()->hasRepresentation(),
            "value_metatype instruction must have a metatype representation");
    require(MI->getOperand().getType().isAnyExistentialType(),
            "existential_metatype operand must be of protocol type");
    auto formalInstanceTy
      = MI->getType().castTo<ExistentialMetatypeType>().getInstanceType();
    require(isLoweringOf(MI->getOperand().getType(), formalInstanceTy),
            "existential_metatype result must be formal metatype of "
            "lowered operand type");
  }

  void checkStrongRetainInst(StrongRetainInst *RI) {
    requireReferenceValue(RI->getOperand(), "Operand of strong_retain");
  }
  void checkStrongRetainAutoreleasedInst(StrongRetainAutoreleasedInst *RI) {
    require(RI->getOperand().getType().isObject(),
            "Operand of strong_retain_autoreleased must be an object");
    require(RI->getOperand().getType().hasRetainablePointerRepresentation(),
            "Operand of strong_retain_autoreleased must be a retainable pointer");
    require(isa<ApplyInst>(RI->getOperand()) ||
            isa<SILArgument>(RI->getOperand()),
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
    requireObjectType(BuiltinNativeObjectType, DI->getOperand(),
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
            "invalid field index for tuple_extract instruction");
    require(EI->getType().getSwiftRValueType()
            == operandTy.getElementType(EI->getFieldNo()),
            "type of tuple_extract does not match type of element");
  }

  void checkStructExtractInst(StructExtractInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    require(operandTy.isObject(),
            "cannot struct_extract from address");
    require(EI->getType().isObject(),
            "result of struct_extract cannot be address");
    StructDecl *sd = operandTy.getStructOrBoundGenericStruct();
    require(sd, "must struct_extract from struct");
    require(!EI->getField()->isStatic(),
            "cannot get address of static property with struct_element_addr");
    require(EI->getField()->hasStorage(),
            "cannot load computed property with struct_extract");

    require(EI->getField()->getDeclContext() == sd,
            "struct_extract field is not a member of the struct");

    SILType loweredFieldTy = operandTy.getFieldType(EI->getField(),
                                                    F.getModule());
    require(loweredFieldTy == EI->getType(),
            "result of struct_extract does not match type of field");
  }

  void checkTupleElementAddrInst(TupleElementAddrInst *EI) {
    SILType operandTy = EI->getOperand().getType();
    require(operandTy.isAddress(),
            "must derive element_addr from address");
    require(EI->getType(0).isAddress(),
            "result of tuple_element_addr must be address");
    require(operandTy.is<TupleType>(),
            "must derive tuple_element_addr from tuple");

    ArrayRef<TupleTypeElt> fields = operandTy.castTo<TupleType>()->getElements();
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
    require(!EI->getField()->isStatic(),
            "cannot get address of static property with struct_element_addr");
    require(EI->getField()->hasStorage(),
            "cannot get address of computed property with struct_element_addr");

    require(EI->getField()->getDeclContext() == sd,
            "struct_element_addr field is not a member of the struct");

    SILType loweredFieldTy = operandTy.getFieldType(EI->getField(),
                                                    F.getModule());
    require(loweredFieldTy == EI->getType(),
            "result of struct_element_addr does not match type of field");
  }

  void checkRefElementAddrInst(RefElementAddrInst *EI) {
    requireReferenceValue(EI->getOperand(), "Operand of ref_element_addr");
    require(EI->getType(0).isAddress(),
            "result of ref_element_addr must be lvalue");
    require(!EI->getField()->isStatic(),
            "cannot get address of static property with struct_element_addr");
    require(EI->getField()->hasStorage(),
            "cannot get address of computed property with ref_element_addr");
    SILType operandTy = EI->getOperand().getType();
    ClassDecl *cd = operandTy.getClassOrBoundGenericClass();
    require(cd, "ref_element_addr operand must be a class instance");

    require(EI->getField()->getDeclContext() == cd,
            "ref_element_addr field must be a member of the class");

    SILType loweredFieldTy = operandTy.getFieldType(EI->getField(),
                                                    F.getModule());
    require(loweredFieldTy == EI->getType(),
            "result of ref_element_addr does not match type of field");
    EI->getFieldNo();  // Make sure we can access the field without crashing.
  }

  SILType getMethodSelfType(CanSILFunctionType ft) {
    return ft->getParameters().back().getSILType();
  }
  CanType getMethodSelfInstanceType(CanSILFunctionType ft) {
    auto selfTy = getMethodSelfType(ft);
    if (auto metaTy = selfTy.getAs<AnyMetatypeType>())
      return metaTy.getInstanceType();
    return selfTy.getSwiftRValueType();
  }

  void checkWitnessMethodInst(WitnessMethodInst *AMI) {
    auto methodType = requireObjectType(SILFunctionType, AMI,
                                        "result of witness_method");

    auto *protocol
      = dyn_cast<ProtocolDecl>(AMI->getMember().getDecl()->getDeclContext());
    require(protocol,
            "witness_method method must be a protocol method");

    require(methodType->getRepresentation()
              == F.getModule().Types.getProtocolWitnessRepresentation(protocol),
            "result of witness_method must have correct representation for protocol");

    require(methodType->isPolymorphic(),
            "result of witness_method must be polymorphic");

    auto selfGenericParam
      = methodType->getGenericSignature()->getGenericParams()[0];
    require(selfGenericParam->getDepth() == 0
            && selfGenericParam->getIndex() == 0,
            "method should be polymorphic on Self parameter at depth 0 index 0");
    auto selfMarker
      = methodType->getGenericSignature()->getRequirements()[0];
    require(selfMarker.getKind() == RequirementKind::WitnessMarker
            && selfMarker.getFirstType()->isEqual(selfGenericParam),
            "method's Self parameter should appear first in requirements");
    auto selfRequirement
      = methodType->getGenericSignature()->getRequirements()[1];
    require(selfRequirement.getKind() == RequirementKind::Conformance
            && selfRequirement.getFirstType()->isEqual(selfGenericParam)
            && selfRequirement.getSecondType()->getAs<ProtocolType>()
              ->getDecl() == protocol,
            "method's Self parameter should be constrained by protocol");

    auto lookupType = AMI->getLookupType();
    if (isOpenedArchetype(lookupType))
      require(AMI->hasOperand(), "Must have an opened existential operand");
    if (isa<ArchetypeType>(lookupType) || lookupType->isAnyExistentialType()) {
      require(AMI->getConformance() == nullptr,
              "archetype or existential lookup should have null conformance");
    } else {
      require(AMI->getConformance(),
              "concrete type lookup requires conformance");
      require(AMI->getConformance()->getType()
                ->isEqual(AMI->getLookupType()),
              "concrete type lookup requires conformance that matches type");
      require(AMI->getModule().lookUpWitnessTable(AMI->getConformance(),
                                                  false).first,
              "Could not find witness table for conformance.");
    }
  }

  bool isSelfArchetype(CanType t, ArrayRef<ProtocolDecl*> protocols) {
    ArchetypeType *archetype = dyn_cast<ArchetypeType>(t);
    if (!archetype)
      return false;

    auto selfProto = archetype->getSelfProtocol();
    if (!selfProto)
      return false;

    for (auto checkProto : protocols) {
      if (checkProto == selfProto || checkProto->inheritsFrom(selfProto))
        return true;
    }

    return false;
  }

  bool isOpenedArchetype(CanType t) {
    ArchetypeType *archetype = dyn_cast<ArchetypeType>(t);
    if (!archetype)
      return false;

    return !archetype->getOpenedExistentialType().isNull();
  }

  // Get the expected type of a dynamic method reference.
  SILType getDynamicMethodType(SILType selfType, SILDeclRef method) {
    auto &C = F.getASTContext();
    
    // The type of the dynamic method must match the usual type of the method,
    // but with the more opaque Self type.
    auto methodTy = F.getModule().Types.getConstantType(method)
      .castTo<SILFunctionType>();
    
    auto params = methodTy->getParameters();
    SmallVector<SILParameterInfo, 4>
      dynParams(params.begin(), params.end() - 1);
    dynParams.push_back(SILParameterInfo(selfType.getSwiftRValueType(),
                                         params.back().getConvention()));
    
    auto dynResult = methodTy->getResult();

    // If the method returns Self, substitute AnyObject for the result type.
    if (auto fnDecl = dyn_cast<FuncDecl>(method.getDecl())) {
      if (fnDecl->hasDynamicSelf()) {
        auto anyObjectTy = C.getProtocol(KnownProtocolKind::AnyObject)
                             ->getDeclaredType();
        auto newResultTy
          = dynResult.getType()->replaceCovariantResultType(anyObjectTy, 0);
        dynResult = SILResultInfo(newResultTy->getCanonicalType(),
                                  dynResult.getConvention());
      }
    }

    auto fnTy = SILFunctionType::get(nullptr,
                                     methodTy->getExtInfo(),
                                     methodTy->getCalleeConvention(),
                                     dynParams,
                                     dynResult,
                                     methodTy->getOptionalErrorResult(),
                                     F.getASTContext());
    return SILType::getPrimitiveObjectType(fnTy);
  }
  
  void checkDynamicMethodInst(DynamicMethodInst *EMI) {
    requireObjectType(SILFunctionType, EMI, "result of dynamic_method");
    SILType operandType = EMI->getOperand().getType();

    require(EMI->getMember().getDecl()->isObjC(), "method must be @objc");
    if (!EMI->getMember().getDecl()->isInstanceMember()) {
      require(operandType.getSwiftType()->is<MetatypeType>(),
              "operand must have metatype type");
      require(operandType.getSwiftType()->castTo<MetatypeType>()
                ->getInstanceType()->mayHaveSuperclass(),
              "operand must have metatype of class or class-bound type");
    }
    
    requireSameType(EMI->getType(),
                    getDynamicMethodType(operandType, EMI->getMember()),
                    "result must be of the method's type");
  }

  void checkClassMethodInst(ClassMethodInst *CMI) {
    require(CMI->getType() == TC.getConstantType(CMI->getMember()),
            "result type of class_method must match type of method");
    auto methodType = requireObjectType(SILFunctionType, CMI,
                                        "result of class_method");
    require(!methodType->getExtInfo().hasContext(),
            "result method must be of a context-free function type");
    SILType operandType = CMI->getOperand().getType();
    require(operandType.isClassOrClassMetatype(),
            "operand must be of a class type");
    require(getMethodSelfType(methodType).isClassOrClassMetatype(),
            "result must be a method of a class");
    
    require(CMI->getMember().isForeign
            || !CMI->getMember().getDecl()->hasClangNode(),
            "foreign method cannot be dispatched natively");

    require(CMI->getMember().isForeign
            || !isa<ExtensionDecl>(CMI->getMember().getDecl()->getDeclContext()),
            "extension method cannot be dispatched natively");
    
    /* TODO: We should enforce that ObjC methods are dispatched on ObjC
       metatypes, but IRGen appears not to care right now.
    if (auto metaTy = operandType.getAs<AnyMetatypeType>()) {
      bool objcMetatype
        = metaTy->getRepresentation() == MetatypeRepresentation::ObjC;
      bool objcMethod = CMI->getMember().isForeign;
      require(objcMetatype == objcMethod,
              "objc class methods must be invoked on objc metatypes");
    }
     */
  }

  void checkSuperMethodInst(SuperMethodInst *CMI) {
    require(CMI->getType() == TC.getConstantType(CMI->getMember()),
            "result type of super_method must match type of method");
    auto methodType = requireObjectType(SILFunctionType, CMI,
                                        "result of super_method");
    require(!methodType->getExtInfo().hasContext(),
            "result method must be of a context-free function type");
    SILType operandType = CMI->getOperand().getType();
    require(operandType.isClassOrClassMetatype(),
            "operand must be of a class type");
    require(getMethodSelfType(methodType).isClassOrClassMetatype(),
            "result must be a method of a class");

    Type methodClass;
    auto decl = CMI->getMember().getDecl();
    if (auto classDecl = dyn_cast<ClassDecl>(decl))
      methodClass = classDecl->getDeclaredTypeInContext();
    else
      methodClass = decl->getDeclContext()->getDeclaredTypeInContext();

    require(methodClass->getClassOrBoundGenericClass(),
            "super_method must look up a class method");
    require(!methodClass->isEqual(operandType.getSwiftType()),
            "super_method operand should be a subtype of the "
            "lookup class type");
  }

  void checkOpenExistentialAddrInst(OpenExistentialAddrInst *OEI) {
    SILType operandType = OEI->getOperand().getType();
    require(operandType.isAddress(),
            "open_existential_addr must be applied to address");
    require(operandType.canUseExistentialRepresentation(F.getModule(),
                                        ExistentialRepresentation::Opaque),
           "open_existential_addr must be applied to opaque existential");

    require(OEI->getType().isAddress(),
            "open_existential_addr result must be an address");

    require(isOpenedArchetype(OEI->getType().getSwiftRValueType()),
        "open_existential_addr result must be an opened existential archetype");
  }

  void checkOpenExistentialRefInst(OpenExistentialRefInst *OEI) {
    SILType operandType = OEI->getOperand().getType();
    require(operandType.isObject(),
            "open_existential_ref operand must not be address");

    require(operandType.canUseExistentialRepresentation(F.getModule(),
                                              ExistentialRepresentation::Class),
            "open_existential_ref operand must be class existential");

    CanType resultInstanceTy = OEI->getType().getSwiftRValueType();

    require(OEI->getType().isObject(),
            "open_existential_ref result must be an address");

    require(isOpenedArchetype(resultInstanceTy),
            "open_existential_ref result must be an opened existential");
  }

  void checkOpenExistentialBoxInst(OpenExistentialBoxInst *OEI) {
    SILType operandType = OEI->getOperand().getType();
    require(operandType.isObject(),
            "open_existential_box operand must not be address");

    require(operandType.canUseExistentialRepresentation(F.getModule(),
                                              ExistentialRepresentation::Boxed),
            "open_existential_box operand must be boxed existential");

    CanType resultInstanceTy = OEI->getType().getSwiftRValueType();

    require(OEI->getType().isAddress(),
            "open_existential_box result must be an address");

    require(isOpenedArchetype(resultInstanceTy),
            "open_existential_box result must be an opened existential");
  }

  void checkOpenExistentialMetatypeInst(OpenExistentialMetatypeInst *I) {
    SILType operandType = I->getOperand().getType();
    require(operandType.isObject(),
            "open_existential_metatype operand must not be address");
    require(operandType.is<ExistentialMetatypeType>(),
            "open_existential_metatype operand must be existential metatype");
    require(operandType.castTo<ExistentialMetatypeType>()->hasRepresentation(),
            "open_existential_metatype operand must have a representation");

    SILType resultType = I->getType();
    require(resultType.isObject(),
            "open_existential_metatype result must not be address");
    require(resultType.is<MetatypeType>(),
            "open_existential_metatype result must be metatype");
    require(resultType.castTo<MetatypeType>()->hasRepresentation(),
            "open_existential_metatype result must have a representation");
    require(operandType.castTo<ExistentialMetatypeType>()->getRepresentation()
              == resultType.castTo<MetatypeType>()->getRepresentation(),
            "open_existential_metatype result must match representation of "
            "operand");

    CanType operandInstTy =
      operandType.castTo<ExistentialMetatypeType>().getInstanceType();
    CanType resultInstTy = 
      resultType.castTo<MetatypeType>().getInstanceType();

    while (auto operandMetatype =
             dyn_cast<ExistentialMetatypeType>(operandInstTy)) {
      require(isa<MetatypeType>(resultInstTy),
              "metatype depth mismatch in open_existential_metatype result");
      operandInstTy = operandMetatype.getInstanceType();
      resultInstTy = cast<MetatypeType>(resultInstTy).getInstanceType();
    }

    require(operandInstTy.isExistentialType(),
            "ill-formed existential metatype in open_existential_metatype "
            "operand");
    require(isOpenedArchetype(resultInstTy),
            "open_existential_metatype result must be an opened existential "
            "metatype");
  }
  
  void checkAllocExistentialBoxInst(AllocExistentialBoxInst *AEBI) {
    SILType exType = AEBI->getExistentialType();
    require(exType.isObject(),
            "alloc_existential_box #0 result should be a value");
    require(exType.canUseExistentialRepresentation(F.getModule(),
                                             ExistentialRepresentation::Boxed,
                                             AEBI->getFormalConcreteType()),
            "alloc_existential_box must be used with a boxed existential "
            "type");
    
    // The lowered type must be the properly-abstracted form of the AST type.
    auto archetype = ArchetypeType::getOpened(exType.getSwiftRValueType());
    
    auto loweredTy = F.getModule().Types.getLoweredType(
                                Lowering::AbstractionPattern(archetype),
                                AEBI->getFormalConcreteType())
                      .getAddressType();
    
    requireSameType(loweredTy, AEBI->getLoweredConcreteType(),
                    "alloc_existential_box #1 result should be the lowered "
                    "concrete type at the right abstraction level");
    require(isLoweringOf(AEBI->getLoweredConcreteType(),
                         AEBI->getFormalConcreteType()),
        "alloc_existential_box payload must be a lowering of the formal "
        "concrete type");

    for (ProtocolConformance *C : AEBI->getConformances())
      // We allow for null conformances.
      require(!C || AEBI->getModule().lookUpWitnessTable(C, false).first,
              "Could not find witness table for conformance.");
  }

  void checkInitExistentialAddrInst(InitExistentialAddrInst *AEI) {
    SILType exType = AEI->getOperand().getType();
    require(exType.isAddress(),
            "init_existential_addr must be applied to an address");
    require(exType.canUseExistentialRepresentation(F.getModule(),
                                       ExistentialRepresentation::Opaque,
                                       AEI->getFormalConcreteType()),
            "init_existential_addr must be used with an opaque "
            "existential type");
    
    // The lowered type must be the properly-abstracted form of the AST type.
    auto archetype = ArchetypeType::getOpened(exType.getSwiftRValueType());
    
    auto loweredTy = F.getModule().Types.getLoweredType(
                                Lowering::AbstractionPattern(archetype),
                                AEI->getFormalConcreteType())
                      .getAddressType();
    
    requireSameType(loweredTy, AEI->getLoweredConcreteType(),
                    "init_existential_addr result type must be the lowered "
                    "concrete type at the right abstraction level");

    require(isLoweringOf(AEI->getLoweredConcreteType(),
                         AEI->getFormalConcreteType()),
            "init_existential_addr payload must be a lowering of the formal "
            "concrete type");
    
    for (ProtocolConformance *C : AEI->getConformances())
      // We allow for null conformances.
      require(!C || AEI->getModule().lookUpWitnessTable(C, false).first,
              "Could not find witness table for conformance.");
  }

  void checkInitExistentialRefInst(InitExistentialRefInst *IEI) {
    SILType concreteType = IEI->getOperand().getType();
    require(concreteType.getSwiftType()->isBridgeableObjectType(),
            "init_existential_ref operand must be a class instance");
    require(IEI->getType().canUseExistentialRepresentation(F.getModule(),
                                     ExistentialRepresentation::Class,
                                     IEI->getFormalConcreteType()),
            "init_existential_ref must be used with a class existential type");
    require(IEI->getType().isObject(),
            "init_existential_ref result must not be an address");
    
    // The operand must be at the right abstraction level for the existential.
    auto archetype = ArchetypeType::getOpened(
                                          IEI->getType().getSwiftRValueType());
    auto loweredTy = F.getModule().Types.getLoweredType(
                                       Lowering::AbstractionPattern(archetype),
                                       IEI->getFormalConcreteType());
    requireSameType(concreteType, loweredTy,
                    "init_existential_ref operand must be lowered to the right "
                    "abstraction level for the existential");
    
    require(isLoweringOf(IEI->getOperand().getType(),
                         IEI->getFormalConcreteType()),
            "init_existential_ref operand must be a lowering of the formal "
            "concrete type");
    
    for (ProtocolConformance *C : IEI->getConformances())
      // We allow for null conformances.
      require(!C || IEI->getModule().lookUpWitnessTable(C, false).first,
              "Could not find witness table for conformance.");
  }

  void checkDeinitExistentialAddrInst(DeinitExistentialAddrInst *DEI) {
    SILType exType = DEI->getOperand().getType();
    require(exType.isAddress(),
            "deinit_existential_addr must be applied to an address");
    require(exType.canUseExistentialRepresentation(F.getModule(),
                                       ExistentialRepresentation::Opaque),
            "deinit_existential_addr must be applied to an opaque "
            "existential");
  }
  
  void checkDeallocExistentialBoxInst(DeallocExistentialBoxInst *DEBI) {
    SILType exType = DEBI->getOperand().getType();
    require(exType.isObject(),
            "dealloc_existential_box must be applied to a value");
    require(exType.canUseExistentialRepresentation(F.getModule(),
                                       ExistentialRepresentation::Boxed),
            "dealloc_existential_box must be applied to a boxed "
            "existential");
  }

  void checkInitExistentialMetatypeInst(InitExistentialMetatypeInst *I) {
    SILType operandType = I->getOperand().getType();
    require(operandType.isObject(),
            "init_existential_metatype operand must not be an address");
    require(operandType.is<MetatypeType>(),
            "init_existential_metatype operand must be a metatype");
    require(operandType.castTo<MetatypeType>()->hasRepresentation(),
            "init_existential_metatype operand must have a representation");

    SILType resultType = I->getType();
    require(resultType.is<ExistentialMetatypeType>(),
            "init_existential_metatype result must be an existential metatype");
    require(resultType.isObject(),
            "init_existential_metatype result must not be an address");
    require(resultType.castTo<ExistentialMetatypeType>()->hasRepresentation(),
            "init_existential_metatype result must have a representation");
    require(resultType.castTo<ExistentialMetatypeType>()->getRepresentation()
              == operandType.castTo<MetatypeType>()->getRepresentation(),
            "init_existential_metatype result must match representation of "
            "operand");
    
    for (ProtocolConformance *C : I->getConformances())
      // We allow for null conformances.
      require(!C || I->getModule().lookUpWitnessTable(C, false).first,
              "Could not find witness table for conformance.");
  }

  void verifyCheckedCast(bool isExact, SILType fromTy, SILType toTy) {
    // Verify common invariants.
    require(fromTy.isObject() && toTy.isObject(),
            "value checked cast src and dest must be objects");

    auto fromCanTy = fromTy.getSwiftRValueType();
    auto toCanTy = toTy.getSwiftRValueType();

    // Peel off metatypes. If two types are checked-cast-able, so are their
    // metatypes.
    unsigned MetatyLevel = 0;
    while (isa<AnyMetatypeType>(fromCanTy) && isa<AnyMetatypeType>(toCanTy)) {
      auto fromMetaty = cast<AnyMetatypeType>(fromCanTy);
      auto toMetaty = cast<AnyMetatypeType>(toCanTy);
      
      // Check representations only for the top-level metatypes as only
      // those are SIL-lowered.
      if (!MetatyLevel) {
        // The representations must match.
        require(fromMetaty->getRepresentation() == toMetaty->getRepresentation(),
                "metatype checked cast cannot change metatype representation");

        // We can't handle the 'thin' case yet, but it shouldn't really even be
        // interesting.
        require(fromMetaty->getRepresentation() != MetatypeRepresentation::Thin,
                "metatype checked cast cannot check thin metatypes");
      }

      fromCanTy = fromMetaty.getInstanceType();
      toCanTy = toMetaty.getInstanceType();
      MetatyLevel++;
    }

    if (isExact) {
      require(fromCanTy.getClassOrBoundGenericClass(),
              "downcast operand must be a class type");
      require(toCanTy.getClassOrBoundGenericClass(),
              "downcast must convert to a class type");
      require(SILType::getPrimitiveObjectType(fromCanTy).
              isSuperclassOf(SILType::getPrimitiveObjectType(toCanTy)),
              "downcast must convert to a subclass");
    }
  }

  void checkUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *CI) {
    verifyCheckedCast(/*exact*/ false,
                      CI->getOperand().getType(),
                      CI->getType());
  }

  void checkCheckedCastBranchInst(CheckedCastBranchInst *CBI) {
    verifyCheckedCast(CBI->isExact(),
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

  void checkThinToThickFunctionInst(ThinToThickFunctionInst *TTFI) {
    auto opFTy = requireObjectType(SILFunctionType, TTFI->getOperand(),
                                   "thin_to_thick_function operand");
    auto resFTy = requireObjectType(SILFunctionType, TTFI,
                                    "thin_to_thick_function result");
    require(opFTy->isPolymorphic() == resFTy->isPolymorphic(),
            "thin_to_thick_function operand and result type must differ only "
            " in thinness");
    requireSameFunctionComponents(opFTy, resFTy,
                                  "thin_to_thick_function operand and result");

    require(opFTy->getRepresentation() == SILFunctionType::Representation::Thin,
            "operand of thin_to_thick_function must be thin");
    require(resFTy->getRepresentation() == SILFunctionType::Representation::Thick,
            "result of thin_to_thick_function must be thick");

    auto adjustedOperandExtInfo = opFTy->getExtInfo().withRepresentation(
                                           SILFunctionType::Representation::Thick);
    require(adjustedOperandExtInfo == resFTy->getExtInfo(),
            "operand and result of thin_to_think_function must agree in particulars");
  }

  void checkThickToObjCMetatypeInst(ThickToObjCMetatypeInst *TTOCI) {
    auto opTy = requireObjectType(AnyMetatypeType, TTOCI->getOperand(),
                                  "thick_to_objc_metatype operand");
    auto resTy = requireObjectType(AnyMetatypeType, TTOCI,
                                   "thick_to_objc_metatype result");

    require(TTOCI->getOperand().getType().is<MetatypeType>() ==
            TTOCI->getType().is<MetatypeType>(),
            "thick_to_objc_metatype cannot change metatype kinds");
    require(opTy->getRepresentation() == MetatypeRepresentation::Thick,
            "operand of thick_to_objc_metatype must be thick");
    require(resTy->getRepresentation() == MetatypeRepresentation::ObjC,
            "operand of thick_to_objc_metatype must be ObjC");

    require(opTy->getInstanceType()->isEqual(resTy->getInstanceType()),
            "thick_to_objc_metatype instance types do not match");
  }

  void checkObjCToThickMetatypeInst(ObjCToThickMetatypeInst *OCTTI) {
    auto opTy = requireObjectType(AnyMetatypeType, OCTTI->getOperand(),
                                  "objc_to_thick_metatype operand");
    auto resTy = requireObjectType(AnyMetatypeType, OCTTI,
                                   "objc_to_thick_metatype result");

    require(OCTTI->getOperand().getType().is<MetatypeType>() ==
            OCTTI->getType().is<MetatypeType>(),
            "objc_to_thick_metatype cannot change metatype kinds");
    require(opTy->getRepresentation() == MetatypeRepresentation::ObjC,
            "operand of objc_to_thick_metatype must be ObjC");
    require(resTy->getRepresentation() == MetatypeRepresentation::Thick,
            "operand of objc_to_thick_metatype must be thick");

    require(opTy->getInstanceType()->isEqual(resTy->getInstanceType()),
            "objc_to_thick_metatype instance types do not match");
  }

  void checkRefToUnownedInst(RefToUnownedInst *I) {
    requireReferenceStorageCapableValue(I->getOperand(),
                                        "Operand of ref_to_unowned");
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
    requireReferenceStorageCapableValue(I, "Result of unowned_to_ref");
    auto resultType = I->getType().getSwiftRValueType();
    require(operandType.getReferentType() == resultType,
            "Operand of unowned_to_ref does not have the "
            "operand's type as its referent type");
  }

  void checkRefToUnmanagedInst(RefToUnmanagedInst *I) {
    requireReferenceStorageCapableValue(I->getOperand(),
                                        "Operand of ref_to_unmanaged");
    auto operandType = I->getOperand().getType().getSwiftRValueType();
    auto resultType = requireObjectType(UnmanagedStorageType, I,
                                        "Result of ref_to_unmanaged");
    require(resultType.getReferentType() == operandType,
            "Result of ref_to_unmanaged does not have the "
            "operand's type as its referent type");
  }

  void checkUnmanagedToRefInst(UnmanagedToRefInst *I) {
    auto operandType = requireObjectType(UnmanagedStorageType,
                                         I->getOperand(),
                                         "Operand of unmanaged_to_ref");
    requireReferenceStorageCapableValue(I, "Result of unmanaged_to_ref");
    auto resultType = I->getType().getSwiftRValueType();
    require(operandType.getReferentType() == resultType,
            "Operand of unmanaged_to_ref does not have the "
            "operand's type as its referent type");
  }

  void checkUpcastInst(UpcastInst *UI) {
    require(UI->getType() != UI->getOperand().getType(),
            "can't upcast to same type");
    if (UI->getType().is<MetatypeType>()) {
      CanType instTy(UI->getType().castTo<MetatypeType>()->getInstanceType());
      require(UI->getOperand().getType().is<MetatypeType>(),
              "upcast operand must be a class or class metatype instance");
      CanType opInstTy(UI->getOperand().getType().castTo<MetatypeType>()
                         ->getInstanceType());
      require(instTy->getClassOrBoundGenericClass(),
              "upcast must convert a class metatype to a class metatype");
      require(instTy->isSuperclassOf(opInstTy, nullptr),
              "upcast must cast to a superclass or an existential metatype");
      return;
    }

    require(UI->getType().getCategory() ==
            UI->getOperand().getType().getCategory(),
            "Upcast can only upcast in between types of the same "
            "SILValueCategory. This prevents address types from being cast to "
            "object types or vis-a-versa");
    require(UI->getType().getClassOrBoundGenericClass(),
            "upcast must convert a class instance to a class type");
    require(UI->getType().isSuperclassOf(UI->getOperand().getType()),
            "upcast must cast to a superclass");
  }

  void checkIsNonnullInst(IsNonnullInst *II) {
    // The operand must be a function type or a class type.
    auto OpTy = II->getOperand().getType().getSwiftType();
    require(OpTy->mayHaveSuperclass() || OpTy->is<SILFunctionType>(),
            "is_nonnull operand must be a class or function type");
  }

  void checkAddressToPointerInst(AddressToPointerInst *AI) {
    require(AI->getOperand().getType().isAddress(),
            "address-to-pointer operand must be an address");
    require(AI->getType().getSwiftType()->isEqual(
                              AI->getType().getASTContext().TheRawPointerType),
            "address-to-pointer result type must be RawPointer");
  }
  
  void checkUncheckedRefCastInst(UncheckedRefCastInst *AI) {
    require(AI->getOperand().getType().isObject(),
            "unchecked_ref_cast operand must be a value");
    require(AI->getOperand().getType().isHeapObjectReferenceType(),
            "unchecked_ref_cast operand must be a heap object reference");
    require(AI->getType().isObject(),
            "unchecked_ref_cast result must be an object");
    require(AI->getType().isHeapObjectReferenceType(),
            "unchecked_ref_cast result must be a heap object reference");
  }
  
  void checkUncheckedAddrCastInst(UncheckedAddrCastInst *AI) {
    require(AI->getOperand().getType().isAddress(),
            "unchecked_addr_cast operand must be an address");
    require(AI->getType().isAddress(),
            "unchecked_addr_cast result must be an address");
  }
  
  void checkUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *BI) {
    require(BI->getOperand().getType().isObject(),
            "unchecked_trivial_bit_cast must operate on a value");
    require(BI->getType().isObject(),
            "unchecked_trivial_bit_cast must produce a value");
    require(BI->getType().isTrivial(F.getModule()),
            "unchecked_trivial_bit_cast must produce a value of trivial type");
  }

  void checkUncheckedRefBitCastInst(UncheckedRefBitCastInst *BI) {
    require(BI->getOperand().getType().isObject(),
            "unchecked_ref_bit_cast must operate on a value");
    require(BI->getType().isObject(),
            "unchecked_ref_bit_cast must produce a value");
    
    // TODO: A deeper comparison of the source and destination types to ensure
    // they're reference-counting-identical.
    auto &M = F.getModule();
    require(BI->getOperand().getType().isTrivial(M)
              == BI->getType().isTrivial(M),
            "unchecked_ref_bit_cast cannot change the reference count semantics"
            " of the value");
  }

  void checkRefToRawPointerInst(RefToRawPointerInst *AI) {
    require(AI->getOperand().getType().getSwiftType()
              ->isAnyClassReferenceType(),
            "ref-to-raw-pointer operand must be a class reference or"
            " NativeObject");
    require(AI->getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheRawPointerType),
            "ref-to-raw-pointer result must be RawPointer");
  }

  void checkRawPointerToRefInst(RawPointerToRefInst *AI) {
    require(AI->getType()
              .getSwiftType()->isBridgeableObjectType()
            || AI->getType().getSwiftType()->isEqual(
                             AI->getType().getASTContext().TheNativeObjectType)
            || AI->getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheUnknownObjectType),
        "raw-pointer-to-ref result must be a class reference or NativeObject");
    require(AI->getOperand().getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheRawPointerType),
            "raw-pointer-to-ref operand must be NativeObject");
  }
  
  void checkRefToBridgeObjectInst(RefToBridgeObjectInst *RI) {
    require(RI->getConverted().getType().isObject(),
            "ref_to_bridge_object must convert from a value");
    require(RI->getConverted().getType().getSwiftRValueType()
              ->isBridgeableObjectType(),
            "ref_to_bridge_object must convert from a heap object ref");
    require(RI->getBitsOperand().getType()
              == SILType::getBuiltinWordType(F.getASTContext()),
            "ref_to_bridge_object must take a Builtin.Word bits operand");
    require(RI->getType() == SILType::getBridgeObjectType(F.getASTContext()),
            "ref_to_bridge_object must produce a BridgeObject");
  }
  
  void checkBridgeObjectToRefInst(BridgeObjectToRefInst *RI) {
    require(RI->getConverted().getType()
               == SILType::getBridgeObjectType(F.getASTContext()),
            "bridge_object_to_ref must take a BridgeObject");
    require(RI->getType().isObject(),
            "bridge_object_to_ref must produce a value");
    require(RI->getType().getSwiftRValueType()->isBridgeableObjectType(),
            "bridge_object_to_ref must produce a heap object reference");
  }
  void checkBridgeObjectToWordInst(BridgeObjectToWordInst *RI) {
    require(RI->getConverted().getType()
               == SILType::getBridgeObjectType(F.getASTContext()),
            "bridge_object_to_word must take a BridgeObject");
    require(RI->getType().isObject(),
            "bridge_object_to_word must produce a value");
    require(RI->getType() == SILType::getBuiltinWordType(F.getASTContext()),
            "bridge_object_to_word must produce a Word");
  }

  void checkConvertFunctionInst(ConvertFunctionInst *ICI) {
    auto opTI = requireObjectType(SILFunctionType, ICI->getOperand(),
                                  "convert_function operand");
    auto resTI = requireObjectType(SILFunctionType, ICI,
                                   "convert_function result");

    // convert_function is required to be an ABI-compatible conversion.
    requireABICompatibleFunctionTypes(opTI, resTI,
                                 "convert_function cannot change function ABI");
  }

  void checkThinFunctionToPointerInst(ThinFunctionToPointerInst *CI) {
    auto opTI = requireObjectType(SILFunctionType, CI->getOperand(),
                                  "thin_function_to_pointer operand");
    requireObjectType(BuiltinRawPointerType, CI,
                      "thin_function_to_pointer result");

    require(opTI->getRepresentation() == SILFunctionType::Representation::Thin,
            "thin_function_to_pointer only works on thin functions");
  }

  void checkPointerToThinFunctionInst(PointerToThinFunctionInst *CI) {
    auto resultTI = requireObjectType(SILFunctionType, CI,
                                      "pointer_to_thin_function result");
    requireObjectType(BuiltinRawPointerType, CI->getOperand(),
                      "pointer_to_thin_function operand");

    require(resultTI->getRepresentation() == SILFunctionType::Representation::Thin,
            "pointer_to_thin_function only works on thin functions");
  }

  void checkCondFailInst(CondFailInst *CFI) {
    require(CFI->getOperand().getType()
              == SILType::getBuiltinIntegerType(1, F.getASTContext()),
            "cond_fail operand must be a Builtin.Int1");
  }

  void checkReturnInst(ReturnInst *RI) {
    DEBUG(RI->print(llvm::dbgs()));

    CanSILFunctionType ti = F.getLoweredFunctionType();
    SILType functionResultType
      = F.mapTypeIntoContext(ti->getResult().getSILType());
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

    CanSILFunctionType ti = F.getLoweredFunctionType();
    SILType functionResultType
      = F.mapTypeIntoContext(ti->getResult().getSILType());
    SILType instResultType = RI->getOperand().getType();
    DEBUG(llvm::dbgs() << "function return type: ";
          functionResultType.dump();
          llvm::dbgs() << "return inst type: ";
          instResultType.dump(););
    require(functionResultType == instResultType,
            "return value type does not match return type of function");
    require(instResultType.isObject(),
            "autoreleased return value cannot be an address");
    require(instResultType.hasRetainablePointerRepresentation(),
            "autoreleased return value must be a reference type");
  }

  void checkThrowInst(ThrowInst *TI) {
    DEBUG(TI->print(llvm::dbgs()));

    CanSILFunctionType fnType = F.getLoweredFunctionType();
    require(fnType->hasErrorResult(),
            "throw in function that doesn't have an error result");

    SILType functionResultType
      = F.mapTypeIntoContext(fnType->getErrorResult().getSILType());
    SILType instResultType = TI->getOperand().getType();
    DEBUG(llvm::dbgs() << "function error result type: ";
          functionResultType.dump();
          llvm::dbgs() << "throw operand type: ";
          instResultType.dump(););
    require(functionResultType == instResultType,
            "throw operand type does not match error result type of function");
  }
  
  void checkSelectEnumCases(SelectEnumInstBase *I) {
    EnumDecl *eDecl = I->getEnumOperand().getType().getEnumOrBoundGenericEnum();
    require(eDecl, "select_enum operand must be an enum");

    // Find the set of enum elements for the type so we can verify
    // exhaustiveness.
    // FIXME: We also need to consider if the enum is resilient, in which case
    // we're never guaranteed to be exhaustive.
    llvm::DenseSet<EnumElementDecl*> unswitchedElts;
    eDecl->getAllElements(unswitchedElts);

    // Verify the set of enum cases we dispatch on.
    for (unsigned i = 0, e = I->getNumCases(); i < e; ++i) {
      EnumElementDecl *elt;
      SILValue result;
      std::tie(elt, result) = I->getCase(i);

      require(elt->getDeclContext() == eDecl,
              "select_enum dispatches on enum element that is not part of "
              "its type");
      require(unswitchedElts.count(elt),
              "select_enum dispatches on same enum element more than once");
      unswitchedElts.erase(elt);

      // The result value must match the type of the instruction.
      requireSameType(result.getType(), I->getType(),
                    "select_enum case operand must match type of instruction");
    }

    // If the switch is non-exhaustive, we require a default.
    require(unswitchedElts.empty() || I->hasDefault(),
            "nonexhaustive select_enum must have a default destination");
    if (I->hasDefault()) {
      requireSameType(I->getDefaultResult().getType(),
                  I->getType(),
                  "select_enum default operand must match type of instruction");
    }
  }

  void checkSelectEnumInst(SelectEnumInst *SEI) {
    require(SEI->getEnumOperand().getType().isObject(),
            "select_enum operand must be an object");
    
    checkSelectEnumCases(SEI);
  }
  void checkSelectEnumAddrInst(SelectEnumAddrInst *SEI) {
    require(SEI->getEnumOperand().getType().isAddress(),
            "select_enum_addr operand must be an address");
    
    checkSelectEnumCases(SEI);
  }

  void checkSwitchValueInst(SwitchValueInst *SVI) {
    // TODO: Type should be either integer or function
    auto Ty = SVI->getOperand().getType();
    require(Ty.getAs<BuiltinIntegerType>() || Ty.getAs<SILFunctionType>(),
            "switch_value operand should be either of an integer "
            "or function type");

    auto ult = [](const SILValue &a, const SILValue &b) { 
      return a == b || a < b; 
    };

    std::set<SILValue, decltype(ult)> cases(ult);

    for (unsigned i = 0, e = SVI->getNumCases(); i < e; ++i) {
      SILValue value;
      SILBasicBlock *dest;
      std::tie(value, dest) = SVI->getCase(i);

      require(value.getType() == Ty,
             "switch_value case value should have the same type as its operand");

      require(!cases.count(value),
              "multiple switch_value cases for same value");
      cases.insert(value);

      require(dest->bbarg_empty(),
              "switch_value case destination cannot take arguments");
    }

    if (SVI->hasDefault())
      require(SVI->getDefaultBB()->bbarg_empty(),
              "switch_value default destination cannot take arguments");
  }

  void checkSelectValueCases(SelectValueInst *I) {
    struct APIntCmp {
      bool operator()(const APInt &a, const APInt &b) const {
        return a.ult(b);
      };
    };

    llvm::SmallSet<APInt, 16, APIntCmp> seenCaseValues;

    // Verify the set of cases we dispatch on.
    for (unsigned i = 0, e = I->getNumCases(); i < e; ++i) {
      SILValue casevalue;
      SILValue result;
      std::tie(casevalue, result) = I->getCase(i);
      auto  *il = dyn_cast<IntegerLiteralInst>(casevalue);
      require(il,
              "select_value case operands should refer to integer literals");
      APInt elt = il->getValue();

      require(!seenCaseValues.count(elt),
              "select_value dispatches on same case value more than once");

      seenCaseValues.insert(elt);

      // The result value must match the type of the instruction.
      requireSameType(result.getType(), I->getType(),
                    "select_value case operand must match type of instruction");
    }

    require(I->hasDefault(),
            "select_value should always have a default");
    requireSameType(I->getDefaultResult().getType(),
                  I->getType(),
                  "select_value default operand must match type of instruction");
  }

  void checkSelectValueInst(SelectValueInst *SVI) {
    require(SVI->getOperand().getType().isObject(),
            "select_value operand must be an object");

    checkSelectValueCases(SVI);
  }

  void checkSwitchEnumInst(SwitchEnumInst *SOI) {
    require(SOI->getOperand().getType().isObject(),
            "switch_enum operand must be an object");

    SILType uTy = SOI->getOperand().getType();
    EnumDecl *uDecl = uTy.getEnumOrBoundGenericEnum();
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
          SILType eltArgTy = uTy.getEnumElementType(elt, F.getModule());
          SILType bbArgTy = dest->getBBArgs()[0]->getType();
          require(eltArgTy == bbArgTy,
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

  void checkSwitchEnumAddrInst(SwitchEnumAddrInst *SOI){
    require(SOI->getOperand().getType().isAddress(),
            "switch_enum_addr operand must be an object");

    SILType uTy = SOI->getOperand().getType();
    EnumDecl *uDecl = uTy.getEnumOrBoundGenericEnum();
    require(uDecl, "switch_enum_addr operand must be an enum");

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
              "switch_enum_addr dispatches on enum element that "
              "is not part of its type");
      require(unswitchedElts.count(elt),
              "switch_enum_addr dispatches on same enum element "
              "more than once");
      unswitchedElts.erase(elt);

      // The destination BB must not have BB arguments.
      require(dest->getBBArgs().size() == 0,
              "switch_enum_addr destination must take no BB args");
    }

    // If the switch is non-exhaustive, we require a default.
    require(unswitchedElts.empty() || SOI->hasDefault(),
            "nonexhaustive switch_enum_addr must have a default "
            "destination");
    if (SOI->hasDefault())
      require(SOI->getDefaultBB()->bbarg_empty(),
              "switch_enum_addr default destination must take "
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
    require(CBI->getTrueBB() != CBI->getFalseBB(),
            "identical destinations");
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

    require(DMBI->getMember().getDecl()->isObjC(), "method must be @objc");
    if (!DMBI->getMember().getDecl()->isInstanceMember()) {
      require(operandType.getSwiftType()->is<MetatypeType>(),
              "operand must have metatype type");
      require(operandType.getSwiftType()->castTo<MetatypeType>()
                ->getInstanceType()->mayHaveSuperclass(),
              "operand must have metatype of class or class-bound type");
    }

    // Check that the branch argument is of the expected dynamic method type.
    require(DMBI->getHasMethodBB()->bbarg_size() == 1,
            "true bb for dynamic_method_br must take an argument");
    
    requireSameType(DMBI->getHasMethodBB()->bbarg_begin()[0]->getType(),
                    getDynamicMethodType(operandType, DMBI->getMember()),
              "bb argument for dynamic_method_br must be of the method's type");
  }
  
  void checkProjectBlockStorageInst(ProjectBlockStorageInst *PBSI) {
    require(PBSI->getOperand().getType().isAddress(),
            "operand must be an address");
    auto storageTy = PBSI->getOperand().getType().getAs<SILBlockStorageType>();
    require(storageTy, "operand must be a @block_storage type");
    
    require(PBSI->getType().isAddress(),
            "result must be an address");
    auto captureTy = PBSI->getType().getSwiftRValueType();
    require(storageTy->getCaptureType() == captureTy,
            "result must be the capture type of the @block_storage type");
  }
  
  void checkInitBlockStorageHeaderInst(InitBlockStorageHeaderInst *IBSHI) {
    require(IBSHI->getBlockStorage().getType().isAddress(),
            "block storage operand must be an address");
    auto storageTy
      = IBSHI->getBlockStorage().getType().getAs<SILBlockStorageType>();
    require(storageTy, "block storage operand must be a @block_storage type");
    
    require(IBSHI->getInvokeFunction().getType().isObject(),
            "invoke function operand must be a value");
    auto invokeTy
      = IBSHI->getInvokeFunction().getType().getAs<SILFunctionType>();
    require(invokeTy, "invoke function operand must be a function");
    require(invokeTy->getRepresentation()
              == SILFunctionType::Representation::CFunctionPointer,
            "invoke function operand must be a c function");
    require(invokeTy->getParameters().size() >= 1,
            "invoke function must take at least one parameter");
    auto storageParam = invokeTy->getParameters()[0];
    require(storageParam.getConvention() == ParameterConvention::Indirect_Inout,
            "invoke function must take block storage as @inout parameter");
    require(storageParam.getType() == storageTy,
            "invoke function must take block storage type as first parameter");
    
    require(IBSHI->getType().isObject(), "result must be a value");
    auto blockTy = IBSHI->getType().getAs<SILFunctionType>();
    require(blockTy, "result must be a function");
    require(blockTy->getRepresentation() == SILFunctionType::Representation::Block,
            "result must be a cdecl block function");
    require(blockTy->getResult() == invokeTy->getResult(),
            "result must have same return type as invoke function");
    
    require(blockTy->getParameters().size() + 1
              == invokeTy->getParameters().size(),
          "result must match all parameters of invoke function but the first");
    auto blockParams = blockTy->getParameters();
    auto invokeBlockParams = invokeTy->getParameters().slice(1);
    for (unsigned i : indices(blockParams)) {
      require(blockParams[i] == invokeBlockParams[i],
          "result must match all parameters of invoke function but the first");
    }
  }
  
  void checkObjCProtocolInst(ObjCProtocolInst *OPI) {
    require(OPI->getProtocol()->isObjC(),
            "objc_protocol must be applied to an @objc protocol");
    auto classTy = OPI->getType();
    require(classTy.isObject(), "objc_protocol must produce a value");
    auto classDecl = classTy.getClassOrBoundGenericClass();
    require(classDecl, "objc_protocol must produce a class instance");
    require(classDecl->getName() == F.getASTContext().Id_Protocol,
            "objc_protocol must produce an instance of ObjectiveC.Protocol class");
    require(classDecl->getModuleContext()->Name == F.getASTContext().Id_ObjectiveC,
            "objc_protocol must produce an instance of ObjectiveC.Protocol class");
  }
  
  void checkObjCMetatypeToObjectInst(ObjCMetatypeToObjectInst *OMOI) {
    require(OMOI->getOperand().getType().isObject(),
            "objc_metatype_to_object must take a value");
    auto fromMetaTy = OMOI->getOperand().getType().getAs<MetatypeType>();
    require(fromMetaTy, "objc_metatype_to_object must take an @objc metatype value");
    require(fromMetaTy->getRepresentation() == MetatypeRepresentation::ObjC,
            "objc_metatype_to_object must take an @objc metatype value");
    require(OMOI->getType().isObject(),
            "objc_metatype_to_object must produce a value");
    require(OMOI->getType().getSwiftRValueType()->isAnyObject(),
            "objc_metatype_to_object must produce an AnyObject value");
  }

  void checkObjCExistentialMetatypeToObjectInst(
                                    ObjCExistentialMetatypeToObjectInst *OMOI) {
    require(OMOI->getOperand().getType().isObject(),
            "objc_metatype_to_object must take a value");
    auto fromMetaTy = OMOI->getOperand().getType()
      .getAs<ExistentialMetatypeType>();
    require(fromMetaTy, "objc_metatype_to_object must take an @objc existential metatype value");
    require(fromMetaTy->getRepresentation() == MetatypeRepresentation::ObjC,
            "objc_metatype_to_object must take an @objc existential metatype value");
    require(OMOI->getType().isObject(),
            "objc_metatype_to_object must produce a value");
    require(OMOI->getType().getSwiftRValueType()->isAnyObject(),
            "objc_metatype_to_object must produce an AnyObject value");
  }

  void verifyEntryPointArguments(SILBasicBlock *entry) {
    SILFunctionType *ti = F.getLoweredFunctionType();

    DEBUG(llvm::dbgs() << "Argument types for entry point BB:\n";
          for (auto *arg : make_range(entry->bbarg_begin(), entry->bbarg_end()))
            arg->getType().dump();
          llvm::dbgs() << "Input types for SIL function type ";
          ti->print(llvm::dbgs());
          llvm::dbgs() << ":\n";
          for (auto input : ti->getParameters())
            input.getSILType().dump(););

    require(entry->bbarg_size() == ti->getParameters().size(),
            "entry point has wrong number of arguments");


    require(std::equal(entry->bbarg_begin(), entry->bbarg_end(),
                      ti->getParameterSILTypes().begin(),
                      [&](SILArgument *bbarg, SILType ty) {
                        auto mappedTy = F.mapTypeIntoContext(ty);
                        if (bbarg->getType() != mappedTy) {
                          llvm::errs() << "argument type mismatch!\n";
                          llvm::errs() << "  argument: "; bbarg->dump();
                          llvm::errs() << "  expected: "; mappedTy.dump();
                          return false;
                        }
                        return true;
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

  bool
  isUnreachableAlongAllPathsStartingAt(SILBasicBlock *StartBlock,
                                       SmallPtrSet<SILBasicBlock *, 16> &Visited) {
    if (isa<UnreachableInst>(StartBlock->getTerminator()))
      return true;
    else if (isa<ReturnInst>(StartBlock->getTerminator()))
      return false;
    else if (isa<AutoreleaseReturnInst>(StartBlock->getTerminator()))
      return false;

    // Recursively check all successors.
    for (const auto &SuccBB : StartBlock->getSuccessors())
      if (!Visited.insert(SuccBB.getBB()).second)
        if (!isUnreachableAlongAllPathsStartingAt(SuccBB.getBB(), Visited))
          return false;

    return true;
  }

  void verifySILFunctionType(CanSILFunctionType FTy) {
    // Make sure that if FTy's calling convention implies that it must have a
    // self parameter.
    require(!FTy->hasSelfParam() || !FTy->getParameters().empty(),
            "Functions with a calling convention with self parameter must "
            "have at least one argument for self.");

    // Make sure that FTy does not have any out parameters except for the first
    // parameter.
    if (FTy->getParameters().size() < 2)
      return;

    for (SILParameterInfo PInfo : FTy->getParameters().slice(1)) {
      require(!PInfo.isIndirectResult(),
              "Indirect results can only be the first argument of a "
              "SILFunction.");
    }
  }

  void verifyStackHeight(SILFunction *F) {
    llvm::DenseMap<SILBasicBlock*, std::vector<AllocStackInst*>> visitedBBs;
    SmallVector<SILBasicBlock*, 16> Worklist;
    visitedBBs[F->begin()] = {};
    Worklist.push_back(F->begin());
    while (!Worklist.empty()) {
      SILBasicBlock *BB = Worklist.pop_back_val();
      std::vector<AllocStackInst*> stack = visitedBBs[BB];
      for (SILInstruction &i : *BB) {
        CurInstruction = &i;

        if (auto alloc = dyn_cast<AllocStackInst>(&i)) {
          stack.push_back(alloc);
        }
        if (auto dealloc = dyn_cast<DeallocStackInst>(&i)) {
          SILValue op = dealloc->getOperand();
          require(op.getResultNumber() == 0,
                  "dealloc_stack operand is not local storage of alloc_inst");
          require(!stack.empty(),
                  "dealloc_stack with empty stack");
          require(op.getDef() == stack.back(),
                  "dealloc_stack does not match most recent alloc_stack");
          stack.pop_back();
        }
        if (isa<ReturnInst>(&i) || isa<AutoreleaseReturnInst>(&i)) {
          require(stack.empty(),
                  "return with alloc_stacks that haven't been deallocated");
        }
        if (auto term = dyn_cast<TermInst>(&i)) {
          for (auto &successor : term->getSuccessors()) {
            SILBasicBlock *SuccBB = successor.getBB();
            auto found = visitedBBs.find(SuccBB);
            if (found != visitedBBs.end()) {
              // Check that the stack height is consistent coming from all entry
              // points into this BB. We only care about consistency if there is
              // a possible return from this function along the path starting at
              // this successor bb.
              SmallPtrSet<SILBasicBlock *, 16> Visited;
              require(isUnreachableAlongAllPathsStartingAt(SuccBB, Visited) ||
                          stack == found->second,
                      "inconsistent stack heights entering basic block");
              continue;
            }
            Worklist.push_back(SuccBB);
            visitedBBs.insert({SuccBB, stack});
          }
        }
      }
    }
  }

  void verifyBranches(SILFunction *F) {
    // If we are not in canonical SIL return early.
    if (F->getModule().getStage() != SILStage::Canonical)
      return;

    // Verify that there is no non_condbr critical edge.
    auto isCriticalEdgePred = [](const TermInst *T, unsigned EdgeIdx) {
      assert(T->getSuccessors().size() > EdgeIdx && "Not enough successors");

      // A critical edge has more than one outgoing edges from the source
      // block.
      auto SrcSuccs = T->getSuccessors();
      if (SrcSuccs.size() <= 1)
        return false;

      // And its destination block has more than one predecessor.
      SILBasicBlock *DestBB = SrcSuccs[EdgeIdx];
      assert(!DestBB->pred_empty() && "There should be a predecessor");
      if (DestBB->getSinglePredecessor())
        return false;

      return true;
    };

    // Check for non-cond_br critical edges.
    for (auto &BB : *F) {
      TermInst *TI = BB.getTerminator();
      if (isa<CondBranchInst>(TI))
        continue;

      for (unsigned Idx = 0, e = BB.getSuccessors().size(); Idx != e; ++Idx) {
        require(!isCriticalEdgePred(TI, Idx),
                "non cond_br critical edges not allowed");
      }
    }
  }

  void visitSILBasicBlock(SILBasicBlock *BB) {
    // Make sure that each of the successors/predecessors of this basic block
    // have this basic block in its predecessor/successor list.
    for (const SILSuccessor &S : BB->getSuccessors()) {
      SILBasicBlock *SuccBB = S.getBB();
      bool FoundSelfInSuccessor = false;
      for (const SILBasicBlock *PredBB : SuccBB->getPreds()) {
        if (PredBB == BB) {
          FoundSelfInSuccessor = true;
          break;
        }
      }
      require(FoundSelfInSuccessor, "Must be a predecessor of each successor.");
    }

    for (const SILBasicBlock *PredBB : BB->getPreds()) {
      bool FoundSelfInPredecessor = false;
      for (const SILSuccessor &S : PredBB->getSuccessors()) {
        if (S.getBB() == BB) {
          FoundSelfInPredecessor = true;
          break;
        }
      }
      require(FoundSelfInPredecessor, "Must be a successor of each predecessor.");
    }
    
    SILVisitor::visitSILBasicBlock(BB);
  }

  void visitSILFunction(SILFunction *F) {
    PrettyStackTraceSILFunction stackTrace("verifying", F);

    if (F->getLinkage() == SILLinkage::PrivateExternal) {
      // FIXME: uncomment these checks.
      // <rdar://problem/18635841> SILGen can create non-fragile external
      // private_external declarations
      //
      // assert(!isExternalDeclaration() &&
      //        "PrivateExternal should not be an external declaration");
      // assert(isFragile() &&
      //        "PrivateExternal should be fragile (otherwise, how did it appear "
      //        "in this module?)");
    }

    CanSILFunctionType FTy = F->getLoweredFunctionType();
    verifySILFunctionType(FTy);

    if (F->isExternalDeclaration()) {
      assert(F->isAvailableExternally() &&
             "external declaration of internal SILFunction not allowed");
      assert(!hasSharedVisibility(F->getLinkage()) &&
             "external declarations of SILFunctions with shared visiblity is not "
             "allowed");
      // If F is an external declaration, there is nothing further to do,
      // return.
      return;
    }

    // Make sure that our SILFunction only has context generic params if our
    // SILFunctionType is non-polymorphic.
    if (F->getContextGenericParams()) {
      require(FTy->isPolymorphic(),
              "generic function definition must have context archetypes");
    } else {
      require(!FTy->isPolymorphic(),
              "non-generic function definitions cannot have context "
              "archetypes");
    }

    // Otherwise, verify the body of the function.
    verifyEntryPointArguments(F->getBlocks().begin());
    verifyEpilogBlock(F);
    verifyStackHeight(F);
    verifyBranches(F);
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
void SILFunction::verify() const {
#ifndef NDEBUG
  // Please put all checks in visitSILFunction in SILVerifier, not here. This
  // ensures that the pretty stack trace in the verifier is included with the
  // back trace when the verifier crashes.
  SILVerifier(*this).verify();
#endif
}

/// Verify that a vtable follows invariants.
void SILVTable::verify(const SILModule &M) const {
#ifndef NDEBUG
  for (auto &entry : getEntries()) {
    // All vtable entries must be decls in a class context.
    assert(entry.first.hasDecl() && "vtable entry is not a decl");
    auto baseInfo = M.Types.getConstantInfo(entry.first);
    ValueDecl *decl = entry.first.getDecl();
    
    assert((!isa<FuncDecl>(decl)
            || !cast<FuncDecl>(decl)->isObservingAccessor())
           && "observing accessors shouldn't have vtable entries");
    
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
    assert(!entry.first.isCurried && "vtable entry must not be curried");

    // Foreign entry points shouldn't appear in vtables.
    assert(!entry.first.isForeign && "vtable entry must not be foreign");
    
    // The vtable entry must be ABI-compatible with the overridden vtable slot.
    SmallString<32> baseName;
    {
      llvm::raw_svector_ostream os(baseName);
      entry.first.print(os);
      os.flush();
    }
    
    SILVerifier(*entry.second)
      .requireABICompatibleFunctionTypes(
                    baseInfo.getSILType().castTo<SILFunctionType>(),
                    entry.second->getLoweredFunctionType(),
                    "vtable entry for " + baseName + " must be ABI-compatible");
  }
#endif
}

/// Verify that a witness table follows invariants.
void SILWitnessTable::verify(const SILModule &M) const {
#ifndef NDEBUG
  if (isDeclaration())
    assert(getEntries().size() == 0 &&
           "A witness table declaration should not have any entries.");

  // Currently all witness tables have public conformances, thus witness tables
  // should not reference SILFunctions without public/public_external linkage.
  // FIXME: Once we support private conformances, update this.
  for (const Entry &E : getEntries())
    if (E.getKind() == SILWitnessTable::WitnessKind::Method) {
      SILFunction *F = E.getMethodWitness().Witness;
      if (F) {
        assert(!isLessVisibleThan(F->getLinkage(), getLinkage()) &&
               "Witness tables should not reference less visible functions.");
      }
    }
#endif
}

/// Verify that a global variable follows invariants.
void SILGlobalVariable::verify() const {
#ifndef NDEBUG
  assert(getLoweredType().isObject()
         && "global variable cannot have address type");

  // Verify the static initializer.
  if (InitializerF)
    assert(SILGlobalVariable::canBeStaticInitializer(InitializerF) &&
           "illegal static initializer");
#endif
}

/// Verify the module.
void SILModule::verify() const {
#ifndef NDEBUG
  // Uniquing set to catch symbol name collisions.
  llvm::StringSet<> symbolNames;

  // Check all functions.
  for (const SILFunction &f : *this) {
    if (!symbolNames.insert(f.getName()).second) {
      llvm::errs() << "Symbol redefined: " << f.getName() << "!\n";
      assert(false && "triggering standard assertion failure routine");
    }
    f.verify();
  }

  // Check all globals.
  for (const SILGlobalVariable &g : getSILGlobals()) {
    if (!symbolNames.insert(g.getName()).second) {
      llvm::errs() << "Symbol redefined: " << g.getName() << "!\n";
      assert(false && "triggering standard assertion failure routine");
    }
    g.verify();
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

  // Check all witness tables.
  DEBUG(llvm::dbgs() << "*** Checking witness tables for duplicates ***\n");
  llvm::DenseSet<NormalProtocolConformance*> wtableConformances;
  for (const SILWitnessTable &wt : getWitnessTables()) {
    DEBUG(llvm::dbgs() << "Witness Table:\n"; wt.dump());
    auto conformance = wt.getConformance();
    if (!wtableConformances.insert(conformance).second) {
      llvm::errs() << "Witness table redefined: ";
      conformance->printName(llvm::errs());
      assert(false && "triggering standard assertion failure routine");
    }
    wt.verify(*this);
  }
#endif
}

#ifndef NDEBUG
/// Determine whether an instruction may not have a SILDebugScope.
bool swift::maybeScopeless(SILInstruction &I) {
  // This list is supposed to get ever shorter as we are tightening
  // the requirements for the optimizer.
  switch (I.getLoc().getKind()) {
  case SILLocation::CleanupKind:
  case SILLocation::ImplicitReturnKind:
  case SILLocation::ArtificialUnreachableKind:
  case SILLocation::SILFileKind:
    return true;
  default: break;
  }
  if (!I.getLoc().isNull() || I.getLoc().isAutoGenerated())
    return true;

  auto *Fn = I.getFunction();
  return Fn->isTransparent() || Fn->isBare();
}
#endif
