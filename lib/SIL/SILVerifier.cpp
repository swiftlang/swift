//===--- Verifier.cpp - Verification of Swift SIL Code --------------------===//
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

#define DEBUG_TYPE "sil-verifier"
#include "TransitivelyUnreachableBlocks.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Range.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/PostOrder.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILOpenedArchetypesTracker.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
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

//===----------------------------------------------------------------------===//
//                                SILVerifier
//===----------------------------------------------------------------------===//

/// Returns true if A is an opened existential type or is equal to an
/// archetype from F's generic context.
static bool isArchetypeValidInFunction(ArchetypeType *A, SILFunction *F) {
  if (!A->getOpenedExistentialType().isNull())
    return true;

  // Find the primary archetype.
  A = A->getPrimary();

  // Ok, we have a primary archetype, make sure it is in the nested generic
  // environment of our caller.
  if (auto *genericEnv = F->getGenericEnvironment())
    if (genericEnv->containsPrimaryArchetype(A))
      return true;

  return false;
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
  ModuleDecl *M;
  const SILFunction &F;
  SILFunctionConventions fnConv;
  Lowering::TypeConverter &TC;
  SILOpenedArchetypesTracker OpenedArchetypes;
  SmallVector<StringRef, 16> DebugVars;
  const SILInstruction *CurInstruction = nullptr;
  DominanceInfo *Dominance = nullptr;
  llvm::Optional<PostOrderFunctionInfo> PostOrderInfo;
  llvm::Optional<TransitivelyUnreachableBlocksInfo> UnreachableBlockInfo;
  bool SingleFunction = true;

  SILVerifier(const SILVerifier&) = delete;
  void operator=(const SILVerifier&) = delete;
public:
  bool isSILOwnershipEnabled() const {
    return F.getModule().getOptions().EnableSILOwnership;
  }

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
    return _requireObjectType<T>(value->getType(), valueDescription, typeName);
  }
#define requireObjectType(type, value, valueDescription) \
  _requireObjectType<type>(value, valueDescription, #type)

  template <class T>
  typename CanTypeWrapperTraits<T>::type
  _forbidObjectType(SILType type, const Twine &valueDescription,
                    const char *typeName) {
    _require(type.isObject(), valueDescription + " must be an object");
    auto result = type.getAs<T>();
    _require(!bool(result),
             valueDescription + " must not have type " + typeName);
    return result;
  }
  template <class T>
  typename CanTypeWrapperTraits<T>::type
  _forbidObjectType(SILValue value, const Twine &valueDescription,
                    const char *typeName) {
    return _forbidObjectType<T>(value->getType(), valueDescription, typeName);
  }
#define forbidObjectType(type, value, valueDescription)                        \
  _forbidObjectType<type>(value, valueDescription, #type)

  // Require that the operand is a non-optional, non-unowned reference-counted
  // type.
  void requireReferenceValue(SILValue value, const Twine &valueDescription) {
    require(value->getType().isObject(), valueDescription +" must be an object");
    require(value->getType().isReferenceCounted(F.getModule()),
            valueDescription + " must have reference semantics");
    forbidObjectType(UnownedStorageType, value, valueDescription);
  }

  // Require that the operand is a reference-counted type, or an Optional
  // thereof.
  void requireReferenceOrOptionalReferenceValue(SILValue value,
                                                const Twine &valueDescription) {
    require(value->getType().isObject(), valueDescription +" must be an object");
    
    auto objectTy = value->getType().unwrapAnyOptionalType();
    
    require(objectTy.isReferenceCounted(F.getModule()),
            valueDescription + " must have reference semantics");
  }
  
  // Require that the operand is a type that supports reference storage
  // modifiers.
  void requireReferenceStorageCapableValue(SILValue value,
                                           const Twine &valueDescription) {
    requireReferenceOrOptionalReferenceValue(value, valueDescription);
    require(!value->getType().is<SILFunctionType>(),
            valueDescription + " cannot apply to a function type");
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
        
        // FIXME: If one or both types are dependent, we can't accurately assess
        // whether they're ABI-compatible without a generic context. We can
        // do a better job here when dependent types are related to their
        // generic signatures.
        if (aa.hasTypeParameter() || bb.hasTypeParameter())
          continue;
        
        // Bridgeable object types are interchangeable.
        if (aa.isBridgeableObjectType() && bb.isBridgeableObjectType())
          continue;
        
        // Optional and IUO are interchangeable if their elements are.
        auto aObject = aa.getAnyOptionalObjectType();
        auto bObject = bb.getAnyOptionalObjectType();
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
        
        // Optional thick metatypes are ABI-interchangeable with non-optionals
        // too.
        if (aObject)
          if (auto aObjMeta = aObject.getAs<MetatypeType>())
            if (auto bMeta = bb.getAs<MetatypeType>())
              if (aObjMeta->getRepresentation() == bMeta->getRepresentation()
                  && bMeta->getRepresentation() != MetatypeRepresentation::Thin)
                continue;
        if (bObject)
          if (auto aMeta = aa.getAs<MetatypeType>())
            if (auto bObjMeta = bObject.getAs<MetatypeType>())
              if (aMeta->getRepresentation() == bObjMeta->getRepresentation()
                  && aMeta->getRepresentation() != MetatypeRepresentation::Thin)
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
    
    // Check the results.
    _require(type1->getNumResults() == type2->getNumResults(), what,
             complain("different number of results"));
    for (unsigned i : indices(type1->getResults())) {
      auto result1 = type1->getResults()[i];
      auto result2 = type2->getResults()[i];

      _require(result1.getConvention() == result2.getConvention(), what,
               complain("Different return value conventions"));
      _require(areABICompatibleParamsOrReturns(result1.getSILStorageType(),
                                               result2.getSILStorageType()),
               what, complain("ABI-incompatible return values"));
    }

    // Our error result conventions are designed to be ABI compatible
    // with functions lacking error results.  Just make sure that the
    // actual conventions match up.
    if (type1->hasErrorResult() && type2->hasErrorResult()) {
      auto error1 = type1->getErrorResult();
      auto error2 = type2->getErrorResult();
      _require(error1.getConvention() == error2.getConvention(), what,
               complain("Different error result conventions"));
      _require(areABICompatibleParamsOrReturns(error1.getSILStorageType(),
                                               error2.getSILStorageType()),
               what, complain("ABI-incompatible error results"));
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
      _require(areABICompatibleParamsOrReturns(param1.getSILStorageType(),
                                               param2.getSILStorageType()),
               what, complainBy([=] {
                 llvm::dbgs() << "ABI-incompatible types for parameter " << i;
               }));
    }
  }

  void requireSameFunctionComponents(CanSILFunctionType type1,
                                     CanSILFunctionType type2,
                                     const Twine &what) {
    require(type1->getNumResults() == type2->getNumResults(),
            "results of " + what + " do not match in count");
    for (auto i : indices(type1->getResults())) {
      require(type1->getResults()[i] == type2->getResults()[i],
              "result " + Twine(i) + " of " + what + " do not match");
    }
    require(type1->getParameters().size() ==
            type2->getParameters().size(),
            "inputs of " + what + " do not match in count");
    for (auto i : indices(type1->getParameters())) {
      require(type1->getParameters()[i] ==
              type2->getParameters()[i],
              "input " + Twine(i) + " of " + what + " do not match");
    }
  }

  SILVerifier(const SILFunction &F, bool SingleFunction = true)
      : M(F.getModule().getSwiftModule()), F(F),
        fnConv(F.getLoweredFunctionType(), F.getModule()),
        TC(F.getModule().Types), OpenedArchetypes(F), Dominance(nullptr),
        PostOrderInfo(), UnreachableBlockInfo(),
        SingleFunction(SingleFunction) {
    if (F.isExternalDeclaration())
      return;
      
    // Check to make sure that all blocks are well formed.  If not, the
    // SILVerifier object will explode trying to compute dominance info.
    for (auto &BB : F) {
      require(!BB.empty(), "Basic blocks cannot be empty");
      require(isa<TermInst>(BB.back()),
              "Basic blocks must end with a terminator instruction");
    }

    Dominance = new DominanceInfo(const_cast<SILFunction *>(&F));
    if (isSILOwnershipEnabled()) {
      PostOrderInfo.emplace(const_cast<SILFunction *>(&F));
      UnreachableBlockInfo.emplace(PostOrderInfo.getValue());
    }

    auto *DebugScope = F.getDebugScope();
    require(DebugScope, "All SIL functions must have a debug scope");
    require(DebugScope->Parent.get<SILFunction *>() == &F,
            "Scope of SIL function points to different function");
  }

  ~SILVerifier() {
    if (Dominance)
      delete Dominance;
  }

  void visitSILArgument(SILArgument *arg) {
    checkLegalType(arg->getFunction(), arg, nullptr);
    checkValueBaseOwnership(arg);
  }

  void visitSILInstruction(SILInstruction *I) {
    CurInstruction = I;
    OpenedArchetypes.registerOpenedArchetypes(I);
    checkSILInstruction(I);

    // Check the SILLLocation attached to the instruction.
    checkInstructionsSILLocation(I);

    checkLegalType(I->getFunction(), I, I);

    // Check ownership.
    SILFunction *F = I->getFunction();
    assert(F && "Expected value base with parent function");

    // Check ownership.
    checkValueBaseOwnership(I);
  }

  void checkValueBaseOwnership(ValueBase *V) {
    // If ownership is not enabled, bail.
    if (!isSILOwnershipEnabled())
      return;

    // If V does not have a value, bail.
    if (!V->hasValue())
      return;

    SILFunction *F = V->getFunction();
    assert(F && "Expected value base with parent function");
    // If we do not have qualified ownership, then do not verify value base
    // ownership.
    if (!F->hasQualifiedOwnership())
      return;
    SILValue(V).verifyOwnership(F->getModule(),
                                &UnreachableBlockInfo.getValue());
  }

  void checkSILInstruction(SILInstruction *I) {
    const SILBasicBlock *BB = I->getParent();
    require(BB, "Instruction with null parent");
    require(I->getFunction(), "Instruction not in function");

    // Check that non-terminators look ok.
    if (!isa<TermInst>(I)) {
      require(!BB->empty(), "Can't be in a parent block if it is empty");
      require(&*BB->rbegin() != I,
              "Non-terminators cannot be the last in a block");
    } else {
      // Skip the check for UnreachableInst, if explicitly asked to do so.
      if (!isa<UnreachableInst>(I) || !SkipUnreachableMustBeLastErrors)
        require(&*BB->rbegin() == I,
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
      require(operand.get(), "instruction has null operand");

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

      if (I->isTypeDependentOperand(operand)) {
        require(isa<SILInstruction>(I),
               "opened archetype operand should refer to a SILInstruction");
      }

      // Make sure that if operand is generic that its primary archetypes match
      // the function context.
      checkLegalType(I->getFunction(), operand.get(), I);
    }

    // TODO: There should be a use of an opened archetype inside the instruction for
    // each opened archetype operand of the instruction.
  }

  void checkInstructionsSILLocation(SILInstruction *I) {
    // Check the debug scope.
    auto *DS = I->getDebugScope();
    if (DS && !maybeScopeless(*I))
      require(DS, "instruction has a location, but no scope");

    require(!DS || DS->getParentFunction() == I->getFunction(),
            "debug scope of instruction belongs to a different function");

    // Check the location kind.
    SILLocation L = I->getLoc();
    SILLocation::LocationKind LocKind = L.getKind();
    ValueKind InstKind = I->getKind();

    // Check that there is at most one debug variable defined
    // for each argument slot. This catches SIL transformations
    // that accidentally remove inline information (stored in the SILDebugScope)
    // from debug-variable-carrying instructions.
    if (!DS->InlinedCallSite) {
      SILDebugVariable VarInfo;
      if (auto *DI = dyn_cast<AllocStackInst>(I)) {
        VarInfo = DI->getVarInfo();
      } else if (auto *DI = dyn_cast<AllocBoxInst>(I)) {
        VarInfo = DI->getVarInfo();
      } else if (auto *DI = dyn_cast<DebugValueInst>(I)) {
        VarInfo = DI->getVarInfo();
      } else if (auto *DI = dyn_cast<DebugValueAddrInst>(I)) {
        VarInfo = DI->getVarInfo();
      }

      if (unsigned ArgNo = VarInfo.ArgNo) {
        // It is a function argument.
        if (ArgNo < DebugVars.size() && !DebugVars[ArgNo].empty()) {
          require(DebugVars[ArgNo] == VarInfo.Name,
                  "Scope contains conflicting debug variables for one function "
                  "argument");
        } else {
          // Reserve enough space.
          while (DebugVars.size() <= ArgNo) {
            DebugVars.push_back(StringRef());
          }
        }
        DebugVars[ArgNo] = VarInfo.Name;
      }
    }

    // Regular locations are allowed on all instructions.
    if (LocKind == SILLocation::RegularKind)
      return;

#if 0
    // FIXME: This check was tautological before the removal of
    // AutoreleaseReturnInst, and it turns out that we're violating it.
    // Fix incoming.
    if (LocKind == SILLocation::CleanupKind ||
        LocKind == SILLocation::InlinedKind)
      require(InstKind != ValueKind::ReturnInst ||
              InstKind != ValueKind::AutoreleaseReturnInst,
        "cleanup and inlined locations are not allowed on return instructions");
#endif

    if (LocKind == SILLocation::ReturnKind ||
        LocKind == SILLocation::ImplicitReturnKind)
      require(InstKind == ValueKind::BranchInst ||
              InstKind == ValueKind::ReturnInst ||
              InstKind == ValueKind::UnreachableInst,
        "return locations are only allowed on branch and return instructions");

    if (LocKind == SILLocation::ArtificialUnreachableKind)
      require(InstKind == ValueKind::UnreachableInst,
        "artificial locations are only allowed on Unreachable instructions");
  }

  /// Check that the types of this value producer are all legal in the function
  /// context in which it exists.
  void checkLegalType(SILFunction *F, ValueBase *value, SILInstruction *I) {
    if (SILType type = value->getType()) {
      checkLegalType(F, type, I);
    }
  }

  /// Check that the given type is a legal SIL value type.
  void checkLegalType(SILFunction *F, SILType type, SILInstruction *I) {
    checkLegalSILType(F, type.getSwiftRValueType(), I);
  }

  /// Check that the given type is a legal SIL value type.
  void checkLegalSILType(SILFunction *F, CanType rvalueType, SILInstruction *I) {
    // These types should have been removed by lowering.
    require(!isa<LValueType>(rvalueType),
            "l-value types are not legal in SIL");
    require(!isa<AnyFunctionType>(rvalueType),
            "AST function types are not legal in SIL");

    // Tuples should have had their element lowered.
    if (auto tuple = dyn_cast<TupleType>(rvalueType)) {
      for (auto eltTy : tuple.getElementTypes()) {
        checkLegalSILType(F, eltTy, I);
      }
      return;
    }

    // Optionals should have had their objects lowered.
    OptionalTypeKind optKind;
    if (auto objectType = rvalueType.getAnyOptionalObjectType(optKind)) {
      require(optKind == OTK_Optional,
              "ImplicitlyUnwrappedOptional is not legal in SIL values");
      return checkLegalSILType(F, objectType, I);
    }

    // Metatypes should have explicit representations.
    if (auto metatype = dyn_cast<AnyMetatypeType>(rvalueType)) {
      require(metatype->hasRepresentation(),
              "metatypes in SIL must have a representation");;
      // fallthrough for archetype check
    }

    rvalueType.visit([&](Type t) {
      auto *A = dyn_cast<ArchetypeType>(t.getPointer());
      if (!A)
        return;
      require(isArchetypeValidInFunction(A, F),
              "Operand is of an ArchetypeType that does not exist in the "
              "Caller's generic param list.");
      if (auto OpenedA = getOpenedArchetypeOf(CanType(A))) {
        auto Def = OpenedArchetypes.getOpenedArchetypeDef(OpenedA);
        require (Def, "Opened archetype should be registered in SILFunction");
        require(I == nullptr || Def == I ||
                Dominance->properlyDominates(cast<SILInstruction>(Def), I),
                "Use of an opened archetype should be dominated by a "
                "definition of this opened archetype");
      }
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
    require(AI->getType().isAddress(),
            "result of alloc_stack must be an address type");

    verifyOpenedArchetype(AI, AI->getElementType().getSwiftRValueType());

    // Scan the parent block of AI and check that the users of AI inside this
    // block are inside the lifetime of the allocated memory.
    SILBasicBlock *SBB = AI->getParent();
    bool Allocated = true;
    for (auto Inst = AI->getIterator(), E = SBB->end(); Inst != E; ++Inst) {
      if (LoadInst *LI = dyn_cast<LoadInst>(Inst))
        if (LI->getOperand() == AI)
          require(Allocated, "AllocStack used by Load outside its lifetime");

      if (StoreInst *SI = dyn_cast<StoreInst>(Inst))
        if (SI->getDest() == AI)
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

  void checkAllocRefBase(AllocRefInstBase *ARI) {
    requireReferenceValue(ARI, "Result of alloc_ref");
    verifyOpenedArchetype(ARI, ARI->getType().getSwiftRValueType());
    auto Types = ARI->getTailAllocatedTypes();
    auto Counts = ARI->getTailAllocatedCounts();
    unsigned NumTypes = Types.size();
    require(NumTypes == Counts.size(), "Mismatching types and counts");
    require(NumTypes == 0 || !ARI->isObjC(),
            "Can't tail allocate with ObjC class");
    for (unsigned Idx = 0; Idx < NumTypes; ++Idx) {
      verifyOpenedArchetype(ARI, Types[Idx].getSwiftRValueType());
      require(Counts[Idx].get()->getType().is<BuiltinIntegerType>(),
              "count needs integer type");
    }
  }

  void checkAllocRefInst(AllocRefInst *AI) {
    require(AI->isObjC() || AI->getType().getClassOrBoundGenericClass(),
            "alloc_ref must allocate class");
    checkAllocRefBase(AI);
  }

  void checkAllocRefDynamicInst(AllocRefDynamicInst *ARDI) {
    SILValue Metadata = ARDI->getMetatypeOperand();
    require(Metadata->getType().is<AnyMetatypeType>(),
            "operand of alloc_ref_dynamic must be of metatype type");
    auto metaTy = Metadata->getType().castTo<AnyMetatypeType>();
    require(metaTy->hasRepresentation(),
            "operand of alloc_ref_dynamic must have a metatype representation");
    if (ARDI->isObjC()) {
      require(metaTy->getRepresentation() == MetatypeRepresentation::ObjC,
              "alloc_ref_dynamic @objc requires operand of ObjC metatype");
    } else {
      require(metaTy->getRepresentation() == MetatypeRepresentation::Thick,
              "alloc_ref_dynamic requires operand of thick metatype");
    }
    checkAllocRefBase(ARDI);
  }

  /// Check the substitutions passed to an apply or partial_apply.
  CanSILFunctionType checkApplySubstitutions(SubstitutionList subs,
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
    return fnTy->substGenericArgs(F.getModule(), subs);
  }

  /// Check that for each opened archetype or dynamic self type in substitutions
  /// or the calle type, there is a type dependent operand.
  void checkApplyTypeDependentArguments(ApplySite AS) {
    SILInstruction *AI = AS.getInstruction();

    llvm::DenseSet<ArchetypeType *> FoundOpenedArchetypes;
    unsigned hasDynamicSelf = 0;

    // Function to collect opened archetypes in FoundOpenedArchetypes and set
    // hasDynamicSelf.
    auto HandleType = [&](Type Ty) {
      if (Ty->isOpenedExistential()) {
        auto *A = Ty->getAs<ArchetypeType>();
        require(isArchetypeValidInFunction(A, AI->getFunction()),
                "Archetype to be substituted must be valid in function.");
        // Collect all opened archetypes used in the substitutions list.
        FoundOpenedArchetypes.insert(A);
        // Also check that they are properly tracked inside the current
        // function.
        auto Def =
          OpenedArchetypes.getOpenedArchetypeDef(A);
        require(Def, "Opened archetype should be registered in SILFunction");
        require(Def == AI ||
                Dominance->properlyDominates(cast<SILInstruction>(Def), AI),
                "Use of an opened archetype should be dominated by a "
                "definition of this opened archetype");
      }
      if (Ty->hasDynamicSelfType()) {
        hasDynamicSelf = 1;
      }
    };

    // Search for opened archetypes and dynamic self.
    for (auto &Sub : AS.getSubstitutions()) {
      Sub.getReplacement().visit(HandleType);
    }
    AS.getSubstCalleeType().visit(HandleType);

    require(FoundOpenedArchetypes.size() + hasDynamicSelf ==
                AI->getTypeDependentOperands().size(),
            "Number of opened archetypes and dynamic self in the substitutions "
            "list should match the number of type dependent operands");

    for (auto &Op : AI->getTypeDependentOperands()) {
      auto V = Op.get();
      if (isa<SILArgument>(V)) {
        require(hasDynamicSelf,
                "dynamic self operand without dynamic self type");
        require(AI->getFunction()->hasSelfMetadataParam(),
                "self metadata operand in function without self metadata param");
        require((ValueBase *)V == AI->getFunction()->getSelfMetadataArgument(),
                "wrong self metadata operand");
      } else {
        require(isa<SILInstruction>(V),
                "opened archetype operand should refer to a SIL instruction");
        auto Archetype = getOpenedArchetypeOf(cast<SILInstruction>(V));
        require(Archetype,
                "opened archetype operand should define an opened archetype");
        require(FoundOpenedArchetypes.count(Archetype),
                "opened archetype operand does not correspond to any opened "
                "archetype from the substitutions list");
      }
    }
  }

  void checkFullApplySite(FullApplySite site) {
    checkApplyTypeDependentArguments(site);

    // Then make sure that we have a type that can be substituted for the
    // callee.
    auto substTy = checkApplySubstitutions(site.getSubstitutions(),
                                           site.getCallee()->getType());
    require(site.getOrigCalleeType()->getRepresentation() ==
            site.getSubstCalleeType()->getRepresentation(),
            "calling convention difference between types");

    require(!site.getSubstCalleeType()->isPolymorphic(),
            "substituted callee type should not be generic");

    requireSameType(SILType::getPrimitiveObjectType(substTy),
                    SILType::getPrimitiveObjectType(site.getSubstCalleeType()),
            "substituted callee type does not match substitutions");

    // Check that the arguments and result match.
    SILFunctionConventions substConv(substTy, F.getModule());
    //require(site.getArguments().size() == substTy->getNumSILArguments(),
    require(site.getNumCallArguments() == substConv.getNumSILArguments(),
            "apply doesn't have right number of arguments for function");
    for (size_t i = 0, size = site.getNumCallArguments(); i < size; ++i) {
      requireSameType(site.getArguments()[i]->getType(),
                      substConv.getSILArgumentType(i),
                      "operand of 'apply' doesn't match function input type");
    }
  }

  void checkApplyInst(ApplyInst *AI) {
    checkFullApplySite(AI);

    SILFunctionConventions calleeConv(AI->getSubstCalleeType(), F.getModule());
    require(AI->getType() == calleeConv.getSILResultType(),
            "type of apply instruction doesn't match function result type");
    if (AI->isNonThrowing()) {
      require(calleeConv.funcTy->hasErrorResult(),
              "nothrow flag used for callee without error result");
    } else {
      require(!calleeConv.funcTy->hasErrorResult(),
              "apply instruction cannot call function with error result");
    }

    // Check that if the apply is of a noreturn callee, make sure that an
    // unreachable is the next instruction.
    if (AI->getModule().getStage() == SILStage::Raw ||
        !AI->isCalleeNoReturn())
      return;
    require(isa<UnreachableInst>(std::next(SILBasicBlock::iterator(AI))),
            "No return apply without an unreachable as a next instruction.");
  }

  void checkTryApplyInst(TryApplyInst *AI) {
    checkFullApplySite(AI);

    SILFunctionConventions calleeConv(AI->getSubstCalleeType(), F.getModule());

    auto normalBB = AI->getNormalBB();
    require(normalBB->args_size() == 1,
            "normal destination of try_apply must take one argument");
    requireSameType((*normalBB->args_begin())->getType(),
                    calleeConv.getSILResultType(),
                    "normal destination of try_apply must take argument "
                    "of normal result type");

    auto errorBB = AI->getErrorBB();
    require(calleeConv.funcTy->hasErrorResult(),
            "try_apply must call function with error result");
    require(errorBB->args_size() == 1,
            "error destination of try_apply must take one argument");
    requireSameType((*errorBB->args_begin())->getType(),
                    calleeConv.getSILErrorType(),
                    "error destination of try_apply must take argument "
                    "of error result type");
  }

  void verifyLLVMIntrinsic(BuiltinInst *BI, llvm::Intrinsic::ID ID) {
    // Certain llvm intrinsic require constant values as their operands.
    // Consequently, these must not be phi nodes (aka. basic block arguments).
    switch (ID) {
    default:
      break;
    case llvm::Intrinsic::ctlz: // llvm.ctlz
    case llvm::Intrinsic::cttz: // llvm.cttz
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

    checkApplyTypeDependentArguments(PAI);

    auto substTy = checkApplySubstitutions(PAI->getSubstitutions(),
                                        PAI->getCallee()->getType());

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

    SILFunctionConventions substConv(substTy, F.getModule());
    unsigned appliedArgStartIdx =
        substConv.getNumSILArguments() - PAI->getNumArguments();
    for (unsigned i = 0, size = PAI->getArguments().size(); i < size; ++i) {
      require(PAI->getArguments()[i]->getType()
                  == substConv.getSILArgumentType(appliedArgStartIdx + i),
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

    require(resultInfo->getNumResults() == substTy->getNumResults(),
            "applied results do not agree in count with function type");
    for (unsigned i = 0, size = resultInfo->getNumResults(); i < size; ++i) {
      auto originalResult = resultInfo->getResults()[i];
      auto expectedResult = substTy->getResults()[i];

      // The "returns inner pointer" convention doesn't survive through a
      // partial application, since the thunk takes responsibility for
      // lifetime-extending 'self'.
      if (expectedResult.getConvention()
            == ResultConvention::UnownedInnerPointer) {
        expectedResult = SILResultInfo(expectedResult.getType(),
                                       ResultConvention::Unowned);
        require(originalResult == expectedResult,
                "result type of result function type for partially applied "
                "@unowned_inner_pointer function should have @unowned"
                "convention");

      // The "autoreleased" convention doesn't survive through a
      // partial application, since the thunk takes responsibility for
      // retaining the return value.
      } else if (expectedResult.getConvention()
            == ResultConvention::Autoreleased) {
        expectedResult = SILResultInfo(expectedResult.getType(),
                                       ResultConvention::Owned);
        require(originalResult == expectedResult,
                "result type of result function type for partially applied "
                "@autoreleased function should have @owned convention");

      } else {
        require(originalResult == expectedResult,
                "result type of result function type does not match original "
                "function");
      }
    }
    
    // TODO: Impose additional constraints when partial_apply when the
    // -disable-sil-partial-apply flag is enabled. We want to reduce
    // partial_apply to being only a means of associating a closure invocation
    // function with its context.
    //
    // When we reach that point, we should be able to more deeply redesign
    // PartialApplyInst to simplify the representation to carry a single
    // argument.
    if (PAI->getModule().getOptions().DisableSILPartialApply) {
      // Should only be one context argument.
      require(PAI->getArguments().size() == 1,
              "partial_apply should have a single context argument");
      
      // Callee should already have the thin convention, and result should be
      // thick.
      require(resultInfo->getRepresentation() ==
                SILFunctionTypeRepresentation::Thick,
              "partial_apply result should have thick convention");
      require(PAI->getCallee()->getType().castTo<SILFunctionType>()
                ->getRepresentation() ==
              SILFunctionTypeRepresentation::Thin,
              "partial_apply callee should have thin convention");
      
      // TODO: Check that generic signature matches box's generic signature,
      // once we have boxes with generic signatures.
      require(!PAI->getCalleeFunction()->getGenericEnvironment(),
              "partial_apply context must capture generic environment for "
              "callee");
      
      // Result's callee convention should match context argument's convention.
      require(substTy->getParameters().back().getConvention()
                == resultInfo->getCalleeConvention(),
              "partial_apply context argument must have the same convention "
              "as the resulting function's callee convention");
      
      auto isSwiftRefcounted = [](SILType t) -> bool {
        if (t.is<SILBoxType>())
          return true;
        if (t.getSwiftRValueType() == t.getASTContext().TheNativeObjectType)
          return true;
        if (auto clas = t.getClassOrBoundGenericClass())
          // Must be a class defined in Swift.
          return clas->hasKnownSwiftImplementation();
        return false;
      };
      
      // The context argument must be a swift-refcounted box or class.
      require(isSwiftRefcounted(PAI->getArguments().front()->getType()),
              "partial_apply context argument must be swift-refcounted");
    }
  }

  void checkBuiltinInst(BuiltinInst *BI) {
    // Check for special constraints on llvm intrinsics.
    if (BI->getIntrinsicInfo().ID != llvm::Intrinsic::not_intrinsic)
      verifyLLVMIntrinsic(BI, BI->getIntrinsicInfo().ID);
  }
  
  void checkFunctionRefInst(FunctionRefInst *FRI) {
    auto fnType = requireObjectType(SILFunctionType, FRI,
                                    "result of function_ref");
    require(!fnType->getExtInfo().hasContext(),
            "function_ref should have a context-free function result");
    if (F.isFragile()) {
      SILFunction *RefF = FRI->getReferencedFunction();
      require((SingleFunction && RefF->isExternalDeclaration()) ||
              RefF->hasValidLinkageForFragileRef(),
              "function_ref inside fragile function cannot "
              "reference a private or hidden symbol");
    }
    verifySILFunctionType(fnType);
  }

  void checkAllocGlobalInst(AllocGlobalInst *AGI) {
    if (F.isFragile()) {
      SILGlobalVariable *RefG = AGI->getReferencedGlobal();
      require(RefG->isFragile()
                || hasPublicVisibility(RefG->getLinkage()),
              "alloc_global inside fragile function cannot "
              "reference a private or hidden symbol");
    }
  }

  void checkGlobalAddrInst(GlobalAddrInst *GAI) {
    require(GAI->getType().isAddress(),
            "global_addr must have an address result type");
    require(GAI->getType().getObjectType() ==
              GAI->getReferencedGlobal()->getLoweredType(),
            "global_addr must be the address type of the variable it "
            "references");
    if (F.isFragile()) {
      SILGlobalVariable *RefG = GAI->getReferencedGlobal();
      require(RefG->isFragile()
                || hasPublicVisibility(RefG->getLinkage()),
              "global_addr inside fragile function cannot "
              "reference a private or hidden symbol");
    }
  }

  void checkIntegerLiteralInst(IntegerLiteralInst *ILI) {
    require(ILI->getType().is<BuiltinIntegerType>(),
            "invalid integer literal type");
  }

  void checkLoadInst(LoadInst *LI) {
    require(LI->getType().isObject(), "Result of load must be an object");
    require(!fnConv.useLoweredAddresses()
                || LI->getType().isLoadable(LI->getModule()),
            "Load must have a loadable type");
    require(LI->getOperand()->getType().isAddress(),
            "Load operand must be an address");
    require(LI->getOperand()->getType().getObjectType() == LI->getType(),
            "Load operand type and result type mismatch");

    // Ownership semantic checks.
    switch (LI->getOwnershipQualifier()) {
    case LoadOwnershipQualifier::Unqualified:
      // We should not see loads with unqualified ownership when SILOwnership is
      // enabled.
      require(F.hasUnqualifiedOwnership(),
              "Load with unqualified ownership in a qualified function");
      break;
    case LoadOwnershipQualifier::Copy:
    case LoadOwnershipQualifier::Take:
      require(F.hasQualifiedOwnership(),
              "Load with qualified ownership in an unqualified function");
      // TODO: Could probably make this a bit stricter.
      require(!LI->getType().isTrivial(LI->getModule()),
              "load [copy] or load [take] can only be applied to non-trivial "
              "types");
      break;
    case LoadOwnershipQualifier::Trivial:
      require(F.hasQualifiedOwnership(),
              "Load with qualified ownership in an unqualified function");
      require(LI->getType().isTrivial(LI->getModule()),
              "A load with trivial ownership must load a trivial type");
      break;
    }
  }

  void checkLoadBorrowInst(LoadBorrowInst *LBI) {
    require(
        F.hasQualifiedOwnership(),
        "Inst with qualified ownership in a function that is not qualified");
    require(LBI->getType().isObject(), "Result of load must be an object");
    require(LBI->getType().isLoadable(LBI->getModule()),
            "Load must have a loadable type");
    require(LBI->getOperand()->getType().isAddress(),
            "Load operand must be an address");
    require(LBI->getOperand()->getType().getObjectType() == LBI->getType(),
            "Load operand type and result type mismatch");
  }

  void checkEndBorrowInst(EndBorrowInst *EBI) {
    require(
        F.hasQualifiedOwnership(),
        "Inst with qualified ownership in a function that is not qualified");
    // We allow for end_borrow to express relationships in between addresses and
    // values, but we require that the types are the same ignoring value
    // category.
    require(EBI->getBorrowedValue()->getType().getObjectType() ==
                EBI->getOriginalValue()->getType().getObjectType(),
            "end_borrow can only relate the same types ignoring value "
            "category");
  }

  void checkStoreInst(StoreInst *SI) {
    require(SI->getSrc()->getType().isObject(),
            "Can't store from an address source");
    require(!fnConv.useLoweredAddresses()
                || SI->getSrc()->getType().isLoadable(SI->getModule()),
            "Can't store a non loadable type");
    require(SI->getDest()->getType().isAddress(),
            "Must store to an address dest");
    require(SI->getDest()->getType().getObjectType() == SI->getSrc()->getType(),
            "Store operand type and dest type mismatch");

    // Perform ownership checks.
    switch (SI->getOwnershipQualifier()) {
    case StoreOwnershipQualifier::Unqualified:
      // We should not see loads with unqualified ownership when SILOwnership is
      // enabled.
      require(F.hasUnqualifiedOwnership(),
              "Qualified store in function with unqualified ownership?!");
      break;
    case StoreOwnershipQualifier::Init:
    case StoreOwnershipQualifier::Assign:
      require(
          F.hasQualifiedOwnership(),
          "Inst with qualified ownership in a function that is not qualified");
      // TODO: Could probably make this a bit stricter.
      require(!SI->getSrc()->getType().isTrivial(SI->getModule()),
              "store [init] or store [assign] can only be applied to "
              "non-trivial types");
      break;
    case StoreOwnershipQualifier::Trivial:
      require(
          F.hasQualifiedOwnership(),
          "Inst with qualified ownership in a function that is not qualified");
      require(SI->getSrc()->getType().isTrivial(SI->getModule()),
              "A store with trivial ownership must store a trivial type");
      break;
    }
  }

  void checkAssignInst(AssignInst *AI) {
    SILValue Src = AI->getSrc(), Dest = AI->getDest();
    require(AI->getModule().getStage() == SILStage::Raw,
            "assign instruction can only exist in raw SIL");
    require(Src->getType().isObject(), "Can't assign from an address source");
    require(Dest->getType().isAddress(), "Must store to an address dest");
    require(Dest->getType().getObjectType() == Src->getType(),
            "Store operand type and dest type mismatch");
  }

  void checkLoadUnownedInst(LoadUnownedInst *LUI) {
    require(LUI->getType().isObject(), "Result of load must be an object");
    auto PointerType = LUI->getOperand()->getType();
    auto PointerRVType = PointerType.getSwiftRValueType();
    require(PointerType.isAddress() &&
            PointerRVType->is<UnownedStorageType>(),
            "load_unowned operand must be an unowned address");
    require(PointerRVType->getReferenceStorageReferent()->getCanonicalType() ==
            LUI->getType().getSwiftType(),
            "Load operand type and result type mismatch");
  }

  void checkStoreUnownedInst(StoreUnownedInst *SUI) {
    require(SUI->getSrc()->getType().isObject(),
            "Can't store from an address source");
    auto PointerType = SUI->getDest()->getType();
    auto PointerRVType = PointerType.getSwiftRValueType();
    require(PointerType.isAddress() &&
            PointerRVType->is<UnownedStorageType>(),
            "store_unowned address operand must be an unowned address");
    require(PointerRVType->getReferenceStorageReferent()->getCanonicalType() ==
            SUI->getSrc()->getType().getSwiftType(),
            "Store operand type and dest type mismatch");
  }

  void checkLoadWeakInst(LoadWeakInst *LWI) {
    require(LWI->getType().isObject(), "Result of load must be an object");
    require(LWI->getType().getSwiftType()->getAnyOptionalObjectType(),
            "Result of weak load must be an optional");
    auto PointerType = LWI->getOperand()->getType();
    auto PointerRVType = PointerType.getSwiftRValueType();
    require(PointerType.isAddress() &&
            PointerRVType->is<WeakStorageType>(),
            "load_weak operand must be a weak address");
    require(PointerRVType->getReferenceStorageReferent()->getCanonicalType() ==
            LWI->getType().getSwiftType(),
            "Load operand type and result type mismatch");
  }

  void checkStoreWeakInst(StoreWeakInst *SWI) {
    require(SWI->getSrc()->getType().isObject(),
            "Can't store from an address source");
    require(SWI->getSrc()->getType().getSwiftType()->getAnyOptionalObjectType(),
            "store_weak must be of an optional value");
    auto PointerType = SWI->getDest()->getType();
    auto PointerRVType = PointerType.getSwiftRValueType();
    require(PointerType.isAddress() &&
            PointerRVType->is<WeakStorageType>(),
            "store_weak address operand must be a weak address");
    require(PointerRVType->getReferenceStorageReferent()->getCanonicalType() ==
            SWI->getSrc()->getType().getSwiftType(),
            "Store operand type and dest type mismatch");
  }

  void checkMarkUninitializedInst(MarkUninitializedInst *MU) {
    SILValue Src = MU->getOperand();
    require(MU->getModule().getStage() == SILStage::Raw,
            "mark_uninitialized instruction can only exist in raw SIL");
    require(Src->getType().isAddress() ||
            Src->getType().getSwiftRValueType()->getClassOrBoundGenericClass(),
            "mark_uninitialized must be an address or class");
    require(Src->getType() == MU->getType(),"operand and result type mismatch");
  }
  
  void checkMarkUninitializedBehaviorInst(MarkUninitializedBehaviorInst *MU) {
    require(MU->getModule().getStage() == SILStage::Raw,
            "mark_uninitialized instruction can only exist in raw SIL");
    auto InitStorage = MU->getInitStorageFunc();
    auto InitStorageTy = InitStorage->getType().getAs<SILFunctionType>();
    require(InitStorageTy,
            "mark_uninitialized initializer must be a function");
    auto SubstInitStorageTy = InitStorageTy->substGenericArgs(F.getModule(),
                                             MU->getInitStorageSubstitutions());
    // FIXME: Destructured value or results?
    require(SubstInitStorageTy->getResults().size() == 1,
            "mark_uninitialized initializer must have one result");
    auto StorageTy = SILType::getPrimitiveAddressType(
                              SubstInitStorageTy->getSingleResult().getType());
    requireSameType(StorageTy, MU->getStorage()->getType(),
                    "storage must be address of initializer's result type");
    
    auto Setter = MU->getSetterFunc();
    auto SetterTy = Setter->getType().getAs<SILFunctionType>();
    require(SetterTy,
            "mark_uninitialized setter must be a function");
    auto SubstSetterTy = SetterTy->substGenericArgs(F.getModule(),
                                               MU->getSetterSubstitutions());
    require(SubstSetterTy->getParameters().size() == 2,
            "mark_uninitialized setter must have a value and self param");
    requireSameType(fnConv.getSILType(SubstSetterTy->getSelfParameter()),
                    MU->getSelf()->getType(),
                    "self type must match setter's self parameter type");

    auto ValueTy = SubstInitStorageTy->getParameters()[0].getType();
    requireSameType(SILType::getPrimitiveAddressType(ValueTy),
                    SILType::getPrimitiveAddressType(
                      SubstSetterTy->getParameters()[0].getType()),
                    "value parameter type must match between initializer "
                    "and setter");
    
    auto ValueAddrTy = SILType::getPrimitiveAddressType(ValueTy);
    requireSameType(ValueAddrTy, MU->getType(),
                    "result of mark_uninitialized_behavior should be address "
                    "of value parameter to setter and initializer");
  }
  
  void checkMarkFunctionEscapeInst(MarkFunctionEscapeInst *MFE) {
    require(MFE->getModule().getStage() == SILStage::Raw,
            "mark_function_escape instruction can only exist in raw SIL");
    for (auto Elt : MFE->getElements())
      require(Elt->getType().isAddress(), "MFE must refer to variable addrs");
  }

  void checkCopyAddrInst(CopyAddrInst *SI) {
    require(SI->getSrc()->getType().isAddress(),
            "Src value should be lvalue");
    require(SI->getDest()->getType().isAddress(),
            "Dest address should be lvalue");
    require(SI->getDest()->getType() == SI->getSrc()->getType(),
            "Store operand type and dest type mismatch");
  }

  void checkRetainValueInst(RetainValueInst *I) {
    require(I->getOperand()->getType().isObject(),
            "Source value should be an object value");
    require(F.hasUnqualifiedOwnership(),
            "retain_value is only in functions with unqualified ownership");
  }

  void checkCopyValueInst(CopyValueInst *I) {
    require(I->getOperand()->getType().isObject(),
            "Source value should be an object value");
    require(!fnConv.useLoweredAddresses() || F.hasQualifiedOwnership(),
            "copy_value is only valid in functions with qualified "
            "ownership");
  }

  void checkCopyUnownedValueInst(CopyUnownedValueInst *I) {
    auto unownedType = requireObjectType(UnownedStorageType, I->getOperand(),
                                         "Operand of unowned_retain");
    require(unownedType->isLoadable(ResilienceExpansion::Maximal),
            "unowned_retain requires unowned type to be loadable");
    require(F.hasQualifiedOwnership(),
            "copy_unowned_value is only valid in functions with qualified "
            "ownership");
  }

  void checkDestroyValueInst(DestroyValueInst *I) {
    require(I->getOperand()->getType().isObject(),
            "Source value should be an object value");
    require(!fnConv.useLoweredAddresses() || F.hasQualifiedOwnership(),
            "destroy_value is only valid in functions with qualified "
            "ownership");
  }

  void checkReleaseValueInst(ReleaseValueInst *I) {
    require(I->getOperand()->getType().isObject(),
            "Source value should be an object value");
    require(F.hasUnqualifiedOwnership(),
            "release_value is only in functions with unqualified ownership");
  }
  
  void checkAutoreleaseValueInst(AutoreleaseValueInst *I) {
    require(I->getOperand()->getType().isObject(),
            "Source value should be an object value");
    // TODO: This instruction could in principle be generalized.
    require(I->getOperand()->getType().hasRetainablePointerRepresentation(),
            "Source value must be a reference type or optional thereof");
  }
  
  void checkSetDeallocatingInst(SetDeallocatingInst *I) {
    require(I->getOperand()->getType().isObject(),
            "Source value should be an object value");
    require(I->getOperand()->getType().hasRetainablePointerRepresentation(),
            "Source value must be a reference type");
  }
  
  void checkCopyBlockInst(CopyBlockInst *I) {
    require(I->getOperand()->getType().isBlockPointerCompatible(),
            "operand of copy_block should be a block");
    require(I->getOperand()->getType() == I->getType(),
            "result of copy_block should be same type as operand");
  }

  void checkAllocValueBufferInst(AllocValueBufferInst *I) {
    require(I->getOperand()->getType().isAddress(),
            "Operand value should be an address");
    require(I->getOperand()->getType().is<BuiltinUnsafeValueBufferType>(),
            "Operand value should be a Builtin.UnsafeValueBuffer");
    verifyOpenedArchetype(I, I->getValueType().getSwiftRValueType());
  }

  void checkProjectValueBufferInst(ProjectValueBufferInst *I) {
    require(I->getOperand()->getType().isAddress(),
            "Operand value should be an address");
    require(I->getOperand()->getType().is<BuiltinUnsafeValueBufferType>(),
            "Operand value should be a Builtin.UnsafeValueBuffer");
  }

  void checkProjectBoxInst(ProjectBoxInst *I) {
    require(I->getOperand()->getType().isObject(),
            "project_box operand should be a value");
    auto boxTy = I->getOperand()->getType().getAs<SILBoxType>();
    require(boxTy, "project_box operand should be a @box type");
    require(I->getType() == boxTy->getFieldType(F.getModule(),
                                                I->getFieldIndex()),
            "project_box result should be address of boxed type");
  }

  void checkProjectExistentialBoxInst(ProjectExistentialBoxInst *PEBI) {
    SILType operandType = PEBI->getOperand()->getType();
    require(operandType.isObject(),
            "project_existential_box operand must not be address");

    require(operandType.canUseExistentialRepresentation(F.getModule(),
                                              ExistentialRepresentation::Boxed),
            "project_existential_box operand must be boxed existential");

    require(PEBI->getType().isAddress(),
            "project_existential_box result must be an address");

    if (auto *AEBI = dyn_cast<AllocExistentialBoxInst>(PEBI->getOperand())) {
      // The lowered type must be the properly-abstracted form of the AST type.
      SILType exType = AEBI->getExistentialType();
      auto archetype = ArchetypeType::getOpened(exType.getSwiftRValueType());

      auto loweredTy = F.getModule().Types.getLoweredType(
                                      Lowering::AbstractionPattern(archetype),
                                      AEBI->getFormalConcreteType())
                                        .getAddressType();

      requireSameType(loweredTy, PEBI->getType(),
              "project_existential_box result should be the lowered "
              "concrete type of its alloc_existential_box");
    }
  }
  
  void checkDeallocValueBufferInst(DeallocValueBufferInst *I) {
    require(I->getOperand()->getType().isAddress(),
            "Operand value should be an address");
    require(I->getOperand()->getType().is<BuiltinUnsafeValueBufferType>(),
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
      require((*opi)->getType() == loweredType,
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
    require(UI->hasOperand() == !!UI->getElement()->getArgumentInterfaceType(),
            "EnumInst must take an argument iff the element does");

    if (UI->getElement()->getArgumentInterfaceType()) {
      require(UI->getOperand()->getType().isObject(),
              "EnumInst operand must be an object");
      SILType caseTy = UI->getType().getEnumElementType(UI->getElement(),
                                                        F.getModule());
      require(caseTy == UI->getOperand()->getType(),
              "EnumInst operand type does not match type of case");
    }
  }

  void checkInitEnumDataAddrInst(InitEnumDataAddrInst *UI) {
    EnumDecl *ud = UI->getOperand()->getType().getEnumOrBoundGenericEnum();
    require(ud, "InitEnumDataAddrInst must take an enum operand");
    require(UI->getElement()->getParentEnum() == ud,
            "InitEnumDataAddrInst case must be a case of the enum operand type");
    require(UI->getElement()->getArgumentInterfaceType(),
            "InitEnumDataAddrInst case must have a data type");
    require(UI->getOperand()->getType().isAddress(),
            "InitEnumDataAddrInst must take an address operand");
    require(UI->getType().isAddress(),
            "InitEnumDataAddrInst must produce an address");

    SILType caseTy =
      UI->getOperand()->getType().getEnumElementType(UI->getElement(),
                                                    F.getModule());
    requireSameType(caseTy, UI->getType(),
            "InitEnumDataAddrInst result does not match type of enum case");
  }

  void checkUncheckedEnumDataInst(UncheckedEnumDataInst *UI) {
    EnumDecl *ud = UI->getOperand()->getType().getEnumOrBoundGenericEnum();
    require(ud, "UncheckedEnumData must take an enum operand");
    require(UI->getElement()->getParentEnum() == ud,
            "UncheckedEnumData case must be a case of the enum operand type");
    require(UI->getElement()->getArgumentInterfaceType(),
            "UncheckedEnumData case must have a data type");
    require(UI->getOperand()->getType().isObject(),
            "UncheckedEnumData must take an address operand");
    require(UI->getType().isObject(),
            "UncheckedEnumData must produce an address");

    SILType caseTy =
      UI->getOperand()->getType().getEnumElementType(UI->getElement(),
                                                    F.getModule());
    require(caseTy == UI->getType(),
            "UncheckedEnumData result does not match type of enum case");
  }

  void checkUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *UI) {
    EnumDecl *ud = UI->getOperand()->getType().getEnumOrBoundGenericEnum();
    require(ud, "UncheckedTakeEnumDataAddrInst must take an enum operand");
    require(UI->getElement()->getParentEnum() == ud,
            "UncheckedTakeEnumDataAddrInst case must be a case of the enum operand type");
    require(UI->getElement()->getArgumentInterfaceType(),
            "UncheckedTakeEnumDataAddrInst case must have a data type");
    require(UI->getOperand()->getType().isAddress(),
            "UncheckedTakeEnumDataAddrInst must take an address operand");
    require(UI->getType().isAddress(),
            "UncheckedTakeEnumDataAddrInst must produce an address");

    SILType caseTy =
      UI->getOperand()->getType().getEnumElementType(UI->getElement(),
                                                    F.getModule());
    require(caseTy == UI->getType(),
            "UncheckedTakeEnumDataAddrInst result does not match type of enum case");
  }

  void checkInjectEnumAddrInst(InjectEnumAddrInst *IUAI) {
    require(IUAI->getOperand()->getType().is<EnumType>()
              || IUAI->getOperand()->getType().is<BoundGenericEnumType>(),
            "InjectEnumAddrInst must take an enum operand");
    require(IUAI->getElement()->getParentEnum()
              == IUAI->getOperand()->getType().getEnumOrBoundGenericEnum(),
            "InjectEnumAddrInst case must be a case of the enum operand type");
    require(IUAI->getOperand()->getType().isAddress(),
            "InjectEnumAddrInst must take an address operand");
  }

  void checkTupleInst(TupleInst *TI) {
    CanTupleType ResTy = requireObjectType(TupleType, TI, "Result of tuple");

    require(TI->getElements().size() == ResTy->getNumElements(),
            "Tuple field count mismatch!");

    for (size_t i = 0, size = TI->getElements().size(); i < size; ++i) {
      require(TI->getElement(i)->getType().getSwiftType()
               ->isEqual(ResTy.getElementType(i)),
              "Tuple element arguments do not match tuple type!");
    }
  }

  // Is a SIL type a potential lowering of a formal type?
  static bool isLoweringOf(SILType loweredType,
                           CanType formalType) {
    
    
    // Dynamic self has the same lowering as its contained type.
    if (auto dynamicSelf = dyn_cast<DynamicSelfType>(formalType))
      formalType = CanType(dynamicSelf->getSelfType());

    // Optional lowers its contained type. The difference between Optional
    // and IUO is lowered away.
    SILType loweredObjectType = loweredType
        .getAnyOptionalObjectType();
    CanType formalObjectType = formalType
        .getAnyOptionalObjectType();

    if (loweredObjectType) {
      return formalObjectType &&
             isLoweringOf(loweredObjectType, formalObjectType);
    }

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
    require(MI->getType().is<MetatypeType>(),
            "metatype instruction must be of metatype type");
    auto MetaTy = MI->getType().castTo<MetatypeType>();
    require(MetaTy->hasRepresentation(),
            "metatype instruction must have a metatype representation");
    verifyOpenedArchetype(MI, MetaTy.getInstanceType());
  }
  void checkValueMetatypeInst(ValueMetatypeInst *MI) {
    require(MI->getType().is<MetatypeType>(),
            "value_metatype instruction must be of metatype type");
    require(MI->getType().castTo<MetatypeType>()->hasRepresentation(),
            "value_metatype instruction must have a metatype representation");
    auto formalInstanceTy
      = MI->getType().castTo<MetatypeType>().getInstanceType();
    require(isLoweringOf(MI->getOperand()->getType(), formalInstanceTy),
            "value_metatype result must be formal metatype of "
            "lowered operand type");
  }
  void checkExistentialMetatypeInst(ExistentialMetatypeInst *MI) {
    require(MI->getType().is<ExistentialMetatypeType>(),
            "existential_metatype instruction must be of metatype type");
    require(MI->getType().castTo<ExistentialMetatypeType>()->hasRepresentation(),
            "value_metatype instruction must have a metatype representation");
    require(MI->getOperand()->getType().isAnyExistentialType(),
            "existential_metatype operand must be of protocol type");
    auto formalInstanceTy
      = MI->getType().castTo<ExistentialMetatypeType>().getInstanceType();
    require(isLoweringOf(MI->getOperand()->getType(), formalInstanceTy),
            "existential_metatype result must be formal metatype of "
            "lowered operand type");
  }

  void checkStrongRetainInst(StrongRetainInst *RI) {
    requireReferenceValue(RI->getOperand(), "Operand of strong_retain");
    require(F.hasUnqualifiedOwnership(),
            "strong_retain is only in functions with unqualified ownership");
  }
  void checkStrongReleaseInst(StrongReleaseInst *RI) {
    requireReferenceValue(RI->getOperand(), "Operand of release");
    require(F.hasUnqualifiedOwnership(),
            "strong_release is only in functions with unqualified ownership");
  }
  void checkStrongRetainUnownedInst(StrongRetainUnownedInst *RI) {
    auto unownedType = requireObjectType(UnownedStorageType, RI->getOperand(),
                                         "Operand of strong_retain_unowned");
    require(unownedType->isLoadable(ResilienceExpansion::Maximal),
            "strong_retain_unowned requires unowned type to be loadable");
  }
  void checkUnownedRetainInst(UnownedRetainInst *RI) {
    auto unownedType = requireObjectType(UnownedStorageType, RI->getOperand(),
                                          "Operand of unowned_retain");
    require(unownedType->isLoadable(ResilienceExpansion::Maximal),
            "unowned_retain requires unowned type to be loadable");
  }
  void checkUnownedReleaseInst(UnownedReleaseInst *RI) {
    auto unownedType = requireObjectType(UnownedStorageType, RI->getOperand(),
                                         "Operand of unowned_release");
    require(unownedType->isLoadable(ResilienceExpansion::Maximal),
            "unowned_release requires unowned type to be loadable");
  }
  void checkDeallocStackInst(DeallocStackInst *DI) {
    require(isa<SILUndef>(DI->getOperand()) ||
                isa<AllocStackInst>(DI->getOperand()),
            "Operand of dealloc_stack must be an alloc_stack");
  }
  void checkDeallocRefInst(DeallocRefInst *DI) {
    require(DI->getOperand()->getType().isObject(),
            "Operand of dealloc_ref must be object");
    require(DI->getOperand()->getType().getClassOrBoundGenericClass(),
            "Operand of dealloc_ref must be of class type");
  }
  void checkDeallocPartialRefInst(DeallocPartialRefInst *DPRI) {
    require(DPRI->getInstance()->getType().isObject(),
            "First operand of dealloc_partial_ref must be object");
    auto class1 = DPRI->getInstance()->getType().getClassOrBoundGenericClass();
    require(class1,
            "First operand of dealloc_partial_ref must be of class type");
    require(DPRI->getMetatype()->getType().is<MetatypeType>(),
            "Second operand of dealloc_partial_ref must be a metatype");
    auto class2 = DPRI->getMetatype()->getType().castTo<MetatypeType>()
        ->getInstanceType()->getClassOrBoundGenericClass();
    require(class2,
            "Second operand of dealloc_partial_ref must be a class metatype");
    while (class1 != class2) {
      class1 = class1->getSuperclass()->getClassOrBoundGenericClass();
      require(class1, "First operand not superclass of second instance type");
    }
  }

  void checkAllocBoxInst(AllocBoxInst *AI) {
    auto boxTy = AI->getType().getAs<SILBoxType>();
    require(boxTy, "alloc_box must have a @box type");

    require(AI->getType().isObject(),
            "result of alloc_box must be an object");
    for (unsigned field : indices(AI->getBoxType()->getLayout()->getFields()))
      verifyOpenedArchetype(AI,
                   AI->getBoxType()->getFieldLoweredType(F.getModule(), field));
  }

  void checkDeallocBoxInst(DeallocBoxInst *DI) {
    auto boxTy = DI->getOperand()->getType().getAs<SILBoxType>();
    require(boxTy, "operand must be a @box type");
    require(DI->getOperand()->getType().isObject(),
            "operand must be an object");
  }

  void checkDestroyAddrInst(DestroyAddrInst *DI) {
    require(DI->getOperand()->getType().isAddress(),
            "Operand of destroy_addr must be address");
  }

  void checkBindMemoryInst(BindMemoryInst *BI) {
    require(!BI->getType(), "BI must not produce a type");
    require(BI->getBoundType(), "BI must have a bound type");
    require(BI->getBase()->getType().is<BuiltinRawPointerType>(),
            "bind_memory base be a RawPointer");
    require(BI->getIndex()->getType()
            == SILType::getBuiltinWordType(F.getASTContext()),
            "bind_memory index must be a Word");
  }

  void checkIndexAddrInst(IndexAddrInst *IAI) {
    require(IAI->getType().isAddress(), "index_addr must produce an address");
    require(IAI->getType() == IAI->getBase()->getType(),
            "index_addr must produce an address of the same type as its base");
    require(IAI->getIndex()->getType().is<BuiltinIntegerType>(),
            "index_addr index must be of a builtin integer type");
  }

  void checkTailAddrInst(TailAddrInst *IAI) {
    require(IAI->getType().isAddress(), "tail_addr must produce an address");
    require(IAI->getIndex()->getType().is<BuiltinIntegerType>(),
            "tail_addr index must be of a builtin integer type");
  }

  void checkIndexRawPointerInst(IndexRawPointerInst *IAI) {
    require(IAI->getType().is<BuiltinRawPointerType>(),
            "index_raw_pointer must produce a RawPointer");
    require(IAI->getBase()->getType().is<BuiltinRawPointerType>(),
            "index_raw_pointer base must be a RawPointer");
    require(IAI->getIndex()->getType().is<BuiltinIntegerType>(),
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
    SILType operandTy = EI->getOperand()->getType();
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
    SILType operandTy = EI->getOperand()->getType();
    require(operandTy.isAddress(),
            "must derive element_addr from address");
    require(EI->getType().isAddress(),
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
    SILType operandTy = EI->getOperand()->getType();
    require(operandTy.isAddress(),
            "must derive struct_element_addr from address");
    StructDecl *sd = operandTy.getStructOrBoundGenericStruct();
    require(sd, "struct_element_addr operand must be struct address");
    require(EI->getType().isAddress(),
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
    require(EI->getType().isAddress(),
            "result of ref_element_addr must be lvalue");
    require(!EI->getField()->isStatic(),
            "cannot get address of static property with struct_element_addr");
    require(EI->getField()->hasStorage(),
            "cannot get address of computed property with ref_element_addr");
    SILType operandTy = EI->getOperand()->getType();
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

  void checkRefTailAddrInst(RefTailAddrInst *RTAI) {
    requireReferenceValue(RTAI->getOperand(), "Operand of ref_tail_addr");
    require(RTAI->getType().isAddress(),
            "result of ref_tail_addr must be lvalue");
    SILType operandTy = RTAI->getOperand()->getType();
    ClassDecl *cd = operandTy.getClassOrBoundGenericClass();
    require(cd, "ref_tail_addr operand must be a class instance");
  }
  
  SILType getMethodSelfType(CanSILFunctionType ft) {
    return fnConv.getSILType(ft->getParameters().back());
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

    auto genericSig = methodType->getGenericSignature();

    auto selfGenericParam = genericSig->getGenericParams()[0];
    require(selfGenericParam->getDepth() == 0
            && selfGenericParam->getIndex() == 0,
            "method should be polymorphic on Self parameter at depth 0 index 0");
    Optional<Requirement> selfRequirement;
    for (auto req : genericSig->getRequirements()) {
      if (req.getKind() != RequirementKind::SameType) {
        selfRequirement = req;
        break;
      }
    }

    require(selfRequirement &&
            selfRequirement->getKind() == RequirementKind::Conformance,
            "first non-same-typerequirement should be conformance requirement");
    auto conformsTo = genericSig->getConformsTo(selfGenericParam,
                                                *F.getModule().getSwiftModule());
    require(conformsTo.size() == 1,
            "requirement Self parameter must conform to exactly one protocol");
    require(conformsTo[0] == protocol,
            "requirement Self parameter should be constrained by protocol");

    auto lookupType = AMI->getLookupType();
    if (getOpenedArchetypeOf(lookupType)) {
      require(AMI->getTypeDependentOperands().size() == 1,
              "Must have a type dependent operand for the opened archetype");
      verifyOpenedArchetype(AMI, lookupType);
    } else {
      require(AMI->getTypeDependentOperands().empty(),
              "Should not have an operand for the opened existential");
    }
    if (isa<ArchetypeType>(lookupType) || lookupType->isAnyExistentialType()) {
      require(AMI->getConformance().isAbstract(),
              "archetype or existential lookup should have abstract conformance");
    } else {
      require(AMI->getConformance().isConcrete(),
              "concrete type lookup requires concrete conformance");
      auto conformance = AMI->getConformance().getConcrete();
      require(conformance->getType()->isEqual(AMI->getLookupType()),
              "concrete type lookup requires conformance that matches type");
      require(AMI->getModule().lookUpWitnessTable(conformance, false),
              "Could not find witness table for conformance");
    }
  }

  // Get the expected type of a dynamic method reference.
  SILType getDynamicMethodType(SILType selfType, SILDeclRef method) {
    auto &C = F.getASTContext();

    // The type of the dynamic method must match the usual type of the method,
    // but with the more opaque Self type.
    auto constantInfo = F.getModule().Types.getConstantInfo(method);
    auto methodTy = constantInfo.SILFnType;

    // Map interface types to archetypes.
    if (auto *env = constantInfo.GenericEnv) {
      auto subs = env->getForwardingSubstitutions();
      methodTy = methodTy->substGenericArgs(F.getModule(), subs);
    }
    assert(!methodTy->isPolymorphic());

    // Replace Self parameter with type of 'self' at the call site.
    auto params = methodTy->getParameters();
    SmallVector<SILParameterInfo, 4>
      dynParams(params.begin(), params.end() - 1);
    dynParams.push_back(SILParameterInfo(selfType.getSwiftRValueType(),
                                         params.back().getConvention()));

    auto results = methodTy->getResults();
    SmallVector<SILResultInfo, 4> dynResults(results.begin(), results.end());    

    // If the method returns Self, substitute AnyObject for the result type.
    if (auto fnDecl = dyn_cast<FuncDecl>(method.getDecl())) {
      if (fnDecl->hasDynamicSelf()) {
        auto anyObjectTy = C.getProtocol(KnownProtocolKind::AnyObject)
                             ->getDeclaredType();
        for (auto &dynResult : dynResults) {
          auto newResultTy
            = dynResult.getType()->replaceCovariantResultType(anyObjectTy, 0);
          dynResult = SILResultInfo(newResultTy->getCanonicalType(),
                                    dynResult.getConvention());
        }
      }
    }

    auto fnTy = SILFunctionType::get(nullptr,
                                     methodTy->getExtInfo(),
                                     methodTy->getCalleeConvention(),
                                     dynParams,
                                     dynResults,
                                     methodTy->getOptionalErrorResult(),
                                     F.getASTContext());
    return SILType::getPrimitiveObjectType(fnTy);
  }
  
  void checkDynamicMethodInst(DynamicMethodInst *EMI) {
    requireObjectType(SILFunctionType, EMI, "result of dynamic_method");
    SILType operandType = EMI->getOperand()->getType();

    require(EMI->getMember().getDecl()->isObjC(), "method must be @objc");
    if (!EMI->getMember().getDecl()->isInstanceMember()) {
      require(operandType.getSwiftType()->is<MetatypeType>(),
              "operand must have metatype type");
      require(operandType.getSwiftType()->castTo<MetatypeType>()
                ->getInstanceType()->mayHaveSuperclass(),
              "operand must have metatype of class or class-bounded type");
    }
    
    require(getDynamicMethodType(operandType, EMI->getMember())
              .getSwiftRValueType()
              ->isBindableTo(EMI->getType().getSwiftRValueType(), nullptr),
            "result must be of the method's type");
    verifyOpenedArchetype(EMI, EMI->getType().getSwiftRValueType());
  }

  void checkClassMethodInst(ClassMethodInst *CMI) {
    auto overrideTy = TC.getConstantOverrideType(CMI->getMember());
    requireSameType(CMI->getType(),
                    SILType::getPrimitiveObjectType(overrideTy),
        "result type of class_method must match abstracted type of method");
    auto methodType = requireObjectType(SILFunctionType, CMI,
                                        "result of class_method");
    require(!methodType->getExtInfo().hasContext(),
            "result method must be of a context-free function type");
    SILType operandType = CMI->getOperand()->getType();
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
    
    // TODO: We should enforce that ObjC methods are dispatched on ObjC
    // metatypes, but IRGen appears not to care right now.
#if 0
    if (auto metaTy = operandType.getAs<AnyMetatypeType>()) {
      bool objcMetatype
        = metaTy->getRepresentation() == MetatypeRepresentation::ObjC;
      bool objcMethod = CMI->getMember().isForeign;
      require(objcMetatype == objcMethod,
              "objc class methods must be invoked on objc metatypes");
    }
#endif
  }

  void checkSuperMethodInst(SuperMethodInst *CMI) {
    auto overrideTy = TC.getConstantOverrideType(CMI->getMember());
    requireSameType(CMI->getType(), SILType::getPrimitiveObjectType(overrideTy),
                    "result type of super_method must match abstracted type of method");
    auto methodType = requireObjectType(SILFunctionType, CMI,
                                        "result of super_method");
    require(!methodType->getExtInfo().hasContext(),
            "result method must be of a context-free function type");
    SILType operandType = CMI->getOperand()->getType();
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
    SILType operandType = OEI->getOperand()->getType();
    require(operandType.isAddress(),
            "open_existential_addr must be applied to address");
    require(operandType.canUseExistentialRepresentation(F.getModule(),
                                        ExistentialRepresentation::Opaque),
           "open_existential_addr must be applied to opaque existential");

    require(OEI->getType().isAddress(),
            "open_existential_addr result must be an address");

    auto archetype = getOpenedArchetypeOf(OEI->getType().getSwiftRValueType());
    require(archetype,
        "open_existential_addr result must be an opened existential archetype");
    require(OpenedArchetypes.getOpenedArchetypeDef(archetype) == OEI,
            "Archetype opened by open_existential_addr should be registered in "
            "SILFunction");
  }

  void checkOpenExistentialRefInst(OpenExistentialRefInst *OEI) {
    SILType operandType = OEI->getOperand()->getType();
    require(operandType.isObject(),
            "open_existential_ref operand must not be address");

    require(operandType.canUseExistentialRepresentation(F.getModule(),
                                              ExistentialRepresentation::Class),
            "open_existential_ref operand must be class existential");

    CanType resultInstanceTy = OEI->getType().getSwiftRValueType();

    require(OEI->getType().isObject(),
            "open_existential_ref result must be an address");

    auto archetype = getOpenedArchetypeOf(resultInstanceTy);
    require(archetype,
        "open_existential_ref result must be an opened existential archetype");
    require(OpenedArchetypes.getOpenedArchetypeDef(archetype) == OEI,
            "Archetype opened by open_existential_ref should be registered in "
            "SILFunction");
  }

  void checkOpenExistentialBoxInst(OpenExistentialBoxInst *OEI) {
    SILType operandType = OEI->getOperand()->getType();
    require(operandType.isObject(),
            "open_existential_box operand must not be address");

    require(operandType.canUseExistentialRepresentation(F.getModule(),
                                              ExistentialRepresentation::Boxed),
            "open_existential_box operand must be boxed existential");

    CanType resultInstanceTy = OEI->getType().getSwiftRValueType();

    require(OEI->getType().isAddress(),
            "open_existential_box result must be an address");

    auto archetype = getOpenedArchetypeOf(resultInstanceTy);
    require(archetype,
        "open_existential_box result must be an opened existential archetype");
    require(OpenedArchetypes.getOpenedArchetypeDef(archetype) == OEI,
            "Archetype opened by open_existential_box should be registered in "
            "SILFunction");
  }

  void checkOpenExistentialMetatypeInst(OpenExistentialMetatypeInst *I) {
    SILType operandType = I->getOperand()->getType();
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
    auto archetype = getOpenedArchetypeOf(resultInstTy);
    require(archetype, "open_existential_metatype result must be an opened "
                       "existential metatype");
    require(
        OpenedArchetypes.getOpenedArchetypeDef(archetype) == I,
        "Archetype opened by open_existential_metatype should be registered in "
        "SILFunction");
  }

  void checkOpenExistentialOpaqueInst(OpenExistentialOpaqueInst *OEI) {
    SILType operandType = OEI->getOperand()->getType();
    require(!operandType.isAddress(),
            "open_existential_opaque must not be applied to address");
    require(operandType.canUseExistentialRepresentation(
                F.getModule(), ExistentialRepresentation::Opaque),
            "open_existential_opaque must be applied to opaque existential");

    require(!OEI->getType().isAddress(),
            "open_existential_opaque result must not be an address");

    auto archetype = getOpenedArchetypeOf(OEI->getType().getSwiftRValueType());
    require(archetype, "open_existential_opaque result must be an opened "
                       "existential archetype");
    require(OpenedArchetypes.getOpenedArchetypeDef(archetype) == OEI,
            "Archetype opened by open_existential should be registered in "
            "SILFunction");
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
    
    checkExistentialProtocolConformances(exType, AEBI->getConformances());
    verifyOpenedArchetype(AEBI, AEBI->getFormalConcreteType());
  }

  void checkInitExistentialAddrInst(InitExistentialAddrInst *AEI) {
    SILType exType = AEI->getOperand()->getType();
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
    
    checkExistentialProtocolConformances(exType, AEI->getConformances());
    verifyOpenedArchetype(AEI, AEI->getFormalConcreteType());
  }

  void checkInitExistentialOpaqueInst(InitExistentialOpaqueInst *IEI) {
    SILType concreteType = IEI->getOperand()->getType();
    require(!concreteType.isAddress(),
            "init_existential_opaque must not be used on addresses");
    require(!IEI->getType().isAddress(),
            "init_existential_opaque result must not be an address");
    // The operand must be at the right abstraction level for the existential.
    SILType exType = IEI->getType();
    auto archetype = ArchetypeType::getOpened(exType.getSwiftRValueType());
    auto loweredTy = F.getModule().Types.getLoweredType(
        Lowering::AbstractionPattern(archetype), IEI->getFormalConcreteType());
    requireSameType(
        concreteType, loweredTy,
        "init_existential_opaque operand must be lowered to the right "
        "abstraction level for the existential");

    require(isLoweringOf(IEI->getOperand()->getType(),
                         IEI->getFormalConcreteType()),
            "init_existential_opaque operand must be a lowering of the formal "
            "concrete type");

    checkExistentialProtocolConformances(exType, IEI->getConformances());
    verifyOpenedArchetype(IEI, IEI->getFormalConcreteType());
  }

  void checkInitExistentialRefInst(InitExistentialRefInst *IEI) {
    SILType concreteType = IEI->getOperand()->getType();
    require(concreteType.getSwiftType()->isBridgeableObjectType(),
            "init_existential_ref operand must be a class instance");
    require(IEI->getType().canUseExistentialRepresentation(F.getModule(),
                                     ExistentialRepresentation::Class,
                                     IEI->getFormalConcreteType()),
            "init_existential_ref must be used with a class existential type");
    require(IEI->getType().isObject(),
            "init_existential_ref result must not be an address");
    
    // The operand must be at the right abstraction level for the existential.
    SILType exType = IEI->getType();
    auto archetype = ArchetypeType::getOpened(exType.getSwiftRValueType());
    auto loweredTy = F.getModule().Types.getLoweredType(
                                       Lowering::AbstractionPattern(archetype),
                                       IEI->getFormalConcreteType());
    requireSameType(concreteType, loweredTy,
                    "init_existential_ref operand must be lowered to the right "
                    "abstraction level for the existential");
    
    require(isLoweringOf(IEI->getOperand()->getType(),
                         IEI->getFormalConcreteType()),
            "init_existential_ref operand must be a lowering of the formal "
            "concrete type");
    
    checkExistentialProtocolConformances(exType, IEI->getConformances());
    verifyOpenedArchetype(IEI, IEI->getFormalConcreteType());
  }

  void checkDeinitExistentialAddrInst(DeinitExistentialAddrInst *DEI) {
    SILType exType = DEI->getOperand()->getType();
    require(exType.isAddress(),
            "deinit_existential_addr must be applied to an address");
    require(exType.canUseExistentialRepresentation(F.getModule(),
                                       ExistentialRepresentation::Opaque),
            "deinit_existential_addr must be applied to an opaque "
            "existential");
  }
  
  void checkDeallocExistentialBoxInst(DeallocExistentialBoxInst *DEBI) {
    SILType exType = DEBI->getOperand()->getType();
    require(exType.isObject(),
            "dealloc_existential_box must be applied to a value");
    require(exType.canUseExistentialRepresentation(F.getModule(),
                                       ExistentialRepresentation::Boxed),
            "dealloc_existential_box must be applied to a boxed "
            "existential");
  }

  void checkInitExistentialMetatypeInst(InitExistentialMetatypeInst *I) {
    SILType operandType = I->getOperand()->getType();
    require(operandType.isObject(),
            "init_existential_metatype operand must not be an address");
    require(operandType.is<MetatypeType>(),
            "init_existential_metatype operand must be a metatype");
    require(operandType.castTo<MetatypeType>()->hasRepresentation(),
            "init_existential_metatype operand must have a representation");

    SILType resultType = I->getType();
    require(resultType.is<ExistentialMetatypeType>(),
            "init_existential_metatype result must be an existential metatype");
    auto MetaTy = resultType.castTo<ExistentialMetatypeType>();
    require(resultType.isObject(),
            "init_existential_metatype result must not be an address");
    require(MetaTy->hasRepresentation(),
            "init_existential_metatype result must have a representation");
    require(MetaTy->getRepresentation()
              == operandType.castTo<MetatypeType>()->getRepresentation(),
            "init_existential_metatype result must match representation of "
            "operand");

    checkExistentialProtocolConformances(resultType, I->getConformances());
    verifyOpenedArchetype(I, MetaTy.getInstanceType());
  }

  void checkExistentialProtocolConformances(SILType resultType,
                                ArrayRef<ProtocolConformanceRef> conformances) {
    SmallVector<ProtocolDecl*, 4> protocols;
    resultType.getSwiftRValueType().isAnyExistentialType(protocols);

    require(conformances.size() == protocols.size(),
            "init_existential instruction must have the "
            "right number of conformances");
    for (auto i : indices(conformances)) {
      require(conformances[i].getRequirement() == protocols[i],
              "init_existential instruction must have conformances in "
              "proper order");

      if (conformances[i].isConcrete()) {
        auto conformance = conformances[i].getConcrete();
        require(F.getModule().lookUpWitnessTable(conformance, false),
                "Could not find witness table for conformance.");

      }
    }
  }

  void verifyCheckedCast(bool isExact, SILType fromTy, SILType toTy) {
    // Verify common invariants.
    require(fromTy.isObject() && toTy.isObject(),
            "value checked cast src and dest must be objects");

    auto fromCanTy = fromTy.getSwiftRValueType();
    auto toCanTy = toTy.getSwiftRValueType();

    require(canUseScalarCheckedCastInstructions(F.getModule(),
                                                fromCanTy, toCanTy),
            "invalid value checked cast src or dest types");

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
              isBindableToSuperclassOf(SILType::getPrimitiveObjectType(toCanTy)),
              "downcast must convert to a subclass");
    }
  }

  void checkUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *CI) {
    verifyCheckedCast(/*exact*/ false,
                      CI->getOperand()->getType(),
                      CI->getType());
    verifyOpenedArchetype(CI, CI->getType().getSwiftRValueType());
  }

  /// Verify if a given type is or contains an opened archetype or dynamic self.
  /// If this is the case, verify that the provided instruction has a type
  /// dependent operand for it.
  void verifyOpenedArchetype(SILInstruction *I, CanType Ty) {
    if (!Ty)
      return;
    // Check the type and all of its contained types.
    Ty.visit([&](Type t) {
      SILValue Def;
      if (t->isOpenedExistential()) {
        auto *archetypeTy = t->castTo<ArchetypeType>();
        Def = OpenedArchetypes.getOpenedArchetypeDef(archetypeTy);
        require(Def, "Opened archetype should be registered in SILFunction");
      } else if (t->hasDynamicSelfType()) {
        require(I->getFunction()->hasSelfParam(),
              "Function containing dynamic self type must have self parameter");
        Def = I->getFunction()->getSelfArgument();
      } else {
        return;
      }
      for (auto &TypeDefOp : I->getTypeDependentOperands()) {
        if (TypeDefOp.get() == Def)
          return;
      }
      require(false, "Instruction should contain a type dependent operand for "
                     "every used open archetype or dynamic self");
    });
  }

  void checkCheckedCastBranchInst(CheckedCastBranchInst *CBI) {
    verifyCheckedCast(CBI->isExact(),
                      CBI->getOperand()->getType(),
                      CBI->getCastType());
    verifyOpenedArchetype(CBI, CBI->getCastType().getSwiftRValueType());

    require(CBI->getSuccessBB()->args_size() == 1,
            "success dest of checked_cast_br must take one argument");
    require(CBI->getSuccessBB()->args_begin()[0]->getType() ==
                CBI->getCastType(),
            "success dest block argument of checked_cast_br must match type of "
            "cast");
    require(CBI->getFailureBB()->args_empty(),
            "failure dest of checked_cast_br must take no arguments");
  }

  void checkCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *CCABI) {
    require(CCABI->getSrc()->getType().isAddress(),
            "checked_cast_addr_br src must be an address");
    require(CCABI->getDest()->getType().isAddress(),
            "checked_cast_addr_br dest must be an address");

    require(
        CCABI->getSuccessBB()->args_size() == 0,
        "success dest block of checked_cast_addr_br must not take an argument");
    require(
        CCABI->getFailureBB()->args_size() == 0,
        "failure dest block of checked_cast_addr_br must not take an argument");
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

    require(TTOCI->getOperand()->getType().is<MetatypeType>() ==
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

    require(OCTTI->getOperand()->getType().is<MetatypeType>() ==
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
    auto operandType = I->getOperand()->getType().getSwiftRValueType();
    auto resultType = requireObjectType(UnownedStorageType, I,
                                        "Result of ref_to_unowned");
    require(resultType->isLoadable(ResilienceExpansion::Maximal),
            "ref_to_unowned requires unowned type to be loadable");
    require(resultType.getReferentType() == operandType,
            "Result of ref_to_unowned does not have the "
            "operand's type as its referent type");
  }

  void checkUnownedToRefInst(UnownedToRefInst *I) {
    auto operandType = requireObjectType(UnownedStorageType,
                                         I->getOperand(),
                                         "Operand of unowned_to_ref");
    require(operandType->isLoadable(ResilienceExpansion::Maximal),
            "unowned_to_ref requires unowned type to be loadable");
    requireReferenceStorageCapableValue(I, "Result of unowned_to_ref");
    auto resultType = I->getType().getSwiftRValueType();
    require(operandType.getReferentType() == resultType,
            "Operand of unowned_to_ref does not have the "
            "operand's type as its referent type");
  }

  void checkRefToUnmanagedInst(RefToUnmanagedInst *I) {
    requireReferenceStorageCapableValue(I->getOperand(),
                                        "Operand of ref_to_unmanaged");
    auto operandType = I->getOperand()->getType().getSwiftRValueType();
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
    require(UI->getType() != UI->getOperand()->getType(),
            "can't upcast to same type");
    if (UI->getType().is<MetatypeType>()) {
      CanType instTy(UI->getType().castTo<MetatypeType>()->getInstanceType());
      require(UI->getOperand()->getType().is<MetatypeType>(),
              "upcast operand must be a class or class metatype instance");
      CanType opInstTy(UI->getOperand()->getType().castTo<MetatypeType>()
                         ->getInstanceType());
      auto instClass = instTy->getClassOrBoundGenericClass();
      require(instClass,
              "upcast must convert a class metatype to a class metatype");
      
      if (instClass->usesObjCGenericsModel()) {
        require(instClass->getDeclaredTypeInContext()
                  ->isBindableToSuperclassOf(opInstTy, nullptr),
                "upcast must cast to a superclass or an existential metatype");
      } else {
        require(instTy->isExactSuperclassOf(opInstTy, nullptr),
                "upcast must cast to a superclass or an existential metatype");
      }
      return;
    }

    require(UI->getType().getCategory() ==
            UI->getOperand()->getType().getCategory(),
            "Upcast can only upcast in between types of the same "
            "SILValueCategory. This prevents address types from being cast to "
            "object types or vis-a-versa");

    auto ToTy = UI->getType();
    auto FromTy = UI->getOperand()->getType();

    // Upcast from Optional<B> to Optional<A> is legal as long as B is a
    // subclass of A.
    if (ToTy.getSwiftRValueType().getAnyOptionalObjectType() &&
        FromTy.getSwiftRValueType().getAnyOptionalObjectType()) {
      ToTy = SILType::getPrimitiveObjectType(
          ToTy.getSwiftRValueType().getAnyOptionalObjectType());
      FromTy = SILType::getPrimitiveObjectType(
          FromTy.getSwiftRValueType().getAnyOptionalObjectType());
    }

    auto ToClass = ToTy.getClassOrBoundGenericClass();
    require(ToClass,
            "upcast must convert a class instance to a class type");
      if (ToClass->usesObjCGenericsModel()) {
        require(ToClass->getDeclaredTypeInContext()
                  ->isBindableToSuperclassOf(FromTy.getSwiftRValueType(),
                                             nullptr),
                "upcast must cast to a superclass or an existential metatype");
      } else {
        require(ToTy.isExactSuperclassOf(FromTy),
                "upcast must cast to a superclass or an existential metatype");
      }
  }

  void checkIsNonnullInst(IsNonnullInst *II) {
    // The operand must be a function type or a class type.
    auto OpTy = II->getOperand()->getType().getSwiftType();
    require(OpTy->mayHaveSuperclass() || OpTy->is<SILFunctionType>(),
            "is_nonnull operand must be a class or function type");
  }

  void checkAddressToPointerInst(AddressToPointerInst *AI) {
    require(AI->getOperand()->getType().isAddress(),
            "address-to-pointer operand must be an address");
    require(AI->getType().getSwiftType()->isEqual(
                              AI->getType().getASTContext().TheRawPointerType),
            "address-to-pointer result type must be RawPointer");
  }
  
  void checkUncheckedRefCastInst(UncheckedRefCastInst *AI) {
    verifyOpenedArchetype(AI, AI->getType().getSwiftRValueType());
    require(AI->getOperand()->getType().isObject(),
            "unchecked_ref_cast operand must be a value");
    require(AI->getType().isObject(),
            "unchecked_ref_cast result must be an object");
    require(SILType::canRefCast(AI->getOperand()->getType(), AI->getType(),
                                AI->getModule()),
            "unchecked_ref_cast requires a heap object reference type");
  }

  void checkUncheckedRefCastAddrInst(UncheckedRefCastAddrInst *AI) {
    auto srcTy = AI->getSrc()->getType();
    auto destTy = AI->getDest()->getType();
    require(srcTy.isAddress(),
            "unchecked_ref_cast_addr operand must be an address");
    require(destTy.isAddress(),
            "unchecked_ref_cast_addr result must be an address");
    // The static src/dest types cannot be checked here even if they are
    // loadable. unchecked_ref_cast_addr may accept nonreference static types
    // (as a result of specialization). These cases will never be promoted to
    // value bitcast, thus will cause the subsequent runtime cast to fail.
  }
  
  void checkUncheckedAddrCastInst(UncheckedAddrCastInst *AI) {
    verifyOpenedArchetype(AI, AI->getType().getSwiftRValueType());

    require(AI->getOperand()->getType().isAddress(),
            "unchecked_addr_cast operand must be an address");
    require(AI->getType().isAddress(),
            "unchecked_addr_cast result must be an address");
  }
  
  void checkUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *BI) {
    verifyOpenedArchetype(BI, BI->getType().getSwiftRValueType());
    require(BI->getOperand()->getType().isObject(),
            "unchecked_trivial_bit_cast must operate on a value");
    require(BI->getType().isObject(),
            "unchecked_trivial_bit_cast must produce a value");
    require(BI->getType().isTrivial(F.getModule()),
            "unchecked_trivial_bit_cast must produce a value of trivial type");
  }

  void checkUncheckedBitwiseCastInst(UncheckedBitwiseCastInst *BI) {
    verifyOpenedArchetype(BI, BI->getType().getSwiftRValueType());
    require(BI->getOperand()->getType().isObject(),
            "unchecked_bitwise_cast must operate on a value");
    require(BI->getType().isObject(),
            "unchecked_bitwise_cast must produce a value");
  }

  void checkRefToRawPointerInst(RefToRawPointerInst *AI) {
    require(AI->getOperand()->getType().getSwiftType()
              ->isAnyClassReferenceType(),
            "ref-to-raw-pointer operand must be a class reference or"
            " NativeObject");
    require(AI->getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheRawPointerType),
            "ref-to-raw-pointer result must be RawPointer");
  }

  void checkRawPointerToRefInst(RawPointerToRefInst *AI) {
    verifyOpenedArchetype(AI, AI->getType().getSwiftRValueType());
    require(AI->getType()
              .getSwiftType()->isBridgeableObjectType()
            || AI->getType().getSwiftType()->isEqual(
                             AI->getType().getASTContext().TheNativeObjectType)
            || AI->getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheUnknownObjectType),
        "raw-pointer-to-ref result must be a class reference or NativeObject");
    require(AI->getOperand()->getType().getSwiftType()->isEqual(
                            AI->getType().getASTContext().TheRawPointerType),
            "raw-pointer-to-ref operand must be NativeObject");
  }
  
  void checkRefToBridgeObjectInst(RefToBridgeObjectInst *RI) {
    require(RI->getConverted()->getType().isObject(),
            "ref_to_bridge_object must convert from a value");
    require(RI->getConverted()->getType().getSwiftRValueType()
              ->isBridgeableObjectType(),
            "ref_to_bridge_object must convert from a heap object ref");
    require(RI->getBitsOperand()->getType()
              == SILType::getBuiltinWordType(F.getASTContext()),
            "ref_to_bridge_object must take a Builtin.Word bits operand");
    require(RI->getType() == SILType::getBridgeObjectType(F.getASTContext()),
            "ref_to_bridge_object must produce a BridgeObject");
  }
  
  void checkBridgeObjectToRefInst(BridgeObjectToRefInst *RI) {
    verifyOpenedArchetype(RI, RI->getType().getSwiftRValueType());
    require(RI->getConverted()->getType()
               == SILType::getBridgeObjectType(F.getASTContext()),
            "bridge_object_to_ref must take a BridgeObject");
    require(RI->getType().isObject(),
            "bridge_object_to_ref must produce a value");
    require(RI->getType().getSwiftRValueType()->isBridgeableObjectType(),
            "bridge_object_to_ref must produce a heap object reference");
  }
  void checkBridgeObjectToWordInst(BridgeObjectToWordInst *RI) {
    require(RI->getConverted()->getType()
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
    require(CFI->getOperand()->getType()
              == SILType::getBuiltinIntegerType(1, F.getASTContext()),
            "cond_fail operand must be a Builtin.Int1");
  }

  void checkReturnInst(ReturnInst *RI) {
    DEBUG(RI->print(llvm::dbgs()));

    SILType functionResultType =
        F.mapTypeIntoContext(fnConv.getSILResultType());
    SILType instResultType = RI->getOperand()->getType();
    DEBUG(llvm::dbgs() << "function return type: ";
          functionResultType.dump();
          llvm::dbgs() << "return inst type: ";
          instResultType.dump(););
    require(functionResultType == instResultType,
            "return value type does not match return type of function");
  }

  void checkThrowInst(ThrowInst *TI) {
    DEBUG(TI->print(llvm::dbgs()));

    CanSILFunctionType fnType = F.getLoweredFunctionType();
    require(fnType->hasErrorResult(),
            "throw in function that doesn't have an error result");

    SILType functionResultType = F.mapTypeIntoContext(fnConv.getSILErrorType());
    SILType instResultType = TI->getOperand()->getType();
    DEBUG(llvm::dbgs() << "function error result type: ";
          functionResultType.dump();
          llvm::dbgs() << "throw operand type: ";
          instResultType.dump(););
    require(functionResultType == instResultType,
            "throw operand type does not match error result type of function");
  }
  
  void checkSelectEnumCases(SelectEnumInstBase *I) {
    EnumDecl *eDecl = I->getEnumOperand()->getType().getEnumOrBoundGenericEnum();
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
      requireSameType(result->getType(), I->getType(),
                    "select_enum case operand must match type of instruction");
    }

    // If the select is non-exhaustive, we require a default.
    require(unswitchedElts.empty() || I->hasDefault(),
            "nonexhaustive select_enum must have a default destination");
    if (I->hasDefault()) {
      requireSameType(I->getDefaultResult()->getType(),
                  I->getType(),
                  "select_enum default operand must match type of instruction");
    }
  }

  void checkSelectEnumInst(SelectEnumInst *SEI) {
    require(SEI->getEnumOperand()->getType().isObject(),
            "select_enum operand must be an object");
    
    checkSelectEnumCases(SEI);
  }
  void checkSelectEnumAddrInst(SelectEnumAddrInst *SEI) {
    require(SEI->getEnumOperand()->getType().isAddress(),
            "select_enum_addr operand must be an address");
    
    checkSelectEnumCases(SEI);
  }

  void checkSwitchValueInst(SwitchValueInst *SVI) {
    // TODO: Type should be either integer or function
    auto Ty = SVI->getOperand()->getType();
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

      require(value->getType() == Ty,
             "switch_value case value should have the same type as its operand");

      require(!cases.count(value),
              "multiple switch_value cases for same value");
      cases.insert(value);

      require(dest->args_empty(),
              "switch_value case destination cannot take arguments");
    }

    if (SVI->hasDefault())
      require(SVI->getDefaultBB()->args_empty(),
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
      
      if (!isa<SILUndef>(casevalue)) {
        auto  *il = dyn_cast<IntegerLiteralInst>(casevalue);
        require(il,
                "select_value case operands should refer to integer literals");
        APInt elt = il->getValue();

        require(!seenCaseValues.count(elt),
                "select_value dispatches on same case value more than once");

        seenCaseValues.insert(elt);
      }

      requireSameType(I->getOperand()->getType(), casevalue->getType(),
                      "select_value case value must match type of operand");

      // The result value must match the type of the instruction.
      requireSameType(result->getType(), I->getType(),
                    "select_value case result must match type of instruction");
    }

    require(I->hasDefault(),
            "select_value should always have a default");
    requireSameType(I->getDefaultResult()->getType(),
                  I->getType(),
                  "select_value default operand must match type of instruction");
  }

  void checkSelectValueInst(SelectValueInst *SVI) {
    require(SVI->getOperand()->getType().isObject(),
            "select_value operand must be an object");

    checkSelectValueCases(SVI);
  }

  void checkSwitchEnumInst(SwitchEnumInst *SOI) {
    require(SOI->getOperand()->getType().isObject(),
            "switch_enum operand must be an object");

    SILType uTy = SOI->getOperand()->getType();
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
      if (elt->getArgumentInterfaceType()) {
        require(dest->getArguments().size() == 0 ||
                    dest->getArguments().size() == 1,
                "switch_enum destination for case w/ args must take 0 or 1 "
                "arguments");

        if (dest->getArguments().size() == 1) {
          SILType eltArgTy = uTy.getEnumElementType(elt, F.getModule());
          SILType bbArgTy = dest->getArguments()[0]->getType();
          require(eltArgTy == bbArgTy,
                  "switch_enum destination bbarg must match case arg type");
          require(!dest->getArguments()[0]->getType().isAddress(),
                  "switch_enum destination bbarg type must not be an address");
        }

      } else {
        require(dest->getArguments().size() == 0,
                "switch_enum destination for no-argument case must take no "
                "arguments");
      }
    }

    // If the switch is non-exhaustive, we require a default.
    require(unswitchedElts.empty() || SOI->hasDefault(),
            "nonexhaustive switch_enum must have a default destination");
    if (SOI->hasDefault())
      require(SOI->getDefaultBB()->args_empty(),
              "switch_enum default destination must take no arguments");
  }

  void checkSwitchEnumAddrInst(SwitchEnumAddrInst *SOI) {
    require(SOI->getOperand()->getType().isAddress(),
            "switch_enum_addr operand must be an address");

    SILType uTy = SOI->getOperand()->getType();
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
      require(dest->getArguments().size() == 0,
              "switch_enum_addr destination must take no BB args");
    }

    // If the switch is non-exhaustive, we require a default.
    require(unswitchedElts.empty() || SOI->hasDefault(),
            "nonexhaustive switch_enum_addr must have a default "
            "destination");
    if (SOI->hasDefault())
      require(SOI->getDefaultBB()->args_empty(),
              "switch_enum_addr default destination must take "
              "no arguments");
  }

  bool verifyBranchArgs(SILValue branchArg, SILArgument *bbArg) {
    // NOTE: IRGen currently does not support the following method_inst
    // variants as branch arguments.
    // Once this is supported, the check can be removed.
    require(!(isa<MethodInst>(branchArg) &&
              cast<MethodInst>(branchArg)->getMember().isForeign),
        "branch argument cannot be a witness_method or an objc method_inst");
    return branchArg->getType() == bbArg->getType();
  }

  void checkBranchInst(BranchInst *BI) {
    require(BI->getArgs().size() == BI->getDestBB()->args_size(),
            "branch has wrong number of arguments for dest bb");
    require(std::equal(BI->getArgs().begin(), BI->getArgs().end(),
                       BI->getDestBB()->args_begin(),
                       [&](SILValue branchArg, SILArgument *bbArg) {
                         return verifyBranchArgs(branchArg, bbArg);
                       }),
            "branch argument types do not match arguments for dest bb");
  }

  void checkCondBranchInst(CondBranchInst *CBI) {
    // It is important that cond_br keeps an i1 type. ARC Sequence Opts assumes
    // that cond_br does not use reference counted values or decrement reference
    // counted values under the assumption that the instruction that computes
    // the i1 is the use/decrement that ARC cares about and that after that
    // instruction is evaluated, the scalar i1 has a different identity and the
    // object can be deallocated.
    require(CBI->getCondition()->getType() ==
             SILType::getBuiltinIntegerType(1,
                                 CBI->getCondition()->getType().getASTContext()),
            "condition of conditional branch must have Int1 type");

    require(CBI->getTrueArgs().size() == CBI->getTrueBB()->args_size(),
            "true branch has wrong number of arguments for dest bb");
    require(CBI->getTrueBB() != CBI->getFalseBB(),
            "identical destinations");
    require(std::equal(CBI->getTrueArgs().begin(), CBI->getTrueArgs().end(),
                       CBI->getTrueBB()->args_begin(),
                       [&](SILValue branchArg, SILArgument *bbArg) {
                         return verifyBranchArgs(branchArg, bbArg);
                       }),
            "true branch argument types do not match arguments for dest bb");

    require(CBI->getFalseArgs().size() == CBI->getFalseBB()->args_size(),
            "false branch has wrong number of arguments for dest bb");
    require(std::equal(CBI->getFalseArgs().begin(), CBI->getFalseArgs().end(),
                       CBI->getFalseBB()->args_begin(),
                       [&](SILValue branchArg, SILArgument *bbArg) {
                         return verifyBranchArgs(branchArg, bbArg);
                       }),
            "false branch argument types do not match arguments for dest bb");
  }

  void checkDynamicMethodBranchInst(DynamicMethodBranchInst *DMBI) {
    SILType operandType = DMBI->getOperand()->getType();

    require(DMBI->getMember().getDecl()->isObjC(), "method must be @objc");
    if (!DMBI->getMember().getDecl()->isInstanceMember()) {
      require(operandType.getSwiftType()->is<MetatypeType>(),
              "operand must have metatype type");
      require(operandType.getSwiftType()->castTo<MetatypeType>()
                ->getInstanceType()->mayHaveSuperclass(),
              "operand must have metatype of class or class-bound type");
    }

    // Check that the branch argument is of the expected dynamic method type.
    require(DMBI->getHasMethodBB()->args_size() == 1,
            "true bb for dynamic_method_br must take an argument");

    auto bbArgTy = DMBI->getHasMethodBB()->args_begin()[0]->getType();
    require(getDynamicMethodType(operandType, DMBI->getMember())
              .getSwiftRValueType()
              ->isBindableTo(bbArgTy.getSwiftRValueType(), nullptr),
            "bb argument for dynamic_method_br must be of the method's type");
  }

  void checkProjectBlockStorageInst(ProjectBlockStorageInst *PBSI) {
    require(PBSI->getOperand()->getType().isAddress(),
            "operand must be an address");
    auto storageTy = PBSI->getOperand()->getType().getAs<SILBlockStorageType>();
    require(storageTy, "operand must be a @block_storage type");
    
    require(PBSI->getType().isAddress(),
            "result must be an address");
    auto captureTy = PBSI->getType().getSwiftRValueType();
    require(storageTy->getCaptureType() == captureTy,
            "result must be the capture type of the @block_storage type");
  }
  
  void checkInitBlockStorageHeaderInst(InitBlockStorageHeaderInst *IBSHI) {
    require(IBSHI->getBlockStorage()->getType().isAddress(),
            "block storage operand must be an address");
    auto storageTy
      = IBSHI->getBlockStorage()->getType().getAs<SILBlockStorageType>();
    require(storageTy, "block storage operand must be a @block_storage type");
    
    require(IBSHI->getInvokeFunction()->getType().isObject(),
            "invoke function operand must be a value");
    auto invokeTy
      = IBSHI->getInvokeFunction()->getType().getAs<SILFunctionType>();
    require(invokeTy, "invoke function operand must be a function");
    require(invokeTy->getRepresentation()
              == SILFunctionType::Representation::CFunctionPointer,
            "invoke function operand must be a c function");
    require(invokeTy->getParameters().size() >= 1,
            "invoke function must take at least one parameter");
    require(!invokeTy->getGenericSignature() ||
            invokeTy->getExtInfo().isPseudogeneric(),
            "invoke function must not take reified generic parameters");
    
    invokeTy = checkApplySubstitutions(IBSHI->getSubstitutions(),
                                    SILType::getPrimitiveObjectType(invokeTy));
    
    auto storageParam = invokeTy->getParameters()[0];
    require(storageParam.getConvention() ==
            ParameterConvention::Indirect_InoutAliasable,
            "invoke function must take block storage as @inout_aliasable "
            "parameter");
    require(storageParam.getType() == storageTy,
            "invoke function must take block storage type as first parameter");
    
    require(IBSHI->getType().isObject(), "result must be a value");
    auto blockTy = IBSHI->getType().getAs<SILFunctionType>();
    require(blockTy, "result must be a function");
    require(blockTy->getRepresentation() == SILFunctionType::Representation::Block,
            "result must be a cdecl block function");
    require(blockTy->getResults() == invokeTy->getResults(),
            "result must have same results as invoke function");

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
    require(classDecl->getModuleContext()->getName() == F.getASTContext().Id_ObjectiveC,
            "objc_protocol must produce an instance of ObjectiveC.Protocol class");
  }
  
  void checkObjCMetatypeToObjectInst(ObjCMetatypeToObjectInst *OMOI) {
    require(OMOI->getOperand()->getType().isObject(),
            "objc_metatype_to_object must take a value");
    auto fromMetaTy = OMOI->getOperand()->getType().getAs<MetatypeType>();
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
    require(OMOI->getOperand()->getType().isObject(),
            "objc_metatype_to_object must take a value");
    auto fromMetaTy = OMOI->getOperand()->getType()
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
    DEBUG(llvm::dbgs() << "Argument types for entry point BB:\n";
          for (auto *arg
               : make_range(entry->args_begin(), entry->args_end()))
              arg->getType()
                  .dump();
          llvm::dbgs() << "Input types for SIL function type ";
          F.getLoweredFunctionType()->print(llvm::dbgs());
          llvm::dbgs() << ":\n";
          for (auto paramTy
               : fnConv.getParameterSILTypes()) { paramTy.dump(); });

    require(entry->args_size() == (fnConv.getNumIndirectSILResults()
                                   + fnConv.getNumParameters()),
            "entry point has wrong number of arguments");

    bool matched = true;
    auto argI = entry->args_begin();
    SILModule &M = F.getModule();

    auto check = [&](const char *what, SILType ty) {
      auto mappedTy = F.mapTypeIntoContext(ty);
      SILArgument *bbarg = *argI;
      ++argI;
      auto ownershipkind = ValueOwnershipKind(
          M, mappedTy, fnConv.getSILArgumentConvention(bbarg->getIndex()));
      if (bbarg->getType() != mappedTy) {
        llvm::errs() << what << " type mismatch!\n";
        llvm::errs() << "  argument: "; bbarg->dump();
        llvm::errs() << "  expected: "; mappedTy.dump();
        matched = false;
      }

      if (bbarg->getOwnershipKind() != ownershipkind) {
        llvm::errs() << what << " ownership kind mismatch!\n";
        llvm::errs() << "  argument: " << bbarg->getOwnershipKind() << '\n';
        llvm::errs() << "  expected: " << ownershipkind << '\n';
        matched = false;
      }
    };

    for (auto result : fnConv.getIndirectSILResults()) {
      assert(fnConv.isSILIndirect(result));
      check("result", fnConv.getSILType(result));
    }
    for (auto param : F.getLoweredFunctionType()->getParameters()) {
      check("parameter", fnConv.getSILType(param));
    }

    require(matched, "entry point argument types do not match function type");

    // TBAA requirement for all address arguments.
    require(std::equal(entry->args_begin() + fnConv.getNumIndirectSILResults(),
                       entry->args_end(),
                       fnConv.funcTy->getParameters().begin(),
                       [&](SILArgument *bbarg, SILParameterInfo paramInfo) {
                         if (!bbarg->getType().isAddress())
                           return true;
                         switch (paramInfo.getConvention()) {
                         default:
                           return false;
                         case ParameterConvention::Indirect_In:
                         case ParameterConvention::Indirect_Inout:
                         case ParameterConvention::Indirect_InoutAliasable:
                         case ParameterConvention::Indirect_In_Guaranteed:
                           return true;
                         }
                       }),
            "entry point address argument must have an indirect calling "
            "convention");
  }

  void verifyEpilogBlocks(SILFunction *F) {
    bool FoundReturnBlock = false;
    bool FoundThrowBlock = false;
    for (auto &BB : *F) {
      if (isa<ReturnInst>(BB.getTerminator())) {
        require(!FoundReturnBlock,
                "more than one return block in function");
        FoundReturnBlock = true;
      }
      if (isa<ThrowInst>(BB.getTerminator())) {
        require(!FoundThrowBlock,
                "more than one throw block in function");
        FoundThrowBlock = true;
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
    else if (isa<ThrowInst>(StartBlock->getTerminator()))
      return false;

    // Recursively check all successors.
    for (auto *SuccBB : StartBlock->getSuccessorBlocks())
      if (!Visited.insert(SuccBB).second)
        if (!isUnreachableAlongAllPathsStartingAt(SuccBB, Visited))
          return false;

    return true;
  }

  void verifySILFunctionType(CanSILFunctionType FTy) {
    // Make sure that if FTy's calling convention implies that it must have a
    // self parameter.
    require(!FTy->hasSelfParam() || !FTy->getParameters().empty(),
            "Functions with a calling convention with self parameter must "
            "have at least one argument for self.");
  }

  void verifyStackHeight(SILFunction *F) {
    llvm::DenseMap<SILBasicBlock*, std::vector<SILInstruction*>> visitedBBs;
    SmallVector<SILBasicBlock*, 16> Worklist;
    visitedBBs[&*F->begin()] = {};
    Worklist.push_back(&*F->begin());
    while (!Worklist.empty()) {
      SILBasicBlock *BB = Worklist.pop_back_val();
      std::vector<SILInstruction*> stack = visitedBBs[BB];
      for (SILInstruction &i : *BB) {
        CurInstruction = &i;

        if (i.isAllocatingStack()) {
          stack.push_back(&i);
        }
        if (i.isDeallocatingStack()) {
          SILValue op = i.getOperand(0);
          require(!stack.empty(),
                  "stack dealloc with empty stack");
          require(op == stack.back(),
                  "stack dealloc does not match most recent stack alloc");
          stack.pop_back();
        }
        if (auto term = dyn_cast<TermInst>(&i)) {
          if (term->isFunctionExiting()) {
            require(stack.empty(),
                    "return with stack allocs that haven't been deallocated");
          }
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
      if (DestBB->getSinglePredecessorBlock())
        return false;

      return true;
    };

    SILModule &M = F->getModule();
    for (auto &BB : *F) {
      TermInst *TI = BB.getTerminator();

      // Check for non-cond_br critical edges in canonical SIL.
      if (!isa<CondBranchInst>(TI) && M.getStage() == SILStage::Canonical) {
        for (unsigned Idx = 0, e = BB.getSuccessors().size(); Idx != e; ++Idx) {
          require(!isCriticalEdgePred(TI, Idx),
                  "non cond_br critical edges not allowed");
        }
        continue;
      }

      // In ownership qualified SIL, ban critical edges from CondBranchInst that
      // have non-trivial arguments.
      if (!F->hasQualifiedOwnership())
        continue;

      auto *CBI = dyn_cast<CondBranchInst>(TI);
      if (!CBI)
        continue;
      if (isCriticalEdgePred(CBI, CondBranchInst::TrueIdx)) {
        require(
            llvm::all_of(CBI->getTrueArgs(),
                         [](SILValue V) -> bool {
                           return V.getOwnershipKind() ==
                                  ValueOwnershipKind::Trivial;
                         }),
            "cond_br with critical edges must not have a non-trivial value");
      }
      if (isCriticalEdgePred(CBI, CondBranchInst::FalseIdx)) {
        require(
            llvm::all_of(CBI->getFalseArgs(),
                         [](SILValue V) -> bool {
                           return V.getOwnershipKind() ==
                                  ValueOwnershipKind::Trivial;
                         }),
            "cond_br with critical edges must not have a non-trivial value");
      }
    }
  }

  void verifyOpenedArchetypes(SILFunction *F) {
    require(&OpenedArchetypes.getFunction() == F,
           "Wrong SILFunction provided to verifyOpenedArchetypes");
    // Check that definitions of all opened archetypes from
    // OpenedArchetypesDefs are existing instructions
    // belonging to the function F.
    for (auto KV: OpenedArchetypes.getOpenedArchetypeDefs()) {
      require(getOpenedArchetypeOf(KV.first),
              "Only opened archetypes should be registered in SILFunction");
      auto Def = cast<SILInstruction>(KV.second);
      require(Def->getFunction() == F, 
              "Definition of every registered opened archetype should be an"
              " existing instruction in a current SILFunction");
    }
  }

  void visitSILBasicBlock(SILBasicBlock *BB) {
    // Make sure that each of the successors/predecessors of this basic block
    // have this basic block in its predecessor/successor list.
    for (const auto *SuccBB : BB->getSuccessorBlocks()) {
      bool FoundSelfInSuccessor = false;
      if (SuccBB->isPredecessorBlock(BB)) {
        FoundSelfInSuccessor = true;
        break;
      }
      require(FoundSelfInSuccessor, "Must be a predecessor of each successor.");
    }

    for (const SILBasicBlock *PredBB : BB->getPredecessorBlocks()) {
      bool FoundSelfInPredecessor = false;
      if (PredBB->isSuccessorBlock(BB)) {
        FoundSelfInPredecessor = true;
        break;
      }
      require(FoundSelfInPredecessor, "Must be a successor of each predecessor.");
    }
    
    SILVisitor::visitSILBasicBlock(BB);
  }

  void visitSILBasicBlocks(SILFunction *F) {
    // Visit all basic blocks in the RPOT order.
    // This ensures that any open_existential instructions, which
    // open archetypes, are seen before the uses of these archetypes.
    llvm::ReversePostOrderTraversal<SILFunction *> RPOT(F);
    llvm::DenseSet<SILBasicBlock *> VisitedBBs;
    for (auto Iter = RPOT.begin(), E = RPOT.end(); Iter != E; ++Iter) {
      auto *BB = *Iter;
      VisitedBBs.insert(BB);
      visitSILBasicBlock(BB);
    }

    // Visit all basic blocks that were not visited during the RPOT traversal,
    // e.g. unreachable basic blocks.
    for (auto &BB : *F) {
      if (VisitedBBs.count(&BB))
        continue;
      visitSILBasicBlock(&BB);
    }
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
      if (F->hasForeignBody())
        return;

      assert(F->isAvailableExternally() &&
             "external declaration of internal SILFunction not allowed");
      assert(!hasSharedVisibility(F->getLinkage()) &&
             "external declarations of SILFunctions with shared visibility is not "
             "allowed");
      // If F is an external declaration, there is nothing further to do,
      // return.
      return;
    }

    assert(!F->hasForeignBody());

    // Make sure that our SILFunction only has context generic params if our
    // SILFunctionType is non-polymorphic.
    if (F->getGenericEnvironment()) {
      require(FTy->isPolymorphic(),
              "non-generic function definitions cannot have a "
              "generic environment");
    } else {
      require(!FTy->isPolymorphic(),
              "generic function definition must have a generic environment");
    }

    // Otherwise, verify the body of the function.
    verifyEntryPointArguments(&*F->getBlocks().begin());
    verifyEpilogBlocks(F);
    verifyStackHeight(F);
    verifyBranches(F);

    visitSILBasicBlocks(F);

    // Verify archetypes after all basic blocks are visited,
    // because we build the map of archetypes as we visit the
    // instructions.
    verifyOpenedArchetypes(F);
  }

  void verify() {
    visitSILFunction(const_cast<SILFunction*>(&F));
  }
};
} // end anonymous namespace

#undef require
#undef requireObjectType
#endif //NDEBUG

//===----------------------------------------------------------------------===//
//                     Out of Line Verifier Run Functions
//===----------------------------------------------------------------------===//

/// verify - Run the SIL verifier to make sure that the SILFunction follows
/// invariants.
void SILFunction::verify(bool SingleFunction) const {
#ifndef NDEBUG
  // Please put all checks in visitSILFunction in SILVerifier, not here. This
  // ensures that the pretty stack trace in the verifier is included with the
  // back trace when the verifier crashes.
  SILVerifier(*this, SingleFunction).verify();
#endif
}

/// Verify that a vtable follows invariants.
void SILVTable::verify(const SILModule &M) const {
#ifndef NDEBUG
  for (auto &entry : getEntries()) {
    // All vtable entries must be decls in a class context.
    assert(entry.Method.hasDecl() && "vtable entry is not a decl");
    auto baseInfo = M.Types.getConstantInfo(entry.Method);
    ValueDecl *decl = entry.Method.getDecl();
    
    assert((!isa<FuncDecl>(decl)
            || !cast<FuncDecl>(decl)->isObservingAccessor())
           && "observing accessors shouldn't have vtable entries");

    // For ivar destroyers, the decl is the class itself.
    ClassDecl *theClass;
    if (entry.Method.kind == SILDeclRef::Kind::IVarDestroyer)
      theClass = dyn_cast<ClassDecl>(decl);
    else
      theClass = dyn_cast<ClassDecl>(decl->getDeclContext());

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
    assert(!entry.Method.isCurried && "vtable entry must not be curried");

    // Foreign entry points shouldn't appear in vtables.
    assert(!entry.Method.isForeign && "vtable entry must not be foreign");
    
    // The vtable entry must be ABI-compatible with the overridden vtable slot.
    SmallString<32> baseName;
    {
      llvm::raw_svector_ostream os(baseName);
      entry.Method.print(os);
    }
    
    SILVerifier(*entry.Implementation)
      .requireABICompatibleFunctionTypes(
                    baseInfo.getSILType().castTo<SILFunctionType>(),
                    entry.Implementation->getLoweredFunctionType(),
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

  auto *protocol = getConformance()->getProtocol();

  // Currently all witness tables have public conformances, thus witness tables
  // should not reference SILFunctions without public/public_external linkage.
  // FIXME: Once we support private conformances, update this.
  for (const Entry &E : getEntries())
    if (E.getKind() == SILWitnessTable::WitnessKind::Method) {
      SILFunction *F = E.getMethodWitness().Witness;
      if (F) {
        assert(!isLessVisibleThan(F->getLinkage(), getLinkage()) &&
               "Witness tables should not reference less visible functions.");
        assert(F->getLoweredFunctionType()->getRepresentation() ==
               SILFunctionTypeRepresentation::WitnessMethod &&
               "Witnesses must have witness_method representation.");
        auto *witnessSelfProtocol = F->getLoweredFunctionType()
            ->getDefaultWitnessMethodProtocol(*M.getSwiftModule());
        assert((witnessSelfProtocol == nullptr ||
                witnessSelfProtocol == protocol) &&
               "Witnesses must either have a concrete Self, or an "
               "an abstract Self that is constrained to their "
               "protocol.");
      }
    }
#endif
}

/// Verify that a default witness table follows invariants.
void SILDefaultWitnessTable::verify(const SILModule &M) const {
#ifndef NDEBUG
  for (const Entry &E : getEntries()) {
    if (!E.isValid())
      continue;

    SILFunction *F = E.getWitness();
    assert(!isLessVisibleThan(F->getLinkage(), getLinkage()) &&
           "Default witness tables should not reference "
           "less visible functions.");
    assert(F->getLoweredFunctionType()->getRepresentation() ==
           SILFunctionTypeRepresentation::WitnessMethod &&
           "Default witnesses must have witness_method representation.");
    auto *witnessSelfProtocol = F->getLoweredFunctionType()
        ->getDefaultWitnessMethodProtocol(*M.getSwiftModule());
    assert(witnessSelfProtocol == getProtocol() &&
           "Default witnesses must have an abstract Self parameter "
           "constrained to their protocol.");
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
    f.verify(/*SingleFunction=*/false);
  }

  // Check all globals.
  for (const SILGlobalVariable &g : getSILGlobals()) {
    if (!symbolNames.insert(g.getName()).second) {
      llvm::errs() << "Symbol redefined: " << g.getName() << "!\n";
      assert(false && "triggering standard assertion failure routine");
    }
    g.verify();
  }

  // Check all vtables and the vtable cache.
  llvm::DenseSet<ClassDecl*> vtableClasses;
  unsigned EntriesSZ = 0;
  for (const SILVTable &vt : getVTables()) {
    if (!vtableClasses.insert(vt.getClass()).second) {
      llvm::errs() << "Vtable redefined: " << vt.getClass()->getName() << "!\n";
      assert(false && "triggering standard assertion failure routine");
    }
    vt.verify(*this);
    // Check if there is a cache entry for each vtable entry
    for (auto entry : vt.getEntries()) {
      if (VTableEntryCache.find({&vt, entry.Method}) == VTableEntryCache.end()) {
        llvm::errs() << "Vtable entry for function: "
                     << entry.Implementation->getName() << "not in cache!\n";
        assert(false && "triggering standard assertion failure routine");
      }
      EntriesSZ++;
    }
  }
  assert(EntriesSZ == VTableEntryCache.size() &&
         "Cache size is not equal to true number of VTable entries");

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

  // Check all default witness tables.
  DEBUG(llvm::dbgs() << "*** Checking default witness tables for duplicates ***\n");
  llvm::DenseSet<const ProtocolDecl *> defaultWitnessTables;
  for (const SILDefaultWitnessTable &wt : getDefaultWitnessTables()) {
    DEBUG(llvm::dbgs() << "Default Witness Table:\n"; wt.dump());
    if (!defaultWitnessTables.insert(wt.getProtocol()).second) {
      llvm::errs() << "Default witness table redefined: ";
      wt.dump();
      assert(false && "triggering standard assertion failure routine");
    }
    wt.verify(*this);
  }
#endif
}

#ifndef NDEBUG
/// Determine whether an instruction may not have a SILDebugScope.
bool swift::maybeScopeless(SILInstruction &I) {
  if (I.getFunction()->isBare())
    return true;
  return !isa<DebugValueInst>(I) && !isa<DebugValueAddrInst>(I);
}
#endif
