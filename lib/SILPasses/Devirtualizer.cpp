//===-- Devirtualizer.cpp ------ Devirtualize virtual calls ---------------===//
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
//
// Devirtualizes virtual function calls into direct function calls.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-devirtualizer"
#include "swift/Basic/DemangleWrappers.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILAnalysis/ClassHierarchyAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/PassManager.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/SILInliner.h"
#include "swift/AST/ASTContext.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
using namespace swift;

STATISTIC(NumInlineCaches, "Number of monomorphic inline caches inserted");
STATISTIC(NumDevirtualized, "Number of calls devirtualzied");
STATISTIC(NumAMI, "Number of witness_method devirtualzied");

// The number of subclasses to allow when placing polymorphic inline caches.
static const int MaxNumPolymorphicInlineCaches = 6;

//===----------------------------------------------------------------------===//
//                         Class Method Optimization
//===----------------------------------------------------------------------===//

/// Is this an instruction kind which allows us to conclude definitively what
/// the class decls of its results are.
///
/// FIXME: We can expand this to use typed GEPs.
static bool isClassDeclOracle(ValueKind Kind) {
  switch (Kind) {
  case ValueKind::AllocRefInst:
  case ValueKind::AllocRefDynamicInst:
  case ValueKind::MetatypeInst:
    return true;
  default:
    return false;
  }
}

/// \brief Recursively searches the ClassDecl for a class_method operand.
/// Return the ClassDecl from the point of construction of \p S or null.
static ClassDecl *getClassFromConstructor(SILValue S) {
  // First strip off casts.
  S = S.stripCasts();

  // Then if S is not a class decl oracle, we can not ascertain what its results
  // "true" type is.
  if (!isClassDeclOracle(S->getKind()))
    return nullptr;

  // Look for a a static ClassTypes in AllocRefInst or MetatypeInst.
  if (AllocRefInst *ARI = dyn_cast<AllocRefInst>(S))
    return ARI->getType().getClassOrBoundGenericClass();

  auto *MTI = dyn_cast<MetatypeInst>(S);
  if (!MTI)
    return nullptr;

  CanType instTy = MTI->getType().castTo<MetatypeType>().getInstanceType();
  return instTy.getClassOrBoundGenericClass();
}

/// \brief Devirtualize an Apply instruction and a class member obtained
/// using the class_method instruction into a direct call to a specific
/// member of a specific class.
///
/// \p AI is the apply to devirtualize.
/// \p Member is the class member to devirtualize.
/// \p ClassInstance is the operand for the ClassMethodInst or an alternative
///    reference (such as downcasted class reference).
/// \p KnownClass (can be null) is a specific class type to devirtualize to.
static bool devirtMethod(ApplyInst *AI, SILDeclRef Member,
                         SILValue ClassInstance, ClassDecl *CD) {
  DEBUG(llvm::dbgs() << "    Trying to devirtualize : " << *AI);

  // First attempt to lookup the origin for our class method. The origin should
  // either be a metatype or an alloc_ref.
  DEBUG(llvm::dbgs() << "        Origin: " << ClassInstance);

  assert(CD && "Invalid class type");

  // Otherwise lookup from the module the least derived implementing method from
  // the module vtables.
  SILModule &Mod = AI->getModule();
  SILFunction *F = Mod.lookUpSILFunctionFromVTable(CD, Member);

  // If we do not find any such function, we have no function to devirtualize
  // to... so bail.
  if (!F) {
    DEBUG(llvm::dbgs() << "        FAIL: Could not find matching VTable or "
                          "vtable method for this class.\n");
    return false;
  }

  // Ok, we found a function F that we can devirtualization our class method
  // to. We want to do everything on the substituted type in the case of
  // generics. Thus construct our subst callee type for F.
  SILModule &M = F->getModule();
  CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();
  // *NOTE*:
  // Apply instruction substitutions are for the Member. F (the implementing
  // method) may have a different set of generic parameters! For example, Member
  // may have no substitutions at all, whereas F may have some.
  auto ClassInstanceType = ClassInstance.getType();
  ArrayRef<Substitution> Substitutions = AI->getSubstitutions();
  if (Substitutions.empty() && !Class->getGenericParamTypes().empty())
    // Take the parameters from the type of the ClassInstance.
    Substitutions = ClassInstanceType.gatherAllSubstitutions(AI->getModule());

  CanSILFunctionType SubstCalleeType =
    GenCalleeType->substGenericArgs(M, M.getSwiftModule(), Substitutions);


  // If F's this pointer has a different type from CMI's operand and the
  // "this" pointer type is a super class of the CMI's operand, insert an
  // upcast.
  auto paramTypes =
    SubstCalleeType->getParameterSILTypesWithoutIndirectResult();

  // We should always have a this pointer. Assert on debug builds, return
  // nullptr on release builds.
  assert(!paramTypes.empty() &&
         "Must have a this pointer when calling a class method inst.");
  if (paramTypes.empty())
    return false;

  // Grab the self type from the function ref and the self type from the class
  // method inst.
  SILType FuncSelfTy = paramTypes[paramTypes.size() - 1];
  SILType OriginTy = ClassInstance.getType();
  SILBuilderWithScope<16> B(AI);

  // Then compare the two types and if they are unequal...
  if (FuncSelfTy != OriginTy) {
    if (ClassInstance.stripUpCasts().getType().getAs<MetatypeType>()) {
      auto &Module = AI->getModule();
      (void) Module;
      assert(FuncSelfTy.getMetatypeInstanceType(Module).
             isSuperclassOf(OriginTy.getMetatypeInstanceType(Module)) &&
             "Can not call a class method"
             " on a non-subclass of the class_methods class.");
    } else {
      assert(FuncSelfTy.isSuperclassOf(OriginTy) &&
             "Can not call a class method"
             " on a non-subclass of the class_methods class.");
    }
    // Otherwise, upcast origin to the appropriate type.
    ClassInstance = B.createUpcast(AI->getLoc(), ClassInstance, FuncSelfTy);
  }

  // Success! Perform the devirtualization.
  FunctionRefInst *FRI = B.createFunctionRef(AI->getLoc(), F);

  // Construct a new arg list. First process all non-self operands, ref, addr
  // casting them to the appropriate types for F so that we allow for covariant
  // indirect return types and contravariant arguments.
  llvm::SmallVector<SILValue, 8> NewArgs;
  auto Args = AI->getArguments();
  auto allParamTypes = SubstCalleeType->getParameterSILTypes();

  // For each old argument Op...
  for (unsigned i = 0, e = Args.size() - 1; i != e; ++i) {
    SILValue Op = Args[i];
    SILType OpTy = Op.getType();
    SILType FOpTy = allParamTypes[i];

    // If the type matches the type for the given parameter in F, just add it to
    // our arg list and continue.
    if (OpTy == FOpTy) {
      NewArgs.push_back(Op);
      continue;
    }

    // Otherwise we have either a covariant return type or a contravariant
    // argument type. Cast it appropriately.
    assert((OpTy.isAddress() || OpTy.isHeapObjectReferenceType()) &&
           "Only addresses and refs can have their types changed due to "
           "covariant return types or contravariant argument types.");

    // If OpTy is an address, perform an unchecked_addr_cast.
    if (OpTy.isAddress()) {
      NewArgs.push_back(B.createUncheckedAddrCast(AI->getLoc(), Op, FOpTy));
    } else {
      // Otherwise perform a ref cast.
      NewArgs.push_back(B.createUncheckedRefCast(AI->getLoc(), Op, FOpTy));
    }
  }

  // Add in self to the end.
  NewArgs.push_back(ClassInstance);

  // If we have a direct return type, make sure we use the subst callee return
  // type. If we have an indirect return type, AI's return type of the empty
  // tuple should be ok.
  SILType ReturnType = AI->getType();
  if (!SubstCalleeType->hasIndirectResult()) {
    ReturnType = SubstCalleeType->getSILResult();
  }

  SILType SubstCalleeSILType = SILType::getPrimitiveObjectType(SubstCalleeType);
  ApplyInst *NewAI =
      B.createApply(AI->getLoc(), FRI, SubstCalleeSILType, ReturnType,
                    Substitutions, NewArgs);

  // If our return type differs from AI's return type, then we know that we have
  // a covariant return type. Cast it before we RAUW. This can not happen 
  if (ReturnType != AI->getType()) {
    assert((ReturnType.isAddress() || ReturnType.isHeapObjectReferenceType()) &&
           "Only addresses and refs can have their types changed due to "
           "covariant return types or contravariant argument types.");

    SILValue CastedAI = NewAI;
    if (ReturnType.isAddress()) {
      CastedAI = B.createUncheckedAddrCast(AI->getLoc(), NewAI, AI->getType());
    } else {
      CastedAI = B.createUncheckedRefCast(AI->getLoc(), NewAI, AI->getType());
    }
    SILValue(AI).replaceAllUsesWith(CastedAI);
  } else {
    AI->replaceAllUsesWith(NewAI);
  }

  AI->eraseFromParent();

  DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
  NumDevirtualized++;
  return true;
}

//===----------------------------------------------------------------------===//
//                        Witness Method Optimization
//===----------------------------------------------------------------------===//

namespace {

class WitnessMethodDevirtualizer {

  //==-----------
  // Input fields

  /// The apply that we are attempting to optimize.
  ApplyInst *AI;

  /// The witness method that is the callee of the apply.
  WitnessMethodInst *WMI;

  /// The conformance that the witness method is using.
  ProtocolConformance *C;

  /// The SILFunction devirtualization target.
  SILFunction *F;

  /// If we have a specialized protocol conformance, the substitutions for the
  /// conformance.
  ArrayRef<Substitution> Subs;

  /// The witness table which we got F, Subs from.
  SILWitnessTable *WT;


  //==---------------
  // Workspace fields

  /// If we needed to change Self, this field contains the modified self.
  Optional<SILValue> Self;

  /// If we needed to change arguments (e.g. because we need to upcast them),
  // this field contains the modified arguments.
  Optional<SmallVector<SILValue, 4>> Arguments;

  /// If we needed to modify the subst callee type of the function, this is the
  /// new subst callee type.
  Optional<SILType> SubstCalleeSILType;

  /// If we needed to modify the result type of the function, this is the new
  /// result type.
  Optional<SILType> ResultSILType;

public:
  WitnessMethodDevirtualizer(ApplyInst *AI, WitnessMethodInst *WMI,
                             ProtocolConformance *C, SILFunction *F,
                             ArrayRef<Substitution> Subs, SILWitnessTable *WT)
      : AI(AI), WMI(WMI), C(C), F(F), Subs(Subs), WT(WT), Self(), Arguments(),
        SubstCalleeSILType(), ResultSILType() {}

  /// Main entry point.
  bool devirtualize();
private:

  bool processNormalProtocolConformance(bool PartOfSpecialized = false);
  bool processInheritedProtocolConformance();
  bool
  processSpecializedProtocolConformance(SpecializedProtocolConformance *SPC,
                                        ProtocolConformance *GenericConf);
};

} // end anonymous namespace

bool WitnessMethodDevirtualizer::devirtualize() {
  /// Use to quite the unused-private-field warning.
  (void)WMI;

  /// First check if we have a simple normal protocol conformance.
  if (isa<NormalProtocolConformance>(C))
    return processNormalProtocolConformance();

  /// If we dont and do not have any substitutions, we must then have a pure
  /// inherited protocol conformance.
  if (Subs.empty()) {
    assert(isa<InheritedProtocolConformance>(C) &&
           "At this point C must be an inherited protocol conformance.");
    return processInheritedProtocolConformance();
  }

  if (isa<InheritedProtocolConformance>(C)) {
    return processInheritedProtocolConformance();
  }

  /// If we have substitutions, we must have some sort of specialized protocol
  /// conformance. If it is just a simple specialized + normal protocol
  /// conformance, handle it early.
  if (auto *SPC = dyn_cast<SpecializedProtocolConformance>(C)) {
    if (auto *GNC =
          dyn_cast<NormalProtocolConformance>(SPC->getGenericConformance())) {
      return processSpecializedProtocolConformance(SPC, GNC);
    }
  }

  // Otherwise, we have a complicated specialized, inherited protocol
  // conformance that we need to devirtualize.
  return false;
}

bool WitnessMethodDevirtualizer::processNormalProtocolConformance(
                                   bool PartOfSpecialized) {
  // Ok, we found the member we are looking for. Devirtualize away!
  SILBuilderWithScope<2> Builder(AI);
  SILLocation Loc = AI->getLoc();
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, F);

  // Collect args from the apply inst.
  SmallVector<SILValue, 4> Args;

  // First add the non self arguments to the new argument list.
  if (!Arguments) {
    for (SILValue A : AI->getArgumentsWithoutSelf())
      Args.push_back(A);
  } else {
    for (SILValue A : *Arguments)
      Args.push_back(A);
  }

  // Then add the self argument since the self argument is always last.
  if (!Self)
    Self = AI->getSelfArgument();
  Args.push_back(*Self);

  SmallVector<Substitution, 16> NewSubstList(Subs.begin(),
                                             Subs.end());

  // Add the non-self-derived substitutions from the original application.
  for (auto &origSub : AI->getSubstitutionsWithoutSelfSubstitution()) {
    if (!origSub.getArchetype()->isSelfDerived())
      NewSubstList.push_back(origSub);
  }

  if (!SubstCalleeSILType) {
    if (!PartOfSpecialized)
      SubstCalleeSILType = AI->getSubstCalleeSILType();
    else {
      // We do not need to worry about covariant/contravariant return types here
      // since we don't support that for 1.0. But when we do this needs to be
      // updated.
      SILModule &M = F->getModule();
      CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();
      // Here we have the full substitutions, by combining substitutions from
      // the specialized protocol conformance and substitutions from the AI.
      CanSILFunctionType SubstCalleeType =
        GenCalleeType->substGenericArgs(M, M.getSwiftModule(),
                                        NewSubstList);
      SubstCalleeSILType = SILType::getPrimitiveObjectType(SubstCalleeType);
      ResultSILType = SubstCalleeType->getSILResult();
    }
  }
  if (!ResultSILType)
    ResultSILType = AI->getType();

  ApplyInst *SAI = Builder.createApply(Loc, FRI, *SubstCalleeSILType,
                                       *ResultSILType, NewSubstList, Args);
  AI->replaceAllUsesWith(SAI);
  AI->eraseFromParent();
  NumAMI++;
  return true;
}

static SILValue upcastArgument(SILValue Arg, SILType SuperTy, ApplyInst *AI) {
  SILType ArgTy = Arg.getType();
  if (dyn_cast<MetatypeInst>(Arg) || dyn_cast<ValueMetatypeInst>(Arg)) {
    // In case of metatypes passed as parameters, we need to upcast to a
    // metatype of a superclass.
    auto &Module = AI->getModule();
    assert(SuperTy.isSuperclassOf(ArgTy.getMetatypeInstanceType(Module)) &&
           "Should only create upcasts for sub class devirtualization.");
    SuperTy = Lowering::TypeConverter(Module).getLoweredLoadableType(
        SuperTy.getClassOrBoundGenericClass()->getType());
  } else {
    assert(SuperTy.isSuperclassOf(ArgTy) &&
           "Should only create upcasts for sub class devirtualization.");
    (void)ArgTy;
  }

  Arg = SILBuilderWithScope<1>(AI).createUpcast(AI->getLoc(), Arg, SuperTy);
  return Arg;
}

bool WitnessMethodDevirtualizer::processInheritedProtocolConformance() {
  // Since we do not need to worry about substitutions, we can just insert an
  // upcast of self to the appropriate type.
  Self = AI->getSelfArgument();
  CanType Ty = WT->getConformance()->getType()->getCanonicalType();
  SILType SILTy = SILType::getPrimitiveType(Ty, Self->getType().getCategory());

  if (!Subs.empty()) {
    TypeSubstitutionMap SubstMap;
    for (auto Sub : Subs) {
      SubstMap[Sub.getArchetype()] = Sub.getReplacement();
    }
    // We need to perform substitution
    SILTy = SILTy.subst(AI->getModule(), AI->getModule().getSwiftModule(),
                        SubstMap);
    Ty = Ty.subst(AI->getModule().getSwiftModule(), SubstMap, false, nullptr)
             ->getCanonicalType();
  }

  SILType SelfTy = Self->getType();
  (void)SelfTy;

  Self = upcastArgument(Self.getValue(), SILTy, AI);

  SmallVector<Substitution, 16> SelfDerivedSubstitutions;
  for (auto &origSub : AI->getSubstitutions())
    if (origSub.getArchetype()->isSelfDerived())
      SelfDerivedSubstitutions.push_back(origSub);

  // If we have more than 1 substitution on AI that is self derived, that means
  // we either have a property or a typealias. We currently do not specialize
  // those correctly implying that we will have an archetype instead of a
  // concrete type here that we can not work with. Thus bail and don't do
  // anything.
  if (SelfDerivedSubstitutions.size() > 1)
    return false;

  // Grab self and substitute into the old generic callee type to get the new
  // non-generic callee type.
  assert(SelfDerivedSubstitutions.size() == 1 &&
         "Must have a substitution for self.");
  Substitution NewSelfSub{
    AI->getSelfSubstitution().getArchetype(),
    Ty,
    AI->getSelfSubstitution().getConformances(),
  };

  CanSILFunctionType OrigType = AI->getOrigCalleeType();
  CanSILFunctionType SubstCalleeType = OrigType->substGenericArgs(
      AI->getModule(), AI->getModule().getSwiftModule(),
      ArrayRef<Substitution>(NewSelfSub));

  SubstCalleeSILType = SILType::getPrimitiveObjectType(SubstCalleeType);

  // Collect arguments from the apply instruction.
  Arguments = SmallVector<SILValue, 4>();

  auto CanCalleeSILType = SubstCalleeSILType->getSwiftRValueType();
  SILFunctionType *FT = dyn_cast<SILFunctionType>(CanCalleeSILType);
  auto ParamTypes = FT->getParameterSILTypes();
  // Type of the current parameter being processed
  auto ParamType = ParamTypes.begin();
  // Iterate over the non self arguments and add them to the
  // new argument list, upcasting when required.
  for (SILValue A : AI->getArgumentsWithoutSelf()) {
    if (A.getType() != *ParamType) {
      // Upcast argument
      A = upcastArgument(A, *ParamType, AI);
    }
    Arguments->push_back(A);
    ++ParamType;
  }

  // Then pass of our new self to the normal protocol conformance witness method
  // handling code.
  return processNormalProtocolConformance();
}

bool
WitnessMethodDevirtualizer::
processSpecializedProtocolConformance(SpecializedProtocolConformance *SPC,
                                      ProtocolConformance *GenericConf) {
  return processNormalProtocolConformance(true/*PartOfSpecialized*/);
}

/// Devirtualize apply instructions that call witness_method instructions:
///
///   %8 = witness_method $Optional<UInt16>, #LogicValue.boolValue!getter.1
///   %9 = apply %8<Self = CodeUnit?>(%6#1) : ...
///
static bool optimizeWitnessMethod(ApplyInst *AI, WitnessMethodInst *WMI) {
  ProtocolConformance *C = WMI->getConformance();
  if (!C) {
    DEBUG(llvm::dbgs() << "        FAIL: Null conformance.\n");
    return false;
  }

  // Lookup the function reference in the witness tables.
  SILFunction *F;
  ArrayRef<Substitution> Subs;
  SILWitnessTable *WT;
  std::tie(F, WT, Subs) =
      AI->getModule().findFuncInWitnessTable(C, WMI->getMember());

  if (!F) {
    assert(!WT && "WitnessTable should always be null if F is.");
    DEBUG(llvm::dbgs() << "        FAIL: Did not find a matching witness "
                          "table or witness method.\n");
    return false;
  }
  assert(WT && "WitnessTable should never be null if F is not.");

  WitnessMethodDevirtualizer WMD{AI, WMI, C, F, Subs, WT};

  return WMD.devirtualize();
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

/// Return the final class decl based on access control information.
static ClassDecl *getClassFromAccessControl(ClassMethodInst *CMI) {
  const DeclContext *associatedDC = CMI->getModule().getAssociatedContext();
  if (!associatedDC) {
    // Without an associated context, we can't perform any access-based
    // optimizations.
    return nullptr;
  }

  SILDeclRef Member = CMI->getMember();
  FuncDecl *FD = Member.getFuncDecl();
  SILType ClassType = CMI->getOperand().stripUpCasts().getType();
  ClassDecl *CD = ClassType.getClassOrBoundGenericClass();

  // Only handle valid non-dynamic non-overridden members.
  if (!CD || !FD || FD->isInvalid() || FD->isDynamic() || FD->isOverridden())
    return nullptr;

  // Only handle members defined within the SILModule's associated context.
  if (!FD->isChildContextOf(associatedDC))
    return nullptr;

  if (!FD->hasAccessibility())
    return nullptr;

  // Only consider 'private' members, unless we are in whole-module compilation.
  switch (FD->getAccessibility()) {
  case Accessibility::Public:
    return nullptr;
  case Accessibility::Internal:
    if (!CMI->getModule().isWholeModule())
      return nullptr;
    break;
  case Accessibility::Private:
    break;
  }

  Type selfTypeInMember = FD->getDeclContext()->getDeclaredTypeInContext();
  return selfTypeInMember->getClassOrBoundGenericClass();
}

static bool optimizeApplyInst(ApplyInst *AI) {
  DEBUG(llvm::dbgs() << "    Trying to optimize ApplyInst : " << *AI);

  // Devirtualize apply instructions that call witness_method instructions:
  //
  //   %8 = witness_method $Optional<UInt16>, #LogicValue.boolValue!getter.1
  //   %9 = apply %8<Self = CodeUnit?>(%6#1) : ...
  //
  if (auto *AMI = dyn_cast<WitnessMethodInst>(AI->getCallee()))
    return optimizeWitnessMethod(AI, AMI);

  /// Optimize a class_method and alloc_ref pair into a direct function
  /// reference:
  ///
  /// \code
  /// %XX = alloc_ref $Foo
  /// %YY = class_method %XX : $Foo, #Foo.get!1 : $@cc(method) @thin ...
  /// \endcode
  ///
  ///  or
  ///
  /// %XX = metatype $...
  /// %YY = class_method %XX : ...
  ///
  ///  into
  ///
  /// %YY = function_ref @...
  if (auto *CMI = dyn_cast<ClassMethodInst>(AI->getCallee())) {
    // Check if the class member is known to be final.
    if (ClassDecl *C = getClassFromAccessControl(CMI))
      return devirtMethod(AI, CMI->getMember(), CMI->getOperand(), C);

    // Try to search for the point of construction.
    if (ClassDecl *C = getClassFromConstructor(CMI->getOperand()))
      return devirtMethod(AI, CMI->getMember(),
                          CMI->getOperand().stripUpCasts(), C);
  }

  return false;
}

namespace {

class SILDevirtualizationPass : public SILModuleTransform {
public:
  virtual ~SILDevirtualizationPass() {}

  /// The entry point to the transformation.
  virtual void run() {

    bool Changed = false;

    // Perform devirtualization locally and compute potential polymorphic
    // arguments for all existing functions.
    for (auto &F : *getModule()) {
      DEBUG(llvm::dbgs() << "*** Devirtualizing Function: "
              << demangle_wrappers::demangleSymbolAsString(F.getName())
              << "\n");
      for (auto &BB : F) {
        for (auto II = BB.begin(), IE = BB.end(); II != IE;) {
          ApplyInst *AI = dyn_cast<ApplyInst>(&*II);
          ++II;

          if (!AI)
            continue;

          Changed |= optimizeApplyInst(AI);
        }
      }
      DEBUG(llvm::dbgs() << "\n");
    }

    if (Changed) {
      PM->scheduleAnotherIteration();
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
    }
  }

  StringRef getName() override { return "Devirtualization"; }
};

} // end anonymous namespace

SILTransform *swift::createDevirtualization() {
  return new SILDevirtualizationPass();
}

// A utility function for cloning the apply instruction.
static ApplyInst *CloneApply(ApplyInst *AI, SILBuilder &Builder) {
  // Clone the Apply.
  auto Args = AI->getArguments();
  SmallVector<SILValue, 8> Ret(Args.size());
  for (unsigned i = 0, e = Args.size(); i != e; ++i)
    Ret[i] = Args[i];

  auto NAI = Builder.createApply(AI->getLoc(), AI->getCallee(),
                                 AI->getSubstCalleeSILType(),
                                 AI->getType(),
                                 AI->getSubstitutions(),
                                 Ret, AI->isTransparent());
  NAI->setDebugScope(AI->getDebugScope());
  return NAI;
}

/// Insert monomorphic inline caches for a specific class type \p SubClassTy.
static ApplyInst* insertMonomorphicInlineCaches(ApplyInst *AI,
                                                SILType SubClassTy) {
  ClassMethodInst *CMI = cast<ClassMethodInst>(AI->getCallee());
  SILValue ClassInstance = CMI->getOperand();
  ClassDecl *CD = SubClassTy.getClassOrBoundGenericClass();

  // If method implementation for a SubClassTy is not known,
  // bail out early as we won't be able to devirtualize it.
  // This may happen e.g in case of a -primary-file
  // compilations, where information about methods implemented
  // in other files is unavailable.
  // Early exit guarantees that we do not create two
  // basic blocks which both perform virtual calls of a method.
  auto &Mod = AI->getModule();
  auto *Method = Mod.lookUpSILFunctionFromVTable(CD, CMI->getMember());
  if (!Method)
    return nullptr;

  bool IsValueMetatype = false;
  SILType RealSubClassTy = SubClassTy;

  if (isa<ValueMetatypeInst>(ClassInstance.stripUpCasts())) {
    // Convert this type to its metatype type.
    Lowering::TypeConverter TC(AI->getModule());
    RealSubClassTy = TC.getLoweredLoadableType(CD->getType());
    IsValueMetatype = true;
  }

  // Create a diamond shaped control flow and a checked_cast_branch
  // instruction that checks the exact type of the object.
  // This cast selects between two paths: one that calls the slow dynamic
  // dispatch and one that calls the specific method.
  SILBasicBlock::iterator It = AI;
  SILFunction *F = AI->getFunction();
  SILBasicBlock *Entry = AI->getParent();

  // Iden is the basic block containing the direct call.
  SILBasicBlock *Iden = F->createBasicBlock();
  // Virt is the block containing the slow virtual call.
  SILBasicBlock *Virt = F->createBasicBlock();
  Iden->createBBArg(RealSubClassTy);

  SILBasicBlock *Continue = Entry->splitBasicBlock(It);

  SILBuilderWithScope<> Builder(Entry, AI->getDebugScope());
  // Create the checked_cast_branch instruction that checks at runtime if the
  // class instance is identical to the SILType.
  if (!IsValueMetatype)
    assert(SubClassTy.getClassOrBoundGenericClass() &&
           "Dest type must be a class type");

  It = Builder.createCheckedCastBranch(AI->getLoc(), /*exact*/ true,
                                       ClassInstance, RealSubClassTy, Iden, Virt);

  SILBuilder VirtBuilder(Virt);
  SILBuilder IdenBuilder(Iden);
  // This is the class reference downcasted into subclass SubClassTy.
  SILValue DownCastedClassInstance = Iden->getBBArg(0);

  // Try sinking the retain of the class instance into the diamond. This may
  // allow additional ARC optimizations on the fast path.
  if (It != Entry->begin()) {
    StrongRetainInst *SRI = dyn_cast<StrongRetainInst>(--It);
    // Try to skip another instruction, in case the class_method came first.
    if (!SRI && It != Entry->begin())
      SRI = dyn_cast<StrongRetainInst>(--It);
    if (SRI && SRI->getOperand() == ClassInstance) {
      VirtBuilder.createStrongRetain(SRI->getLoc(), ClassInstance)
        ->setDebugScope(SRI->getDebugScope());
      IdenBuilder.createStrongRetain(SRI->getLoc(), DownCastedClassInstance)
        ->setDebugScope(SRI->getDebugScope());
      SRI->eraseFromParent();
    }
  }

  // Copy the two apply instructions into the two blocks.
  ApplyInst *IdenAI = CloneApply(AI, IdenBuilder);
  ApplyInst *VirtAI = CloneApply(AI, VirtBuilder);

  // Create a PHInode for returning the return value from both apply
  // instructions.
  SILArgument *Arg = Continue->createBBArg(AI->getType());
  IdenBuilder.createBranch(AI->getLoc(), Continue, ArrayRef<SILValue>(IdenAI))
    ->setDebugScope(AI->getDebugScope());
  VirtBuilder.createBranch(AI->getLoc(), Continue, ArrayRef<SILValue>(VirtAI))
    ->setDebugScope(AI->getDebugScope());

  // Remove the old Apply instruction.
  AI->replaceAllUsesWith(Arg);
  AI->eraseFromParent();

  // Update the stats.
  NumInlineCaches++;

  // Devirtualize the apply instruction on the identical path.
  devirtMethod(IdenAI, CMI->getMember(), DownCastedClassInstance, CD);

  // Sink class_method instructions down to their single user.
  if (CMI->hasOneUse())
    CMI->moveBefore(CMI->use_begin()->getUser());

  return VirtAI;
}

/// \brief Returns true, if a method implementation to be called by the
/// default case handler of a speculative devirtualization is statically
/// known. This happens if it can be proven that generated
/// checked_cast_br instructions cover all other possible cases.
///
/// \p CHA class hierarchy analysis to be used
/// \p AI  invocation instruction
/// \p CD  static class of the instance whose method is being invoked
/// \p Subs set of direct subclasses of this class
static bool isDefaultCaseKnown(ClassHierarchyAnalysis *CHA,
                               ApplyInst *AI,
                               ClassDecl *CD,
                               ClassHierarchyAnalysis::ClassList &Subs) {
  ClassMethodInst *CMI = cast<ClassMethodInst>(AI->getCallee());
  auto *Method = CMI->getMember().getFuncDecl();
  const DeclContext *DC = AI->getModule().getAssociatedContext();

  // Without an associated context we cannot perform any
  // access-based optimizations.
  if (!DC)
    return false;

  // Only handle classes defined within the SILModule's associated context.
  if (!CD->isChildContextOf(DC))
    return false;

  if (!CD->hasAccessibility())
    return false;

  // Only consider 'private' members, unless we are in whole-module compilation.
  switch (CD->getAccessibility()) {
  case Accessibility::Public:
    return false;
  case Accessibility::Internal:
    if (!AI->getModule().isWholeModule())
      return false;
    break;
  case Accessibility::Private:
    break;
  }

  // This is a private or a module internal class.
  //
  // We can analyze the class hierarchy rooted at it and
  // eventually devirtualize a method call more efficiently.

  // First, analyze all direct subclasses.
  // We know that a dedicated checked_cast_br check is
  // generated for each direct subclass by insertInlineCaches.
  for (auto S : Subs) {
    // Check if the subclass overrides a method
    auto *FD = S->findOverridingDecl(Method);
    if (!FD)
      continue;
    if (CHA->hasKnownDirectSubclasses(S)) {
      // This subclass has its own subclasses and
      // they will use this implementation or provide
      // their own. In either case it is not covered by
      // checked_cast_br instructions generated by
      // insertInlineCaches. Therefore it increases
      // the number of remaining cases to be handled
      // by the default case handler.
      return false;
    }
  }

  // Then, analyze indirect subclasses.

  // Set of indirect subclasses for the class.
  auto &IndirectSubs = CHA->getIndirectSubClasses(CD);

  // Check if any indirect subclasses use an implementation
  // of the method different from the implementation in
  // the current class. If this is the case, then such
  // an indirect subclass would need a dedicated
  // checked_cast_br check to be devirtualized. But this is
  // not done by insertInlineCaches yet and therefore
  // such a subclass should be handled by the "default"
  // case handler, which essentially means that "default"
  // case cannot be devirtualized since it covers more
  // then one alternative.
  for (auto S : IndirectSubs) {
    auto *ImplFD = S->findImplementingMethod(Method);
    if (ImplFD != Method) {
      // Different implementation is used by a subclass.
      // Therefore, the default case is not known.
      return false;
    }
  }

  return true;
}

/// \brief Try to insert inline cahces for the call \p AI. This function
/// returns true if a change was made.
static bool insertInlineCaches(ApplyInst *AI, ClassHierarchyAnalysis *CHA) {
  ClassMethodInst *CMI = cast<ClassMethodInst>(AI->getCallee());
  assert(CMI && "Invalid class method instruction");

  SILValue ClassInstance = CMI->getOperand();
  // The static type used by the class_method instruction
  // is the class which a given method belongs to.
  // Is either the class of the instance itself or one of its superclasses.
  // Therefore, strip all upcasts to get the real static type
  // of the instance.
  // Specifically, only the upcast to the static class which method belongs to
  // should be stripped.
  SILType InstanceType = ClassInstance.stripUpCasts().getType();
  ClassDecl *CD = InstanceType.getClassOrBoundGenericClass();

  if (auto *VMTI = dyn_cast<ValueMetatypeInst>(ClassInstance.stripUpCasts())) {
    CanType InstTy = VMTI->getType().castTo<MetatypeType>().getInstanceType();
    CD = InstTy.getClassOrBoundGenericClass();
  }

  // Check if it is legal to insert inline caches.
  if (!CD || CMI->isVolatile())
    return false;

  if (ClassInstance.getType() != InstanceType) {
    // The implementation of a method to be invoked may actually
    // be defined by one of the superclasses.
    if (!ClassInstance.getType().isSuperclassOf(InstanceType))
      return false;
    // ClassInstance and InstanceType should match for devirtMethod to work.
    ClassInstance = ClassInstance.stripUpCasts();
  }

  if (!CHA->hasKnownDirectSubclasses(CD)) {
    DEBUG(llvm::dbgs() << "Inserting monomorphic inline caches for class " <<
          CD->getName() << "\n");
    return insertMonomorphicInlineCaches(AI, InstanceType);
  }

  // Collect the direct subclasses for the class.
  auto &Subs = CHA->getDirectSubClasses(CD);

  if (Subs.size() > MaxNumPolymorphicInlineCaches) {
    DEBUG(llvm::dbgs() << "Class " << CD->getName() << " has too many (" <<
          Subs.size() << ") subclasses. Not inserting inline caches.\n");
    return false;
  }

  DEBUG(llvm::dbgs() << "Class " << CD->getName() << " is a superclass. "
        "Inserting polymorphic inline caches.\n");

  // Perform a speculative devirtualization of a method invocation.
  // It replaces an indirect class_method-based call by a code to perform
  // a direct call of the method implementation based on the dynamic class
  // of the instance.
  //
  // The code is generated according to the following principles:
  //
  // - For each direct subclass, a dedicated checked_cast_br instruction
  // is generated to check if a dynamic class of the instance is exactly
  // this subclass.
  //
  // - If this check succeeds, then it jumps to the code which performs a
  // direct call of a method implementation specific to this subclass.
  //
  // - If this check fails, then a different subclass is checked by means of
  // checked_cast_br in a similar way.
  //
  // - Finally, if the instance does not exactly match any of the direct
  // subclasses, the "default" case code is generated, which should handle
  // all remaining alternatives, i.e. it should be able to dispatch to any
  // possible remaining method implementations. Typically this is achieved by
  // using a class_method instruction, which performs an indirect invocation.
  // But if it can be proven that only one specific implementation of
  // a method will be always invoked by this code, then a class_method-based
  // call can be devirtualized and replaced by a more efficient direct
  // invocation of this specific method implementation.
  //
  // Remark: With the current implementation of a speculative devirtualization,
  // if devirtualization of the "default" case is possible, then it would
  // by construction directly invoke the implementation of the method
  // corresponding to the static type of the instance. This may change
  // in the future, if we start using PGO for ordering of checked_cast_br
  // checks.

  // TODO: The ordering of checks may benefit from using a PGO, because
  // the most probable alternatives could be checked first.

  // Number of subclasses which cannot be handled by checked_cast_br checks.
  int NotHandledSubsNum = 0;
  // True if any instructions were changed or generated.
  bool Changed = false;

  for (auto S : Subs) {
    DEBUG(llvm::dbgs() << "Inserting a cache for class " << CD->getName() <<
          " and subclass " << S->getName() << "\n");

    CanType CanClassType = S->getDeclaredType()->getCanonicalType();
    SILType InstanceType = SILType::getPrimitiveObjectType(CanClassType);
    if (!InstanceType.getClassOrBoundGenericClass()) {
      // This subclass cannot be handled. This happens e.g. if it is
      // a generic class.
      NotHandledSubsNum++;
      continue;
    }

    AI = insertMonomorphicInlineCaches(AI, InstanceType);
    assert(AI && "Unable to insert inline caches!");
    Changed = true;
  }

  // Check if there is only a single statically known implementation
  // of the method which can be called by the default case handler.
  if (NotHandledSubsNum || !isDefaultCaseKnown(CHA, AI, CD, Subs)) {
    // Devirtualization of remaining cases is not possible,
    // because more than one implementation of the method
    // needs to be handled here. Thus, an indirect call through
    // the class_method cannot be eliminated completely.
    //
    // But we can still try to devirtualize the static class of instance
    // if it is possible.
    return insertMonomorphicInlineCaches(AI, InstanceType);
  }

  // At this point it is known that there is only one remaining method
  // implementation which is not covered by checked_cast_br checks yet.
  // So, it is safe to replace a class_method invocation by
  // a direct call of this remaining implementation.
  devirtMethod(AI, CMI->getMember(), ClassInstance, CD);

  return true;
}

namespace {
  /// Generate inline caches of virtual calls by speculating that the requested
  /// class is at the bottom of the class hierarchy.
  class SILInlineCaches : public SILFunctionTransform {
  public:
    virtual ~SILInlineCaches() {}

    virtual void run() {
      ClassHierarchyAnalysis *CHA = PM->getAnalysis<ClassHierarchyAnalysis>();

      bool Changed = false;

      // Collect virtual calls that may be specialized.
      SmallVector<ApplyInst *, 16> ToSpecialize;
      for (auto &BB : *getFunction()) {
        for (auto II = BB.begin(), IE = BB.end(); II != IE; ++II) {
          ApplyInst *AI = dyn_cast<ApplyInst>(&*II);
          if (AI && isa<ClassMethodInst>(AI->getCallee()))
            ToSpecialize.push_back(AI);
        }
      }

      // Create the inline caches.
      for (auto AI : ToSpecialize)
        Changed |= insertInlineCaches(AI, CHA);

      if (Changed) {
        invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
      }
    }

    StringRef getName() override { return "Inline Caches"; }
  };

} // end anonymous namespace

SILTransform *swift::createInlineCaches() {
  return new SILInlineCaches();
}

