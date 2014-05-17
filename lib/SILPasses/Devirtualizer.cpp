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
#include "swift/Basic/Demangle.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/CallGraph.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
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

STATISTIC(NumDevirtualized, "Number of calls devirtualzied");
STATISTIC(NumDynApply, "Number of dynamic apply devirtualzied");
STATISTIC(NumAMI, "Number of witness_method devirtualzied");

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

/// \brief Recursively searches the ClassDecl for the type of \p S, or null.
static ClassDecl *findClassDeclForOperand(SILValue S) {
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

/// Optimize a class_method and alloc_ref pair into a direct function
/// reference:
///
/// \code
/// %XX = alloc_ref $Foo
/// %YY = class_method %XX : $Foo, #Foo.get!1 : $@cc(method) @thin ...
/// apply %YY(...)
/// \endcode
///
///  or
///
/// %XX = metatype $...
/// %YY = class_method %XX : ...
/// apply %YY(...)
///
///  into
///
/// %YY = function_ref @...
/// apply %YY(...)
///
static bool optimizeClassMethod(ApplyInst *AI, ClassMethodInst *CMI) {
  DEBUG(llvm::dbgs() << "    Trying to optimize : " << *CMI);

  // First attempt to lookup the origin for our class method. The origin should
  // either be a metatype or an alloc_ref.
  SILValue Origin = CMI->getOperand().stripCasts();
  DEBUG(llvm::dbgs() << "        Origin: " << Origin);

  // Then attempt to lookup the class for origin.
  ClassDecl *Class = findClassDeclForOperand(Origin);

  // If we are unable to lookup a class decl for origin there is nothing here
  // that we can deserialize.
  if (!Class) {
    DEBUG(llvm::dbgs() << "        FAIL: Could not find class decl for "
                          "SILValue.\n");
    return false;
  }

  // Otherwise lookup from the module the least derived implementing method from
  // the module vtables.
  SILModule &Mod = CMI->getModule();
  SILFunction *F = Mod.lookUpSILFunctionFromVTable(Class, CMI->getMember());

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
  CanSILFunctionType SubstCalleeType =
    GenCalleeType->substInterfaceGenericArgs(M, M.getSwiftModule(),
                                             AI->getSubstitutions());


  // If F's this pointer has a different type from CMI's operand and the
  // "this" pointer type is a super class of the CMI's operand, insert an
  // upcast.
  auto paramTypes =
    SubstCalleeType->getInterfaceParameterSILTypesWithoutIndirectResult();

  // We should always have a this pointer. Assert on debug builds, return
  // nullptr on release builds.
  assert(!paramTypes.empty() &&
         "Must have a this pointer when calling a class method inst.");
  if (paramTypes.empty())
    return false;

  // Grab the self type from the function ref and the self type from the class
  // method inst.
  SILType FuncSelfTy = paramTypes[paramTypes.size() - 1];
  SILType OriginTy = Origin.getType();
  SILBuilder B(AI);

  // Then compare the two types and if they are unequal...
  if (FuncSelfTy != OriginTy) {
    assert(FuncSelfTy.isSuperclassOf(OriginTy) &&
           "Can not call a class method on a non-subclass of the class_methods"
           " class.");

    // Otherwise, upcast origin to the appropriate type.
    Origin = B.createUpcast(CMI->getLoc(), Origin, FuncSelfTy);
  }

  // Success! Perform the devirtualization.
  FunctionRefInst *FRI = B.createFunctionRef(CMI->getLoc(), F);

  // Construct a new arg list. First process all non-self operands, ref, addr
  // casting them to the appropriate types for F so that we allow for covariant
  // indirect return types and contravariant arguments.
  llvm::SmallVector<SILValue, 8> NewArgs;
  auto Args = AI->getArguments();
  auto allParamTypes = SubstCalleeType->getInterfaceParameterSILTypes();

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
  NewArgs.push_back(Origin);

  // If we have a direct return type, make sure we use the subst callee return
  // type. If we have an indirect return type, AI's return type of the empty
  // tuple should be ok.
  SILType ReturnType = AI->getType();
  if (!SubstCalleeType->hasIndirectResult()) {
    ReturnType = SubstCalleeType->getSILInterfaceResult();
  }

  SILType SubstCalleeSILType = SILType::getPrimitiveObjectType(SubstCalleeType);
  ApplyInst *NewAI =
      B.createApply(AI->getLoc(), FRI, SubstCalleeSILType, ReturnType,
                    AI->getSubstitutions(), NewArgs);

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
  if (CMI->use_empty())
    CMI->eraseFromParent();

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

  /// If we had other substitutions that
  SmallVector<Substitution, 16> SubstList;

  /// Field which distinguishes the SubstList being empty from it not having a
  /// value. This could be as apart of an optional but I don't think that
  bool HasSubstList;

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
      : AI(AI), WMI(WMI), C(C), F(F), Subs(Subs), WT(WT), Self(), SubstList(),
        HasSubstList(false), SubstCalleeSILType(), ResultSILType() {}

  /// Main entry point.
  bool devirtualize();
private:

  bool processNormalProtocolConformance();
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

bool WitnessMethodDevirtualizer::processNormalProtocolConformance() {
  // Ok, we found the member we are looking for. Devirtualize away!
  SILBuilder Builder(AI);
  SILLocation Loc = AI->getLoc();
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, F);

  // Collect args from the apply inst.
  SmallVector<SILValue, 4> Args;

  // First add the non self arguments to the new argument list.
  for (SILValue A : AI->getArgumentsWithoutSelf())
    Args.push_back(A);

  // Then add the self argument since the self argument is always last.
  if (!Self)
    Self = AI->getSelfArgument();
  Args.push_back(*Self);

  SmallVector<Substitution, 16> NewSubstList(Subs.begin(),
                                             Subs.end());

  // Add the non-self-derived substitutions from the original application.
  for (auto &origSub : AI->getSubstitutionsWithoutSelfSubstitution()) {
    if (!origSub.Archetype->isSelfDerived())
      NewSubstList.push_back(origSub);
  }

  if (!SubstCalleeSILType)
    SubstCalleeSILType = AI->getSubstCalleeSILType();
  if (!ResultSILType)
    ResultSILType = AI->getType();

  ApplyInst *SAI = Builder.createApply(Loc, FRI, *SubstCalleeSILType,
                                       *ResultSILType, NewSubstList, Args);
  AI->replaceAllUsesWith(SAI);
  AI->eraseFromParent();
  NumAMI++;
  return true;
}

bool WitnessMethodDevirtualizer::processInheritedProtocolConformance() {
  // Since we do not need to worry about substitutions, we can just insert an
  // upcast of self to the appropriate type.
  Self = AI->getSelfArgument();
  CanType Ty = WT->getConformance()->getType()->getCanonicalType();
  SILType SILTy = SILType::getPrimitiveType(Ty, Self->getType().getCategory());
  SILType SelfTy = Self->getType();
  (void)SelfTy;

  assert(SILTy.isSuperclassOf(SelfTy) &&
         "Should only create upcasts for sub class devirtualization.");
  Self = SILBuilder(AI).createUpcast(AI->getLoc(), *Self, SILTy);

  SmallVector<Substitution, 16> SelfDerivedSubstitutions;
  for (auto &origSub : AI->getSubstitutions())
    if (origSub.Archetype->isSelfDerived())
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
  Substitution NewSelfSub = AI->getSelfSubstitution();
  NewSelfSub.Replacement = Ty;

  CanSILFunctionType OrigType = AI->getOrigCalleeType();
  CanSILFunctionType SubstCalleeType = OrigType->substInterfaceGenericArgs(
      AI->getModule(), AI->getModule().getSwiftModule(),
      ArrayRef<Substitution>(NewSelfSub));

  SubstCalleeSILType = SILType::getPrimitiveObjectType(SubstCalleeType);

  // Then pass of our new self to the normal protocol conformance witness method
  // handling code.
  return processNormalProtocolConformance();
}

bool
WitnessMethodDevirtualizer::
processSpecializedProtocolConformance(SpecializedProtocolConformance *SPC,
                                      ProtocolConformance *GenericConf) {
  // We do not need to worry about covariant/contravariant return types here
  // since we don't support that for 1.0. But when we do this needs to be
  // updated.
  SILModule &M = F->getModule();
  CanSILFunctionType GenCalleeType = F->getLoweredFunctionType();
  CanSILFunctionType SubstCalleeType =
    GenCalleeType->substInterfaceGenericArgs(M, M.getSwiftModule(),
                                             Subs);

  std::copy(Subs.begin(), Subs.end(), SubstList.begin());
  HasSubstList = true;
  SubstCalleeSILType = SILType::getPrimitiveObjectType(SubstCalleeType);
  ResultSILType = SubstCalleeType->getSILInterfaceResult();

  return processNormalProtocolConformance();
}

/// Devirtualize apply instructions that call witness_method instructions:
///
///   %8 = witness_method $Optional<UInt16>, #LogicValue.getLogicValue!1
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
//                        Protocol Method Optimization
//===----------------------------------------------------------------------===//


/// \brief Scan the uses of the protocol object and return the initialization
/// instruction, which can be copy_addr or init_existential.
/// There needs to be only one initialization instruction and the
/// object must not be captured by any instruction that may re-initialize it.
static SILInstruction *
findSingleInitNoCaptureProtocol(SILValue ProtocolObject) {
  DEBUG(llvm::dbgs() << "        Checking if protocol object is captured: "
        << ProtocolObject);
  SILInstruction *Init = 0;
  for (auto UI = ProtocolObject->use_begin(), E = ProtocolObject->use_end();
       UI != E; UI++) {
    switch (UI.getUser()->getKind()) {
    case ValueKind::CopyAddrInst: {
      // If we are reading the content of the protocol (to initialize
      // something else) then its okay.
      if (cast<CopyAddrInst>(UI.getUser())->getSrc() == ProtocolObject)
        continue;

      // Fall through.
      SWIFT_FALLTHROUGH;
    }

    case ValueKind::InitExistentialInst: {
      // Make sure there is a single initialization:
      if (Init) {
        DEBUG(llvm::dbgs() << "            FAIL: Multiple Protocol "
              "initializers: " << *UI.getUser() << " and " << *Init);
        return nullptr;
      }
      // This is the first initialization.
      Init = UI.getUser();
      continue;
    }
    case ValueKind::OpenExistentialInst:
    case ValueKind::ProjectExistentialInst:
    case ValueKind::ProtocolMethodInst:
    case ValueKind::DeallocBoxInst:
    case ValueKind::DeallocRefInst:
    case ValueKind::DeallocStackInst:
    case ValueKind::StrongReleaseInst:
    case ValueKind::DestroyAddrInst:
    case ValueKind::ReleaseValueInst:
      continue;

    // A thin apply inst that uses this object as a callee does not capture it.
    case ValueKind::ApplyInst: {
      auto *AI = cast<ApplyInst>(UI.getUser());
      if (AI->isCalleeThin() && AI->getCallee() == ProtocolObject)
        continue;

      // Fallthrough
      SWIFT_FALLTHROUGH;
    }

    default: {
      DEBUG(llvm::dbgs() << "            FAIL: Protocol captured by: "
                         << *UI.getUser());
      return nullptr;
    }
    }
  }
  DEBUG(llvm::dbgs() << "            Protocol not captured.\n");
  return Init;
}

/// \brief Replaces a virtual ApplyInst instruction with a new ApplyInst
/// instruction that does not use a project_existential \p PEI and calls \p F
/// directly. See visitApplyInst.
static ApplyInst *replaceDynApplyWithStaticApply(ApplyInst *AI, SILFunction *F,
                                                 ArrayRef<Substitution> Subs,
                                                 InitExistentialInst *In,
                                                 ProjectExistentialInst *PEI) {
  // Creates a new FunctionRef Inst and inserts it to the basic block.
  FunctionRefInst *FRI = new (AI->getModule()) FunctionRefInst(AI->getLoc(), F);
  AI->getParent()->getInstList().insert(AI, FRI);
  SmallVector<SILValue, 4> Args;

  // Push all of the args and replace uses of PEI with the InitExistentional.
  MutableArrayRef<Operand> OrigArgs = AI->getArgumentOperands();
  for (unsigned i = 0; i < OrigArgs.size(); i++) {
    SILValue A = OrigArgs[i].get();
    Args.push_back(A.getDef() == PEI ? In : A);
  }

  // Create a new non-virtual ApplyInst.
  SILType SubstFnTy = FRI->getType().substInterfaceGenericArgs(F->getModule(),
                                                               Subs);
  ApplyInst *SAI = ApplyInst::create(
      AI->getLoc(), FRI, SubstFnTy,
      SubstFnTy.castTo<SILFunctionType>()->getInterfaceResult().getSILType(),
      Subs, Args, false, *F);
  AI->getParent()->getInstList().insert(AI, SAI);
  AI->replaceAllUsesWith(SAI);
  AI->eraseFromParent();
  return SAI;
}

static bool substitutionListHasUnboundGenerics(ArrayRef<Substitution> Subs) {
  for (auto &Sub : Subs)
    if (hasUnboundGenericTypes(Sub.Replacement->getCanonicalType()))
      return true;
  return false;
}

/// Temperary helper method that should be removed when inherited protocol
/// conformances are implemented for protocol methods.
static bool
inheritenceProtocolConformanceFreeConformance(ProtocolConformance *C) {
  if (isa<NormalProtocolConformance>(C))
    return true;

  if (auto *SPC = dyn_cast<SpecializedProtocolConformance>(C))
    if (isa<NormalProtocolConformance>(SPC->getGenericConformance()))
      return true;

  return false;
}

/// Devirtualize protocol_method + project_existential + init_existential
/// instructions.  For example:
///
/// %0 = alloc_box $Pingable
/// %1 = init_existential %0#1 : $*Pingable, $*Foo  <-- Foo is the static type!
/// %4 = project_existential %0#1 : $*Pingable to $*@sil_self Pingable
/// %5 = protocol_method %0#1 : $*Pingable, #Pingable.ping!1 :
/// %8 = apply %5(ARGUMENTS ... , %4) :
static bool optimizeProtocolMethod(ApplyInst *AI, ProtocolMethodInst *PMI) {
  if (!PMI)
    return false;

  DEBUG(llvm::dbgs() << "        Found ProtocolMethodInst: " << *PMI);

  // Find the last argument, which is the Self argument, which may be a
  // project_existential instruction.
  if (!AI->getNumArguments())
    return false;

  SILValue Self = AI->getSelfArgument();
  ProjectExistentialInst *PEI = dyn_cast<ProjectExistentialInst>(Self);
  if (!PEI)
    return false;

  DEBUG(llvm::dbgs() << "        Found ProjectExistentialInst: " << *PEI);

  // Make sure that the project_existential and protocol_method instructions
  // use the same protocol.
  SILValue ProtocolObject = PMI->getOperand();
  if (PEI->getOperand().getDef() != ProtocolObject.getDef())
    return false;

  DEBUG(llvm::dbgs() << "        Protocol to devirtualize : "
                     << *ProtocolObject.getDef());

  // Find a single initialization point, and make sure the protocol is not
  // captured. We also handle the case where the initializer is the copy_addr
  // instruction by looking at the source object.
  SILInstruction *InitInst = findSingleInitNoCaptureProtocol(ProtocolObject);
  if (CopyAddrInst *CAI = dyn_cast_or_null<CopyAddrInst>(InitInst)) {
    if (!CAI->isInitializationOfDest() || !CAI->isTakeOfSrc())
      return false;

    InitInst = findSingleInitNoCaptureProtocol(CAI->getSrc());
  }

  InitExistentialInst *Init = dyn_cast_or_null<InitExistentialInst>(InitInst);
  if (!Init)
    return false;
  DEBUG(llvm::dbgs() << "        InitExistentialInst : " << *Init);

  SILModule &Mod = Init->getModule();
  // For each protocol that our type conforms to:
  for (ProtocolConformance *Conf : Init->getConformances()) {

    SILFunction *StaticRef;
    ArrayRef<Substitution> Subs;
    SILWitnessTable *WT;
    std::tie(StaticRef, WT, Subs) =
      Mod.findFuncInWitnessTable(Conf, PMI->getMember());

    if (!StaticRef) {
      assert(!WT && "WT must always be null if static ref is.");
      continue;
    }
    assert(WT && "WT must never be null if static ref is also not.");

    // If any of our subs is generic, bail...
    if (substitutionListHasUnboundGenerics(Subs))
      continue;

    // If we have any form of inherited protocol conformance, bail.
    //
    // FIXME: Handle this along the lines of what was done with the witness
    // method devirtualization.
    if (!inheritenceProtocolConformanceFreeConformance(Conf))
      return false;

    DEBUG(llvm::dbgs() << "        SUCCESS! Devirtualized : " << *AI);
    ApplyInst *NewApply =
        replaceDynApplyWithStaticApply(AI, StaticRef, Subs, Init, PEI);
    DEBUG(llvm::dbgs() << "                To : " << *NewApply);
    NumDynApply++;
    return true;
  }

  DEBUG(llvm::dbgs() << "        FAIL: Could not find a witness table "
                        "for: " << *PMI);
  return false;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static bool optimizeApplyInst(ApplyInst *AI) {
  DEBUG(llvm::dbgs() << "    Trying to optimize ApplyInst : " << *AI);

  // Devirtualize apply instructions that call witness_method instructions:
  //
  //   %8 = witness_method $Optional<UInt16>, #LogicValue.getLogicValue!1
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
  if (auto *CMI = dyn_cast<ClassMethodInst>(AI->getCallee()))
    return optimizeClassMethod(AI, CMI);

  // Devirtualize protocol_method + project_existential + init_existential
  // instructions.  For example:
  //
  // %0 = alloc_box $Pingable
  // %1 = init_existential %0#1 : $*Pingable, $*Foo  <-- Foo is the static type!
  // %4 = project_existential %0#1 : $*Pingable to $*@sil_self Pingable
  // %5 = protocol_method %0#1 : $*Pingable, #Pingable.ping!1 :
  // %8 = apply %5(ARGUMENTS ... , %4) :
  ProtocolMethodInst *PMI = dyn_cast<ProtocolMethodInst>(AI->getCallee());
  if (!PMI)
    return false;
  return optimizeProtocolMethod(AI, PMI);
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
                       << Demangle::demangleSymbolAsString(F.getName())
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
