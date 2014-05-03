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
#include "swift/SIL/SILCloner.h"
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

static const unsigned RecursionMaxDepth = 8;

STATISTIC(NumDevirtualized, "Number of calls devirtualzied");
STATISTIC(NumDynApply, "Number of dynamic apply devirtualzied");
STATISTIC(NumAMI, "Number of witness_method devirtualzied");

namespace {
struct SILDevirtualizer {
  /// The SIL Module.
  SILModule *M = nullptr;

  bool Changed = false;

  /// The instruction to be deleted.
  SILInstruction *toBeDeleted = nullptr;

  /// True if another round of specialization may help.
  bool AttemptToSpecialize = false;

  SILDevirtualizer(SILModule *M)
    : M(M), Changed(false) {}

  /// Check for type mismatch when replacing a ClassMethodInst with a
  /// FunctionRefInst.
  bool checkDevirtType(SILFunction *F, ClassMethodInst *CMI);

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
  void optimizeClassMethodInst(ClassMethodInst *CMI);

  void optimizeApplyInst(ApplyInst *Inst);
  void optimizeFuncBody(SILFunction *F);

  bool optimizeApplyOfWitnessMethod(ApplyInst *AI, WitnessMethodInst *AMI);
  bool optimizeApplyOfProtocolMethod(ApplyInst *AI, ProtocolMethodInst *PMI);

  bool run();
};

} // anonymous namespace.

/// \brief Returns the index of the argument that the function returns or -1
/// if the return value is not always an argument.
static int functionReturnsArgument(SILFunction *F) {
  if (F->getBlocks().size() != 1)
    return -1;

  // Check if there is a single terminator which is a ReturnInst.
  ReturnInst *RI = dyn_cast<ReturnInst>(F->begin()->getTerminator());
  if (!RI)
    return -1;

  // Check that the single return instruction that we found returns the
  // correct argument. Scan all of the argument and check if the return inst
  // returns them.
  ValueBase *ReturnedVal = RI->getOperand().getDef();
  for (int i = 0, e = F->begin()->getNumBBArg(); i != e; ++i)
    if (F->begin()->getBBArg(i) == ReturnedVal)
      return i;

  // The function does not return an argument.
  return -1;
}

/// \brief Returns the single return value if there is one.
static SILValue functionSingleReturn(SILFunction *F) {
  if (F->getBlocks().size() != 1)
    return SILValue();

  // Check if there is a single terminator which is a ReturnInst.
  ReturnInst *RI = dyn_cast<ReturnInst>(F->begin()->getTerminator());
  if (!RI)
    return SILValue();
  return RI->getOperand();
}

static SILValue findOrigin(SILValue S) {
  SILValue Origin = S;
  unsigned Depth = 0;
  for (; Depth < RecursionMaxDepth; ++Depth) {
    switch (Origin->getKind()) {
      default:
        break;
      case ValueKind::UpcastInst:
      case ValueKind::UnconditionalCheckedCastInst:
      case ValueKind::UncheckedRefCastInst:
        Origin = cast<SILInstruction>(Origin)->getOperand(0);
        continue;
      case ValueKind::ApplyInst: {
        ApplyInst *AI = cast<ApplyInst>(Origin);
        FunctionRefInst *FR =
          dyn_cast<FunctionRefInst>(AI->getCallee());
        if (!FR)
          break;

        SILFunction *F = FR->getReferencedFunction();
        if (F->isExternalDeclaration()) {
          if (!F->getModule().linkFunction(F, SILModule::LinkingMode::LinkAll))
            break;
        }

        // Does this function return one of its arguments ?
        int RetArg = functionReturnsArgument(F);
        if (RetArg != -1) {
          Origin = AI->getOperand(1 /* 1st operand is Callee */ + RetArg);
          continue;
        }
        SILValue RetV = functionSingleReturn(F);
        if (RetV.isValid()) {
          Origin = RetV;
          continue;
        }
        break;
      }
    }
    // No cast or pass-thru args found.
    break;
  }
  DEBUG(if (Depth == RecursionMaxDepth)
          llvm::dbgs() << "findMetaType: Max recursion depth.\n");

  return Origin;
}

/// \brief Scan the use-def chain and skip cast instructions that don't change
/// the value of the class. Stop on classes that define a class type.
static SILInstruction *findMetaType(SILValue S, unsigned Depth = 0) {
  SILInstruction *Inst = dyn_cast<SILInstruction>(findOrigin(S));
  if (!Inst)
    return nullptr;

  switch (Inst->getKind()) {
  case ValueKind::AllocRefInst:
  case ValueKind::AllocRefDynamicInst:
  case ValueKind::MetatypeInst:
    return Inst;
  default:
    return nullptr;
  }
}

/// \brief Recursively searches the ClassDecl for the type of \p S, or null.
static ClassDecl *findClassDeclForOperand(SILValue S) {
  // Look for an instruction that defines a class type.
  SILInstruction *Meta = findMetaType(S);
  if (!Meta)
    return nullptr;

  // Look for a a static ClassTypes in AllocRefInst or MetatypeInst.
  if (AllocRefInst *ARI = dyn_cast<AllocRefInst>(Meta)) {
    return ARI->getType().getClassOrBoundGenericClass();
  } else if (MetatypeInst *MTI = dyn_cast<MetatypeInst>(Meta)) {
    CanType instTy = MTI->getType().castTo<MetatypeType>().getInstanceType();
    return instTy.getClassOrBoundGenericClass();
  } else {
    return nullptr;
  }
}

bool SILDevirtualizer::checkDevirtType(SILFunction *F, ClassMethodInst *CMI) {
  auto paramTypes = F->getLoweredFunctionType()
                     ->getInterfaceParametersWithoutIndirectResult();
  if (paramTypes.empty())
    return true;

  auto paramTy = paramTypes[paramTypes.size() - 1].getType();
  auto argTy = CMI->getOperand().getType().getSwiftRValueType();
  if (paramTy == argTy ||
      paramTy->getClassOrBoundGenericClass() ==
      argTy->getClassOrBoundGenericClass())
    return true;

  // When there is a type mismatch, we currently only allow upcast where
  // the source operand has the expected paramTy. Another option is to perform
  // a downcast to have the correct type.
  if (CMI->getOperand()->getKind() != ValueKind::UpcastInst)
    return false;
  auto origin = cast<SILInstruction>(CMI->getOperand())->getOperand(0);

  // See if Origin has any substitutions. If it does, we need to specialize
  // paramTy.
  //
  // FIXME: See if this can be moved to the beginning.
  SILModule &Mod = F->getModule();
  SILType originTy = origin.getType();
  ArrayRef<Substitution> originSubs = originTy.gatherAllSubstitutions(Mod);
  if (originSubs.size()) {
    CanSILFunctionType FTy = F->getLoweredFunctionType();
    FTy = FTy->substInterfaceGenericArgs(Mod,
                                         Mod.getSwiftModule(),
                                         originSubs);
    paramTypes = FTy->getInterfaceParametersWithoutIndirectResult();
    paramTy = paramTypes[paramTypes.size() - 1].getType();
  }

  if (paramTy != originTy.getSwiftRValueType())
    return false;

  // We handle updating of ApplyInst only.
  for (auto UI = CMI->use_begin(), UE = CMI->use_end(); UI != UE; ++UI)
    if (!isa<ApplyInst>(UI->getUser()))
      return false;

  // Find the next instruction of CMI before modifying anything.
  SILBasicBlock::iterator iter(CMI);
  ++iter;
  SILInstruction *nextI = (iter == CMI->getParent()->end()) ? nullptr : iter;
  for (auto UI = CMI->use_begin(), UE = CMI->use_end(); UI != UE; ++UI)
    if (ApplyInst *AI = dyn_cast<ApplyInst>(UI->getUser())) {
      // Update the last argument for ApplyInst.
      SILBuilder Builder(AI);

      SmallVector<SILValue, 4> Args;
      ArrayRef<Operand> ApplyArgs = AI->getArgumentOperands();
      for (auto &A : ApplyArgs.slice(0, ApplyArgs.size() - 1))
        Args.push_back(A.get());
      Args.push_back(origin);

      ApplyInst *SAI = Builder.createApply(AI->getLoc(), AI->getCallee(),
                                           AI->getSubstCalleeSILType(),
                                           AI->getType(),
                                           AI->getSubstitutions(), Args);
       AI->replaceAllUsesWith(SAI);
       // If AI is the next instruction after CMI, do not erase here. It can
       // cause invalid iterator for the loop in optimizeFuncBody.
       if (AI != nextI)
         AI->eraseFromParent();
       else
         toBeDeleted = AI;
    }
  return true;
}

void SILDevirtualizer::optimizeClassMethodInst(ClassMethodInst *CMI) {
  DEBUG(llvm::dbgs() << "    Trying to optimize : " << *CMI);

  // Attempt to find a ClassDecl for our operand.
  ClassDecl *Class = findClassDeclForOperand(findOrigin(CMI->getOperand()));

  // If we fail, bail...
  if (!Class) {
    DEBUG(llvm::dbgs() << "        FAIL: Could not find class decl for "
          "SILValue.\n");
    return;
  }

  // Otherwise walk up the class heirarchy until there are no more super classes
  // (i.e. Class becomes null) or we find a match with member.
  SILModule &Mod = CMI->getModule();
  SILFunction *F = Mod.lookUpSILFunctionFromVTable(Class, CMI->getMember());
  if (!F) {
    DEBUG(llvm::dbgs() << "        FAIL: Could not find matching VTable or "
          "vtable method for this class.\n");
    return;
  }

  // If F's this pointer has a different type from the CMI's operand, we
  // will have type checking issues later on. For now, we only devirtualize
  // if the operand is set with "upcast" and the source operand of "upcast"
  // has the required type.
  bool allowDevirt = checkDevirtType(F, CMI);
  if (!allowDevirt) {
    DEBUG(llvm::dbgs() <<
          " *** Disable devirtualization due to type mismatch : " << *CMI);
    return;
  }

  // Success! We found the method! Create a direct reference to it.
  FunctionRefInst *FRI =
    new (CMI->getModule()) FunctionRefInst(CMI->getLoc(), F);

  CMI->getParent()->getInstList().insert(CMI, FRI);
  CMI->replaceAllUsesWith(FRI);
  CMI->eraseFromParent();

  DEBUG(llvm::dbgs() << "        SUCCESS: " << F->getName() << "\n");
  NumDevirtualized++;
  Changed = true;
}

/// \brief Scan the uses of the protocol object and return the initialization
/// instruction, which can be copy_addr or init_existential.
/// There needs to be only one initialization instruction and the
/// object must not be captured by any instruction that may re-initialize it.
static SILInstruction *
findSingleInitNoCaptureProtocol(SILValue ProtocolObject) {
  DEBUG(llvm::dbgs() << "        Checking if protocol object is captured: " << ProtocolObject);
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
        DEBUG(llvm::dbgs() << "            FAIL: Multiple Protocol initializers: "
                           << *UI.getUser() << " and " << *Init);
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

/// Devirtualize apply instructions that call witness_method instructions:
///
///   %8 = witness_method $Optional<UInt16>, #LogicValue.getLogicValue!1
///   %9 = apply %8<Self = CodeUnit?>(%6#1) : ...
///
bool SILDevirtualizer::optimizeApplyOfWitnessMethod(ApplyInst *AI,
                                                    WitnessMethodInst *AMI) {
  ProtocolConformance *C = AMI->getConformance();
  if (!C) {
    DEBUG(llvm::dbgs() << "        FAIL: Null conformance.\n");
    return false;
  }

  // Lookup the function reference in the witness tables.
  SILFunction *F;
  ArrayRef<Substitution> Subs;
  SILWitnessTable *WitnessTable;
  std::tie(F, WitnessTable, Subs) =
      AI->getModule().findFuncInWitnessTable(C, AMI->getMember());

  if (!F) {
    assert(!WitnessTable && "WitnessTable should always be null if F is.");
    DEBUG(llvm::dbgs() << "        FAIL: Did not find a matching witness "
                          "table or witness method.\n");
    return false;
  }
  assert(WitnessTable && "WitnessTable should never be null if F is not.");

  // Ok, we found the member we are looking for. Devirtualize away!
  SILBuilder Builder(AI);
  SILLocation Loc = AI->getLoc();
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, F);

  // Collect args from the apply inst.
  ArrayRef<Operand> ApplyArgs = AI->getArgumentOperands();
  SmallVector<SILValue, 4> Args;

  // Begin by upcasting self to the appropriate type if we have an inherited
  // conformance...
  SILValue Self = ApplyArgs[0].get();
  if (C->getKind() == ProtocolConformanceKind::Inherited) {
    CanType Ty = WitnessTable->getConformance()->getType()->getCanonicalType();
    SILType SILTy = SILType::getPrimitiveType(Ty, Self.getType().getCategory());
    SILType SelfTy = Self.getType();
    (void)SelfTy;

    if (Subs.size()) {
      // If we have substitutions then we are an inherited specialized
      // conformance. Substitute in the generic type so we upcast correctly.
      GenericParamList *GP = C->getSubstitutedGenericParams();
      if (!GP)
        return false;

      TypeSubstitutionMap TSM = GP->getSubstitutionMap(Subs);
      SILTy = SILTy.subst(F->getModule(), F->getModule().getSwiftModule(), TSM);
      SelfTy =
          SelfTy.subst(F->getModule(), F->getModule().getSwiftModule(), TSM);
    }

    assert(SILTy.isSuperclassOf(SelfTy) &&
           "Should only create upcasts for sub class devirtualization.");
    Self = Builder.createUpcast(Loc, Self, SILTy);
  }
  // and then adding in either the original or newly upcasted value.
  Args.push_back(Self);

  // Then add the rest of the arguments.
  for (auto &A : ApplyArgs.slice(1))
    Args.push_back(A.get());

  SmallVector<Substitution, 16> NewSubstList(Subs.begin(), Subs.end());

  // Add the non-self-derived substitutions from the original application.
  assert(AI->getSubstitutions().size() && "Subst list must not be empty");
  assert(AI->getSubstitutions()[0].Archetype->getSelfProtocol() &&
         "The first substitution needS to be a 'self' substitution.");
  for (auto &origSub : AI->getSubstitutions().slice(1)) {
    if (!origSub.Archetype->isSelfDerived())
      NewSubstList.push_back(origSub);
  }

  ApplyInst *SAI = Builder.createApply(Loc, FRI, AI->getSubstCalleeSILType(),
                                       AI->getType(), NewSubstList, Args);
  AI->replaceAllUsesWith(SAI);
  AI->eraseFromParent();
  NumAMI++;
  return true;
}

/// Devirtualize protocol_method + project_existential + init_existential
/// instructions.  For example:
///
/// %0 = alloc_box $Pingable
/// %1 = init_existential %0#1 : $*Pingable, $*Foo  <-- Foo is the static type!
/// %4 = project_existential %0#1 : $*Pingable to $*@sil_self Pingable
/// %5 = protocol_method %0#1 : $*Pingable, #Pingable.ping!1 :
/// %8 = apply %5(ARGUMENTS ... , %4) :
bool SILDevirtualizer::optimizeApplyOfProtocolMethod(ApplyInst *AI,
                                                     ProtocolMethodInst *PMI) {
  if (!PMI)
    return false;

  DEBUG(llvm::dbgs() << "        Found ProtocolMethodInst: " << *PMI);

  // Find the last argument, which is the Self argument, which may be a
  // project_existential instruction.
  MutableArrayRef<Operand> Args = AI->getArgumentOperands();
  if (Args.size() < 1)
    return false;

  SILValue LastArg = Args[Args.size() - 1].get();
  ProjectExistentialInst *PEI = dyn_cast<ProjectExistentialInst>(LastArg);
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

    // If any of our subs is generic, don't replace anything.
    bool FoundGenericSub = false;
    for (auto &Sub : Subs)
      if (hasUnboundGenericTypes(Sub.Replacement->getCanonicalType()))
        FoundGenericSub = true;

    if (FoundGenericSub)
      continue;

    DEBUG(llvm::dbgs() << "        SUCCESS! Devirtualized : " << *AI);
    ApplyInst *NewApply =
        replaceDynApplyWithStaticApply(AI, StaticRef, Subs, Init, PEI);
    DEBUG(llvm::dbgs() << "                To : " << *NewApply);
    NumDynApply++;
    Changed = true;
    return true;
  }

  DEBUG(llvm::dbgs() << "        FAIL: Could not find a witness table "
                        "for: " << *PMI);
  return false;
}

void SILDevirtualizer::optimizeApplyInst(ApplyInst *AI) {
  DEBUG(llvm::dbgs() << "    Trying to optimize ApplyInst : " << *AI);

  // Devirtualize apply instructions that call witness_method instructions:
  //
  //   %8 = witness_method $Optional<UInt16>, #LogicValue.getLogicValue!1
  //   %9 = apply %8<Self = CodeUnit?>(%6#1) : ...
  //
  if (auto *AMI = dyn_cast<WitnessMethodInst>(AI->getCallee())) {
    Changed |= optimizeApplyOfWitnessMethod(AI, AMI);
    return;
  }

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
    return;
  Changed |= optimizeApplyOfProtocolMethod(AI, PMI);
}

void SILDevirtualizer::optimizeFuncBody(SILFunction *F) {
  DEBUG(llvm::dbgs() << "*** Devirtualizing Function: "
        << Demangle::demangleSymbolAsString(F->getName()) << "\n");
  for (auto &BB : *F) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = I++; // Inst may be erased.
      if (Inst == toBeDeleted) {
        toBeDeleted = nullptr;
        Inst->eraseFromParent();
        continue;
      }
      if (ClassMethodInst *CMI = dyn_cast<ClassMethodInst>(Inst))
        optimizeClassMethodInst(CMI);
      else if (ApplyInst *AI = dyn_cast<ApplyInst>(Inst))
        optimizeApplyInst(AI);
    }
  }

  DEBUG(llvm::dbgs() << "\n");
}

bool SILDevirtualizer::run() {
  // Perform devirtualization locally and compute potential polymorphic
  // arguments for all existing functions.
  for (auto &F : *M)
    optimizeFuncBody(&F);
  return Changed;
}

namespace {
class SILDevirtualizationPass : public SILModuleTransform {
public:
  virtual ~SILDevirtualizationPass() {}

  /// The entry point to the transformation.
  virtual void run() {
    SILDevirtualizer DevirtImpl(getModule());
    bool Changed = DevirtImpl.run();
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
