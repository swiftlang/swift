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

#define DEBUG_TYPE "devirtualization"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/AST/ASTContext.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumDevirtualized, "Number of calls devirtualzied");
STATISTIC(NumDynApply, "Number of dynamic apply devirtualzied");

namespace {

struct SILDevirtualizer {
  /// The SIL Module.
  SILModule *M;
  bool Changed;

  SILDevirtualizer(SILModule *Mod) : M(Mod), Changed(false) {}

  void optimizeClassMethodInst(ClassMethodInst *CMI);
  void optimizeApplyInst(ApplyInst *Inst);

  bool run() {
    for (auto &F : *M)
      for (auto &BB : F) {
        auto I = BB.begin(), E = BB.end();
        while (I != E) {
          SILInstruction *Inst = I++;
          if (ClassMethodInst *CMI = dyn_cast<ClassMethodInst>(Inst))
            optimizeClassMethodInst(CMI);
          else if (ApplyInst *AI = dyn_cast<ApplyInst>(Inst))
            optimizeApplyInst(AI);
        }
      }

    return Changed;
  }
};

} // end anonymous namespace.

/// \brief Scan the use-def chain and skip cast instructions that don't change
/// the value of the class. Stop on classes that define a class type.
SILInstruction *findMetaType(SILValue S) {
  SILInstruction *Inst = dyn_cast<SILInstruction>(S);
  if (!Inst)
    return nullptr;

  switch (Inst->getKind()) {
  case ValueKind::AllocRefInst:
  case ValueKind::MetatypeInst:
    return Inst;
  case ValueKind::UpcastInst:
  case ValueKind::UnconditionalCheckedCastInst:
    return findMetaType(Inst->getOperand(0));
  default:
    return nullptr;
  }
}

/// \brief Recursively searches the ClassDecl for the type of \p S, or null.
ClassDecl *findClassTypeForOperand(SILValue S) {
  // Look for an instruction that defines a class type.
  SILInstruction *Meta = findMetaType(S);
  if (!Meta)
    return nullptr;

  // Look for a a static ClassTypes in AllocRefInst or MetatypeInst.
  if (AllocRefInst *ARI = dyn_cast<AllocRefInst>(Meta)) {
    return ARI->getType().getClassOrBoundGenericClass();
  } else if (MetatypeInst *MTI = dyn_cast<MetatypeInst>(Meta)) {
    CanType MetaTy = MTI->getType().getSwiftRValueType();
    TypeBase *T = cast<MetatypeType>(MetaTy)->getInstanceType().getPointer();
    return T->getClassOrBoundGenericClass();
  } else {
    return nullptr;
  }
}

void SILDevirtualizer::optimizeClassMethodInst(ClassMethodInst *CMI) {
  // Optimize a class_method and alloc_ref pair into a direct function
  // reference:
  //
  // %XX = alloc_ref $Foo
  // %YY = class_method %XX : $Foo, #Foo.get!1 : $@cc(method) @thin ...
  //
  //  or
  //
  //  %XX = metatype $...
  //  %YY = class_method %XX : ...
  //
  //  into
  //
  //  %YY = function_ref @...
  ClassDecl *Class = findClassTypeForOperand(CMI->getOperand());
  if (!Class)
    return;

  // Walk up the class hierarchy and scan all members.
  // TODO: There has to be a faster way of doing this scan.
  SILDeclRef Member = CMI->getMember();
  while (Class) {
    // Search all of the vtables in the module.
    for (auto &Vtbl : CMI->getModule().getVTableList()) {
      if (Vtbl.getClass() != Class)
        continue;

      // If found the requested method.
      if (SILFunction *F = Vtbl.getImplementation(CMI->getModule(), Member)) {
        // Create a direct reference to the method.
        SILInstruction *FRI = new (*M) FunctionRefInst(CMI->getLoc(), F);
        CMI->getParent()->getInstList().insert(CMI, FRI);

        CMI->replaceAllUsesWith(FRI);
        CMI->eraseFromParent();
        NumDevirtualized++;
        Changed = true;
        return;
      }
    }

    // We could not find the member in our class. Moving to our superclass.
    if (Type T = Class->getSuperclass())
      Class = T->getClassOrBoundGenericClass();
    else
      break;
  }

  return;
}

/// \brief Scan the uses of the protocol object and return the initialization
/// instruction, which can be copy_addr or init_existential.
/// There needs to be only one initialization instruction and the
/// object must not be captured by any instruction that may re-initialize it.
static SILInstruction *
findSingleInitNoCaptureProtocol(SILValue ProtocolObject) {
  SILInstruction *Init = 0;
  for (auto UI = ProtocolObject->use_begin(), E = ProtocolObject->use_end();
       UI != E; UI++) {
    switch (UI.getUser()->getKind()) {
    case ValueKind::CopyAddrInst: {
      // If we are reading the content of the protocol (to initialize
      // something else) then its okay.
      if (cast<CopyAddrInst>(UI.getUser())->getSrc() == ProtocolObject)
        continue;

      // fallthrough: ...
    }
    case ValueKind::InitExistentialInst: {
      // Make sure there is a single initialization:
      if (Init) {
        DEBUG(llvm::dbgs() << " *** Multiple Protocol initializers: "
                           << *UI.getUser() << " and " << *Init);
        return nullptr;
      }
      // This is the first initialization.
      Init = UI.getUser();
      continue;
    }
    case ValueKind::ProjectExistentialInst:
    case ValueKind::ProtocolMethodInst:
    case ValueKind::DeallocBoxInst:
    case ValueKind::DeallocRefInst:
    case ValueKind::DeallocStackInst:
    case ValueKind::StrongReleaseInst:
    case ValueKind::DestroyAddrInst:
    case ValueKind::DestroyValueInst:
      continue;

    default: {
      DEBUG(llvm::dbgs() << " *** Protocol captured by: " << *UI.getUser());
      return nullptr;
    }
    }
  }
  return Init;
}

/// \brief Replaces a virtual ApplyInst instruction with a new ApplyInst
/// instruction that does not use a project_existencial \p PEI and calls \p F
/// directly. See visitApplyInst.
static void replaceDynApplyWithStaticApply(ApplyInst *AI, SILFunction *F,
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
  SILType FnTy = FRI->getType();
  SILInstruction *SAI = ApplyInst::create(
      AI->getLoc(), FRI, FnTy,
      FnTy.castTo<SILFunctionType>()->getInterfaceResult().getSILType(),
      ArrayRef<Substitution>(), Args, false, *F);
  AI->getParent()->getInstList().insert(AI, SAI);
  AI->replaceAllUsesWith(SAI);
  AI->eraseFromParent();
}

void SILDevirtualizer::optimizeApplyInst(ApplyInst *AI) {
  // Devirtualize protocol_method + project_existential + init_existential
  // instructions.  For example:
  //
  // %0 = alloc_box $Pingable
  // %1 = init_existential %0#1 : $*Pingable, $*Foo  <-- Foo is the static type!
  // %4 = project_existential %0#1 : $*Pingable to $*@sil_self Pingable
  // %5 = protocol_method %0#1 : $*Pingable, #Pingable.ping!1 :
  // %8 = apply %5(ARGUMENTS ... , %4) :

  // Find the protocol_method instruction.
  ProtocolMethodInst *PMI = dyn_cast<ProtocolMethodInst>(AI->getCallee());
  if (!PMI)
    return;

  // Find the last argument, which is the Self argument, which may be a
  // project_existential instruction.
  MutableArrayRef<Operand> Args = AI->getArgumentOperands();
  if (Args.size() < 1)
    return;

  SILValue LastArg = Args[Args.size() - 1].get();
  ProjectExistentialInst *PEI = dyn_cast<ProjectExistentialInst>(LastArg);
  if (!PEI)
    return;

  // Make sure that the project_existential and protocol_method instructions
  // use the same protocol.
  SILValue ProtocolObject = PMI->getOperand();
  if (PEI->getOperand().getDef() != ProtocolObject.getDef())
    return;

  DEBUG(llvm::dbgs() << " *** Protocol to devirtualize : "
                     << *ProtocolObject.getDef());

  // Find a single initialization point, and make sure the protocol is not
  // captured. We also handle the case where the initializer is the copy_addr
  // instruction by looking at the source object.
  SILInstruction *InitInst = findSingleInitNoCaptureProtocol(ProtocolObject);
  if (CopyAddrInst *CAI = dyn_cast_or_null<CopyAddrInst>(InitInst)) {
    if (!CAI->isInitializationOfDest() || !CAI->isTakeOfSrc())
      return;

    InitInst = findSingleInitNoCaptureProtocol(CAI->getSrc());
  }

  InitExistentialInst *Init = dyn_cast_or_null<InitExistentialInst>(InitInst);
  if (!Init)
    return;

  // Strip the @InOut qualifier.
  CanType ConcreteTy = Init->getConcreteType().getSwiftType();
  if (InOutType *IOT = dyn_cast<InOutType>(ConcreteTy)) {
    ConcreteTy = IOT->getObjectType()->getCanonicalType();
  }

  SILDeclRef Member = PMI->getMember();
  // For each protocol that our type conforms to:
  for (auto &Conf : Init->getConformances()) {
    // Scan all of the witness tables in search of a matching method.
    for (SILWitnessTable &Witness : AI->getModule().getWitnessTableList()) {
      ProtocolDecl *WitnessProtocol = Witness.getConformance()->getProtocol();
      // Is this the correct protocol?

      if (WitnessProtocol != Conf->getProtocol() ||
          !ConcreteTy.getPointer()->isEqual(
               Witness.getConformance()->getType()))
        continue;

      DEBUG(llvm::dbgs() << " *** Found witness table for : " << *Init);

      // Okay, we found the right witness table. Now look for the method.
      for (auto &Entry : Witness.getEntries()) {
        // Look at method entries only.
        if (Entry.getKind() != SILWitnessTable::WitnessKind::Method)
          continue;

        SILWitnessTable::MethodWitness MethodEntry = Entry.getMethodWitness();
        // Check if this is the member we were looking for.
        if (MethodEntry.Requirement != Member)
          continue;

        // We found the correct witness function. Devirtualize this Apply.
        DEBUG(llvm::dbgs() << " *** Devirtualized : " << *AI);
        SILFunction *StaticRef = MethodEntry.Witness;
        replaceDynApplyWithStaticApply(AI, StaticRef, Init, PEI);
        NumDynApply++;
        Changed = true;
        return;
      }
    }
  }
  DEBUG(llvm::dbgs() << " *** Could not find a witness table for: " << *PMI);
}

bool swift::performSILDevirtualization(SILModule *M) {
  return SILDevirtualizer(M).run();
}
