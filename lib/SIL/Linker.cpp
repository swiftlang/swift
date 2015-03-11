//===--- Linker.cpp -------------------------------------------------------===//
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

#define DEBUG_TYPE "sil-linker"
#include "Linker.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace Lowering;

STATISTIC(NumFuncLinked, "Number of SIL functions linked");

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

/// \return True if the function \p F should be imported into the current
/// module.
static bool shouldImportFunction(SILFunction *F) {
  // Skip functions that are marked with the 'no import' tag. These
  // are functions that we don't want to copy from the module.
  if (F->hasSemanticsString("stdlib_binary_only"))
    return false;

  return true;
}

//===----------------------------------------------------------------------===//
//                               Linker Helpers
//===----------------------------------------------------------------------===//

/// Process F, recursively deserializing any thing F may reference.
bool SILLinkerVisitor::processFunction(SILFunction *F) {
  if (Mode == LinkingMode::LinkNone)
    return false;

  if (!shouldImportFunction(F))
    return false;

  // If F is a declaration, first deserialize it.
  auto NewFn = F->isExternalDeclaration() ? Loader->lookupSILFunction(F) : F;
  if (!NewFn || NewFn->empty())
    return false;

  ++NumFuncLinked;

  // Try to transitively deserialize everything referenced by NewFn.
  Worklist.push_back(NewFn);
  process();

  // Since we successfully processed at least one function, return true.
  return true;
}

/// Deserialize the VTable mapped to C if it exists and all SIL the VTable
/// transitively references.
///
/// This method assumes that the caller made sure that no vtable existed in
/// Mod.
SILVTable *SILLinkerVisitor::processClassDecl(const ClassDecl *C) {
  // If we are not linking anything, bail.
  if (Mode == LinkingMode::LinkNone)
    return nullptr;

  // Attempt to load the VTable from the SerializedSILLoader. If we
  // fail... bail...
  SILVTable *Vtbl = Loader->lookupVTable(C);
  if (!Vtbl)
    return nullptr;

  // Otherwise, add all the vtable functions in Vtbl to the function
  // processing list...
  for (auto &E : Vtbl->getEntries())
    Worklist.push_back(E.second);

  // And then transitively deserialize all SIL referenced by those functions.
  process();

  // Return the deserialized Vtbl.
  return Vtbl;
}

bool SILLinkerVisitor::linkInVTable(ClassDecl *D) {
  // Attempt to lookup the Vtbl from the SILModule.
  SILVTable *Vtbl = Mod.lookUpVTable(D);

  // If the SILModule does not have the VTable, attempt to deserialize the
  // VTable. If we fail to do that as well, bail.
  if (!Vtbl || !(Vtbl = Loader->lookupVTable(D->getName())))
    return false;

  // Ok we found our VTable. Visit each function referenced by the VTable. If
  // any of the functions are external declarations, add them to the worklist
  // for processing.
  bool Result = false;
  for (auto P : Vtbl->getEntries()) {
    if (P.second->isExternalDeclaration()) {
      Result = true;
      addFunctionToWorklist(P.second);
    }
  }
  return Result;
}

//===----------------------------------------------------------------------===//
//                                  Visitors
//===----------------------------------------------------------------------===//

bool SILLinkerVisitor::visitApplyInst(ApplyInst *AI) {
  // If we don't have a function ref inst, just return false. We do not have
  // interesting callees.
  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (!FRI)
    return false;

  // Ok we have a function ref inst, grab the callee.
  SILFunction *Callee = FRI->getReferencedFunction();

  // If the linking mode is not link all, AI is not transparent, and the
  // callee is not shared, we don't want to perform any linking.
  if (!isLinkAll() && !AI->isTransparent() &&
      !hasSharedVisibility(Callee->getLinkage()))
    return false;

  // Otherwise we want to try and link in the callee... Add it to the callee
  // list and return true.
  addFunctionToWorklist(Callee);
  return true;
}

bool SILLinkerVisitor::visitPartialApplyInst(PartialApplyInst *PAI) {
  auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());
  if (!FRI)
    return false;

  SILFunction *Callee = FRI->getReferencedFunction();
  if (!isLinkAll() && !Callee->isTransparent() &&
      !hasSharedVisibility(Callee->getLinkage()))
    return false;

  addFunctionToWorklist(Callee);
  return true;
}

bool SILLinkerVisitor::visitFunctionRefInst(FunctionRefInst *FRI) {
  // Needed to handle closures which are no longer applied, but are left
  // behind as dead code. This shouldn't happen, but if it does don't get into
  // an inconsistent state.
  SILFunction *Callee = FRI->getReferencedFunction();
  if (!isLinkAll() && !Callee->isTransparent() &&
      !hasSharedVisibility(Callee->getLinkage()))
    return false;

  addFunctionToWorklist(FRI->getReferencedFunction());
  return true;
}

bool SILLinkerVisitor::visitProtocolConformance(
    ProtocolConformance *C, const Optional<SILDeclRef> &Member) {
  // If a null protocol conformance was passed in, just return false.
  if (!C)
    return false;

  // Otherwise try and lookup a witness table for C.
  SILWitnessTable *WT = Mod.lookUpWitnessTable(C).first;

  // If we don't find any witness table for the conformance, bail and return
  // false.
  if (!WT) {
    Mod.createWitnessTableDeclaration(
        C, TypeConverter::getLinkageForProtocolConformance(
               C->getRootNormalConformance(), NotForDefinition));
    return false;
  }

  // If the looked up witness table is a declaration, there is nothing we can
  // do here. Just bail and return false.
  if (WT->isDeclaration())
    return false;

  bool performFuncDeserialization = false;
  // For each entry in the witness table...
  for (auto &E : WT->getEntries()) {
    // If the entry is a witness method...
    if (E.getKind() == SILWitnessTable::WitnessKind::Method) {
      // And we are only interested in deserializing a specific requirement
      // and don't have that requirement, don't deserialize this method.
      if (Member.hasValue() && E.getMethodWitness().Requirement != *Member)
        continue;

      // The witness could be removed by dead function elimination.
      if (!E.getMethodWitness().Witness)
        continue;

      // Otherwise if it is the requirement we are looking for or we just want
      // to deserialize everything, add the function to the list of functions
      // to deserialize.
      performFuncDeserialization = true;
      addFunctionToWorklist(E.getMethodWitness().Witness);
    }
  }

  return performFuncDeserialization;
}

bool SILLinkerVisitor::visitInitExistentialAddrInst(
    InitExistentialAddrInst *IEI) {
  // Link in all protocol conformances that this touches.
  //
  // TODO: There might be a two step solution where the init_existential_inst
  // causes the witness table to be brought in as a declaration and then the
  // protocol method inst causes the actual deserialization. For now we are
  // not going to be smart about this to enable avoiding any issues with
  // visiting the open_existential_addr/witness_method before the
  // init_existential_inst.
  bool performFuncDeserialization = false;
  for (ProtocolConformance *C : IEI->getConformances()) {
    performFuncDeserialization |=
        visitProtocolConformance(C, Optional<SILDeclRef>());
  }
  return performFuncDeserialization;
}

bool SILLinkerVisitor::visitInitExistentialRefInst(
    InitExistentialRefInst *IERI) {
  // Link in all protocol conformances that this touches.
  //
  // TODO: There might be a two step solution where the init_existential_inst
  // causes the witness table to be brought in as a declaration and then the
  // protocol method inst causes the actual deserialization. For now we are
  // not going to be smart about this to enable avoiding any issues with
  // visiting the protocol_method before the init_existential_inst.
  bool performFuncDeserialization = false;
  for (ProtocolConformance *C : IERI->getConformances()) {
    performFuncDeserialization |=
        visitProtocolConformance(C, Optional<SILDeclRef>());
  }
  return performFuncDeserialization;
}

bool SILLinkerVisitor::visitAllocRefInst(AllocRefInst *ARI) {
  // Grab the class decl from the alloc ref inst.
  ClassDecl *D = ARI->getType().getClassOrBoundGenericClass();
  if (!D)
    return false;

  return linkInVTable(D);
}

bool SILLinkerVisitor::visitMetatypeInst(MetatypeInst *MI) {
  CanType instTy = MI->getType().castTo<MetatypeType>().getInstanceType();
  ClassDecl *C = instTy.getClassOrBoundGenericClass();
  if (!C)
    return false;

  return linkInVTable(C);
}

//===----------------------------------------------------------------------===//
//                             Top Level Routine
//===----------------------------------------------------------------------===//

// Main loop of the visitor. Called by one of the other *visit* methods.
bool SILLinkerVisitor::process() {
  // Process everything transitively referenced by one of the functions in the
  // worklist.
  bool Result = false;
  while (!Worklist.empty()) {
    auto Fn = Worklist.pop_back_val();

    if (!shouldImportFunction(Fn))
      continue;

    for (auto &BB : *Fn) {
      for (auto &I : BB) {
        // Should we try linking?
        if (visit(&I)) {
          for (auto F : FunctionDeserializationWorklist) {

            if (!shouldImportFunction(F))
              continue;

            // The ExternalSource may wish to rewrite non-empty bodies.
            if (!F->empty() && ExternalSource)
              if (auto NewFn = ExternalSource->lookupSILFunction(F)) {
                NewFn->verify();
                Worklist.push_back(NewFn);
                ++NumFuncLinked;
                Result = true;
                continue;
              }

            F->setBare(IsBare);

            if (F->empty())
              if (auto NewFn = Loader->lookupSILFunction(F)) {
                NewFn->verify();
                Worklist.push_back(NewFn);
                Result = true;
                ++NumFuncLinked;
              }
          }
          FunctionDeserializationWorklist.clear();
        } else {
          assert(FunctionDeserializationWorklist.empty() &&
                 "Worklist should "
                 "always be empty if visit does not return true.");
        }
      }
    }
  }

  // If we return true, we deserialized at least one function.
  return Result;
}
