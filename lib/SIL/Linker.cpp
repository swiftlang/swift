//===--- Linker.cpp -------------------------------------------------------===//
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
///
/// \file
///
/// The SIL linker walks the call graph beginning at a starting function,
/// deserializing functions, vtables and witness tables.
///
/// The behavior of the linker is controlled by a LinkMode value. The LinkMode
/// has three possible values:
///
/// - LinkNone: The linker does not deserialize anything. This is only used for
///   debugging and testing purposes, and never during normal operation.
///
/// - LinkNormal: The linker deserializes bodies for declarations that must be
///   emitted into the client because they do not have definitions available
///   externally. This includes:
///
///   - witness tables for imported conformances
///
///   - functions with shared linkage
///
/// - LinkAll: All reachable functions (including public functions) are
///   deserialized, including public functions.
///
/// The primary entry point into the linker is the SILModule::linkFunction()
/// function, which recursively walks the call graph starting from the given
/// function.
///
/// In the mandatory pipeline (-Onone), the linker is invoked from the mandatory
/// SIL linker pass, which pulls in just enough to allow us to emit code, using
/// LinkNormal mode.
///
/// In the performance pipeline, after guaranteed optimizations but before
/// performance optimizations, the 'performance SILLinker' pass links
/// transitively all reachable functions, to uncover optimization opportunities
/// that might be missed from deserializing late. The performance pipeline uses
/// LinkAll mode.
///
/// *NOTE*: In LinkAll mode, we deserialize all vtables and witness tables,
/// even those with public linkage. This is not strictly necessary, since the
/// devirtualizer deserializes vtables and witness tables as needed. However,
/// doing so early creates more opportunities for optimization.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-linker"
#include "Linker.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Debug.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/FormalLinkage.h"
#include <functional>

using namespace swift;
using namespace Lowering;

STATISTIC(NumFuncLinked, "Number of SIL functions linked");

//===----------------------------------------------------------------------===//
//                               Linker Helpers
//===----------------------------------------------------------------------===//

void SILLinkerVisitor::addFunctionToWorklist(SILFunction *F) {
  assert(F->isExternalDeclaration());

  LLVM_DEBUG(llvm::dbgs() << "Imported function: "
                          << F->getName() << "\n");
  if (Mod.loadFunction(F)) {
    if (F->isExternalDeclaration())
      return;

    F->setBare(IsBare);
    F->verify();
    Worklist.push_back(F);
    Changed = true;
    ++NumFuncLinked;
  }
}

/// Deserialize a function and add it to the worklist for processing.
void SILLinkerVisitor::maybeAddFunctionToWorklist(SILFunction *F) {
  // Don't need to do anything if the function already has a body.
  if (!F->isExternalDeclaration())
    return;

  // In the performance pipeline, we deserialize all reachable functions.
  if (isLinkAll())
    return addFunctionToWorklist(F);

  // Otherwise, make sure to deserialize shared functions; we need to
  // emit them into the client binary since they're not available
  // externally.
  if (hasSharedVisibility(F->getLinkage()))
    return addFunctionToWorklist(F);

  // Functions with PublicNonABI linkage are deserialized as having
  // HiddenExternal linkage when they are declarations, then they
  // become SharedExternal after the body has been deserialized.
  // So try deserializing HiddenExternal functions too.
  if (F->getLinkage() == SILLinkage::HiddenExternal)
    return addFunctionToWorklist(F);
}

/// Process F, recursively deserializing any thing F may reference.
bool SILLinkerVisitor::processFunction(SILFunction *F) {
  // If F is a declaration, first deserialize it.
  if (F->isExternalDeclaration()) {
    maybeAddFunctionToWorklist(F);
  } else {
    Worklist.push_back(F);
  }

  process();
  return Changed;
}

/// Deserialize the given VTable all SIL the VTable transitively references.
void SILLinkerVisitor::linkInVTable(ClassDecl *D) {
  // Devirtualization already deserializes vtables as needed in both the
  // mandatory and performance pipelines, and we don't support specialized
  // vtables that might have shared linkage yet, so this is only needed in
  // the performance pipeline to deserialize more functions early, and expose
  // optimization opportunities.
  assert(isLinkAll());

  // Attempt to lookup the Vtbl from the SILModule.
  SILVTable *Vtbl = Mod.lookUpVTable(D);
  if (!Vtbl)
    return;

  // Ok we found our VTable. Visit each function referenced by the VTable. If
  // any of the functions are external declarations, add them to the worklist
  // for processing.
  for (auto P : Vtbl->getEntries()) {
    // Deserialize and recursively walk any vtable entries that do not have
    // bodies yet.
    maybeAddFunctionToWorklist(P.Implementation);
  }
}

//===----------------------------------------------------------------------===//
//                                  Visitors
//===----------------------------------------------------------------------===//

static template<typename Inst>
bool applyInstCalleeIsGeneric(Inst AI) {
  return AI->getCallee()->getType().template castTo<SILFunctionType>()
           ->getGenericSignature();
}

void SILLinkerVisitor::visitApplyInst(ApplyInst *AI) {
  if (applyInstCalleeIsGeneric(AI)) {
    visitApplySubstitutions(AI->getSubstitutionMap());
  }
}

void SILLinkerVisitor::visitTryApplyInst(TryApplyInst *TAI) {
  if (applyInstCalleeIsGeneric(TAI)) {
    visitApplySubstitutions(TAI->getSubstitutionMap());
  }
}

void SILLinkerVisitor::visitPartialApplyInst(PartialApplyInst *PAI) {
  if (applyInstCalleeIsGeneric(PAI)) {
    visitApplySubstitutions(PAI->getSubstitutionMap());
  }
}

void SILLinkerVisitor::visitFunctionRefInst(FunctionRefInst *FRI) {
  maybeAddFunctionToWorklist(FRI->getReferencedFunction());
}

// Eagerly visiting all used conformances leads to a large blowup
// in the amount of SIL we read in. For optimization purposes we can defer
// reading in most conformances until we need them for devirtualization.
// However, we *must* pull in shared clang-importer-derived conformances
// we potentially use, since we may not otherwise have a local definition.
static bool mustDeserializeProtocolConformance(SILModule &M,
                                               ProtocolConformanceRef c) {
  if (!c.isConcrete())
    return false;
  auto conformance = c.getConcrete()->getRootNormalConformance();
  return M.Types.protocolRequiresWitnessTable(conformance->getProtocol())
    && isa<ClangModuleUnit>(conformance->getDeclContext()
                                       ->getModuleScopeContext());
}

void SILLinkerVisitor::visitProtocolConformance(
    ProtocolConformanceRef ref, const Optional<SILDeclRef> &Member) {
  // If an abstract protocol conformance was passed in, just return false.
  if (ref.isAbstract())
    return;
  
  bool mustDeserialize = mustDeserializeProtocolConformance(Mod, ref);

  // Otherwise try and lookup a witness table for C.
  auto C = ref.getConcrete();
  
  if (!VisitedConformances.insert(C).second)
    return;
  
  SILWitnessTable *WT = Mod.lookUpWitnessTable(C, true);

  // If we don't find any witness table for the conformance, bail and return
  // false.
  if (!WT) {
    Mod.createWitnessTableDeclaration(
        C, getLinkageForProtocolConformance(
               C->getRootNormalConformance(), NotForDefinition));

    // Adding the declaration may allow us to now deserialize the body.
    // Force the body if we must deserialize this witness table.
    if (mustDeserialize) {
      WT = Mod.lookUpWitnessTable(C, true);
      assert(WT && WT->isDefinition()
             && "unable to deserialize witness table when we must?!");
    } else {
      return;
    }
  }

  // If the looked up witness table is a declaration, there is nothing we can
  // do here. Just bail and return false.
  if (WT->isDeclaration())
    return;

  auto maybeVisitRelatedConformance = [&](ProtocolConformanceRef c) {
    // Formally all conformances referenced by a used conformance are used.
    // However, eagerly visiting them all at this point leads to a large blowup
    // in the amount of SIL we read in. For optimization purposes we can defer
    // reading in most conformances until we need them for devirtualization.
    // However, we *must* pull in shared clang-importer-derived conformances
    // we potentially use, since we may not otherwise have a local definition.
    if (mustDeserializeProtocolConformance(Mod, c))
      visitProtocolConformance(c, None);
  };
  
  // For each entry in the witness table...
  for (auto &E : WT->getEntries()) {
    switch (E.getKind()) {
    // If the entry is a witness method...
    case SILWitnessTable::WitnessKind::Method: {
      // And we are only interested in deserializing a specific requirement
      // and don't have that requirement, don't deserialize this method.
      if (Member.hasValue() && E.getMethodWitness().Requirement != *Member)
        continue;

      // The witness could be removed by dead function elimination.
      if (!E.getMethodWitness().Witness)
        continue;

      // Otherwise, deserialize the witness if it has shared linkage, or if
      // we were asked to deserialize everything.
      maybeAddFunctionToWorklist(E.getMethodWitness().Witness);
      break;
    }
    
    // If the entry is a related witness table, see whether we need to
    // eagerly deserialize it.
    case SILWitnessTable::WitnessKind::BaseProtocol: {
      auto baseConformance = E.getBaseProtocolWitness().Witness;
      maybeVisitRelatedConformance(ProtocolConformanceRef(baseConformance));
      break;
    }
    case SILWitnessTable::WitnessKind::AssociatedTypeProtocol: {
      auto assocConformance = E.getAssociatedTypeProtocolWitness().Witness;
      maybeVisitRelatedConformance(assocConformance);
      break;
    }
    
    case SILWitnessTable::WitnessKind::AssociatedType:
    case SILWitnessTable::WitnessKind::Invalid:
      break;
    }
  }
}

void SILLinkerVisitor::visitApplySubstitutions(SubstitutionMap subs) {
  for (auto conformance : subs.getConformances()) {
    // Formally all conformances referenced in a function application are
    // used. However, eagerly visiting them all at this point leads to a
    // large blowup in the amount of SIL we read in, and we aren't very
    // systematic about laziness. For optimization purposes we can defer
    // reading in most conformances until we need them for devirtualization.
    // However, we *must* pull in shared clang-importer-derived conformances
    // we potentially use, since we may not otherwise have a local definition.
    if (mustDeserializeProtocolConformance(Mod, conformance)) {
      visitProtocolConformance(conformance, None);
    }
  }
}

void SILLinkerVisitor::visitInitExistentialAddrInst(
    InitExistentialAddrInst *IEI) {
  // Link in all protocol conformances that this touches.
  //
  // TODO: There might be a two step solution where the init_existential_inst
  // causes the witness table to be brought in as a declaration and then the
  // protocol method inst causes the actual deserialization. For now we are
  // not going to be smart about this to enable avoiding any issues with
  // visiting the open_existential_addr/witness_method before the
  // init_existential_inst.
  for (ProtocolConformanceRef C : IEI->getConformances()) {
    visitProtocolConformance(C, Optional<SILDeclRef>());
  }
}

void SILLinkerVisitor::visitInitExistentialRefInst(
    InitExistentialRefInst *IERI) {
  // Link in all protocol conformances that this touches.
  //
  // TODO: There might be a two step solution where the init_existential_inst
  // causes the witness table to be brought in as a declaration and then the
  // protocol method inst causes the actual deserialization. For now we are
  // not going to be smart about this to enable avoiding any issues with
  // visiting the protocol_method before the init_existential_inst.
  for (ProtocolConformanceRef C : IERI->getConformances()) {
    visitProtocolConformance(C, Optional<SILDeclRef>());
  }
}

void SILLinkerVisitor::visitAllocRefInst(AllocRefInst *ARI) {
  if (!isLinkAll())
    return;

  // Grab the class decl from the alloc ref inst.
  ClassDecl *D = ARI->getType().getClassOrBoundGenericClass();
  if (!D)
    return;

  linkInVTable(D);
}

void SILLinkerVisitor::visitMetatypeInst(MetatypeInst *MI) {
  if (!isLinkAll())
    return;

  CanType instTy = MI->getType().castTo<MetatypeType>().getInstanceType();
  ClassDecl *C = instTy.getClassOrBoundGenericClass();
  if (!C)
    return;

  linkInVTable(C);
}

//===----------------------------------------------------------------------===//
//                             Top Level Routine
//===----------------------------------------------------------------------===//

// Main loop of the visitor. Called by one of the other *visit* methods.
void SILLinkerVisitor::process() {
  // Process everything transitively referenced by one of the functions in the
  // worklist.
  while (!Worklist.empty()) {
    auto *Fn = Worklist.pop_back_val();

    if (Fn->getModule().isSerialized()) {
      // If the containing module has been serialized,
      // Remove The Serialized state (if any)
      //  This allows for more optimizations
      Fn->setSerialized(IsSerialized_t::IsNotSerialized);
    }

    LLVM_DEBUG(llvm::dbgs() << "Process imports in function: "
                            << Fn->getName() << "\n");

    for (auto &BB : *Fn) {
      for (auto &I : BB) {
        visit(&I);
      }
    }
  }
}
