//===--- DeadFunctionElimination.cpp - Eliminate dead functions -----------===//
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

#define DEBUG_TYPE "sil-dead-function-elimination"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumDeadFunc, "Number of dead functions eliminated");
STATISTIC(NumEliminatedExternalDefs, "Number of external function definitions eliminated");

/// This is a base class for passes that are based on function liveness
/// computations like e.g. dead function elimination.
/// It provides a common logic for computing live (i.e. reachable) functions.
class FunctionLivenessComputation {
protected:
  /// Stores which functions implement a vtable or witness table method.
  struct MethodInfo {

    MethodInfo() : isAlive(false) {}

    SmallVector<SILFunction *, 8> implementingFunctions;

    /// True, if the whole method is alive, which implies that all implementing
    /// functions are alive.
    bool isAlive;
  };

  SILModule *Module;

  llvm::DenseMap<AbstractFunctionDecl *, MethodInfo *> MethodInfos;
  llvm::SpecificBumpPtrAllocator<MethodInfo> MethodInfoAllocator;

  llvm::SmallSetVector<SILFunction *, 16> Worklist;

  llvm::SmallPtrSet<SILFunction *, 100> AliveFunctions;

  /// Checks is a function is alive, e.g. because it is visible externally.
  bool isAnchorFunction(SILFunction *F) {

    // Remove internal functions that are not referenced by anything.
    if (isPossiblyUsedExternally(F->getLinkage(), Module->isWholeModule()))
      return true;

    // TODO: main is currently marked as internal so we explicitly check
    // for functions with this name and keep them around.
    if (F->getName() == SWIFT_ENTRY_POINT_FUNCTION)
      return true;

    // ObjC functions are called through the runtime and are therefore alive
    // even if not referenced inside SIL.
    if (F->getRepresentation() == SILFunctionTypeRepresentation::ObjCMethod)
      return true;

    return false;
  }

  /// Gets or creates the MethodInfo for a vtable or witness table method.
  /// \p decl The method declaration. In case of a vtable method this is always
  ///         the most overriden method.
  MethodInfo *getMethodInfo(AbstractFunctionDecl *decl) {
    MethodInfo *&entry = MethodInfos[decl];
    if (entry == nullptr) {
      entry = new (MethodInfoAllocator.Allocate()) MethodInfo();
    }
    return entry;
  }

  /// Adds a function which implements a vtable or witness method.
  void addImplementingFunction(MethodInfo *mi, SILFunction *F) {
    if (mi->isAlive)
      ensureAlive(F);
    mi->implementingFunctions.push_back(F);
  }

  /// Returns true if a function is marked as alive.
  bool isAlive(SILFunction *F) { return AliveFunctions.count(F) != 0; }

  /// Marks a function as alive.
  void ensureAlive(SILFunction *F) {
    if (!isAlive(F)) {
      AliveFunctions.insert(F);
      assert(F && "function does not exist");
      Worklist.insert(F);
    }
  }

  /// Marks all implementing functions of a method as alive.
  void ensureAlive(MethodInfo *mi) {
    if (!mi->isAlive) {
      mi->isAlive = true;
      for (SILFunction *F : mi->implementingFunctions) {
        ensureAlive(F);
      }
    }
  }

  /// Gets the base implementation of a method.
  /// We always use the most overridden function to describe a method.
  AbstractFunctionDecl *getBase(AbstractFunctionDecl *FD) {
    while (FD->getOverriddenDecl()) {
      FD = FD->getOverriddenDecl();
    }
    return FD;
  }

  /// Scans all references inside a function.
  void scanFunction(SILFunction *F) {
    for (SILBasicBlock &BB : *F) {
      for (SILInstruction &I : BB) {
        if (auto *MI = dyn_cast<MethodInst>(&I)) {
          auto *funcDecl =
              dyn_cast<AbstractFunctionDecl>(MI->getMember().getDecl());
          MethodInfo *mi = getMethodInfo(getBase(funcDecl));
          ensureAlive(mi);
        } else if (auto *FRI = dyn_cast<FunctionRefInst>(&I)) {
          ensureAlive(FRI->getReferencedFunction());
        }
      }
    }
  }

  /// Retrieve the visiblity information from the AST.
  bool isVisibleExternally(ValueDecl *decl) {
    Accessibility accessibility = decl->getEffectiveAccess();
    SILLinkage linkage;
    switch (accessibility) {
    case Accessibility::Private:
      linkage = SILLinkage::Private;
      break;
    case Accessibility::Internal:
      linkage = SILLinkage::Hidden;
      break;
    case Accessibility::Public:
      linkage = SILLinkage::Public;
      break;
    }
    if (isPossiblyUsedExternally(linkage, Module->isWholeModule()))
      return true;

    // If a vtable or witness table (method) is only visible in another module
    // it can be accessed inside that module and we don't see this access.
    // We hit this case e.g. if a table is imported from the stdlib.
    if (decl->getDeclContext()->getParentModule() !=
        Module->getAssociatedContext()->getParentModule())
      return true;

    return false;
  }

  /// Find anchors in vtables and witness tables, if required.
  virtual void findAnchorsInTables() = 0;

  /// Find all functions which are alive from the beginning.
  /// For example, functions which may be referenced externally.
  void findAnchors() {

    findAnchorsInTables();

    for (SILFunction &F : *Module) {
      if (isAnchorFunction(&F)) {
        DEBUG(llvm::dbgs() << "  anchor function: " << F.getName() << "\n");
        ensureAlive(&F);
      }
    }

    for (SILGlobalVariable &G : Module->getSILGlobalList()) {
      if (SILFunction *initFunc = G.getInitializer()) {
        ensureAlive(initFunc);
      }
    }
  }

public:
  FunctionLivenessComputation(SILModule *module) : Module(module) {}

  /// The main entry point of the optimization.
  bool findAliveFunctions() {

    DEBUG(llvm::dbgs() << "running function elimination\n");

    // Find everything which may not be eliminated, e.g. because it is accessed
    // externally.
    findAnchors();

    // The core of the algorithm: Mark functions as alive which can be reached
    // from the anchors.
    while (!Worklist.empty()) {
      SILFunction *F = Worklist.back();
      Worklist.pop_back();
      scanFunction(F);
    }

    return false;
  }

  virtual ~FunctionLivenessComputation() {}
};

//===----------------------------------------------------------------------===//
//                             DeadFunctionElimination
//===----------------------------------------------------------------------===//

class DeadFunctionElimination : FunctionLivenessComputation {

  /// DeadFunctionElimination pass takes functions
  /// reachable via vtables and witness_tables into account
  /// when computing a function liveness information.
  void findAnchorsInTables() {
    // Check vtable methods.
    for (SILVTable &vTable : Module->getVTableList()) {
      for (auto &entry : vTable.getEntries()) {
        SILFunction *F = entry.second;
        auto *fd = dyn_cast<AbstractFunctionDecl>(entry.first.getDecl());
        fd = getBase(fd);
        MethodInfo *mi = getMethodInfo(fd);
        addImplementingFunction(mi, F);

        if (// Destructors are alive because they are called from swift_release
            entry.first.isDestructor()
            // A conservative approach: if any of the overridden functions is
            // visible externally, we mark the whole method as alive.
            || isPossiblyUsedExternally(F->getLinkage(), Module->isWholeModule())
            // We also have to check the method declaration's accessibility.
            // Needed if it's a public base method declared in another
            // compilation unit (for this we have no SILFunction).
            || isVisibleExternally(fd)
            // Declarations are always accessible externally, so they are alive.
            || !F->isDefinition()) {
          ensureAlive(mi);
        }
      }
    }

    // Check witness methods.
    for (SILWitnessTable &WT : Module->getWitnessTableList()) {
      bool tableIsAlive = isVisibleExternally(WT.getConformance()->getProtocol());
      for (const SILWitnessTable::Entry &entry : WT.getEntries()) {
        if (entry.getKind() == SILWitnessTable::Method) {
          auto methodWitness = entry.getMethodWitness();
          auto *fd = dyn_cast<AbstractFunctionDecl>(methodWitness.Requirement.
                                                    getDecl());
          assert(fd == getBase(fd) && "key in witness table is overridden");
          SILFunction *F = methodWitness.Witness;
          if (F) {
            MethodInfo *mi = getMethodInfo(fd);
            addImplementingFunction(mi, F);
            if (tableIsAlive || !F->isDefinition())
              ensureAlive(mi);
          }
        }
      }
    }
  }

  /// Removes all dead methods from vtables and witness tables.
  void removeDeadEntriesFromTables() {
    for (SILVTable &vTable : Module->getVTableList()) {
      vTable.removeEntries_if([this](SILVTable::Pair &entry) -> bool {
        if (!isAlive(entry.second)) {
          DEBUG(llvm::dbgs() << "  erase dead vtable method " <<
                entry.second->getName() << "\n");
          return true;
        }
        return false;
      });
    }

    auto &WitnessTables = Module->getWitnessTableList();
    for (auto WI = WitnessTables.begin(), EI = WitnessTables.end(); WI != EI;) {
      SILWitnessTable *WT = WI++;
      WT->clearMethods_if([this](const SILWitnessTable::MethodWitness &MW) -> bool {
        if (!isAlive(MW.Witness)) {
          DEBUG(llvm::dbgs() << "  erase dead witness method " <<
                MW.Witness->getName() << "\n");
          return true;
        }
        return false;
      });
    }
  }

public:
  DeadFunctionElimination(SILModule *module)
      : FunctionLivenessComputation(module) {}

  /// The main entry point of the optimization.
  void eliminateFunctions(SILModuleTransform *DFEPass) {

    DEBUG(llvm::dbgs() << "running dead function elimination\n");
    findAliveFunctions();

    bool CallGraphChanged = false;

    removeDeadEntriesFromTables();

    // First drop all references so that we don't get problems with non-null
    // reference counts of dead functions.
    for (SILFunction &F : *Module) {
      if (!isAlive(&F)) {
        F.dropAllReferences();
      }
    }

    // Next step: delete all dead functions.
    for (auto FI = Module->begin(), EI = Module->end(); FI != EI;) {
      SILFunction *F = FI++;
      if (!isAlive(F)) {
        DEBUG(llvm::dbgs() << "  erase dead function " << F->getName() << "\n");
        NumDeadFunc++;
        Module->eraseFunction(F);
        CallGraphChanged = true;
        DFEPass->invalidateAnalysis(F, SILAnalysis::PreserveKind::Nothing);
      }
    }
  }
};

//===----------------------------------------------------------------------===//
//                        ExternalFunctionDefinitionsElimination
//===----------------------------------------------------------------------===//

/// This pass performs removal of external function definitions for a sake of
/// reducing the amount of code to run through IRGen. It is supposed to run very
/// late in the pipeline, after devirtualization, inlining and specialization
/// passes.
///
/// NOTE:
/// Overall, this pass does not try to remove any information which can be
/// useful for LLVM code generation, e.g. for analysis of function's
/// side-effects. Therefore it does not remove bodies of any external functions
/// that are alive, because LLVM may analyze their bodies to determine their
/// side-effects and use it to achieve a better optimization.
///
/// Current implementation does not consider functions which are reachable only
/// via vtables or witness_tables as alive and removes their bodies, because
/// even if they would be kept around, LLVM does not know how to look at
/// function definitions through Swift's vtables and witness_tables.
///
/// TODO:
/// Once there is a proper support for IPO in Swift compiler and/or there is
/// a way to communicate function's side-effects without providing its body
/// (e.g. by means of SILFunction flags, attributes, etc), it should be
/// safe to remove bodies of all external definitions.

class ExternalFunctionDefinitionsElimination : FunctionLivenessComputation {

  /// ExternalFunctionDefinitionsElimination pass does not take functions
  /// reachable via vtables and witness_tables into account when computing
  /// a function liveness information.
  void findAnchorsInTables() {
  }

  bool findAliveFunctions() {
    /// TODO: Once there is a proper support for IPO,
    /// bodies of all external functions can be removed.
    /// Therefore there is no need for a livesness computation.
    /// The next line can be just replaced by:
    /// return false;
    return FunctionLivenessComputation::findAliveFunctions();
  }

  /// Try to convert definition into declaration.
  /// Returns true if function was erased from the module.
  bool tryToConvertExtenralDefinitionIntoDeclaration(SILFunction *F) {
    bool FunctionWasErased = false;
    // Bail if it is a declaration already
    if (!F->isDefinition())
      return false;
    // Bail if there is no external implementation of this function.
    if (!F->isAvailableExternally())
      return false;
    // Bail if has a shared visibility, as there are no guarantees
    // that an implementation is available elsewhere.
    if (hasSharedVisibility(F->getLinkage()))
      return false;
    // Make this definition a declaration by removing the body of a function.

    DEBUG(llvm::dbgs() << "  removed external function " << F->getName()
          << "\n");
    F->dropAllReferences();
    auto &Blocks = F->getBlocks();
    Blocks.clear();
    assert(F->isExternalDeclaration() &&
           "Function should be an external declaration");
    if (F->getRefCount() == 0) {
      Module->eraseFunction(F);
      FunctionWasErased = true;
    }
    NumEliminatedExternalDefs++;
    return FunctionWasErased;
  }

public:
  ExternalFunctionDefinitionsElimination(SILModule *module)
      : FunctionLivenessComputation(module) {}

  /// Eliminate bodies of external functions which are not alive.
  ///
  /// Bodies of alive functions should not be removed, as LLVM may
  /// still need them for analyzing their side-effects.
  void eliminateFunctions(SILModuleTransform *DFEPass) {

    findAliveFunctions();
    // Get rid of definitions for all global functions that are not marked as
    // alive.
    for (auto FI = Module->begin(), EI = Module->end(); FI != EI;) {
      SILFunction *F = FI++;
      // Do not remove bodies of any functions that are alive.
      if (!isAlive(F)) {
        if (tryToConvertExtenralDefinitionIntoDeclaration(F)) {
          DFEPass->invalidateAnalysis(F, SILAnalysis::PreserveKind::Nothing);
        }
      }
    }
  }
};

//===----------------------------------------------------------------------===//
//                      Pass Definition and Entry Points
//===----------------------------------------------------------------------===//

namespace {

class SILDeadFuncElimination : public SILModuleTransform {
  void run() override {

    DEBUG(llvm::dbgs() << "Running DeadFuncElimination\n");

    // Avoid that Deserializers keep references to functions in their caches.
    getModule()->invalidateSILLoaderCaches();

    DeadFunctionElimination deadFunctionElimination(getModule());
    deadFunctionElimination.eliminateFunctions(this);
  }
  
  StringRef getName() override { return "Dead Function Elimination"; }
};

class SILExternalFuncDefinitionsElimination : public SILModuleTransform {
  void run() override {

    DEBUG(llvm::dbgs() << "Running ExternalFunctionDefinitionsElimination\n");

    // Avoid that Deserializers keep references to functions in their caches.
    getModule()->invalidateSILLoaderCaches();

    ExternalFunctionDefinitionsElimination EFDFE(getModule());
    EFDFE.eliminateFunctions(this);
 }

  StringRef getName() override {
    return "External Function Definitions Elimination";
  }
};

} // end anonymous namespace

SILTransform *swift::createDeadFunctionElimination() {
  return new SILDeadFuncElimination();
}

SILTransform *swift::createExternalFunctionDefinitionsElimination() {
  return new SILExternalFuncDefinitionsElimination();
}
