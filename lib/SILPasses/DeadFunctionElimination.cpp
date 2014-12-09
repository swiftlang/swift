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

//===----------------------------------------------------------------------===//
//                             DeadFunctionElimination
//===----------------------------------------------------------------------===//

class DeadFunctionElimination {

  /// Stores which functions implement a vtable or witness table method.
  struct MethodInfo {
    
    MethodInfo() : isAlive(false) { }
    
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
    if (F->getLoweredFunctionType()->getAbstractCC() == AbstractCC::ObjCMethod)
      return true;
    
    return false;
  }
  
  /// Gets or creates the MethodInfo for a vtable or witness table method.
  /// \p decl The method declaration. In case of a vtable method this is always
  ///         the most overriden method.
  MethodInfo *getMethodInfo(AbstractFunctionDecl *decl) {
    MethodInfo * &entry = MethodInfos[decl];
    if (entry == nullptr) {
      entry = new (MethodInfoAllocator.Allocate()) MethodInfo();
    }
    return entry;
  }
  
  /// Returns true if a function is marked as alive.
  bool isAlive(SILFunction *F) {
    return AliveFunctions.count(F) != 0;
  }
  
  /// Marks a function as alive.
  void ensureAlive(SILFunction *F) {
    if (!isAlive(F)) {
      AliveFunctions.insert(F);
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
          auto *funcDecl = dyn_cast<AbstractFunctionDecl>(MI->getMember().
                                                          getDecl());
          MethodInfo *mi = getMethodInfo(getBase(funcDecl));
          ensureAlive(mi);
        } else if (auto *FRI = dyn_cast<FunctionRefInst>(&I)) {
          ensureAlive(FRI->getReferencedFunction());
        }
      }
    }
  }
  
  /// Retrieve the visiblity information from the AST accessibility.
  bool isVisibleExternally(Accessibility accessibility) {
    SILLinkage linkage;
    switch (accessibility) {
      case Accessibility::Private: linkage = SILLinkage::Private; break;
      case Accessibility::Internal: linkage = SILLinkage::Hidden; break;
      case Accessibility::Public: linkage = SILLinkage::Public; break;
    }
    return isPossiblyUsedExternally(linkage, Module->isWholeModule());
  }
  
  /// Find all functions which are alive from the beginning.
  /// For example, functions which may be referenced externally.
  void findAnchors() {
    
    // Check vtable methods.
    for (SILVTable &vTable : Module->getVTableList()) {
      for (auto &entry : vTable.getEntries()) {
        SILFunction *F = entry.second;
        auto *fd = dyn_cast<AbstractFunctionDecl>(entry.first.getDecl());
        fd = getBase(fd);
        MethodInfo *mi = getMethodInfo(fd);
        if (mi->isAlive) {
          ensureAlive(F);
        }
        mi->implementingFunctions.push_back(F);
        
        if (// Destructors are alive because they are called from swift_release
            entry.first.isDestructor()
            // A conservative approach: if any of the overridden functions is
            // visible externally, we mark the whole method as alive.
            || isPossiblyUsedExternally(F->getLinkage(), Module->isWholeModule())
            // We also have to check the method declaration's accessibility.
            // Needed if it's a public base method declared in another
            // compilation unit (for this we have no SILFunction).
            || isVisibleExternally(fd->getAccessibility())
            // Declarations are always accessible externally, so they are alive.
            || !F->isDefinition()) {
          ensureAlive(mi);
        }
      }
    }
    
    // Check witness methods.
    for (SILWitnessTable &WT : Module->getWitnessTableList()) {
      for (const SILWitnessTable::Entry &entry : WT.getEntries()) {
        if (entry.getKind() == SILWitnessTable::Method) {
          auto methodWitness = entry.getMethodWitness();
          auto *fd = dyn_cast<AbstractFunctionDecl>(methodWitness.Requirement.
                                                    getDecl());
          assert(fd == getBase(fd) && "key in witness table is overridden");
          MethodInfo *mi = getMethodInfo(fd);
          mi->implementingFunctions.push_back(methodWitness.Witness);
        }
      }
    }
    
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
  
  DeadFunctionElimination(SILModule *module) : Module(module) {}
  
  /// The main entry point of the optimization.
  bool eliminateDeadFunctions() {
    
    DEBUG(llvm::dbgs() << "running dead function elimination\n");
    
    // Find everything which may not be eliminated, e.g. because it is accessed
    // externally.
    findAnchors();
    
    // The core of the algorithm: Mark functions as alive which can be reached
    // from the anchors.
    while(!Worklist.empty()) {
      SILFunction *F = Worklist.back();
      Worklist.pop_back();
      scanFunction(F);
    }
    
    removeDeadEntriesFromTables();
    
    // First drop all references so that we don't get problems with non-null
    // reference counts of dead functions.
    for (SILFunction &F : *Module) {
      if (!isAlive(&F)) {
        F.dropAllReferences();
      }
    }
    // Last step: delete all dead functions.
    bool CallGraphChanged = false;
    for (auto FI = Module->begin(), EI = Module->end(); FI != EI;) {
      SILFunction *F = FI++;
      if (!isAlive(F)) {
        DEBUG(llvm::dbgs() << "  erase dead function " << F->getName() << "\n");
        NumDeadFunc++;
        Module->eraseFunction(F);
        CallGraphChanged = true;
      }
    }
    return CallGraphChanged;
  }
};

//===----------------------------------------------------------------------===//
//                      Pass Definition and Entry Points
//===----------------------------------------------------------------------===//

namespace {

class SILDeadFuncElimination : public SILModuleTransform {

  void run() override {
    // Avoid that Deserializers keep references to functions in their caches.
    getModule()->invalidateSILLoaderCaches();

    DeadFunctionElimination deadFunctionElimination(getModule());
    bool changed = deadFunctionElimination.eliminateDeadFunctions();
    
    if (changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
  }
  
  StringRef getName() override { return "Dead Function Elimination"; }
};

} // end anonymous namespace

SILTransform *swift::createDeadFunctionElimination() {
  return new SILDeadFuncElimination();
}
