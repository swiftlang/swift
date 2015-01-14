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

  // If set, this pass is running very late in the pipeline.
  bool isLate;

  llvm::DenseMap<AbstractFunctionDecl *, MethodInfo *> MethodInfos;
  llvm::SpecificBumpPtrAllocator<MethodInfo> MethodInfoAllocator;
  
  llvm::SmallSetVector<SILFunction *, 16> Worklist;
  
  llvm::SmallPtrSet<SILFunction *, 100> AliveFunctions;
  llvm::SmallPtrSet<SILFunction *, 100> GlobalInitFunctions;
  
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
  
  /// Adds a function which implements a vtable or witness method.
  void addImplementingFunction(MethodInfo *mi, SILFunction *F) {
    if (mi->isAlive)
      ensureAlive(F);
    mi->implementingFunctions.push_back(F);
  }

  /// Returns true if a function is marked as alive.
  bool isAlive(SILFunction *F) {
    return AliveFunctions.count(F) != 0;
  }
  
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
  
  /// Retrieve the visiblity information from the AST.
  bool isVisibleExternally(ValueDecl *decl) {
    Accessibility accessibility = decl->getAccessibility();
    SILLinkage linkage;
    switch (accessibility) {
      case Accessibility::Private: linkage = SILLinkage::Private; break;
      case Accessibility::Internal: linkage = SILLinkage::Hidden; break;
      case Accessibility::Public: linkage = SILLinkage::Public; break;
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
    
    for (SILFunction &F : *Module) {
      if (isAnchorFunction(&F)) {
        DEBUG(llvm::dbgs() << "  anchor function: " << F.getName() << "\n");
        ensureAlive(&F);
      }
    }
    
    for (SILGlobalVariable &G : Module->getSILGlobalList()) {
      if (SILFunction *initFunc = G.getInitializer()) {
        ensureAlive(initFunc);
        GlobalInitFunctions.insert(initFunc);
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

  // Try to convert definition into declaration.
  // Returns true if function was erased from the module.
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
    F->dropAllReferences();
    auto &Blocks = F->getBlocks();
    Blocks.clear();
    assert(F->isExternalDeclaration() &&
           "Function should be an external declaration");
    if (F->getRefCount() == 0) {
      Module->eraseFunction(F);
      FunctionWasErased = true;
    }
    DEBUG(llvm::dbgs() << "  removed external function " << F->getName()
                       << "\n");
    return FunctionWasErased;
  }

  // Eliminate external function definitions by removing their bodies and
  // converting them into declarations. It is safe, because such functions are
  // defined elsewhere and where required only for analysis/optimization
  // purposes.
  //
  // This reduces the amount of SIL code to be processed by IRRegn. And it may
  // also reduce the code size of the final object file.
  //
  // Returns true if callgraph was changed.
  bool removeExternalDefinitions() {
    bool CallGraphChanged = false;

    for (SILVTable &vTable : Module->getVTableList()) {
      for (auto &pair : vTable.getEntries()) {
        auto *F = pair.second;
        tryToConvertExtenralDefinitionIntoDeclaration(F);
      }
    }

    auto &WitnessTables = Module->getWitnessTableList();
    for (auto WI = WitnessTables.begin(), EI = WitnessTables.end(); WI != EI;) {
      SILWitnessTable *WT = WI++;
      for (auto &entry : WT->getEntries()) {
        if (entry.getKind() != SILWitnessTable::WitnessKind::Method)
          continue;
        auto *F = entry.getMethodWitness().Witness;
        if (!F)
          continue;
        tryToConvertExtenralDefinitionIntoDeclaration(F);
      }
    }

    // Get rid of definitions for global functions that are externally available.
    for (auto FI = Module->begin(), EI = Module->end(); FI != EI;) {
      SILFunction *F = FI++;
      // If this function is dead already, no need to remove it
      // again.
      if (F->isZombie())
        continue;
      // Do not remove definitions of global initializers.
      if (GlobalInitFunctions.count(F))
        continue;
      CallGraphChanged |= tryToConvertExtenralDefinitionIntoDeclaration(F);
    }

    return CallGraphChanged;
  }

public:
  DeadFunctionElimination(SILModule *module, bool late)
      : Module(module), isLate(late) {}
  
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

    // Next step: delete all dead functions.
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

    // If this pass running as a late pass, remove external definitions.
    if (isLate)
      CallGraphChanged |= removeExternalDefinitions();

    return CallGraphChanged;
  }
};

//===----------------------------------------------------------------------===//
//                      Pass Definition and Entry Points
//===----------------------------------------------------------------------===//

namespace {

class SILDeadFuncElimination : public SILModuleTransform {
  // If set, this pass is running very late in the pipeline.
  bool isLate;

  void run() override {

    DEBUG(llvm::dbgs() << "Running DeadFuncElimination (isLate = " << isLate
                       << ")\n");

    // Avoid that Deserializers keep references to functions in their caches.
    getModule()->invalidateSILLoaderCaches();

    DeadFunctionElimination deadFunctionElimination(getModule(), isLate);
    bool changed = deadFunctionElimination.eliminateDeadFunctions();
    
    if (changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
  }
  
  StringRef getName() override { return "Dead Function Elimination"; }

public:
  SILDeadFuncElimination(bool late) : isLate(late) {}
};

} // end anonymous namespace

SILTransform *swift::createDeadFunctionElimination() {
  return new SILDeadFuncElimination(false);
}

// This pass is an extension of the dead function elimination, which
// additionally performs removal of external function definitions for a sake of
// reducing the amount of code to run through IRGen. It is supposed to run very
// late in the pipeline, after devirtualization, inlining and all specialization
// passes.
SILTransform *swift::createLateDeadFunctionElimination() {
  return new SILDeadFuncElimination(true);
}
