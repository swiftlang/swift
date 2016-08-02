//===--- DeadFunctionElimination.cpp - Eliminate dead functions -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-dead-function-elimination"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumDeadFunc, "Number of dead functions eliminated");
STATISTIC(NumEliminatedExternalDefs, "Number of external function definitions eliminated");

namespace {

/// This is a base class for passes that are based on function liveness
/// computations like e.g. dead function elimination.
/// It provides a common logic for computing live (i.e. reachable) functions.
class FunctionLivenessComputation {
protected:
  /// Stores which functions implement a vtable or witness table method.
  struct MethodInfo {

    MethodInfo() : isAnchor(false) {}

    /// All functions which implement the method. Together with the class for
    /// which the function implements the method. In case of a witness method,
    /// the class pointer is null.
    SmallVector<std::pair<SILFunction *, ClassDecl *>, 8> implementingFunctions;

    /// True, if the whole method is alive, e.g. because it's class is visible
    /// from outside. This implies that all implementing functions are alive.
    bool isAnchor;
  };

  SILModule *Module;

  llvm::DenseMap<AbstractFunctionDecl *, MethodInfo *> MethodInfos;
  llvm::SpecificBumpPtrAllocator<MethodInfo> MethodInfoAllocator;

  llvm::SmallSetVector<SILFunction *, 16> Worklist;

  llvm::SmallPtrSet<SILFunction *, 32> AliveFunctions;

  /// Checks is a function is alive, e.g. because it is visible externally.
  bool isAnchorFunction(SILFunction *F) {

    // Remove internal functions that are not referenced by anything.
    if (isPossiblyUsedExternally(F->getLinkage(), Module->isWholeModule()))
      return true;

    // ObjC functions are called through the runtime and are therefore alive
    // even if not referenced inside SIL.
    if (F->getRepresentation() == SILFunctionTypeRepresentation::ObjCMethod)
      return true;

    // If function is marked as "keep-as-public", don't remove it.
    // Change its linkage to public, so that other applications can refer to it.
    // It is important that this transformation is done at the end of
    // a pipeline, as it may break some optimizations.
    if (F->isKeepAsPublic()) {
      F->setLinkage(SILLinkage::Public);
      F->setFragile(IsFragile_t::IsNotFragile);
      DEBUG(llvm::dbgs() << "DFE: Preserve the specialization "
                         << F->getName() << '\n');
      return true;
    }

    return false;
  }

  /// Gets or creates the MethodInfo for a vtable or witness table method.
  /// \p decl The method declaration. In case of a vtable method this is always
  ///         the most overridden method.
  MethodInfo *getMethodInfo(AbstractFunctionDecl *decl) {
    MethodInfo *&entry = MethodInfos[decl];
    if (entry == nullptr) {
      entry = new (MethodInfoAllocator.Allocate()) MethodInfo();
    }
    return entry;
  }

  /// Adds a function which implements a vtable or witness method. If it's a
  /// vtable method, \p C is the class for which the function implements the
  /// method. For witness methods \C is null.
  void addImplementingFunction(MethodInfo *mi, SILFunction *F, ClassDecl *C) {
    if (mi->isAnchor)
      ensureAlive(F);
    mi->implementingFunctions.push_back(std::make_pair(F, C));
  }

  /// Returns true if a function is marked as alive.
  bool isAlive(SILFunction *F) { return AliveFunctions.count(F) != 0; }

  /// Marks a function as alive.
  void makeAlive(SILFunction *F) {
    AliveFunctions.insert(F);
    assert(F && "function does not exist");
    Worklist.insert(F);
  }
  
  /// Marks a function as alive if it is not alive yet.
  void ensureAlive(SILFunction *F) {
    if (!isAlive(F))
      makeAlive(F);
  }

  /// Returns true if \a Derived is the same as \p Base or derived from it.
  static bool isDerivedOrEqual(ClassDecl *Derived, ClassDecl *Base) {
    for (;;) {
      if (Derived == Base)
        return true;
      if (!Derived->hasSuperclass())
        break;
      Derived = Derived->getSuperclass()->getClassOrBoundGenericClass();
    }
    return false;
  }

  /// Returns true if the implementation of method \p FD in class \p ImplCl
  /// may be called when the type of the class_method's operand is \p MethodCl.
  /// Both, \p MethodCl and \p ImplCl, may by null if not known or if it's a
  /// protocol method.
  static bool canHaveSameImplementation(FuncDecl *FD, ClassDecl *MethodCl,
                                        ClassDecl *ImplCl) {
    if (!FD || !MethodCl || !ImplCl)
      return true;

    // All implementations of derived classes may be called.
    if (isDerivedOrEqual(ImplCl, MethodCl))
      return true;
    
    // Check if the method implementation is the same in a super class, i.e.
    // it is not overridden in the derived class.
    auto *Impl1 = MethodCl->findImplementingMethod(FD);
    assert(Impl1);
    auto *Impl2 = ImplCl->findImplementingMethod(FD);
    assert(Impl2);
    
    return Impl1 == Impl2;
  }

  /// Marks the implementing functions of the method \p FD as alive. If it is a
  /// class method, \p MethodCl is the type of the class_method instruction's
  /// operand.
  void ensureAlive(MethodInfo *mi, FuncDecl *FD, ClassDecl *MethodCl) {
    for (auto &Pair : mi->implementingFunctions) {
      SILFunction *FImpl = Pair.first;
      if (!isAlive(FImpl) &&
          canHaveSameImplementation(FD, MethodCl, Pair.second)) {
        makeAlive(FImpl);
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
          auto *funcDecl = getBase(
              cast<AbstractFunctionDecl>(MI->getMember().getDecl()));
          MethodInfo *mi = getMethodInfo(funcDecl);
          ClassDecl *MethodCl = nullptr;
          if (MI->getNumOperands() == 1)
            MethodCl = MI->getOperand(0)->getType().getClassOrBoundGenericClass();
          ensureAlive(mi, dyn_cast<FuncDecl>(funcDecl), MethodCl);
        } else if (auto *FRI = dyn_cast<FunctionRefInst>(&I)) {
          ensureAlive(FRI->getReferencedFunction());
        }
      }
    }
  }

  /// Retrieve the visibility information from the AST.
  bool isVisibleExternally(ValueDecl *decl) {
    Accessibility accessibility = decl->getEffectiveAccess();
    SILLinkage linkage;
    switch (accessibility) {
    case Accessibility::Private:
    case Accessibility::FilePrivate:
      linkage = SILLinkage::Private;
      break;
    case Accessibility::Internal:
      linkage = SILLinkage::Hidden;
      break;
    case Accessibility::Public:
    case Accessibility::Open:
      linkage = SILLinkage::Public;
      break;
    }
    if (isPossiblyUsedExternally(linkage, Module->isWholeModule()))
      return true;

    // If a vtable or witness table (method) is only visible in another module
    // it can be accessed inside that module and we don't see this access.
    // We hit this case e.g. if a table is imported from the stdlib.
    if (decl->getDeclContext()->getParentModule() != Module->getSwiftModule())
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

      if (!F.shouldOptimize()) {
        DEBUG(llvm::dbgs() << "  anchor a no optimization function: " << F.getName() << "\n");
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
  FunctionLivenessComputation(SILModule *module) :
    Module(module) {}

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

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                             DeadFunctionElimination
//===----------------------------------------------------------------------===//

namespace {

class DeadFunctionElimination : FunctionLivenessComputation {

  /// DeadFunctionElimination pass takes functions
  /// reachable via vtables and witness_tables into account
  /// when computing a function liveness information.
  void findAnchorsInTables() {
    // Check vtable methods.
    for (SILVTable &vTable : Module->getVTableList()) {
      for (auto &entry : vTable.getEntries()) {
        // Destructors are alive because they are called from swift_release
        if (entry.first.kind == SILDeclRef::Kind::Deallocator ||
            entry.first.kind == SILDeclRef::Kind::IVarDestroyer) {
          ensureAlive(entry.second);
          continue;
        }

        SILFunction *F = entry.second;
        auto *fd = cast<AbstractFunctionDecl>(entry.first.getDecl());
        fd = getBase(fd);
        MethodInfo *mi = getMethodInfo(fd);
        addImplementingFunction(mi, F, vTable.getClass());

        if (// A conservative approach: if any of the overridden functions is
            // visible externally, we mark the whole method as alive.
            isPossiblyUsedExternally(F->getLinkage(), Module->isWholeModule())
            // We also have to check the method declaration's accessibility.
            // Needed if it's a public base method declared in another
            // compilation unit (for this we have no SILFunction).
            || isVisibleExternally(fd)
            // Declarations are always accessible externally, so they are alive.
            || !F->isDefinition()) {
          ensureAlive(mi, nullptr, nullptr);
        }
      }
    }

    // Check witness methods.
    for (SILWitnessTable &WT : Module->getWitnessTableList()) {
      bool tableIsAlive = isVisibleExternally(WT.getConformance()->getProtocol());
      for (const SILWitnessTable::Entry &entry : WT.getEntries()) {
        if (entry.getKind() != SILWitnessTable::Method)
          continue;

        auto methodWitness = entry.getMethodWitness();
        auto *fd = cast<AbstractFunctionDecl>(methodWitness.Requirement.
                                              getDecl());
        assert(fd == getBase(fd) && "key in witness table is overridden");
        SILFunction *F = methodWitness.Witness;
        if (!F)
          continue;

        MethodInfo *mi = getMethodInfo(fd);
        addImplementingFunction(mi, F, nullptr);
        if (tableIsAlive || !F->isDefinition())
          ensureAlive(mi, nullptr, nullptr);
      }
    }

    // Check default witness methods.
    for (SILDefaultWitnessTable &WT : Module->getDefaultWitnessTableList()) {
      for (const SILDefaultWitnessTable::Entry &entry : WT.getEntries()) {
        if (!entry.isValid())
          continue;

        SILFunction *F = entry.getWitness();
        auto *fd = cast<AbstractFunctionDecl>(entry.getRequirement().getDecl());

        MethodInfo *mi = getMethodInfo(fd);
        addImplementingFunction(mi, F, nullptr);
        ensureAlive(mi, nullptr, nullptr);
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
      SILWitnessTable *WT = &*WI;
      WI++;
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

    removeDeadEntriesFromTables();

    // First drop all references so that we don't get problems with non-zero
    // reference counts of dead functions.
    for (SILFunction &F : *Module)
      if (!isAlive(&F))
        F.dropAllReferences();

    // Next step: delete all dead functions.
    for (auto FI = Module->begin(), EI = Module->end(); FI != EI;) {
      SILFunction *F = &*FI;
      ++FI;
      if (!isAlive(F)) {
        DEBUG(llvm::dbgs() << "  erase dead function " << F->getName() << "\n");
        NumDeadFunc++;
        DFEPass->invalidateAnalysisForDeadFunction(F,
                                     SILAnalysis::InvalidationKind::Everything);
        Module->eraseFunction(F);
      }
    }
  }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                        ExternalFunctionDefinitionsElimination
//===----------------------------------------------------------------------===//

namespace {

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
    /// Therefore there is no need for a liveness computation.
    /// The next line can be just replaced by:
    /// return false;
    return FunctionLivenessComputation::findAliveFunctions();
  }

  /// Try to convert definition into declaration.
  /// Returns true if function was erased from the module.
  bool tryToConvertExternalDefinitionIntoDeclaration(SILFunction *F) {
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
    NumEliminatedExternalDefs++;
    return true;
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
      SILFunction *F = &*FI;
      ++FI;
      // Do not remove bodies of any functions that are alive.
      if (!isAlive(F)) {
        if (tryToConvertExternalDefinitionIntoDeclaration(F)) {
          DFEPass->invalidateAnalysisForDeadFunction(F,
                                    SILAnalysis::InvalidationKind::Everything);
          if (F->getRefCount() == 0)
            F->getModule().eraseFunction(F);
        }
      }
    }
  }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                      Pass Definition and Entry Points
//===----------------------------------------------------------------------===//

namespace {

class SILDeadFuncElimination : public SILModuleTransform {
  void run() override {
    DEBUG(llvm::dbgs() << "Running DeadFuncElimination\n");

    // The deserializer caches functions that it deserializes so that if it is
    // asked to deserialize that function again, it does not do extra work. This
    // causes the function's reference count to be incremented causing it to be
    // alive unnecessarily. We invalidate the SILLoaderCaches here so that we
    // can eliminate such functions.
    getModule()->invalidateSILLoaderCaches();

    DeadFunctionElimination deadFunctionElimination(getModule());
    deadFunctionElimination.eliminateFunctions(this);
  }
  
  StringRef getName() override { return "Dead Function Elimination"; }
};

class SILExternalFuncDefinitionsElimination : public SILModuleTransform {
  void run() override {
    DEBUG(llvm::dbgs() << "Running ExternalFunctionDefinitionsElimination\n");

    // The deserializer caches functions that it deserializes so that if it is
    // asked to deserialize that function again, it does not do extra work. This
    // causes the function's reference count to be incremented causing it to be
    // alive unnecessarily. We invalidate the SILLoaderCaches here so that we
    // can eliminate the definitions of such functions.
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
