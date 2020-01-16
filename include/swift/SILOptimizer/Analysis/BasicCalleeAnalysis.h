//===- BasicCalleeAnalysis.h - Determine callees per call site --*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_BASICCALLEEANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_BASICCALLEEANALYSIS_H

#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Allocator.h"

namespace swift {

class ClassDecl;
class SILFunction;
class SILModule;
class SILWitnessTable;

/// CalleeList is a data structure representing the list of potential
/// callees at a particular apply site. It also has a query that
/// allows a client to determine whether the list is incomplete in the
/// sense that there may be unrepresented callees.
class CalleeList {
  llvm::TinyPtrVector<SILFunction *> CalleeFunctions;
  bool IsIncomplete;

public:
  /// Constructor for when we know nothing about the callees and must
  /// assume the worst.
  CalleeList() : IsIncomplete(true) {}

  /// Constructor for the case where we know an apply can target only
  /// a single function.
  CalleeList(SILFunction *F) : CalleeFunctions(F), IsIncomplete(false) {}

  /// Constructor for arbitrary lists of callees.
  CalleeList(llvm::SmallVectorImpl<SILFunction *> &List, bool IsIncomplete)
      : CalleeFunctions(llvm::makeArrayRef(List.begin(), List.end())),
        IsIncomplete(IsIncomplete) {}

  SWIFT_DEBUG_DUMP;

  void print(llvm::raw_ostream &os) const;

  /// Return an iterator for the beginning of the list.
  ArrayRef<SILFunction *>::iterator begin() const {
    return CalleeFunctions.begin();
  }

  /// Return an iterator for the end of the list.
  ArrayRef<SILFunction *>::iterator end() const {
    return CalleeFunctions.end();
  }

  bool isIncomplete() const { return IsIncomplete; }

  /// Returns true if all callees are known and not external.
  bool allCalleesVisible() const;
};

/// CalleeCache is a helper class that builds lists of potential
/// callees for class and witness method applications, and provides an
/// interface for retrieving a (possibly incomplete) CalleeList for
/// any function application site (including those that are simple
/// function_ref, thin_to_thick, or partial_apply callees).
class CalleeCache {
  using Callees = llvm::SmallVector<SILFunction *, 16>;
  using CalleesAndCanCallUnknown = llvm::PointerIntPair<Callees *, 1>;
  using CacheType = llvm::DenseMap<SILDeclRef, CalleesAndCanCallUnknown>;

  SILModule &M;

  // Allocator for the SmallVectors that we will be allocating.
  llvm::SpecificBumpPtrAllocator<Callees> Allocator;

  // The cache of precomputed callee lists for function decls appearing
  // in class virtual dispatch tables and witness tables.
  CacheType TheCache;

public:
  CalleeCache(SILModule &M) : M(M) {
    computeMethodCallees();
    sortAndUniqueCallees();
  }

  ~CalleeCache() {
    Allocator.DestroyAll();
  }

  /// Return the list of callees that can potentially be called at the
  /// given apply site.
  CalleeList getCalleeList(FullApplySite FAS) const;
  /// Return the list of callees that can potentially be called at the
  /// given instruction. E.g. it could be destructors.
  CalleeList getCalleeList(SILInstruction *I) const;

  CalleeList getCalleeList(SILDeclRef Decl) const;

private:
  void enumerateFunctionsInModule();
  void sortAndUniqueCallees();
  CalleesAndCanCallUnknown &getOrCreateCalleesForMethod(SILDeclRef Decl);
  void computeClassMethodCallees();
  void computeWitnessMethodCalleesForWitnessTable(SILWitnessTable &WT);
  void computeMethodCallees();
  SILFunction *getSingleCalleeForWitnessMethod(WitnessMethodInst *WMI) const;
  CalleeList getCalleeList(WitnessMethodInst *WMI) const;
  CalleeList getCalleeList(ClassMethodInst *CMI) const;
  CalleeList getCalleeListForCalleeKind(SILValue Callee) const;
};

class BasicCalleeAnalysis : public SILAnalysis {
  SILModule &M;
  std::unique_ptr<CalleeCache> Cache;

public:
  BasicCalleeAnalysis(SILModule *M)
      : SILAnalysis(SILAnalysisKind::BasicCallee), M(*M), Cache(nullptr) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::BasicCallee;
  }

  /// Invalidate all information in this analysis.
  virtual void invalidate() override {
    Cache.reset();
  }

  /// Invalidate all of the information for a specific function.
  virtual void invalidate(SILFunction *F, InvalidationKind K) override {
    // No invalidation needed because the analysis does not cache anything
    // per call-site in functions.
  }

  /// Notify the analysis about a newly created function.
  virtual void notifyAddedOrModifiedFunction(SILFunction *F) override {
    // Nothing to be done because the analysis does not cache anything
    // per call-site in functions.
  }

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyWillDeleteFunction(SILFunction *F) override {
    // No invalidation needed because the analysis does not cache anything per
    // call-site in functions.
  }

  /// Notify the analysis about changed witness or vtables.
  virtual void invalidateFunctionTables() override {
    Cache.reset();
  }

  SWIFT_DEBUG_DUMP;

  void print(llvm::raw_ostream &os) const;

  void updateCache() {
    if (!Cache)
      Cache = llvm::make_unique<CalleeCache>(M);
  }

  CalleeList getCalleeList(FullApplySite FAS) {
    updateCache();
    return Cache->getCalleeList(FAS);
  }

  CalleeList getCalleeList(SILInstruction *I) {
    updateCache();
    return Cache->getCalleeList(I);
  }
};

} // end namespace swift

#endif
