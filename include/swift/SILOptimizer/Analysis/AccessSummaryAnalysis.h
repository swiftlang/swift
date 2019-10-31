//===--- AccessSummaryAnalysis.h - SIL Access Summary Analysis --*- C++ -*-===//
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
//
// This file implements an interprocedural analysis pass that summarizes
// the formal accesses that a function makes to its address-type arguments.
// These summaries are used to statically diagnose violations of exclusive
// accesses for noescape closures.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_ACCESS_SUMMARY_ANALYSIS_H_
#define SWIFT_SILOPTIMIZER_ANALYSIS_ACCESS_SUMMARY_ANALYSIS_H_

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/BottomUpIPAnalysis.h"
#include "swift/SILOptimizer/Utils/IndexTrie.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class AccessSummaryAnalysis : public BottomUpIPAnalysis {
public:
  class SubAccessSummary {
  private:
    /// The kind of access begun on the argument.
    SILAccessKind Kind;

    /// The location of the access. Used for diagnostics.
    SILLocation AccessLoc = SILLocation((Expr *)nullptr);

    const IndexTrieNode *SubPath = nullptr;

  public:
    SubAccessSummary(SILAccessKind Kind, SILLocation AccessLoc,
                     const IndexTrieNode *SubPath)
        : Kind(Kind), AccessLoc(AccessLoc), SubPath(SubPath) {}

    SILAccessKind getAccessKind() const { return Kind; }

    SILLocation getAccessLoc() const { return AccessLoc; }

    const IndexTrieNode *getSubPath() const { return SubPath; }

    /// The lattice operation on SubAccessSummaries summaries.
    bool mergeWith(const SubAccessSummary &other);

    /// Merge in an access to the argument of the given kind at the given
    /// location with the given suppath. Returns true if the merge caused the
    /// summary to change.
    bool mergeWith(SILAccessKind otherKind, SILLocation otherLoc,
                   const IndexTrieNode *otherSubPath);

    /// Returns a description of the summary. For debugging and testing
    /// purposes.
    std::string getDescription(SILType BaseType, SILModule &M,
                               TypeExpansionContext context) const;
  };

  typedef llvm::SmallDenseMap<const IndexTrieNode *, SubAccessSummary, 8>
      SubAccessMap;

  /// Summarizes the accesses that a function begins on an argument, including
  /// the projection subpath that was accessed.
  class ArgumentSummary {
  private:
    SubAccessMap SubAccesses;

  public:
    /// The lattice operation on argument summaries.
    bool mergeWith(const ArgumentSummary &other);

    /// Merge in an access to the argument of the given kind at the given
    /// location. Returns true if the merge caused the summary to change.
    bool mergeWith(SILAccessKind otherKind, SILLocation otherLoc,
                   const IndexTrieNode *otherSubPath);

    /// Returns a description of the summary. For debugging and testing
    /// purposes.
    std::string getDescription(SILType BaseType, SILModule &M,
                               TypeExpansionContext context) const;

    /// Returns the accesses that the function performs to subpaths of the
    /// argument.
    const SubAccessMap &getSubAccesses() const { return SubAccesses; }

    /// Returns the sorted subaccess summaries into the passed-in storage.
    /// The accesses are sorted lexicographically by increasing subpath
    /// length and projection index.
    void getSortedSubAccesses(SmallVectorImpl<SubAccessSummary> &storage) const;
  };

  /// Summarizes the accesses that a function begins on its arguments or
  /// projections from its arguments.
  class FunctionSummary {
  private:
    llvm::SmallVector<ArgumentSummary, 6> ArgAccesses;

  public:
    FunctionSummary(unsigned argCount) : ArgAccesses(argCount) {}

    /// Returns of summary of the the function accesses that argument at the
    /// given index.
    ArgumentSummary &getAccessForArgument(unsigned argument) {
      return ArgAccesses[argument];
    }

    const ArgumentSummary &getAccessForArgument(unsigned argument) const {
      return ArgAccesses[argument];
    }

    /// Returns the number of argument in the summary.
    unsigned getArgumentCount() const { return ArgAccesses.size(); }

    void print(raw_ostream &os, SILFunction *fn) const;
  };

  class FunctionInfo;
  /// Records a flow of a caller's argument to a called function.
  /// This flow is used to iterate the interprocedural analysis to a fixpoint.
  struct ArgumentFlow {
    /// The index of the argument in the caller.
    const unsigned CallerArgumentIndex;

    /// The index of the argument in the callee.
    const unsigned CalleeArgumentIndex;

    FunctionInfo *const CalleeFunctionInfo;
  };

  /// Records the summary and argument flows for a given function.
  /// Used by the BottomUpIPAnalysis to propagate information
  /// from callees to callers.
  class FunctionInfo : public FunctionInfoBase<FunctionInfo> {
  private:
    FunctionSummary FS;

    SILFunction *F;

    llvm::SmallVector<ArgumentFlow, 8> RecordedArgumentFlows;

  public:
    FunctionInfo(SILFunction *F) : FS(F->getArguments().size()), F(F) {}

    SILFunction *getFunction() const { return F; }

    ArrayRef<ArgumentFlow> getArgumentFlows() const {
      return RecordedArgumentFlows;
    }

    const FunctionSummary &getSummary() const { return FS; }
    FunctionSummary &getSummary() { return FS; }

    /// Record a flow of an argument in this function to a callee.
    void recordFlow(const ArgumentFlow &flow) {
      flow.CalleeFunctionInfo->addCaller(this, nullptr);
      RecordedArgumentFlows.push_back(flow);
    }
  };

private:
  /// Maps functions to the information the analysis keeps for each function.
  llvm::DenseMap<SILFunction *, FunctionInfo *> FunctionInfos;

  llvm::SpecificBumpPtrAllocator<FunctionInfo> Allocator;

  /// A trie of integer indices that gives pointer identity to a path of
  /// projections. This is shared between all functions in the module.
  std::unique_ptr<IndexTrieNode> SubPathTrie;

public:
  AccessSummaryAnalysis() : BottomUpIPAnalysis(SILAnalysisKind::AccessSummary) {
    SubPathTrie.reset(new IndexTrieNode());
  }

  /// Returns a summary of the accesses performed by the given function.
  const FunctionSummary &getOrCreateSummary(SILFunction *Fn);

  IndexTrieNode *getSubPathTrieRoot() {
    return SubPathTrie.get();
  }

  /// Returns an IndexTrieNode that represents the single subpath accessed from
  /// BAI or the root if no such node exists.
  const IndexTrieNode *findSubPathAccessed(BeginAccessInst *BAI);

  virtual void initialize(SILPassManager *PM) override {}
  virtual void invalidate() override;
  virtual void invalidate(SILFunction *F, InvalidationKind K) override;
  virtual void notifyAddedOrModifiedFunction(SILFunction *F) override {}
  virtual void notifyWillDeleteFunction(SILFunction *F) override {
    invalidate(F, InvalidationKind::Nothing);
  }
  virtual void invalidateFunctionTables() override {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::AccessSummary;
  }

  /// Returns a description of the subpath suitable for use in diagnostics.
  /// The base type must be the type of the root of the path.
  static std::string getSubPathDescription(SILType BaseType,
                                           const IndexTrieNode *SubPath,
                                           SILModule &M,
                                           TypeExpansionContext context);

  /// Performs a lexicographic comparison of two subpaths, first by path length
  /// and then by index of the last path component. Returns true when lhs
  /// is less than rhs.
  static bool compareSubPaths(const IndexTrieNode *lhs,
                              const IndexTrieNode *rhs);
private:
  typedef BottomUpFunctionOrder<FunctionInfo> FunctionOrder;

  /// Returns the BottomUpIPAnalysis information for the given function.
  FunctionInfo *getFunctionInfo(SILFunction *F);

  /// Summarizes the given function and iterates the interprocedural analysis
  /// to a fixpoint.
  void recompute(FunctionInfo *initial);

  /// Propagate the access summary from the argument of a called function
  /// to the caller.
  bool propagateFromCalleeToCaller(FunctionInfo *callerInfo,
                                   ArgumentFlow site);

  /// Summarize the given function and schedule it for interprocedural
  /// analysis.
  void processFunction(FunctionInfo *info, FunctionOrder &order);

  /// Summarize how the function uses the given argument.
  void processArgument(FunctionInfo *info, SILFunctionArgument *argment,
                        ArgumentSummary &summary, FunctionOrder &order);

  /// Summarize a partial_apply instruction.
  void processPartialApply(FunctionInfo *callerInfo,
                           unsigned callerArgumentIndex,
                           PartialApplyInst *apply,
                           Operand *applyArgumentOperand, FunctionOrder &order);

  /// Summarize apply or try_apply
  void processFullApply(FunctionInfo *callerInfo, unsigned callerArgumentIndex,
                        FullApplySite apply, Operand *argumentOperand,
                        FunctionOrder &order);

  /// Summarize a call site and schedule it for interprocedural analysis.
  void processCall(FunctionInfo *callerInfo, unsigned callerArgumentIndex,
                   SILFunction *calledFunction, unsigned argumentIndex,
                   FunctionOrder &order);
};

} // end namespace swift

#endif

