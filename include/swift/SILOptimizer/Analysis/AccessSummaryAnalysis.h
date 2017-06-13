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
// the formal accesses that a function makes to its address-type parameters.
// These summaries are used to statically diagnose violations of exclusive
// accesses for noescape closures.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_ACCESS_SUMMARY_ANALYSIS_H_
#define SWIFT_SILOPTIMIZER_ANALYSIS_ACCESS_SUMMARY_ANALYSIS_H_

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/BottomUpIPAnalysis.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class AccessSummaryAnalysis : public BottomUpIPAnalysis {
public:
  /// Summarizes the accesses that a function begins on a parameter.
  class ParameterSummary {
  private:
    /// The kind of access begun on the argument.
    /// 'None' means no access performed.
    Optional<SILAccessKind> Kind = None;

    /// The location of the access. Used for diagnostics.
    SILLocation AccessLoc = SILLocation((Expr *)nullptr);

  public:
    Optional<SILAccessKind> getAccessKind() const { return Kind; }

    SILLocation getAccessLoc() const { return AccessLoc; }

    /// The lattice operation on parameter summaries.
    bool mergeWith(const ParameterSummary &other);

    /// Merge in an access to the parameter of the given kind at the given
    /// location. Returns true if the merge caused the summary to change.
    bool mergeWith(SILAccessKind otherKind, SILLocation otherLoc);

    /// Returns a description of the summary. For debugging and testing
    /// purposes. Return true if the merge caused the summary to change.
    StringRef getDescription() const;
  };

  /// Summarizes the accesses that a function begins on its parameters.
  class FunctionSummary {
  public:
    FunctionSummary(unsigned parameterCount) : ParamAccesses(parameterCount) {}

    /// Returns of summary of the the function accesses that parameter at the
    /// given index.
    ParameterSummary &getAccessForParameter(unsigned parameter) {
      return ParamAccesses[parameter];
    }

    const ParameterSummary &getAccessForParameter(unsigned parameter) const {
      return ParamAccesses[parameter];
    }

    /// Returns the number of parameters in the summary.
    unsigned getParameterCount() const { return ParamAccesses.size(); }

  private:
    llvm::SmallVector<ParameterSummary, 6> ParamAccesses;
  };

  friend raw_ostream &operator<<(raw_ostream &os,
                                 const FunctionSummary &summary);

  class FunctionInfo;
  /// Records a flow of a caller's parameter to a called function.
  /// This flow is used to iterate the interprocedural analysis to a fixpoint.
  struct ParameterFlow {
    /// The index of the parameter in the caller.
    const unsigned CallerParameterIndex;

    /// The index of the argument in the callee.
    const unsigned CalleeArgumentIndex;

    FunctionInfo *const CalleeFunctionInfo;
  };

  /// Records the summary and parameter flows for a given function.
  /// Used by the BottomUpIPAnalysis to propagate information
  /// from callees to callers.
  class FunctionInfo : public FunctionInfoBase<FunctionInfo> {
  private:
    FunctionSummary FS;

    SILFunction *F;

    llvm::SmallVector<ParameterFlow, 8> RecordedParameterFlows;

  public:
    FunctionInfo(SILFunction *F) : FS(F->getArguments().size()), F(F) {}

    SILFunction *getFunction() const { return F; }

    ArrayRef<ParameterFlow> getParameterFlows() const {
      return RecordedParameterFlows;
    }

    const FunctionSummary &getSummary() const { return FS; }
    FunctionSummary &getSummary() { return FS; }

    /// Record a flow of a parameter in this function to a callee.
    void recordFlow(const ParameterFlow &flow) {
      flow.CalleeFunctionInfo->addCaller(this, nullptr);
      RecordedParameterFlows.push_back(flow);
    }
  };

public:
  AccessSummaryAnalysis() : BottomUpIPAnalysis(AnalysisKind::AccessSummary) {}

  /// Returns a summary of the accesses performed by the given function.
  const FunctionSummary &getOrCreateSummary(SILFunction *Fn);

  virtual void initialize(SILPassManager *PM) override {}
  virtual void invalidate() override;
  virtual void invalidate(SILFunction *F, InvalidationKind K) override;
  virtual void notifyAddFunction(SILFunction *F) override {}
  virtual void notifyDeleteFunction(SILFunction *F) override {
    invalidate(F, InvalidationKind::Nothing);
  }
  virtual void invalidateFunctionTables() override {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::AccessSummary;
  }

private:
  typedef BottomUpFunctionOrder<FunctionInfo> FunctionOrder;

  /// Returns the BottomUpIPAnalysis information for the given function.
  FunctionInfo *getFunctionInfo(SILFunction *F);

  /// Summarizes the given function and iterates the interprocedural analysis
  /// to a fixpoint.
  void recompute(FunctionInfo *initial);

  /// Propagate the access summary from the parameter of a called function
  /// to the caller.
  bool propagateFromCalleeToCaller(FunctionInfo *callerInfo,
                                   ParameterFlow site);

  /// Summarize the given function and schedule it for interprocedural
  /// analysis.
  void processFunction(FunctionInfo *info, FunctionOrder &order);

  /// Summarize how the function uses the given parameter.
  void processParameter(FunctionInfo *info, SILFunctionArgument *parameter,
                        ParameterSummary &summary, FunctionOrder &order);

  /// Summarize a partial_apply instruction.
  void processPartialApply(FunctionInfo *callerInfo,
                           unsigned callerParameterIndex,
                           PartialApplyInst *apply,
                           Operand *applyArgumentOperand, FunctionOrder &order);

  /// Summarize apply or try_apply
  void processFullApply(FunctionInfo *callerInfo, unsigned callerParameterIndex,
                        FullApplySite apply, Operand *argumentOperand,
                        FunctionOrder &order);

  /// Summarize a call site and schedule it for interprocedural analysis.
  void processCall(FunctionInfo *callerInfo, unsigned callerParameterIndex,
                   SILFunction *calledFunction, unsigned argumentIndex,
                   FunctionOrder &order);

  /// Maps functions to the information the analysis keeps for each function.
  llvm::DenseMap<SILFunction *, FunctionInfo *> FunctionInfos;

  llvm::SpecificBumpPtrAllocator<FunctionInfo> Allocator;
};

} // end namespace swift

#endif

