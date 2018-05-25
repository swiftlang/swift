//===--- SideEffectAnalysis.h - SIL Side Effect Analysis --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_SIDEEFFECTANALYSIS_H_
#define SWIFT_SILOPTIMIZER_ANALYSIS_SIDEEFFECTANALYSIS_H_

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Analysis/BottomUpIPAnalysis.h"
#include "swift/SILOptimizer/Analysis/ArraySemantic.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class BasicCalleeAnalysis;


/// An enum to represent the kind of scan we perform when we calculate
/// side effects.
enum class RetainObserveKind {
  ObserveRetains,
  IgnoreRetains,
  RetainObserveKindEnd
};

/// Generic base class for any bottom up analysis that summarizes per-function
/// "effects" by first computing local effects, then propagating those effects
/// bottom-up through the call graph.
///
/// FunctionEffects constraints:
/// - void clear()
/// - void setWorstEffects()
/// - bool summarizeFunction(SILFunction *)
/// - bool summarizeCall(FullApplySite)
/// - bool mergeFrom(const FunctionSideEffects &)
/// - bool mergeFromApply(const FunctionEffects &, FullApplySite)
/// - void analyzeInstruction(SILInstruction *)
template <typename FunctionEffects>
class GenericFunctionEffectAnalysis : public BottomUpIPAnalysis {

  /// Stores the analysis data, e.g. side-effects, for a function.
  struct FunctionInfo : public FunctionInfoBase<FunctionInfo> {

    /// The function effects.
    FunctionEffects functionEffects;

    /// Back-link to the function.
    SILFunction *F;

    /// Used during recomputation to indicate if the side-effects of a caller
    /// must be updated.
    bool needUpdateCallers = false;

    FunctionInfo(SILFunction *F) : F(F) {}

    /// Clears the analysis data on invalidation.
    void clear() { functionEffects.clear(); }
  };

  typedef BottomUpFunctionOrder<FunctionInfo> FunctionOrder;

  enum {
    /// The maximum call-graph recursion depth for recomputing the analysis.
    /// This is a relatively small number to reduce compile time in case of
    /// large cycles in the call-graph.
    /// In case of no cycles, we should not hit this limit at all because the
    /// pass manager processes functions in bottom-up order.
    MaxRecursionDepth = 5
  };

  /// All the function effect information for the whole module.
  llvm::DenseMap<SILFunction *, FunctionInfo *> functionInfoMap;

  /// The allocator for the map of values in FunctionInfoMap.
  llvm::SpecificBumpPtrAllocator<FunctionInfo> allocator;

  /// Callee analysis, used for determining the callees at call sites.
  BasicCalleeAnalysis *BCA;

public:
  GenericFunctionEffectAnalysis(AnalysisKind kind) : BottomUpIPAnalysis(kind) {}

  const FunctionEffects &getEffects(SILFunction *F) {
    FunctionInfo *functionInfo = getFunctionInfo(F);
    if (!functionInfo->isValid())
      recompute(functionInfo);
    return functionInfo->functionEffects;
  }

  /// Get the merged effects of all callees at the given call site from the
  /// callee's perspective (don't transform parameter effects).
  void getCalleeEffects(FunctionEffects &calleeEffects,
                        FullApplySite fullApply);

  /// Get the merge effects of all callees at the given call site from the
  /// caller's perspective. Parameter effects are translated into information
  /// for the caller's arguments, and local effects are dropped.
  void getCallSiteEffects(FunctionEffects &callEffects,
                          FullApplySite fullApply) {
    FunctionEffects calleeEffects;
    getCalleeEffects(calleeEffects, fullApply);
    callEffects.mergeFromApply(calleeEffects, fullApply);
  }

  virtual void initialize(SILPassManager *PM) override;

  /// Invalidate all information in this analysis.
  virtual void invalidate() override;

  /// Invalidate all of the information for a specific function.
  virtual void invalidate(SILFunction *F, InvalidationKind K) override;

  /// Notify the analysis about a newly created function.
  virtual void notifyAddFunction(SILFunction *F) override {}

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyDeleteFunction(SILFunction *F) override {
    invalidate(F, InvalidationKind::Nothing);
  }

  /// Notify the analysis about changed witness or vtables.
  virtual void invalidateFunctionTables() override {}

private:
  /// Gets or creates FunctionEffects for \p F.
  FunctionInfo *getFunctionInfo(SILFunction *F) {
    FunctionInfo *&functionInfo = functionInfoMap[F];
    if (!functionInfo) {
      functionInfo = new (allocator.Allocate()) FunctionInfo(F);
    }
    return functionInfo;
  }

  /// Analyze the side-effects of a function, including called functions.
  /// Visited callees are added to \p BottomUpOrder until \p RecursionDepth
  /// reaches MaxRecursionDepth.
  void analyzeFunction(FunctionInfo *functionInfo, FunctionOrder &bottomUpOrder,
                       int recursionDepth);

  void analyzeCall(FunctionInfo *functionInfo, FullApplySite fullApply,
                   FunctionOrder &bottomUpOrder, int recursionDepth);

  /// Recomputes the side-effect information for the function \p Initial and
  /// all called functions, up to a recursion depth of MaxRecursionDepth.
  void recompute(FunctionInfo *initialInfo);
};

/// Set \p dest if \p src is set and return true if \p dest was not set
/// before.
static bool changedFlagByInPlaceOr(bool &dest, bool src) {
  if (src && !dest) {
    dest = src;
    return true;
  }
  return false;
}

/// Side-effect information for the function (global effects) or a specific
/// parameter of the function. See FunctionSideEffects.
class FunctionSideEffectFlags {
  friend class FunctionSideEffects;
  using MemoryBehavior = SILInstruction::MemoryBehavior;

  bool Reads = false;
  bool Writes = false;
  bool Retains = false;
  bool Releases = false;

  /// Sets the most conservative effects.
  void setWorstEffects() {
    Reads = true;
    Writes = true;
    Retains = true;
    Releases = true;
  }

  /// Clears all effects.
  void clear() {
    Reads = false;
    Writes = false;
    Retains = false;
    Releases = false;
  }

public:
  /// Does the function read from memory (excluding reads from locally
  /// allocated memory)?
  bool mayRead() const { return Reads; }

  /// Does the function write to memory (excluding writes to locally
  /// allocated memory)?
  bool mayWrite() const { return Writes; }

  /// Does the function retain objects (excluding retains of locally
  /// allocated objects)?
  bool mayRetain() const { return Retains; }

  /// Does the function release objects (excluding releases of locally
  /// allocated objects)?
  bool mayRelease() const { return Releases; }

  /// Gets the memory behavior considering the global effects and
  /// all parameter effects. If \p ScanKind equals ignoreRetains then retain
  /// instructions are considered as side effects.
  MemoryBehavior getMemBehavior(RetainObserveKind ScanKind) const {
    if (mayRelease())
      return MemoryBehavior::MayHaveSideEffects;

    if (ScanKind == RetainObserveKind::ObserveRetains && mayRetain())
      return MemoryBehavior::MayHaveSideEffects;

    if (mayWrite())
      return mayRead() ? MemoryBehavior::MayReadWrite
                       : MemoryBehavior::MayWrite;

    if (mayRead())
      return MemoryBehavior::MayRead;

    return MemoryBehavior::None;
  }

  /// Merge effects from \p RHS.
  bool mergeFrom(const FunctionSideEffectFlags &RHS) {
    bool Changed = false;
    Changed |= changedFlagByInPlaceOr(Reads, RHS.Reads);
    Changed |= changedFlagByInPlaceOr(Writes, RHS.Writes);
    Changed |= changedFlagByInPlaceOr(Retains, RHS.Retains);
    Changed |= changedFlagByInPlaceOr(Releases, RHS.Releases);
    return Changed;
  }

  friend raw_ostream &operator<<(raw_ostream &os,
                                 const FunctionSideEffectFlags &Effects) {
    if (Effects.mayRead())
      os << 'r';
    if (Effects.mayWrite())
      os << 'w';
    if (Effects.mayRetain())
      os << '+';
    if (Effects.mayRelease())
      os << '-';
    return os;
  }
};

/// Summarizes the side-effects of a function. The side-effect information
/// is divided into global effects and effects for specific function
/// parameters.
/// If a side-effect can be associated to a specific function parameter, it is
/// not added to the global effects of the function. E.g. if a memory write is
/// only done through an @inout parameter, the mayWrite side-effect is only
/// accounted for this parameter.
/// Effects for a parameter make only sense if the parameter is implemented as
/// a pointer or contains a pointer:
/// *) The parameter is an address parameter, e.g. @out, @inout, etc.
/// *) The parameter is a reference
/// *) The parameter is a value type (e.g. struct) and contains a reference.
///    In this case the effects refer to all references in the value type.
///    E.g. if a struct contains 2 references, a mayWrite effect means that
///    memory is written to one of the referenced objects (or to both).
class FunctionSideEffects {
  using MemoryBehavior = SILInstruction::MemoryBehavior;

  /// Side-effects which can be associated to a parameter.
  llvm::SmallVector<FunctionSideEffectFlags, 6> ParamEffects;

  /// All other side-effects which cannot be associated to a parameter.
  FunctionSideEffectFlags GlobalEffects;

  /// Side-effects on locally allocated storage. Such side-effects are not
  /// relevant to optimizations. The LocalEffects are only used to return
  /// "something" for local storage in getEffectsOn().
  FunctionSideEffectFlags LocalEffects;

  /// Does the function allocate objects, boxes, etc., i.e. everything which
  /// has a reference count.
  bool AllocsObjects = false;

  /// Can this function trap or exit the program in any way?
  bool Traps = false;

  /// Does this function read a reference count other than with retain or
  /// release instructions, e.g. isUnique?
  bool ReadsRC = false;

  /// Returns the effects for an address or reference. This might be a
  /// parameter, the LocalEffects or, if the value cannot be associated to one
  /// of them, the GlobalEffects.
  FunctionSideEffectFlags *getEffectsOn(SILValue Addr);

public:
  /// Constructs "empty" function effects. This effects object can later be
  /// populated by summarizeFunction or summarizeCall.
  FunctionSideEffects() {}

  /// Sets the most conservative effects, if we don't know anything about the
  /// function.
  void setWorstEffects() {
    GlobalEffects.setWorstEffects();
    AllocsObjects = true;
    Traps = true;
    ReadsRC = true;
  }

  /// Clears all effects.
  void clear() {
    ParamEffects.clear();
    GlobalEffects.clear();
    AllocsObjects = false;
    Traps = false;
    ReadsRC = false;
  }

  /// Merge the flags from \p RHS.
  bool mergeFlags(const FunctionSideEffects &RHS) {
    bool Changed = false;
    Changed |= changedFlagByInPlaceOr(Traps, RHS.Traps);
    Changed |= changedFlagByInPlaceOr(AllocsObjects, RHS.AllocsObjects);
    Changed |= changedFlagByInPlaceOr(ReadsRC, RHS.ReadsRC);
    return Changed;
  }

  // Summarize the given function's effects using this FunctionSideEffects
  // object.
  //
  // Return true if the function's' effects have been fully summarized without
  // visiting it's body.
  bool summarizeFunction(SILFunction *F);

  /// Summarize the callee side effects of a call instruction using this
  /// FunctionSideEffects object without analyzing the callee function bodies or
  /// scheduling the callees for bottom-up propagation.
  ///
  /// The side effects are represented from the callee's perspective. Parameter
  /// effects are not translated into information on the caller's argument, and
  /// local effects are not dropped.
  ///
  /// Return true if this call-site's effects are summarized without visiting
  /// the callee.
  bool summarizeCall(FullApplySite fullApply);

  /// Merge effects directly from \p RHS.
  bool mergeFrom(const FunctionSideEffects &RHS);

  /// Merge the effects represented in CalleeEffects into this
  /// FunctionSideEffects object. CalleeEffects must correspond to at least one
  /// callee at the apply site `FAS`. Merging drops any local effects, and
  /// translates parameter effects into effects on the caller-side arguments.
  ///
  /// The full caller-side effects at a call site can be obtained with
  /// SideEffectsAnalysis::getCallSiteEffects().
  bool mergeFromApply(const FunctionSideEffects &CalleeEffects,
                      FullApplySite FAS);

  /// Analyze the side-effects of a single SIL instruction \p I.
  /// Visited callees are added to \p BottomUpOrder until \p RecursionDepth
  /// reaches MaxRecursionDepth.
  void analyzeInstruction(SILInstruction *I);

  /// Print the function effects.
  void dump() const;

  /// Does the function allocate objects, boxes, etc., i.e. everything which
  /// has a reference count.
  bool mayAllocObjects() const { return AllocsObjects; }

  /// Can this function trap or exit the program in any way?
  bool mayTrap() const { return Traps; }

  /// Does this function read a reference count other than with retain or
  /// release instructions, e.g. isUnique?
  bool mayReadRC() const { return ReadsRC; }

  /// Gets the memory behavior considering the global effects and
  /// all parameter effects. If \p ScanKind equals ignoreRetains then retain
  /// instructions are considered as side effects.
  MemoryBehavior getMemBehavior(RetainObserveKind ScanKind) const;

  /// Get the global effects for the function. These are effects which cannot
  /// be associated to a specific parameter, e.g. writes to global variables
  /// or writes to unknown pointers.
  const FunctionSideEffectFlags &getGlobalEffects() const {
    return GlobalEffects;
  }

  /// Get the array of parameter effects. If a side-effect can be associated
  /// to a specific parameter, it is contained here instead of the global
  /// effects.
  /// Note that if a parameter effect is mayRelease(), it means that the
  /// global function effects can be anything, because the destructor of an
  /// object can have arbitrary side effects.
  ArrayRef<FunctionSideEffectFlags> getParameterEffects() const {
    return ParamEffects;
  }

protected:
  /// Set the side-effects of a function, which has an @effects attribute.
  /// Returns true if \a F has an @effects attribute which could be handled.
  bool setDefinedEffects(SILFunction *F);

  /// Set the side-effects of a semantic call.
  /// Return true if \p ASC could be handled.
  bool setSemanticEffects(ArraySemanticsCall ASC);

  friend raw_ostream &operator<<(raw_ostream &os,
                                 const FunctionSideEffects &Effects) {
    os << "func=" << Effects.getGlobalEffects();
    int ParamIdx = 0;
    for (auto &E : Effects.getParameterEffects()) {
      os << ",param" << ParamIdx++ << "=" << E;
    }
    if (Effects.mayAllocObjects())
      os << ";alloc";
    if (Effects.mayTrap())
      os << ";trap";
    if (Effects.mayReadRC())
      os << ";readrc";
    return os;
  }
};

/// The SideEffectAnalysis provides information about side-effects of SIL
/// functions. Side-effect information is provided per function and includes:
/// Does the function read or write memory? Does the function retain or release
/// objects? etc.
/// For details see FunctionSideEffects.
class SideEffectAnalysis
    : public GenericFunctionEffectAnalysis<FunctionSideEffects> {
public:
  SideEffectAnalysis()
      : GenericFunctionEffectAnalysis<FunctionSideEffects>(
            AnalysisKind::SideEffect) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::SideEffect;
  }
};

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_ANALYSIS_SIDEEFFECTANALYSIS_H_
