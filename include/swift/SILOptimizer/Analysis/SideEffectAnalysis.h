//===--- SideEffectAnalysis.h - SIL Side Effect Analysis --------*- C++ -*-===//
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

/// The SideEffectAnalysis provides information about side-effects of SIL
/// functions. Side-effect information is provided per function and includes:
/// Does the function read or write memory? Does the function retain or release
/// objects? etc.
/// For details see SideEffectAnalysis::FunctionEffects and
/// SideEffectAnalysis::Effects.
class SideEffectAnalysis : public BottomUpIPAnalysis {
public:

  using MemoryBehavior = SILInstruction::MemoryBehavior;
  /// Set \p dest if \p src is set and return true if \p dest was not set
  /// before.
  static bool updateFlag(bool &dest, bool src) {
    if (src && !dest) {
      dest = src;
      return true;
    }
    return false;
  }

  /// Side-effect information for the function (global effects) or a specific
  /// parameter of the function. See FunctionEffects.
  class Effects {
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

    friend class SideEffectAnalysis;
    
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
        return mayRead() ? MemoryBehavior::MayReadWrite :
                           MemoryBehavior::MayWrite;
      
      if (mayRead())
        return MemoryBehavior::MayRead;
      
      return MemoryBehavior::None;
    }

    /// Merge effects from \p RHS.
    bool mergeFrom(const Effects &RHS) {
      bool Changed = false;
      Changed |= updateFlag(Reads, RHS.Reads);
      Changed |= updateFlag(Writes, RHS.Writes);
      Changed |= updateFlag(Retains, RHS.Retains);
      Changed |= updateFlag(Releases, RHS.Releases);
      return Changed;
    }
  };

  friend raw_ostream &operator<<(raw_ostream &os,
                                 const SideEffectAnalysis::Effects &Effects) {
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
  class FunctionEffects {
    
    /// Side-effects which can be associated to a parameter.
    llvm::SmallVector<Effects, 6> ParamEffects;
    
    /// All other side-effects which cannot be associated to a parameter.
    Effects GlobalEffects;
    
    /// Side-effects on locally allocated storage. Such side-effects are not
    /// relevant to optimizations. The LocalEffects are only used to return
    /// "something" for local storage in getEffectsOn().
    Effects LocalEffects;
    
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
    Effects *getEffectsOn(SILValue Addr);
    
    FunctionEffects(unsigned numParams) : ParamEffects(numParams) { }

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
      GlobalEffects.clear();
      for (Effects &PE : ParamEffects)
        PE.clear();
      AllocsObjects = false;
      Traps = false;
      ReadsRC = false;
    }
  
    /// Merge the flags from \p RHS.
    bool mergeFlags(const FunctionEffects &RHS) {
      bool Changed = false;
      Changed |= updateFlag(Traps, RHS.Traps);
      Changed |= updateFlag(AllocsObjects, RHS.AllocsObjects);
      Changed |= updateFlag(ReadsRC, RHS.ReadsRC);
      return Changed;
    }
    
    friend class SideEffectAnalysis;
    
  public:
    
    /// Constructs "empty" function effects.
    FunctionEffects() { }

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
    const Effects &getGlobalEffects() const { return GlobalEffects; }
    
    /// Get the array of parameter effects. If a side-effect can be associated
    /// to a specific parameter, it is contained here instead of the global
    /// effects.
    ArrayRef<Effects> getParameterEffects() const { return ParamEffects; }
    
    /// Merge effects from \p RHS.
    bool mergeFrom(const FunctionEffects &RHS);

    /// Merge effects from a function apply site within the function.
    bool mergeFromApply(const FunctionEffects &CalleeEffects,
                        SILInstruction *FAS);

    /// Merge effects from an apply site within the function.
    bool mergeFromApply(const FunctionEffects &CalleeEffects,
                        FullApplySite FAS);
    
    /// Print the function effects.
    void dump() const;
  };

  friend raw_ostream &operator<<(raw_ostream &os,
                          const SideEffectAnalysis::FunctionEffects &Effects) {
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

private:

  /// Stores the analysis data, i.e. the side-effects, for a function.
  struct FunctionInfo : public FunctionInfoBase<FunctionInfo> {

    /// The side-effects of the function.
    FunctionEffects FE;

    /// Back-link to the function.
    SILFunction *F;

    /// Used during recomputation to indicate if the side-effects of a caller
    /// must be updated.
    bool NeedUpdateCallers = false;

    FunctionInfo(SILFunction *F) :
      FE(F->empty() ? 0 : F->getArguments().size()), F(F) { }

    /// Clears the analysis data on invalidation.
    void clear() { FE.clear(); }
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

  /// All the side-effect information for the whole module.
  llvm::DenseMap<SILFunction *, FunctionInfo *> Function2Info;
  
  /// The allocator for the map values in Function2Info.
  llvm::SpecificBumpPtrAllocator<FunctionInfo> Allocator;
  
  /// Callee analysis, used for determining the callees at call sites.
  BasicCalleeAnalysis *BCA;

  /// Get the side-effects of a function, which has an @effects attribute.
  /// Returns true if \a F has an @effects attribute which could be handled.
  static bool getDefinedEffects(FunctionEffects &Effects, SILFunction *F);
  
  /// Get the side-effects of a semantic call.
  /// Return true if \p ASC could be handled.
  bool getSemanticEffects(FunctionEffects &Effects, ArraySemanticsCall ASC);
  
  /// Analyze the side-effects of a function, including called functions.
  /// Visited callees are added to \p BottomUpOrder until \p RecursionDepth
  /// reaches MaxRecursionDepth.
  void analyzeFunction(FunctionInfo *FInfo,
                       FunctionOrder &BottomUpOrder,
                       int RecursionDepth);

  /// Analyze the side-effects of a single SIL instruction \p I.
  /// Visited callees are added to \p BottomUpOrder until \p RecursionDepth
  /// reaches MaxRecursionDepth.
  void analyzeInstruction(FunctionInfo *FInfo,
                          SILInstruction *I,
                          FunctionOrder &BottomUpOrder,
                          int RecursionDepth);

  /// Gets or creates FunctionEffects for \p F.
  FunctionInfo *getFunctionInfo(SILFunction *F) {
    FunctionInfo *&FInfo = Function2Info[F];
    if (!FInfo) {
      FInfo = new (Allocator.Allocate()) FunctionInfo(F);
    }
    return FInfo;
  }

  /// Recomputes the side-effect information for the function \p Initial and
  /// all called functions, up to a recursion depth of MaxRecursionDepth.
  void recompute(FunctionInfo *Initial);

public:
  SideEffectAnalysis()
      : BottomUpIPAnalysis(AnalysisKind::SideEffect) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::SideEffect;
  }
  
  virtual void initialize(SILPassManager *PM) override;
  
  /// Get the side-effects of a function.
  const FunctionEffects &getEffects(SILFunction *F) {
    FunctionInfo *FInfo = getFunctionInfo(F);
    if (!FInfo->isValid())
      recompute(FInfo);
    return FInfo->FE;
  }

  /// Get the side-effects of a call site.
  void getEffects(FunctionEffects &ApplyEffects, FullApplySite FAS);
  
  /// No invalidation is needed. See comment for SideEffectAnalysis.
  virtual void invalidate(InvalidationKind K) override;
  
  /// No invalidation is needed. See comment for SideEffectAnalysis.
  virtual void invalidate(SILFunction *F, InvalidationKind K)  override;
};

} // end namespace swift

#endif

