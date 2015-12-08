//===------ SideEffectAnalysis.h - SIL Side Effect Analysis -*- C++ -*-----===//
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

#ifndef SWIFT_SILANALYSIS_SIDEEFFECTANALYSIS_H_
#define SWIFT_SILANALYSIS_SIDEEFFECTANALYSIS_H_

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILAnalysis/Analysis.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class BasicCalleeAnalysis;
class CallGraphAnalysis;
class CallGraph;


/// An enum to represent the kind of scan we perform when we calculate
/// side effects.
enum class RetainObserveKind {ObserveRetains, IgnoreRetains, RetainObserveKindEnd};

/// The SideEffectAnalysis provides information about side-effects of SIL
/// functions. Side-effect information is provided per function and includes:
/// Does the function read or write memory? Does the function retain or release
/// objects? etc.
/// For details see SideEffectAnalysis::FunctionEffects and
/// SideEffectAnalysis::Effects.
///
/// The update and invalidation policy of this analysis is a little bit
/// different than for other analysis. When an optimization pass changes the
/// SIL, there should not be more side-effects on a function than before the
/// transformation. Therefore optimization passes do not _invalidate_ the
/// side-effect information, they may only make it more conservative.
/// For this reason, the invalidate function are no-ops. Instead the
/// UpdateSideEffects pass does recompute the analysis no certain points in the
/// optimization pipeline. This avoids updating the analysis too often.
class SideEffectAnalysis : public SILAnalysis {
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
    
    /// Does this function read a refernce count other than with retain or
    /// release instructions, e.g. isUnique?
    bool ReadsRC = false;
    
    /// Returns the effecs for an address or reference. This might be a
    /// paramter, the LocalEffects or, if the value can not be associated to one
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

    /// Does this function read a refernce count other than with retain or
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

    /// Merge effects from an apply site within the function.
    bool mergeFromApply(const FunctionEffects &CalleeEffects,
                        FullApplySite FAS);
    
    /// Print the function effects.
    void dump();
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
  /// The module being processed.
  SILModule &M;

  /// All the side-effect information for the whole module.
  llvm::DenseMap<SILFunction *, FunctionEffects *> Function2Effects;
  
  /// The allocator for the map values in Function2Effects.
  llvm::SpecificBumpPtrAllocator<FunctionEffects> Allocator;
  
  /// Callee analysis, used for determining the callees at call sites.
  BasicCalleeAnalysis *BCA;

  /// This analysis depends on the call graph.
  CallGraphAnalysis *CGA;
  
  /// If false, nothing has changed between two recompute() calls.
  bool shouldRecompute;
  
  typedef llvm::SetVector<SILFunction *> WorkListType;
  
  /// Get the side-effects of a function, which has an @effects attribute.
  /// Returns true if \a F has an @effects attribute which could be handled.
  static bool getDefinedEffects(FunctionEffects &Effects, SILFunction *F);
  
  /// Get the side-effects of a semantic call.
  /// Return true if \a FAS is a semantic call which could be handled.
  bool getSemanticEffects(FunctionEffects &Effects, FullApplySite FAS);
  
  /// Analyise the side-effects of a function.
  /// If the side-effects changed, the callers are pushed onto the \a WorkList.
  void analyzeFunction(SILFunction *F, WorkListType &WorkList, CallGraph &CG);
  
  /// Analyise the side-effects of a single SIL instruction.
  void analyzeInstruction(FunctionEffects &Effects, SILInstruction *I);

  /// Get the side-effects of a call site.
  void getEffectsOfApply(FunctionEffects &FE, FullApplySite FAS,
                         bool isRecomputing);
  
  /// Gets or creates FunctionEffects for \p F. If \a isRecomputing is true,
  /// the effects are initialized with empty effects, otherwise with most
  /// conservative effects.
  FunctionEffects *getFunctionEffects(SILFunction *F, bool isRecomputing) {
    auto *FE = Function2Effects[F];
    if (!FE) {
      unsigned argSize = F->empty() ? 0 : F->getArguments().size();
      FE = new (Allocator.Allocate()) FunctionEffects(argSize);
      Function2Effects[F] = FE;
      if (!isRecomputing)
        FE->setWorstEffects();
    }
    return FE;
  }
  
public:
  SideEffectAnalysis(SILModule *M)
      : SILAnalysis(AnalysisKind::SideEffect), M(*M), CGA(nullptr),
        shouldRecompute(true) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::SideEffect;
  }
  
  virtual void initialize(SILPassManager *PM);
  
  /// Recomputes the side-effect information for all functions the module.
  void recompute();

  /// Get the side-effects of a function.
  const FunctionEffects &getEffects(SILFunction *F) {
    auto *FE = getFunctionEffects(F, false);
    return *FE;
  }

  /// Get the side-effects of a call site.
  void getEffects(FunctionEffects &FE, FullApplySite FAS);
  
  /// No invalidation is needed. See comment for SideEffectAnalysis.
  virtual void invalidate(InvalidationKind K) {
    shouldRecompute = true;
  }
  
  /// No invalidation is needed. See comment for SideEffectAnalysis.
  virtual void invalidate(SILFunction *F, InvalidationKind K) {
    invalidate(K);
  }
};

} // end namespace swift

#endif

