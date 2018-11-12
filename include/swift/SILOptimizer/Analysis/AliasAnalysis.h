//===--- AliasAnalysis.h - SIL Alias Analysis -------------------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_ALIASANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_ALIASANALYSIS_H

#include "swift/Basic/ValueEnumerator.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/SideEffectAnalysis.h"
#include "llvm/ADT/DenseMap.h"

using swift::RetainObserveKind;

namespace {

  /// A key used for the AliasAnalysis cache.
  ///
  /// This struct represents the argument list to the method 'alias'.  The two
  /// SILValue pointers are mapped to size_t indices because we need an
  /// efficient way to invalidate them (the mechanism is described below). The
  /// Type arguments are translated to void* because their underlying storage is
  /// opaque pointers that never goes away.
  struct AliasKeyTy {
    // The SILValue pair:
    size_t V1, V2;
    // The TBAAType pair:
    void *T1, *T2;
  };

  /// A key used for the MemoryBehavior Analysis cache.
  ///
  /// The two SILValue pointers are mapped to size_t indices because we need an
  /// efficient way to invalidate them (the mechanism is described below).  The
  /// RetainObserveKind represents the inspection mode for the memory behavior
  /// analysis.
  struct MemBehaviorKeyTy {
    // The SILValue pair:
    size_t V1, V2;
    RetainObserveKind InspectionMode; 
  };
}

namespace swift {

class SILInstruction;
class ValueBase;
class SideEffectAnalysis;
class EscapeAnalysis;

/// This class is a simple wrapper around an alias analysis cache. This is
/// needed since we do not have an "analysis" infrastructure.
class AliasAnalysis : public SILAnalysis {
public:

  /// This enum describes the different kinds of aliasing relations between
  /// pointers.
  ///
  /// NoAlias: There is never dependence between memory referenced by the two
  ///          pointers. Example: Two pointers pointing to non-overlapping
  ///          memory ranges.
  ///
  /// MayAlias: Two pointers might refer to the same memory location.
  ///
  ///
  /// PartialAlias: The two memory locations are known to be overlapping
  ///               but do not start at the same address.
  ///
  ///
  /// MustAlias: The two memory locations always start at exactly the same
  ///            location. The pointers are equal.
  ///
  enum class AliasResult : unsigned {
    NoAlias=0,      ///< The two values have no dependencies on each
                    ///  other.
    MayAlias,       ///< The two values cannot be proven to alias or
                    ///  not alias. Anything could happen.
    PartialAlias,   ///< The two values overlap in a partial manner.
    MustAlias,      ///< The two values are equal.
  };

private:
  SILModule *Mod;
  SideEffectAnalysis *SEA;
  EscapeAnalysis *EA;

  using TBAACacheKey = std::pair<SILType, SILType>;

  /// A cache for the computation of TBAA. True means that the types may
  /// alias. False means that the types must not alias.
  ///
  /// We don't need to invalidate this cache because type aliasing relations
  /// never change.
  llvm::DenseMap<TBAACacheKey, bool> TypesMayAliasCache;

  /// AliasAnalysis value cache.
  ///
  /// The alias() method uses this map to cache queries.
  llvm::DenseMap<AliasKeyTy, AliasResult> AliasCache;

  using MemoryBehavior = SILInstruction::MemoryBehavior;
  /// MemoryBehavior value cache.
  ///
  /// The computeMemoryBehavior() method uses this map to cache queries.
  llvm::DenseMap<MemBehaviorKeyTy, MemoryBehavior> MemoryBehaviorCache;

  /// The AliasAnalysis cache can't directly map a pair of ValueBase pointers
  /// to alias results because we'd like to be able to remove deleted pointers
  /// without having to scan the whole map. So, instead of storing pointers we
  /// map pointers to indices and store the indices.
  ValueEnumerator<ValueBase*> AliasValueBaseToIndex;
  
  /// Same as AliasValueBaseToIndex, map a pointer to the indices for
  /// MemoryBehaviorCache.
  ///
  /// NOTE: we do not use the same ValueEnumerator for the alias cache, 
  /// as when either cache is cleared, we can not clear the ValueEnumerator
  /// because doing so could give rise to collisions in the other cache.
  ValueEnumerator<SILNode*> MemoryBehaviorNodeToIndex;

  AliasResult aliasAddressProjection(SILValue V1, SILValue V2,
                                     SILValue O1, SILValue O2);

  /// Perform an alias query to see if V1, V2 refer to the same values.
  AliasResult aliasInner(SILValue V1, SILValue V2,
                         SILType TBAAType1 = SILType(),
                         SILType TBAAType2 = SILType());  

  /// Returns True if memory of type \p T1 and \p T2 may alias.
  bool typesMayAlias(SILType T1, SILType T2);

  virtual void handleDeleteNotification(SILNode *node) override {
    assert(node->isRepresentativeSILNodeInObject());

    // The pointer 'node' is going away.  We can't scan the whole cache
    // and remove all of the occurrences of the pointer. Instead we remove
    // the pointer from the cache that translates pointers to indices.
    auto value = dyn_cast<ValueBase>(node);
    if (!value) return;

    AliasValueBaseToIndex.invalidateValue(value);
    MemoryBehaviorNodeToIndex.invalidateValue(node);
  }

  virtual bool needsNotifications() override { return true; }


public:
  AliasAnalysis(SILModule *M)
      : SILAnalysis(SILAnalysisKind::Alias), Mod(M), SEA(nullptr), EA(nullptr) {
  }

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::Alias;
  }
  
  virtual void initialize(SILPassManager *PM) override;
  
  /// Perform an alias query to see if V1, V2 refer to the same values.
  AliasResult alias(SILValue V1, SILValue V2, SILType TBAAType1 = SILType(),
                    SILType TBAAType2 = SILType());

  /// Convenience method that returns true if V1 and V2 must alias.
  bool isMustAlias(SILValue V1, SILValue V2, SILType TBAAType1 = SILType(),
                   SILType TBAAType2 = SILType()) {
    return alias(V1, V2, TBAAType1, TBAAType2) == AliasResult::MustAlias;
  }

  /// Convenience method that returns true if V1 and V2 partially alias.
  bool isPartialAlias(SILValue V1, SILValue V2, SILType TBAAType1 = SILType(),
                      SILType TBAAType2 = SILType()) {
    return alias(V1, V2, TBAAType1, TBAAType2) == AliasResult::PartialAlias;
  }

  /// Convenience method that returns true if V1, V2 cannot alias.
  bool isNoAlias(SILValue V1, SILValue V2, SILType TBAAType1 = SILType(),
                 SILType TBAAType2 = SILType()) {
    return alias(V1, V2, TBAAType1, TBAAType2) == AliasResult::NoAlias;
  }

  /// Convenience method that returns true if V1, V2 may alias.
  bool isMayAlias(SILValue V1, SILValue V2, SILType TBAAType1 = SILType(),
                  SILType TBAAType2 = SILType()) {
    return alias(V1, V2, TBAAType1, TBAAType2) == AliasResult::MayAlias;
  }

  /// \returns True if the release of the \p Ptr can access memory accessed by
  /// \p User.
  bool mayValueReleaseInterfereWithInstruction(SILInstruction *User,
                                               SILValue Ptr);

  /// Use the alias analysis to determine the memory behavior of Inst with
  /// respect to V.
  ///
  /// TODO: When ref count behavior is separated from generic memory behavior,
  /// the InspectionMode flag will be unnecessary.
  MemoryBehavior computeMemoryBehavior(SILInstruction *Inst, SILValue V,
                                       RetainObserveKind);

  /// Use the alias analysis to determine the memory behavior of Inst with
  /// respect to V.
  ///
  /// TODO: When ref count behavior is separated from generic memory behavior,
  /// the InspectionMode flag will be unnecessary.
  MemoryBehavior computeMemoryBehaviorInner(SILInstruction *Inst, SILValue V,
                                            RetainObserveKind);

  /// Returns true if \p Inst may read from memory in a manner that
  /// affects V.
  bool mayReadFromMemory(SILInstruction *Inst, SILValue V) {
    auto B = computeMemoryBehavior(Inst, V, RetainObserveKind::IgnoreRetains);
    return B == MemoryBehavior::MayRead ||
           B == MemoryBehavior::MayReadWrite ||
           B == MemoryBehavior::MayHaveSideEffects;
  }

  /// Returns true if \p Inst may write to memory in a manner that
  /// affects V.
  bool mayWriteToMemory(SILInstruction *Inst, SILValue V) {
    auto B = computeMemoryBehavior(Inst, V, RetainObserveKind::IgnoreRetains);
    return B == MemoryBehavior::MayWrite ||
           B == MemoryBehavior::MayReadWrite ||
           B == MemoryBehavior::MayHaveSideEffects;
  }

  /// Returns true if \p Inst may read or write to memory in a manner that
  /// affects V.
  bool mayReadOrWriteMemory(SILInstruction *Inst, SILValue V) {
    auto B = computeMemoryBehavior(Inst, V, RetainObserveKind::IgnoreRetains);
    return MemoryBehavior::None != B;
  }

  /// Returns true if Inst may have side effects in a manner that affects V.
  bool mayHaveSideEffects(SILInstruction *Inst, SILValue V) {
    auto B = computeMemoryBehavior(Inst, V, RetainObserveKind::ObserveRetains);
    return B == MemoryBehavior::MayWrite ||
           B == MemoryBehavior::MayReadWrite ||
           B == MemoryBehavior::MayHaveSideEffects;
  }

  /// Returns true if Inst may have side effects in a manner that affects
  /// V. This is independent of whether or not Inst may write to V and is meant
  /// to encode notions such as ref count modifications.
  bool mayHavePureSideEffects(SILInstruction *Inst, SILValue V) {
    auto B = computeMemoryBehavior(Inst, V, RetainObserveKind::ObserveRetains);
    return MemoryBehavior::MayHaveSideEffects == B;
  }

  /// Returns true if \p Ptr may be released in the function call \p FAS.
  bool canApplyDecrementRefCount(FullApplySite FAS, SILValue Ptr);

  /// Returns true if \p Ptr may be released by the builtin \p BI.
  bool canBuiltinDecrementRefCount(BuiltinInst *BI, SILValue Ptr);

  /// Encodes the alias query as a AliasKeyTy.
  /// The parameters to this function are identical to the parameters of alias()
  /// and this method serializes them into a key for the alias analysis cache.
  AliasKeyTy toAliasKey(SILValue V1, SILValue V2, SILType Type1, SILType Type2);

  /// Encodes the memory behavior query as a MemBehaviorKeyTy.
  MemBehaviorKeyTy toMemoryBehaviorKey(SILInstruction *V1, SILValue V2,
                                       RetainObserveKind K);

  virtual void invalidate() override {
    AliasCache.clear();
    MemoryBehaviorCache.clear();
  }

  virtual void invalidate(SILFunction *,
                          SILAnalysis::InvalidationKind K) override {
    invalidate();
  }

  /// Notify the analysis about a newly created function.
  virtual void notifyAddedOrModifiedFunction(SILFunction *F) override {}

  /// Notify the analysis about a function which will be deleted from the
  /// module.
  virtual void notifyWillDeleteFunction(SILFunction *F) override {}

  virtual void invalidateFunctionTables() override { }
};


llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                              AliasAnalysis::AliasResult R);

/// If this value is an address that obeys strict TBAA, return the address type.
/// Otherwise, return an empty type.
SILType computeTBAAType(SILValue V);

/// Check if \p V points to a let-member.
/// Nobody can write into let members.
bool isLetPointer(SILValue V);

} // end namespace swift

namespace llvm {
  template <> struct DenseMapInfo<AliasKeyTy> {
    static inline AliasKeyTy getEmptyKey() {
      auto Allone = std::numeric_limits<size_t>::max();
      return {0, Allone, nullptr, nullptr};
    }
    static inline AliasKeyTy getTombstoneKey() {
      auto Allone = std::numeric_limits<size_t>::max();
      return {Allone, 0, nullptr, nullptr};
    }
    static unsigned getHashValue(const AliasKeyTy Val) {
      unsigned H = 0;
      H ^= DenseMapInfo<size_t>::getHashValue(Val.V1);
      H ^= DenseMapInfo<size_t>::getHashValue(Val.V2);
      H ^= DenseMapInfo<void *>::getHashValue(Val.T1);
      H ^= DenseMapInfo<void *>::getHashValue(Val.T2);
      return H;
    }
    static bool isEqual(const AliasKeyTy LHS, const AliasKeyTy RHS) {
      return LHS.V1 == RHS.V1 &&
             LHS.V2 == RHS.V2 &&
             LHS.T1 == RHS.T1 &&
             LHS.T2 == RHS.T2;
    }
  };

  template <> struct DenseMapInfo<MemBehaviorKeyTy> {
    static inline MemBehaviorKeyTy getEmptyKey() {
      auto Allone = std::numeric_limits<size_t>::max();
      return {0, Allone, RetainObserveKind::RetainObserveKindEnd};
    }
    static inline MemBehaviorKeyTy getTombstoneKey() {
      auto Allone = std::numeric_limits<size_t>::max();
      return {Allone, 0, RetainObserveKind::RetainObserveKindEnd};
    }
    static unsigned getHashValue(const MemBehaviorKeyTy V) {
      unsigned H = 0;
      H ^= DenseMapInfo<size_t>::getHashValue(V.V1);
      H ^= DenseMapInfo<size_t>::getHashValue(V.V2);
      H ^= DenseMapInfo<int>::getHashValue(static_cast<int>(V.InspectionMode));
      return H;
    }
    static bool isEqual(const MemBehaviorKeyTy LHS,
                        const MemBehaviorKeyTy RHS) {
      return LHS.V1 == RHS.V1 &&
             LHS.V2 == RHS.V2 &&
             LHS.InspectionMode == RHS.InspectionMode; 
    }
  };
}

#endif
