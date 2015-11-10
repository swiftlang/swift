//===-------------- AliasAnalysis.h - SIL Alias Analysis -*- C++ -*--------===//
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

#ifndef SWIFT_SILANALYSIS_ALIASANALYSIS_H
#define SWIFT_SILANALYSIS_ALIASANALYSIS_H

#include "swift/SIL/SILInstruction.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/SideEffectAnalysis.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class SILValue;
class SILInstruction;
class SideEffectAnalysis;

/// This class is a simple wrapper around an alias analysis cache. This is
/// needed since we do not have an "analysis" infrastructure.
class AliasAnalysis : public SILAnalysis {
public:
  /// The result of an alias query. This is based off of LLVM's alias
  /// analysis so see LLVM's documentation for more information.
  ///
  /// FIXME: PartialAlias?
  enum class AliasResult : unsigned {
    NoAlias=0,      ///< The two values have no dependencies on each
                    ///  other.
    MayAlias,       ///< The two values can not be proven to alias or
                    ///  not alias. Anything could happen.
    PartialAlias,   ///< The two values overlap in a partial manner.
    MustAlias,      ///< The two values are equal.
  };

private:
  using AliasCacheKey = std::pair<SILValue, SILValue>;
  llvm::DenseMap<AliasCacheKey, AliasResult> AliasCache;
  SILModule *Mod;
  SideEffectAnalysis *SEA;

  using MemoryBehavior = SILInstruction::MemoryBehavior;

  AliasResult cacheValue(AliasCacheKey Key, AliasResult Result);

  AliasResult aliasAddressProjection(SILValue V1, SILValue V2,
                                     SILValue O1, SILValue O2);

  /// Perform an alias query to see if V1, V2 refer to the same values.
  AliasResult aliasInner(SILValue V1, SILValue V2,
                         SILType TBAAType1 = SILType(),
                         SILType TBAAType2 = SILType());  
public:
  AliasAnalysis(SILModule *M) :
    SILAnalysis(AnalysisKind::Alias), Mod(M), SEA(nullptr) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::Alias;
  }
  
  virtual void initialize(SILPassManager *PM);
  
  SideEffectAnalysis *getSideEffectAnalysis() const { return SEA; }

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

  /// Convenience method that returns true if V1, V2 can not alias.
  bool isNoAlias(SILValue V1, SILValue V2, SILType TBAAType1 = SILType(),
                 SILType TBAAType2 = SILType()) {
    return alias(V1, V2, TBAAType1, TBAAType2) == AliasResult::NoAlias;
  }

  /// Convenience method that returns true if V1, V2 may alias.
  bool isMayAlias(SILValue V1, SILValue V2, SILType TBAAType1 = SILType(),
                  SILType TBAAType2 = SILType()) {
    return alias(V1, V2, TBAAType1, TBAAType2) == AliasResult::MayAlias;
  }

  /// Use the alias analysis to determine the memory behavior of Inst with
  /// respect to V.
  ///
  /// TODO: When ref count behavior is separated from generic memory behavior,
  /// the IgnoreRefCountIncrements flag will be unnecessary.
  MemoryBehavior getMemoryBehavior(SILInstruction *Inst, SILValue V,
                                   RetainObserveKind =
                                   RetainObserveKind::ObserveRetains);

  /// Returns true if Inst may read from memory in a manner that affects V.
  bool mayReadFromMemory(SILInstruction *Inst, SILValue V) {
    MemoryBehavior B = getMemoryBehavior(Inst, V,
                                         RetainObserveKind::IgnoreRetains);
    return B == MemoryBehavior::MayRead ||
      B == MemoryBehavior::MayReadWrite ||
      B == MemoryBehavior::MayHaveSideEffects;
  }

  /// Returns true if Inst may write to memory in a manner that affects V.
  bool mayWriteToMemory(SILInstruction *Inst, SILValue V) {
    MemoryBehavior B = getMemoryBehavior(Inst, V,
                                         RetainObserveKind::IgnoreRetains);
    return B == MemoryBehavior::MayWrite ||
      B == MemoryBehavior::MayReadWrite ||
      B == MemoryBehavior::MayHaveSideEffects;
  }

  /// Returns true if Inst may read or write to memory in a manner that affects
  /// V.
  bool mayReadOrWriteMemory(SILInstruction *Inst, SILValue V) {
    auto B = getMemoryBehavior(Inst, V, RetainObserveKind::IgnoreRetains);
    return B != MemoryBehavior::None;
  }

  /// Returns true if Inst may have side effects in a manner that affects V.
  bool mayHaveSideEffects(SILInstruction *Inst, SILValue V) {
    MemoryBehavior B = getMemoryBehavior(Inst, V);
    return B == MemoryBehavior::MayWrite ||
      B == MemoryBehavior::MayReadWrite ||
      B == MemoryBehavior::MayHaveSideEffects;
  }

  /// Returns true if Inst may have side effects in a manner that affects
  /// V. This is independent of whether or not Inst may write to V and is meant
  /// to encode notions such as ref count modifications.
  bool mayHavePureSideEffects(SILInstruction *Inst, SILValue V) {
    return getMemoryBehavior(Inst, V) == MemoryBehavior::MayHaveSideEffects;
  }

  virtual void invalidate(SILAnalysis::InvalidationKind K) { AliasCache.clear(); }

  virtual void invalidate(SILFunction *, SILAnalysis::InvalidationKind K) {
    invalidate(K);
  }
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                              AliasAnalysis::AliasResult R);

/// If this value is an address that obeys strict TBAA, return the address type.
/// Otherwise, return an empty type.
SILType computeTBAAType(SILValue V);

/// Check if V points to a let-variable.
bool isLetPointer(SILValue V);

} // end namespace swift

#endif
