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
    MayAlias,       ///< The two values can not be proven to alias or
                    ///  not alias. Anything could happen.
    PartialAlias,   ///< The two values overlap in a partial manner.
    MustAlias,      ///< The two values are equal.
  };

private:
  SILModule *Mod;
  SideEffectAnalysis *SEA;

  using TBAACacheKey = std::pair<SILType, SILType>;

  /// A cache for the computation of TBAA. True means that the types may
  /// alias. False means that the types must not alias.
  ///
  /// We don't need to invalidate this cache because type aliasing relations
  /// never change.
  llvm::DenseMap<TBAACacheKey, bool> TypesMayAliasCache;

  using MemoryBehavior = SILInstruction::MemoryBehavior;

  AliasResult aliasAddressProjection(SILValue V1, SILValue V2,
                                     SILValue O1, SILValue O2);

  /// Perform an alias query to see if V1, V2 refer to the same values.
  AliasResult aliasInner(SILValue V1, SILValue V2,
                         SILType TBAAType1 = SILType(),
                         SILType TBAAType2 = SILType());  

  /// Returns True if memory of type \p T1 and \p T2 may alias.
  bool typesMayAlias(SILType T1, SILType T2);


  virtual void handleDeleteNotification(SILInstruction *I) override {
  }

public:
  AliasAnalysis(SILModule *M) :
    SILAnalysis(AnalysisKind::Alias), Mod(M), SEA(nullptr) {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::Alias;
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
  /// the InspectionMode flag will be unnecessary.
  MemoryBehavior computeMemoryBehavior(SILInstruction *Inst, SILValue V,
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

  virtual void invalidate(SILAnalysis::InvalidationKind K) override { }

  virtual void invalidate(SILFunction *,
                          SILAnalysis::InvalidationKind K) override {
    invalidate(K);
  }
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

#endif
