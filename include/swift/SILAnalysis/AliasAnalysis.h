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
#include "llvm/ADT/DenseMap.h"

namespace swift {

class SILValue;
class SILInstruction;

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
    MustAlias,      ///< The two values are equal.
  };

private:
  using AliasCacheKey = std::pair<SILValue, SILValue>;
  llvm::DenseMap<AliasCacheKey, AliasResult> AliasCache;

  using MemoryBehavior = SILInstruction::MemoryBehavior;

public:
  AliasAnalysis(SILModule *M) : SILAnalysis(AnalysisKind::Alias) {}

  virtual ~AliasAnalysis() {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::Alias;
  }

  /// Perform an alias query to see if V1, V2 refer to the same values.
  AliasResult alias(SILValue V1, SILValue V2);

  /// Use the alias analysis to determine the memory behavior of Inst with
  /// respect to V.
  MemoryBehavior getMemoryBehavior(SILInstruction *Inst, SILValue V);

  virtual void invalidate(InvalidationKind K) { AliasCache.clear(); }

  virtual void invalidate(SILFunction *, InvalidationKind K) { invalidate(K); }
};

} // end namespace swift

#endif
