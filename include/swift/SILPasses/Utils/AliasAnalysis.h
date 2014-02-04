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

#ifndef SWIFT_SILPASSES_UTILS_ALIASANALYSIS_H
#define SWIFT_SILPASSES_UTILS_ALIASANALYSIS_H

#include "llvm/ADT/DenseMap.h"

namespace swift {

class SILValue;
class SILInstruction;

/// This class is a simple wrapper around an alias analysis cache. This is
/// needed since we do not have an "analysis" infrastructure.
class AliasAnalysis {
public:
  /// The result of an alias query. This is based off of LLVM's alias
  /// analysis so see LLVM's documentation for more information.
  ///
  /// FIXME: PartialAlias?
  enum class Result : unsigned {
    NoAlias=0,      ///< The two values have no dependencies on each
                    ///  other.
    MayAlias,       ///< The two values can not be proven to alias or
                    ///  not alias. Anything could happen.
    MustAlias,      ///< The two values are equal.
  };

private:
  using CacheKey = std::pair<SILValue, SILValue>;

  llvm::DenseMap<CacheKey, Result> Cache;

public:

  /// Perform an alias query to see if V1, V2 refer to the same values.
  Result alias(SILValue V1, SILValue V2);

  /// Perform an alias query on Inst to see if any of its operands
  /// alias V.
  Result alias(SILInstruction *Inst, SILValue V);

  /// Clear the cache.
  ///
  /// FIXME: Add in an API to call to invalidate all of the cache
  /// entries for one SILValue. It is not needed right now, but it
  /// could be useful in the future.
  void invalidateCache() { Cache.clear(); }
};

} // end namespace swift

#endif
