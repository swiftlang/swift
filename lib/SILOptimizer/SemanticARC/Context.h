//===--- Context.h --------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_SEMANTICARC_CONTEXT_H
#define SWIFT_SILOPTIMIZER_SEMANTICARC_CONTEXT_H

#include "OwnershipLiveRange.h"

#include "swift/Basic/BlotSetVector.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/MultiMapCache.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "llvm/Support/Compiler.h"

namespace swift {
namespace semanticarc {

struct LLVM_LIBRARY_VISIBILITY Context {
  SILFunction &fn;
  Optional<DeadEndBlocks> deadEndBlocks;
  ValueLifetimeAnalysis::Frontier lifetimeFrontier;
  SmallMultiMapCache<SILValue, Operand *> addressToExhaustiveWriteListCache;

  /// Are we assuming that we reached a fix point and are re-processing to
  /// prepare to use the phiToIncomingValueMultiMap.
  bool assumingAtFixedPoint = false;

  /// A map from a value that acts as a "joined owned introducer" in the def-use
  /// graph.
  ///
  /// A "joined owned introducer" is a value with owned ownership whose
  /// ownership is derived from multiple non-trivial owned operands of a related
  /// instruction. Some examples are phi arguments, tuples, structs. Naturally,
  /// all of these instructions must be non-unary instructions and only have
  /// this property if they have multiple operands that are non-trivial.
  ///
  /// In such a case, we can not just treat them like normal forwarding concepts
  /// since we can only eliminate optimize such a value if we are able to reason
  /// about all of its operands together jointly. This is not amenable to a
  /// small peephole analysis.
  ///
  /// Instead, as we perform the peephole analysis, using the multimap, we map
  /// each joined owned value introducer to the set of its @owned operands that
  /// we thought we could convert to guaranteed only if we could do the same to
  /// the joined owned value introducer. Then once we finish performing
  /// peepholes, we iterate through the map and see if any of our joined phi
  /// ranges had all of their operand's marked with this property by iterating
  /// over the multimap. Since we are dealing with owned values and we know that
  /// our LiveRange can not see through joined live ranges, we know that we
  /// should only be able to have a single owned value introducer for each
  /// consumed operand.
  FrozenMultiMap<SILValue, Operand *> joinedOwnedIntroducerToConsumedOperands;

  /// If set to true, then we should only run cheap optimizations that do not
  /// build up data structures or analyze code in depth.
  ///
  /// As an example, we do not do load [copy] optimizations here since they
  /// generally involve more complex analysis, but simple peepholes of
  /// copy_values we /do/ allow.
  bool onlyGuaranteedOpts;

  /// Callbacks that we must use to remove or RAUW values.
  InstModCallbacks instModCallbacks;

  using FrozenMultiMapRange =
      decltype(joinedOwnedIntroducerToConsumedOperands)::PairToSecondEltRange;

  DeadEndBlocks &getDeadEndBlocks() {
    if (!deadEndBlocks)
      deadEndBlocks.emplace(&fn);
    return *deadEndBlocks;
  }

  Context(SILFunction &fn, bool onlyGuaranteedOpts, InstModCallbacks callbacks)
      : fn(fn), deadEndBlocks(), lifetimeFrontier(),
        addressToExhaustiveWriteListCache(constructCacheValue),
        onlyGuaranteedOpts(onlyGuaranteedOpts), instModCallbacks(callbacks) {}

  void verify() const;

private:
  static bool
  constructCacheValue(SILValue initialValue,
                      SmallVectorImpl<Operand *> &wellBehavedWriteAccumulator);
};

} // namespace semanticarc
} // namespace swift

#endif
