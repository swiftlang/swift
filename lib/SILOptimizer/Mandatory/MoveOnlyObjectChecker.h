//===--- MoveOnlyObjectChecker.h ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This is for shared code in between the move only object checker and the move
/// only address checker. This is needed since the move only address checker
/// uses the move only object checker to check values loaded from allocations
/// that it is analyzing.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_MOVEONLYOBJECTCHECKER_H
#define SWIFT_SILOPTIMIZER_MANDATORY_MOVEONLYOBJECTCHECKER_H

#include "swift/SILOptimizer/Utils/CanonicalizeOSSALifetime.h"

namespace swift {
namespace siloptimizer {

/// Wrapper around CanonicalizeOSSALifetime that we use to specialize its
/// interface for our purposes.
struct OSSACanonicalizer {
  /// A per mark must check, vector of uses that copy propagation says need a
  /// copy and thus are not final consuming uses.
  SmallVector<Operand *, 32> consumingUsesNeedingCopy;

  /// A per mark must check, vector of consuming uses that copy propagation says
  /// are actual last uses.
  SmallVector<Operand *, 32> finalConsumingUses;

  /// The actual canonicalizer that we use.
  ///
  /// We mark this Optional to avoid UB behavior caused by us needing to
  /// initialize CanonicalizeOSSALifetime with parts of OSSACanoncializer
  /// (specifically with state in our arrays) before the actual constructor has
  /// run. Specifically this avoids:
  ///
  /// 11.9.5p1 class.cdtor: For an object with a non-trivial constructor,
  /// referring to any non-static member or base class of the object before the
  /// constructor begins execution results in undefined behavior.
  Optional<CanonicalizeOSSALifetime> canonicalizer;

  OSSACanonicalizer() {}

  void init(SILFunction *fn, NonLocalAccessBlockAnalysis *accessBlockAnalysis,
            DominanceInfo *domTree, InstructionDeleter &deleter) {
    auto foundConsumingUseNeedingCopy = std::function<void(Operand *)>(
        [&](Operand *use) { consumingUsesNeedingCopy.push_back(use); });
    auto foundConsumingUseNotNeedingCopy = std::function<void(Operand *)>(
        [&](Operand *use) { finalConsumingUses.push_back(use); });

    canonicalizer.emplace(
        false /*pruneDebugMode*/, !fn->shouldOptimize() /*maximizeLifetime*/,
        accessBlockAnalysis, domTree, deleter, foundConsumingUseNeedingCopy,
        foundConsumingUseNotNeedingCopy);
  }

  void clear() {
    consumingUsesNeedingCopy.clear();
    finalConsumingUses.clear();
  }

  bool canonicalize(SILValue value) {
    return canonicalizer->canonicalizeValueLifetime(value);
  }

  bool foundAnyConsumingUses() const {
    return consumingUsesNeedingCopy.size() || finalConsumingUses.size();
  }

  bool foundConsumingUseRequiringCopy() const {
    return consumingUsesNeedingCopy.size();
  }

  bool foundFinalConsumingUses() const { return finalConsumingUses.size(); }

  bool hasPartialApplyConsumingUse() const {
    return llvm::any_of(consumingUsesNeedingCopy,
                        [](Operand *use) {
                          return isa<PartialApplyInst>(use->getUser());
                        }) ||
           llvm::any_of(finalConsumingUses, [](Operand *use) {
             return isa<PartialApplyInst>(use->getUser());
           });
  }

  bool hasNonPartialApplyConsumingUse() const {
    return llvm::any_of(consumingUsesNeedingCopy,
                        [](Operand *use) {
                          return !isa<PartialApplyInst>(use->getUser());
                        }) ||
           llvm::any_of(finalConsumingUses, [](Operand *use) {
             return !isa<PartialApplyInst>(use->getUser());
           });
  }
};

} // namespace siloptimizer
} // namespace swift

#endif
