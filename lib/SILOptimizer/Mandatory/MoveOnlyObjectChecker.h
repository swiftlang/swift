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
#include "llvm/Support/Compiler.h"

namespace swift {
namespace siloptimizer {

class DiagnosticEmitter;

/// Wrapper around CanonicalizeOSSALifetime that we use to specialize its
/// interface for our purposes.
struct OSSACanonicalizer {
  /// A per mark must check, vector of uses that copy propagation says need a
  /// copy and thus are not final consuming uses.
  SmallVector<SILInstruction *, 32> consumingUsesNeedingCopy;

  /// A per mark must check, vector of consuming uses that copy propagation says
  /// are actual last uses.
  SmallVector<SILInstruction *, 32> consumingBoundaryUsers;

  /// A list of non-consuming boundary uses.
  SmallVector<SILInstruction *, 32> nonConsumingBoundaryUsers;

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

  void init(SILFunction *fn, DominanceInfo *domTree,
            InstructionDeleter &deleter) {
    canonicalizer.emplace(false /*pruneDebugMode*/,
                          !fn->shouldOptimize() /*maximizeLifetime*/,
                          nullptr /*accessBlockAnalysis*/, domTree, deleter);
  }

  void clear() {
    consumingUsesNeedingCopy.clear();
    consumingBoundaryUsers.clear();
  }

  bool canonicalize(SILValue value);

  bool computeLiveness(SILValue value) {
    return canonicalizer->computeLiveness(value);
  }

  void computeBoundaryData(SILValue value);

  void rewriteLifetimes() { canonicalizer->rewriteLifetimes(); }

  void findOriginalBoundary(PrunedLivenessBoundary &resultingFoundBoundary) {
    canonicalizer->findOriginalBoundary(resultingFoundBoundary);
  }

  bool foundAnyConsumingUses() const {
    return consumingUsesNeedingCopy.size() || consumingBoundaryUsers.size();
  }

  bool foundConsumingUseRequiringCopy() const {
    return consumingUsesNeedingCopy.size();
  }

  bool foundFinalConsumingUses() const { return consumingBoundaryUsers.size(); }

  bool hasPartialApplyConsumingUse() const {
    return llvm::any_of(consumingUsesNeedingCopy,
                        [](SILInstruction *user) {
                          return isa<PartialApplyInst>(user);
                        }) ||
           llvm::any_of(consumingBoundaryUsers, [](SILInstruction *user) {
             return isa<PartialApplyInst>(user);
           });
  }

  bool hasNonPartialApplyConsumingUse() const {
    return llvm::any_of(consumingUsesNeedingCopy,
                        [](SILInstruction *user) {
                          return !isa<PartialApplyInst>(user);
                        }) ||
           llvm::any_of(consumingBoundaryUsers, [](SILInstruction *user) {
             return !isa<PartialApplyInst>(user);
           });
  }
};

/// Search for candidate object mark_must_checks. If we find one that does not
/// fit a pattern that we understand, emit an error diagnostic telling the
/// programmer that the move checker did not know how to recognize this code
/// pattern.
///
/// \returns true if we deleted a mark_must_check inst that we didn't recognize
/// after emitting the diagnostic.
///
/// To check if an error was emitted call checker.emittedAnyDiagnostics().
///
/// NOTE: This is the routine used by the move only object checker to find mark
/// must checks to process.
bool searchForCandidateObjectMarkMustChecks(
    SILFunction *fn,
    SmallSetVector<MarkMustCheckInst *, 32> &moveIntroducersToProcess,
    DiagnosticEmitter &emitter);

bool cleanupSILAfterEmittingObjectMoveOnlyDiagnostics(SILFunction *fn);

} // namespace siloptimizer
} // namespace swift

#endif
