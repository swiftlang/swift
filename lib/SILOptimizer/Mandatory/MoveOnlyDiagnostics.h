//===--- MoveOnlyDiagnostics.h --------------------------------------------===//
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
/// Shared diagnostic code used by both the move only address checker and move
/// only object checker.
///
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_MOVEONLYDIAGNOSTICS_H
#define SWIFT_SILOPTIMIZER_MANDATORY_MOVEONLYDIAGNOSTICS_H

#include "MoveOnlyObjectCheckerUtils.h"
#include "MoveOnlyTypeUtils.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/SIL/FieldSensitivePrunedLiveness.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {

class FieldSensitivePrunedLivenessBoundary;

namespace siloptimizer {

class DiagnosticEmitter {
  SILFunction *fn;

  /// The canonicalizer that contains the final consuming uses and consuming
  /// uses needing copy for object level diagnostics.
  NullablePtr<OSSACanonicalizer> canonicalizer = nullptr;

  /// Any mark must check inst that we have emitted diagnostics for are placed
  /// here.
  SmallPtrSet<MarkUnresolvedNonCopyableValueInst *, 4> valuesWithDiagnostics;

  /// Track any violating uses we have emitted a diagnostic for so we don't emit
  /// multiple diagnostics for the same use.
  SmallPtrSet<SILInstruction *, 8> useWithDiagnostic;

  /// A count of the total diagnostics emitted so that callers of routines that
  /// take a diagnostic emitter can know if the emitter emitted additional
  /// diagnosics while running a callee.
  unsigned diagnosticCount = 0;

  bool emittedCheckerDoesntUnderstandDiagnostic = false;

  /// This is incremented every time that the checker determines that an earlier
  /// pass emitted a diagnostic while processing a
  /// mark_unresolved_non_copyable_value. In such a case, we want to suppress:
  ///
  /// 1. Emitting the compiler doesn't understand how to check error for the
  ///    specific mark_unresolved_non_copyable_value.
  ///
  /// 2. The "copy of noncopyable type" error over the entire function since us
  ///    stopping processing at some point may have left copies.
  ///
  /// We use a counter rather than a boolean here so that a caller that is
  /// processing an individual mark_unresolved_non_copyable_value can determine
  /// if the checker identified such an earlier pass diagnostic for the specific
  /// allocation so that we can still emit "compiler doesn't understand" errors
  /// for other allocations.
  unsigned diagnosticEmittedByEarlierPassCount = 0;

public:
  DiagnosticEmitter(SILFunction *inputFn) : fn(inputFn) {}

  void initCanonicalizer(OSSACanonicalizer *inputCanonicalizer) {
    canonicalizer = inputCanonicalizer;
  }

  /// Clear our cache of uses that we have diagnosed for a specific
  /// mark_unresolved_non_copyable_value.
  void clearUsesWithDiagnostic() { useWithDiagnostic.clear(); }

  const OSSACanonicalizer &getCanonicalizer() const {
    return *canonicalizer.get();
  }

  /// Returns true if when processing any allocation in the current function:
  ///
  /// 1. This diagnostic emitter emitted a diagnostic.
  /// 2. The user of the diagnostic emitter signaled to the diagnostic emitter
  ///    that it detected an earlier diagnostic was emitted that prevented it
  ///    from performing checking.
  ///
  /// DISCUSSION: This is used by the checker to decide whether or not it should
  /// emit "found copy of a noncopyable type" error. If the checker emitted one
  /// of these diagnostics, then the checker may have stopped processing early
  /// and left copies since it was no longer able to check. In such a case, we
  /// want the user to fix the pre-existing errors and re-run.
  bool emittedDiagnostic() const {
    return getDiagnosticCount() || getDiagnosticEmittedByEarlierPassCount();
  }

  unsigned getDiagnosticCount() const { return diagnosticCount; }

  bool didEmitCheckerDoesntUnderstandDiagnostic() const {
    return emittedCheckerDoesntUnderstandDiagnostic;
  }

  bool getDiagnosticEmittedByEarlierPassCount() const {
    return diagnosticEmittedByEarlierPassCount;
  }

  void
  emitEarlierPassEmittedDiagnostic(MarkUnresolvedNonCopyableValueInst *mmci) {
    ++diagnosticEmittedByEarlierPassCount;
    registerDiagnosticEmitted(mmci);
  }

  /// Used at the end of the MoveOnlyAddressChecker to tell the user in a nice
  /// way to file a bug.
  void emitCheckedMissedCopyError(SILInstruction *copyInst);

  /// Given a drop_deinit of self and an instruction reinitializing self,
  /// emits an error saying that you cannot reinitialize self after a discard.
  void emitReinitAfterDiscardError(SILInstruction *badReinit,
                                   SILInstruction *dropDeinit);

  /// Assuming the given instruction represents the implicit destruction of
  /// 'self', emits an error saying that you needed to explicitly 'consume self'
  /// here because you're in a discarding context.
  void emitMissingConsumeInDiscardingContext(SILInstruction *leftoverDestroy,
                                             SILInstruction *dropDeinit);

  void emitCheckerDoesntUnderstandDiagnostic(
      MarkUnresolvedNonCopyableValueInst *markedValue);
  void emitObjectGuaranteedDiagnostic(
      MarkUnresolvedNonCopyableValueInst *markedValue);
  void
  emitObjectOwnedDiagnostic(MarkUnresolvedNonCopyableValueInst *markedValue);

  bool emittedDiagnosticForValue(
      MarkUnresolvedNonCopyableValueInst *markedValue) const {
    return valuesWithDiagnostics.count(markedValue);
  }

  /// The kind of scope at the end of which an address must be initialized.
  enum class ScopeRequiringFinalInit {
    /// The scope for an inout argument.
    ///
    /// The whole function.
    InoutArgument,
    /// The scope for an address yielded by a coroutine.
    ///
    /// It begins at the begin_apply and ends at all corresponding
    /// end_apply/abort_apply instructions.
    Coroutine,
    /// The scope for an address done through an access scope marker.
    ///
    /// It begins at the begin_access and ends at all corresponding end_access
    /// instructions.
    ModifyMemoryAccess,
  };

  void emitAddressDiagnostic(
      MarkUnresolvedNonCopyableValueInst *markedValue,
      SILInstruction *lastLiveUse, SILInstruction *violatingUse,
      bool isUseConsuming,
      std::optional<ScopeRequiringFinalInit> scopeKind = std::nullopt);
  void emitInOutEndOfFunctionDiagnostic(
      MarkUnresolvedNonCopyableValueInst *markedValue,
      SILInstruction *violatingUse);
  void
  emitAddressDiagnosticNoCopy(MarkUnresolvedNonCopyableValueInst *markedValue,
                              SILInstruction *consumingUse);
  void emitAddressExclusivityHazardDiagnostic(
      MarkUnresolvedNonCopyableValueInst *markedValue,
      SILInstruction *consumingUse);
  void emitObjectDestructureNeededWithinBorrowBoundary(
      MarkUnresolvedNonCopyableValueInst *markedValue,
      SILInstruction *destructureNeedingUse,
      TypeTreeLeafTypeRange destructureNeededBits,
      FieldSensitivePrunedLivenessBoundary &boundary);

  void emitObjectInstConsumesValueTwice(
      MarkUnresolvedNonCopyableValueInst *markedValue,
      Operand *firstConsumingUse, Operand *secondConsumingUse);
  void emitObjectInstConsumesAndUsesValue(
      MarkUnresolvedNonCopyableValueInst *markedValue, Operand *consumingUse,
      Operand *nonConsumingUse);

  /// Emit a diagnostic for a case where we have one of the following cases:
  ///
  /// 1. A partial_apply formed from a borrowed address only value.
  /// 2. A use of a captured value in a closure callee.
  void emitAddressEscapingClosureCaptureLoadedAndConsumed(
      MarkUnresolvedNonCopyableValueInst *markedValue);

  /// Try to emit a diagnostic for a load/consume from an
  /// assignable_but_not_consumable access to a global or a class field. Returns
  /// false if we did not find something we pattern matched as being either of
  /// those cases. Returns true if we emitted a diagnostic.
  bool emitGlobalOrClassFieldLoadedAndConsumed(
      MarkUnresolvedNonCopyableValueInst *markedValue);

  void
  emitPromotedBoxArgumentError(MarkUnresolvedNonCopyableValueInst *markedValue,
                               SILFunctionArgument *arg);

  void emitCannotPartiallyMutateError(
      MarkUnresolvedNonCopyableValueInst *markedValue,
      PartialMutationError error, SILInstruction *user,
      TypeTreeLeafTypeRange usedBits, PartialMutation kind);

private:
  /// Emit diagnostics for the final consuming uses and consuming uses needing
  /// copy. If filter is non-null, allow for the caller to pre-process operands
  /// and emit their own diagnostic. If filter returns true, then we assume that
  /// the caller processed it correctly. false, then we continue to process it.
  void
  emitObjectDiagnosticsForGuaranteedUses(bool ignorePartialApply = false) const;
  void emitObjectDiagnosticsForPartialApplyUses(StringRef capturedVarName) const;

  void registerDiagnosticEmitted(MarkUnresolvedNonCopyableValueInst *value) {
    ++diagnosticCount;
    valuesWithDiagnostics.insert(value);
  }
};

} // namespace siloptimizer
} // namespace swift

#endif
