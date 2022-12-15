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

#include "MoveOnlyObjectChecker.h"
#include "swift/Basic/NullablePtr.h"

namespace swift {
namespace siloptimizer {

struct DiagnosticEmitter {
  SILFunction *fn;

  /// The canonicalizer that contains the final consuming uses and consuming
  /// uses needing copy for object level diagnostics.
  NullablePtr<OSSACanonicalizer> canonicalizer = nullptr;

  /// Any mark must check inst that we have emitted diagnostics for are placed
  /// here.
  SmallPtrSet<MarkMustCheckInst *, 4> valuesWithDiagnostics;

  // Track any violating uses we have emitted a diagnostic for so we don't emit
  // multiple diagnostics for the same use.
  SmallPtrSet<SILInstruction *, 8> useWithDiagnostic;

  /// Clear our cache of uses that we have diagnosed for a specific
  /// mark_must_check.
  void clearUsesWithDiagnostic() { useWithDiagnostic.clear(); }

  const OSSACanonicalizer &getCanonicalizer() const {
    return *canonicalizer.get();
  }

  void emitCheckerDoesntUnderstandDiagnostic(MarkMustCheckInst *markedValue);
  void emitObjectGuaranteedDiagnostic(MarkMustCheckInst *markedValue);
  void emitObjectOwnedDiagnostic(MarkMustCheckInst *markedValue);

  bool emittedAnyDiagnostics() const { return valuesWithDiagnostics.size(); }

  bool emittedDiagnosticForValue(MarkMustCheckInst *markedValue) const {
    return valuesWithDiagnostics.count(markedValue);
  }

  void emitAddressDiagnostic(MarkMustCheckInst *markedValue,
                             SILInstruction *lastLiveUse,
                             SILInstruction *violatingUse, bool isUseConsuming,
                             bool isInOutEndOfFunction = false);
  void emitInOutEndOfFunctionDiagnostic(MarkMustCheckInst *markedValue,
                                        SILInstruction *violatingUse);
  void emitAddressDiagnosticNoCopy(MarkMustCheckInst *markedValue,
                                   SILInstruction *consumingUse);
  void emitAddressExclusivityHazardDiagnostic(MarkMustCheckInst *markedValue,
                                              SILInstruction *consumingUse);

private:
  /// Emit diagnostics for the final consuming uses and consuming uses needing
  /// copy. If filter is non-null, allow for the caller to pre-process operands
  /// and emit their own diagnostic. If filter returns true, then we assume that
  /// the caller processed it correctly. false, then we continue to process it.
  void emitObjectDiagnosticsForFoundUses(bool ignorePartialApply = false) const;
  void emitObjectDiagnosticsForPartialApplyUses() const;
};

} // namespace siloptimizer
} // namespace swift

#endif
