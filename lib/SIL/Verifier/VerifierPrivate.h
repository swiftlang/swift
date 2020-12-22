//===--- VerifierPrivate.h ------------------------------------------------===//
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

#ifndef SWIFT_SIL_VERIFIER_VERIFIERPRIVATE_H
#define SWIFT_SIL_VERIFIER_VERIFIERPRIVATE_H

#include "swift/Basic/MultiMapCache.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILValue.h"

namespace swift {

class BeginAccessInst;
class LoadBorrowInst;
class SILValue;
class Operand;

namespace silverifier {

class LoadBorrowImmutabilityAnalysis {
  SmallMultiMapCache<AccessPath, Operand *> cache;
  DeadEndBlocks &deadEndBlocks;

public:
  LoadBorrowImmutabilityAnalysis(DeadEndBlocks &deadEndBlocks,
                                 const SILFunction *f);

  /// Returns true if exhaustively lbi is guaranteed to never be invalidated by
  /// local writes.
  bool isImmutable(LoadBorrowInst *lbi);

private:
  bool isImmutableInScope(LoadBorrowInst *lbi,
                          ArrayRef<Operand *> endBorrowUses,
                          AccessPath accessPath);
};

/// Out of line verify a SILFunction without going through
/// SILFunction->verify().
///
/// This is a lower level interface to the verification functionality only for
/// use with the C API and SILFunction::verify() itself.
void verifySILFunction(const SILFunction *fn, bool singleFunction);

/// Out of line call to verify that a SILFunction doesn't have critical edges.
void verifyCriticalEdgesSILFunction(const SILFunction *fn);

/// Out of line verify a SIL property. Called by SILProperty::verify().
void verifySILProperty(const SILProperty *prop, const SILModule &mod);

/// Out of line verify a SIL property. Called by SILProperty::verify().
void verifySILVTable(const SILVTable *vt, const SILModule &mod);

/// Out of line verify a SIL witness table. Called by SILWitnessTable::verify().
void verifySILWitnessTable(const SILWitnessTable *wt, const SILModule &mod);

/// Out of line verify a SIL default witness table. Called by
/// SILDefaultWitnessTable::verify().
void verifySILDefaultWitnessTable(const SILDefaultWitnessTable *wt,
                                  const SILModule &mod);

/// Out of line verify a SIL global variable. Called by
/// SILGlobalVariable::verify().
void verifySILGlobalVariable(const SILGlobalVariable *gv);

/// Out of line verify a SILModule without going through
/// SILModule->verify().
///
/// This is a lower level interface to the verification functionality only for
/// use with the C API and SILModule::verify() itself.
void verifySILModule(const SILModule &mod);

} // namespace silverifier
} // namespace swift

#endif
