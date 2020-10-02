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
#include "swift/SIL/SILValue.h"

namespace swift {

class BeginAccessInst;
class LoadBorrowInst;
class SILValue;
class Operand;

namespace silverifier {

class LoadBorrowNeverInvalidatedAnalysis {
  SmallMultiMapCache<SILValue, Operand *> cache;
  DeadEndBlocks &deadEndBlocks;

public:
  LoadBorrowNeverInvalidatedAnalysis(DeadEndBlocks &deadEndBlocks);

  /// Returns true if exhaustively lbi is guaranteed to never be invalidated by
  /// local writes.
  bool isNeverInvalidated(LoadBorrowInst *lbi);

private:
  bool doesAddressHaveWriteThatInvalidatesLoadBorrow(
      LoadBorrowInst *lbi, ArrayRef<Operand *> endBorrowUses, SILValue address);
  bool doesBoxHaveWritesThatInvalidateLoadBorrow(
      LoadBorrowInst *lbi, ArrayRef<Operand *> endBorrowUses, SILValue box);
};

} // namespace silverifier
} // namespace swift

#endif
