//===--- ShrinkBorrowScope.h - Shrink OSSA borrow scopes --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This utility shrinks borrow scopes by rewriting end_borrows.
///
/// It hoists end_borrows up to just after the latest "deinit barrier".
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_SHRINKBORROWSCOPES_H
#define SWIFT_SILOPTIMIZER_UTILS_SHRINKBORROWSCOPES_H

#include "swift/SIL/OwnershipUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"

namespace swift {

//===----------------------------------------------------------------------===//
//                       MARK: ShrinkBorrowScope
//===----------------------------------------------------------------------===//

class ShrinkBorrowScope {
  // The borrow that begins this scope.
  BorrowedValue borrowedValue;

  InstructionDeleter &deleter;

public:
  ShrinkBorrowScope(InstructionDeleter &deleter) : deleter(deleter) {}

  BorrowedValue getBorrowedValue() const { return borrowedValue; }

  InstructionDeleter &getDeleter() { return deleter; }

  bool shrinkBorrowScope(BorrowedValue borrow);
};

} // namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_SHRINKBORROWSCOPES_H
