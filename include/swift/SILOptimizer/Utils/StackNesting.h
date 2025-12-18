//===--- StackNesting.h - Utility for stack nesting -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_STACKNESTING_H
#define SWIFT_SILOPTIMIZER_UTILS_STACKNESTING_H

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/BasicBlockData.h"
#include "llvm/ADT/SmallBitVector.h"

namespace swift {

/// A utility to correct the nesting of stack allocating/deallocating
/// instructions.
///
/// This utility is useful for optimizations which create stack allocations
/// without caring about correct nesting with existing allocations. This may
/// result in code like
/// \code
///   %1 = alloc_stack
///     ...
///   %2 = alloc_stack
///     ...
///   dealloc_stack %1
///     ...
///   dealloc_stack %2
/// \endcode
///
/// The StackNesting utility is able to correct the code:
/// \code
///   %1 = alloc_stack
///     ...
///   %2 = alloc_stack
///     ...
///   dealloc_stack %2
///   dealloc_stack %1
/// \endcode
///
/// Each allocation must still be properly jointly post-dominated by
/// its deallocations. StackNesting only fixes the nesting of allocations
/// deallocations; it does not insert required deallocations that are
/// missing entirely.
class StackNesting {
public:

  /// The possible return values of fixNesting().
  enum class Changes {
    /// No changes are made.
    None,

    /// Only instructions were inserted or deleted.
    Instructions,

    /// Instructions were inserted or deleted and new blocks were inserted.
    CFG
  };

  /// Performs correction of stack nesting by moving stack-deallocation
  /// instructions down the control flow.
  ///
  /// Returns the status of what changes were made.
  static Changes fixNesting(SILFunction *F);
};

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_STACKNESTING_H
