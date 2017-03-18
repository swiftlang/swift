//===--- Statistic.h - Helpers for gathering stats --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_STATISTIC_H
#define SWIFT_BASIC_STATISTIC_H

#include "llvm/ADT/Statistic.h"

#define SWIFT_FUNC_STAT                                                 \
  do {                                                                  \
    static llvm::Statistic FStat =                                      \
      {DEBUG_TYPE, __func__, __func__, {0}, false};                     \
    ++FStat;                                                            \
  } while (0)

namespace swift {
namespace sys {

/// A JSONSerialization-friendly summary of resource-usage data we're interested
/// in from self and subprocesses.
///
/// Note: we do not use llvm::TimeRecord here for a few reasons:
///
///    1. It only measures own-process times, not subprocesses. We want both, so
///       the code to produce it is sunk into swift::sys::TaskQueue anyway.
///
///    2. Its memory-measuring code is controlled by an option we don't want to
///       be controlled-by
///
///    3. It measures the wrong thing anyways (malloc heap, not peak RSS)
struct ResourceStats {
  int64_t UserTimeUsec;       ///< User time elapsed in microseconds.
  int64_t SystemTimeUsec;     ///< System time elapsed in microseconds.
  int64_t MaxResidentBytes;   ///< Peak resident-set / working-set size.
};

} // namespace sys
} // namespace swift

#endif // SWIFT_BASIC_STATISTIC_H
