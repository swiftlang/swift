//===--- Statistic.h - Helpers for llvm::Statistic --------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_STATISTIC_H
#define SWIFT_BASIC_STATISTIC_H

#include "llvm/ADT/Statistic.h"

#define SWIFT_FUNC_STAT                                                 \
  do {                                                                  \
    static llvm::Statistic FStat =                                      \
      {DEBUG_TYPE, __func__, __func__, {0}, false};                     \
    ++FStat;                                                            \
  } while (0)

#endif // SWIFT_BASIC_STATISTIC_H
