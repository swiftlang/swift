//===--- Statistic.h - ------------------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_SUPPORT_STATISTIC_H
#define LLVM_SOURCEKIT_SUPPORT_STATISTIC_H

#include "SourceKit/Support/UIdent.h"
#include <atomic>
#include <string>

namespace SourceKit {

struct Statistic {
  const UIdent name;
  const std::string description;
  std::atomic<int64_t> value = {0};

  Statistic(UIdent name, std::string description)
      : name(name), description(std::move(description)) {}

  int64_t operator++() {
    return 1 + value.fetch_add(1, std::memory_order_relaxed);
  }
  int64_t operator--() {
    return value.fetch_sub(1, std::memory_order_relaxed) - 1;
  }

  void updateMax(int64_t newValue) {
    int64_t prev = value.load(std::memory_order_relaxed);
    // Note: compare_exchange_weak updates 'prev' if it fails.
    while (newValue > prev && !value.compare_exchange_weak(
                                  prev, newValue, std::memory_order_relaxed)) {
    }
  }
};

} // namespace SourceKit

#endif // LLVM_SOURCEKIT_SUPPORT_STATISTIC_H
