//===------------ ProfileCounter.h - PGO Propfile counter -------*- C++ -*-===//
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
//
/// \file Declares ProfileCounter, a convenient type for PGO
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_PROFILECOUNTER_H
#define SWIFT_BASIC_PROFILECOUNTER_H

#include <cassert>
#include <cstdint>

namespace swift {
/// A class designed to be smaller than using Optional<uint64_t> for PGO
class ProfileCounter {
private:
  uint64_t count;

public:
  explicit ProfileCounter() : count(UINT64_MAX) {}
  ProfileCounter(uint64_t Count) : count(Count) {
    if (Count == UINT64_MAX) {
      count = UINT64_MAX - 1;
    }
  }

  bool hasValue() const { return count != UINT64_MAX; }
  uint64_t getValue() const {
    assert(hasValue());
    return count;
  }
  explicit operator bool() const { return hasValue(); }
};
} // end namespace swift

#endif // SWIFT_BASIC_PROFILECOUNTER_H
