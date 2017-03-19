//===--- WitnessIndex.h - Index into a witness table ------------*- C++ -*-===//
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
// This file defines the WitnessIndex type, used for drilling into a
// protocol witness table or value witness table.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_WITNESSINDEX_H
#define SWIFT_IRGEN_WITNESSINDEX_H

#include "ValueWitness.h"

namespace swift {
namespace irgen {

/// A class which encapsulates an index into a witness table.
class WitnessIndex {
  unsigned Value : 31;
  unsigned IsPrefix : 1;
public:
  WitnessIndex() = default;
  WitnessIndex(ValueWitness index) : Value(unsigned(index)) {}
  explicit WitnessIndex(unsigned index, bool isPrefix)
    : Value(index), IsPrefix(isPrefix) {}

  unsigned getValue() const { return Value; }

  bool isPrefix() const { return IsPrefix; }
};

} // end namespace irgen
} // end namespace swift

#endif
