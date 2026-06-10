//===--- ComputedWitnessIndex.h - Index into a witness table ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines the ComputedWitnessIndex type, used to represent an index into a
// witness table, whether known at compile-time or is a computed value.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_COMPUTEDWITNESSINDEX_H
#define SWIFT_IRGEN_COMPUTEDWITNESSINDEX_H

#include "WitnessIndex.h"
#include "llvm/IR/Value.h"

namespace swift {
namespace irgen {

/// A class which encapsulates an either a computed or statically-known index
/// into a witness table.
/// FIXME(naming): this is more of a "generalized" or "maybe static" index.
class ComputedWitnessIndex {
  union {
    llvm::Value *ComputedValue;
    WitnessIndex StaticValue;
  };
  bool hasStaticValue;

public:
  /*implicit*/ ComputedWitnessIndex(WitnessIndex index)
      : StaticValue(index), hasStaticValue(true) {}

  explicit ComputedWitnessIndex(llvm::Value *index)
      : ComputedValue(index), hasStaticValue(false) {}

  bool isStatic() const { return hasStaticValue; }

  std::optional<WitnessIndex> getStaticIndex() const {
    if (isStatic())
      return StaticValue;

    return std::nullopt;
  }

  llvm::Value *getDynamicIndex() const {
    if (isStatic())
      return nullptr;

    return ComputedValue;
  }
};

} // end namespace irgen
} // end namespace swift

#endif
