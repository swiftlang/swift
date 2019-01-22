//===--- ValueUtils.cpp ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/ValueUtils.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/SILFunction.h"

using namespace swift;

Optional<ValueOwnershipKind>
swift::mergeSILValueOwnership(ArrayRef<SILValue> values) {
  // A forwarding inst without operands must be trivial.
  if (values.empty())
    return Optional<ValueOwnershipKind>(ValueOwnershipKind::Trivial);

  // Find the first index where we have a trivial value.
  auto iter = find_if(values, [](SILValue v) -> bool {
    return v.getOwnershipKind() != ValueOwnershipKind::Trivial;
  });
  // All trivial.
  if (iter == values.end()) {
    return Optional<ValueOwnershipKind>(ValueOwnershipKind::Trivial);
  }

  // See if we have any Any. If we do, just return that for now.
  if (any_of(values, [](SILValue v) -> bool {
        return v.getOwnershipKind() == ValueOwnershipKind::Any;
      }))
    return Optional<ValueOwnershipKind>(ValueOwnershipKind::Any);

  unsigned index = std::distance(values.begin(), iter);
  ValueOwnershipKind base = values[index].getOwnershipKind();

  for (SILValue v : values.slice(index + 1)) {
    auto opKind = v.getOwnershipKind();
    if (opKind.merge(ValueOwnershipKind::Trivial))
      continue;

    auto mergedValue = base.merge(opKind.Value);
    if (!mergedValue.hasValue()) {
      return None;
    }
  }

  return base;
}
