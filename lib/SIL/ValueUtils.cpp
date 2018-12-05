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
  auto range = makeTransformRange(values,
                                  [](SILValue v) {
                                    return v.getOwnershipKind();
                                  });
  return ValueOwnershipKind::merge(range);
}
