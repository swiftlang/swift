//===--- SILArgumentKind.h ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILARGUMENTKIND_H
#define SWIFT_SIL_SILARGUMENTKIND_H

#include "swift/SIL/SILValue.h"

namespace swift {

struct SILArgumentKind {
  enum innerty : std::underlying_type<ValueKind>::type {
#define ARGUMENT(ID, PARENT) ID = unsigned(SILNodeKind::ID),
#define ARGUMENT_RANGE(ID, FIRST, LAST) First_##ID = FIRST, Last_##ID = LAST,
#include "swift/SIL/SILNodes.def"
  } value;

  explicit SILArgumentKind(ValueKind kind)
      : value(*SILArgumentKind::fromValueKind(kind)) {}
  SILArgumentKind(innerty value) : value(value) {}
  operator innerty() const { return value; }

  static Optional<SILArgumentKind> fromValueKind(ValueKind kind) {
    switch (kind) {
#define ARGUMENT(ID, PARENT)                                                   \
  case ValueKind::ID:                                                          \
    return SILArgumentKind(ID);
#include "swift/SIL/SILNodes.def"
    default:
      return None;
    }
  }
};

} // namespace swift

#endif
