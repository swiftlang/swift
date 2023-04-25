//===--- AddressUseKind.h -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_ADDRESSUSEKIND_H
#define SWIFT_SIL_ADDRESSUSEKIND_H

namespace swift {

enum class AddressUseKind { NonEscaping, PointerEscape, Unknown };

inline AddressUseKind meet(AddressUseKind lhs, AddressUseKind rhs) {
  return (lhs > rhs) ? lhs : rhs;
}

} // namespace swift

#endif
