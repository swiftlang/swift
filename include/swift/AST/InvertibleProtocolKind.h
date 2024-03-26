//===--- InvertibleProtocolKind.h - -----------------------------*- C++ -*-===//
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
// This header declares the InvertibleProtocolKind enum and some
// related operations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_INVERTIBLEPROTOCOLKIND_H
#define SWIFT_AST_INVERTIBLEPROTOCOLKIND_H

#include <stdint.h>

namespace swift {

enum class InvertibleProtocolKind : uint8_t {
#define INVERTIBLE_PROTOCOL_WITH_NAME(Id, Name) Id,
#include "swift/AST/KnownProtocols.def"
};


} // end namespace swift

#endif // SWIFT_AST_INVERTIBLEPROTOCOLKIND_H
