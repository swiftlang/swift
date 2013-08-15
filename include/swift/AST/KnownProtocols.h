//===--- KnownProtocols.h - Working with compiler protocols -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_KNOWNPROTOCOLS_H
#define SWIFT_AST_KNOWNPROTOCOLS_H

namespace swift {

/// \brief The set of known protocols.
enum class KnownProtocolKind : uint8_t {
#define PROTOCOL(Id) Id,
#include "swift/AST/KnownProtocols.def"
};

enum : uint8_t {
  /// The number of known protocols.
  NumKnownProtocols = 0
#define PROTOCOL(Id) + 1
#include "swift/AST/KnownProtocols.def"
};

} // end namespace swift

#endif
