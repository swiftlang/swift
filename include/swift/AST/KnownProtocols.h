//===--- KnownProtocols.h - Working with compiler protocols -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_KNOWNPROTOCOLS_H
#define SWIFT_AST_KNOWNPROTOCOLS_H

#include "swift/ABI/InvertibleProtocols.h"
#include "swift/Basic/InlineBitfield.h"
#include "swift/Config.h"

namespace llvm {
class StringRef;
}

namespace swift {

/// The set of known protocols.
enum class KnownProtocolKind : uint8_t {
#define PROTOCOL_WITH_NAME(Id, Name) Id,
#include "swift/AST/KnownProtocols.def"
};

enum class RepressibleProtocolKind : uint8_t {
#define REPRESSIBLE_PROTOCOL_WITH_NAME(Id, Name) Id,
#include "swift/AST/KnownProtocols.def"
};

enum : uint8_t {
  // This uses a preprocessor trick to count all the protocols. The enum value
  // expression below expands to "+1+1+1...". (Note that the first plus
  // is parsed as a unary operator.)
#define PROTOCOL_WITH_NAME(Id, Name) +1
  /// The number of known protocols.
  NumKnownProtocols =
#include "swift/AST/KnownProtocols.def"
};

enum : unsigned { NumKnownProtocolKindBits =
  countBitsUsed(static_cast<unsigned>(NumKnownProtocols - 1)) };

/// Retrieve the name of the given known protocol.
llvm::StringRef getProtocolName(KnownProtocolKind kind);

std::optional<RepressibleProtocolKind>
getRepressibleProtocolKind(KnownProtocolKind kp);

KnownProtocolKind getKnownProtocolKind(RepressibleProtocolKind ip);

void simple_display(llvm::raw_ostream &out,
                    const RepressibleProtocolKind &value);

/// MARK: Invertible protocols
///
/// The invertible protocols are a subset of the known protocols.

enum : uint8_t {
  // Use preprocessor trick to count all the invertible protocols.
#define INVERTIBLE_PROTOCOL(Name, Bit) +1
  /// The number of invertible protocols.
  NumInvertibleProtocols =
#include "swift/ABI/InvertibleProtocols.def"
};

using InvertibleProtocolSet = InvertibleProtocolSet;

/// Maps a KnownProtocol to the set of InvertibleProtocols, if a mapping exists.
/// \returns None if the known protocol is not invertible.
std::optional<InvertibleProtocolKind>
getInvertibleProtocolKind(KnownProtocolKind kp);

/// Returns the KnownProtocolKind corresponding to an InvertibleProtocolKind.
KnownProtocolKind getKnownProtocolKind(InvertibleProtocolKind ip);

void simple_display(llvm::raw_ostream &out,
                    const InvertibleProtocolKind &value);

} // end namespace swift

namespace llvm {
template <typename T, typename Enable>
struct DenseMapInfo;
template <>
struct DenseMapInfo<swift::RepressibleProtocolKind> {
  using RepressibleProtocolKind = swift::RepressibleProtocolKind;
  using Impl = DenseMapInfo<uint8_t>;
  static inline RepressibleProtocolKind getEmptyKey() {
    return (RepressibleProtocolKind)Impl::getEmptyKey();
  }
  static inline RepressibleProtocolKind getTombstoneKey() {
    return (RepressibleProtocolKind)Impl::getTombstoneKey();
  }
  static unsigned getHashValue(const RepressibleProtocolKind &Val) {
    return Impl::getHashValue((uint8_t)Val);
  }
  static bool isEqual(const RepressibleProtocolKind &LHS,
                      const RepressibleProtocolKind &RHS) {
    return LHS == RHS;
  }
};
} // namespace llvm

#endif
