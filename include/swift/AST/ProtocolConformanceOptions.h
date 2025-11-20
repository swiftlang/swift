//===--- ProtocolConformanceOptions.h - Conformance Options -----*- C++ -*-===//
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
//
// This file defines the options for protocol conformances.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_PROTOCOLCONFORMANCEOPTIONS_H
#define SWIFT_AST_PROTOCOLCONFORMANCEOPTIONS_H

#include "swift/Basic/OptionSet.h"

namespace swift {

class TypeExpr;

  /// Flags that describe extra attributes on protocol conformances.
enum class ProtocolConformanceFlags {
  /// @unchecked conformance
  Unchecked = 0x01,

  /// @preconcurrency conformance
  Preconcurrency = 0x02,

  /// @unsafe conformance
  Unsafe = 0x04,

  /// @retroactive conformance
  Retroactive = 0x08,

  /// nonisolated, which suppresses and inferred global actor isolation
  Nonisolated = 0x10,

  /// The conformance is global-actor-isolated; the global actor will be
  /// stored separately.
  GlobalActorIsolated = 0x20,

  // Note: whenever you add a bit here, update
  // NumProtocolConformanceOptions below.
};

template<typename TheOptionSet>
struct OptionSetStorageType;

template<typename Flags, typename StorageType>
struct OptionSetStorageType<OptionSet<Flags, StorageType>> {
  using Type = StorageType;
};

/// Options that describe extra attributes on protocol conformances.
class ProtocolConformanceOptions {
  /// The set of options.
  OptionSet<ProtocolConformanceFlags> options;

  /// Global actor isolation for this conformance.
  TypeExpr *globalActorIsolationType = nullptr;

public:
  using StorageType =
      OptionSetStorageType<OptionSet<ProtocolConformanceFlags>>::Type;

  ProtocolConformanceOptions() { }

  ProtocolConformanceOptions(ProtocolConformanceFlags flag)
    : ProtocolConformanceOptions(static_cast<StorageType>(flag), nullptr) { }

  ProtocolConformanceOptions(StorageType flagBits,
                             TypeExpr *globalActorIsolationType)
      : options(flagBits), globalActorIsolationType(globalActorIsolationType) {
    assert(options.contains(ProtocolConformanceFlags::GlobalActorIsolated) ==
           (bool)globalActorIsolationType);
  }

  bool contains(ProtocolConformanceFlags flag) const {
    return options.contains(flag);
  }

  TypeExpr *getGlobalActorIsolationType() const {
    return globalActorIsolationType;
  }

  void setGlobalActorIsolation(TypeExpr *globalActorIsolationType) {
    options |= ProtocolConformanceFlags::GlobalActorIsolated;
    this->globalActorIsolationType = globalActorIsolationType;
  }

  /// Retrieve the raw bits for just the flags part of the options. You also
  /// need to get the global actor isolation (separately) to reconstitute the
  /// options.
  StorageType toRaw() const {
    return options.toRaw();
  }

  ProtocolConformanceOptions &operator|=(ProtocolConformanceFlags flag) {
    assert(flag != ProtocolConformanceFlags::GlobalActorIsolated &&
           "global actor isolation requires a type; use setGlobalActorIsolation");
    options |= flag;
    return *this;
  }

  ProtocolConformanceOptions &
  operator|=(const ProtocolConformanceOptions &other) {
    options |= other.options;
    if (other.globalActorIsolationType && !globalActorIsolationType)
      globalActorIsolationType = other.globalActorIsolationType;
    return *this;
  }

  ProtocolConformanceOptions &operator-=(ProtocolConformanceFlags flag) {
    options -= flag;
    if (flag == ProtocolConformanceFlags::GlobalActorIsolated)
      globalActorIsolationType = nullptr;
    return *this;
  }

  friend ProtocolConformanceOptions operator|(
      const ProtocolConformanceOptions &lhs,
      const ProtocolConformanceOptions &rhs) {
    ProtocolConformanceOptions result(lhs);
    result |= rhs;
    return result;
  }

  friend ProtocolConformanceOptions operator-(
      const ProtocolConformanceOptions &lhs,
      ProtocolConformanceFlags flag) {
    ProtocolConformanceOptions result(lhs);
    result -= flag;
    return result;
  }
};

inline ProtocolConformanceOptions operator|(
    ProtocolConformanceFlags flag1,
    ProtocolConformanceFlags flag2) {
  return ProtocolConformanceOptions(flag1) | flag2;
}

enum : unsigned {
  NumProtocolConformanceOptions = 6
};

} // end namespace swift

#endif
