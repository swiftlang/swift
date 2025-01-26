//===--- AvailabilityDomain.h - Swift Availability Domains ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the AvailabilityDomain class, which represents a domain
// for which availability can be checked.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AVAILABILITY_DOMAIN_H
#define SWIFT_AST_AVAILABILITY_DOMAIN_H

#include "swift/AST/PlatformKind.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {
class ASTContext;

/// Represents a dimension of availability (e.g. macOS platform or Swift
/// language mode).
class AvailabilityDomain final {
public:
  enum class Kind : uint8_t {
    /// The root availability domain. This is used for declarations that are
    /// universally unavailable or deprecated, for example.
    Universal,

    /// Represents availability with respect to Swift language mode.
    SwiftLanguage,

    /// Represents PackageDescription availability.
    PackageDescription,

    /// Represents Embedded Swift availability.
    Embedded,

    /// Represents availability for a specific operating system platform.
    Platform,
  };

private:
  friend struct llvm::PointerLikeTypeTraits<AvailabilityDomain>;
  friend struct llvm::DenseMapInfo<AvailabilityDomain>;

  /// For a subset of domain kinds, all the information needed to represent the
  /// domain can be encapsulated inline in this class.
  class InlineDomain {
    Kind kind;
    PlatformKind platform;

  public:
    using IntReprType = uint32_t;
    enum : uintptr_t {
      SpareBits = 8,
      ReprBits = sizeof(IntReprType) * CHAR_BIT - SpareBits,
      KindShift = ReprBits - sizeof(Kind) * CHAR_BIT,
      PlatformShift = KindShift - sizeof(PlatformKind) * CHAR_BIT,
    };

    InlineDomain(Kind kind, PlatformKind platform)
        : kind(kind), platform(platform) {};
    InlineDomain(IntReprType value)
        : kind(static_cast<Kind>(value >> KindShift)),
          platform(static_cast<PlatformKind>(value >> PlatformShift)) {}

    /// Serializes the domain into an integer value that must be smaller than a
    /// a pointer.
    IntReprType asInteger() const {
      return static_cast<IntReprType>(kind) << KindShift |
             static_cast<IntReprType>(platform) << PlatformShift;
    }

    Kind getKind() const { return kind; }
    PlatformKind getPlatform() { return platform; }
  };

  /// This will eventually be a class storing information about externally
  /// defined availability domains.
  using ExternalDomain = void;

  using InlineDomainPtr =
      llvm::PointerEmbeddedInt<uint32_t, InlineDomain::ReprBits>;
  using Storage = llvm::PointerUnion<ExternalDomain *, InlineDomainPtr>;
  Storage storage;

  AvailabilityDomain(Kind kind)
      : storage(InlineDomain(kind, PlatformKind::none).asInteger()) {
    assert(kind != Kind::Platform);
  };

  AvailabilityDomain(PlatformKind platform)
      : storage(InlineDomain(Kind::Platform, platform).asInteger()) {};

  AvailabilityDomain(Storage storage) : storage(storage) {};

  static AvailabilityDomain fromOpaque(void *opaque) {
    return AvailabilityDomain(Storage::getFromOpaqueValue(opaque));
  }

  void *getOpaqueValue() const { return storage.getOpaqueValue(); }

  std::optional<InlineDomain> getInlineDomain() const {
    return storage.is<InlineDomainPtr>()
               ? static_cast<std::optional<InlineDomain>>(
                     storage.get<InlineDomainPtr>())
               : std::nullopt;
  }

public:
  AvailabilityDomain() {}

  static AvailabilityDomain forUniversal() {
    return AvailabilityDomain(Kind::Universal);
  }

  static AvailabilityDomain forPlatform(PlatformKind platformKind) {
    return AvailabilityDomain(platformKind);
  }

  static AvailabilityDomain forSwiftLanguage() {
    return AvailabilityDomain(Kind::SwiftLanguage);
  }

  static AvailabilityDomain forPackageDescription() {
    return AvailabilityDomain(Kind::PackageDescription);
  }

  static AvailabilityDomain forEmbedded() {
    return AvailabilityDomain(Kind::Embedded);
  }

  /// Returns the built-in availability domain identified by the given string.
  static std::optional<AvailabilityDomain>
  builtinDomainForString(StringRef string);

  Kind getKind() const {
    if (auto inlineDomain = getInlineDomain())
      return inlineDomain->getKind();

    llvm_unreachable("unimplemented");
  }

  bool isUniversal() const { return getKind() == Kind::Universal; }

  bool isPlatform() const { return getKind() == Kind::Platform; }

  bool isSwiftLanguage() const { return getKind() == Kind::SwiftLanguage; }

  bool isPackageDescription() const {
    return getKind() == Kind::PackageDescription;
  }

  bool isEmbedded() const { return getKind() == Kind::Embedded; }

  /// Returns the platform kind for this domain if applicable.
  PlatformKind getPlatformKind() const {
    if (auto inlineDomain = getInlineDomain())
      return inlineDomain->getPlatform();

    return PlatformKind::none;
  }

  /// Returns true if this domain is considered active in the current
  /// compilation context.
  bool isActive(const ASTContext &ctx) const;

  /// Returns the string to use in diagnostics to identify the domain. May
  /// return an empty string.
  llvm::StringRef getNameForDiagnostics() const;

  /// Returns the string to use when printing an `@available` attribute.
  llvm::StringRef getNameForAttributePrinting() const;

  /// Returns true if availability in `other` is a subset of availability in
  /// this domain. The set of all availability domains form a lattice where the
  /// universal domain (`*`) is the bottom element.
  bool contains(const AvailabilityDomain &other) const;

  bool operator==(const AvailabilityDomain &other) const {
    return storage.getOpaqueValue() == other.storage.getOpaqueValue();
  }

  bool operator!=(const AvailabilityDomain &other) const {
    return !(*this == other);
  }

  /// A total, stable ordering on domains.
  bool operator<(const AvailabilityDomain &other) const {
    if (getKind() != other.getKind())
      return getKind() < other.getKind();

    switch (getKind()) {
    case Kind::Universal:
    case Kind::SwiftLanguage:
    case Kind::PackageDescription:
    case Kind::Embedded:
      // These availability domains are singletons.
      return false;
    case Kind::Platform:
      return getPlatformKind() < other.getPlatformKind();
    }
  }

  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.AddPointer(getOpaqueValue());
  }
};

} // end namespace swift

namespace llvm {
using swift::AvailabilityDomain;

// An AvailabilityDomain is "pointer like".
template <typename T>
struct PointerLikeTypeTraits;
template <>
struct PointerLikeTypeTraits<swift::AvailabilityDomain> {
public:
  static inline void *getAsVoidPointer(AvailabilityDomain domain) {
    return domain.storage.getOpaqueValue();
  }
  static inline swift::AvailabilityDomain getFromVoidPointer(void *P) {
    return AvailabilityDomain::fromOpaque(P);
  }
  enum {
    NumLowBitsAvailable =
        PointerLikeTypeTraits<AvailabilityDomain::Storage>::NumLowBitsAvailable
  };
};

template <>
struct DenseMapInfo<AvailabilityDomain> {
  static inline AvailabilityDomain getEmptyKey() {
    return DenseMapInfo<AvailabilityDomain::Storage>::getEmptyKey();
  }
  static inline AvailabilityDomain getTombstoneKey() {
    return DenseMapInfo<AvailabilityDomain::Storage>::getTombstoneKey();
  }
  static bool isEqual(const AvailabilityDomain LHS,
                      const AvailabilityDomain RHS) {
    return LHS == RHS;
  }
};

} // end namespace llvm

#endif
