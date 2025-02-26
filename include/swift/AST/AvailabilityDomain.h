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

#include "swift/AST/ASTAllocated.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/PlatformKind.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {
class ASTContext;
class CustomAvailabilityDomain;
class DeclContext;
class ModuleDecl;

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

    /// Represents availability for an arbitrary domain that is defined at
    /// compile time by a module.
    Custom,
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

  using InlineDomainPtr =
      llvm::PointerEmbeddedInt<uint32_t, InlineDomain::ReprBits>;
  using Storage =
      llvm::PointerUnion<CustomAvailabilityDomain *, InlineDomainPtr>;
  Storage storage;

  AvailabilityDomain(Kind kind)
      : storage(InlineDomain(kind, PlatformKind::none).asInteger()) {
    DEBUG_ASSERT(kind != Kind::Platform);
  };

  AvailabilityDomain(PlatformKind platform)
      : storage(InlineDomain(Kind::Platform, platform).asInteger()) {};

  AvailabilityDomain(CustomAvailabilityDomain *domain) : storage(domain) {};

  AvailabilityDomain(Storage storage) : storage(storage) {};

  std::optional<InlineDomain> getInlineDomain() const {
    return storage.is<InlineDomainPtr>()
               ? static_cast<std::optional<InlineDomain>>(
                     storage.get<InlineDomainPtr>())
               : std::nullopt;
  }

  CustomAvailabilityDomain *getCustomDomain() const {
    ASSERT(isCustom());
    return storage.get<CustomAvailabilityDomain *>();
  }

public:
  AvailabilityDomain() {}

  static AvailabilityDomain forUniversal() {
    return AvailabilityDomain(Kind::Universal);
  }

  static AvailabilityDomain forPlatform(PlatformKind platformKind) {
    bool isPlatform = platformKind != PlatformKind::none;
    ASSERT(isPlatform);
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

  static AvailabilityDomain forCustom(CustomAvailabilityDomain *domain) {
    return AvailabilityDomain(domain);
  }

  /// Returns the most specific platform domain for the target of the
  /// compilation context.
  static std::optional<AvailabilityDomain>
  forTargetPlatform(const ASTContext &ctx);

  /// Returns the most specific platform domain for the target variant of the
  /// compilation context.
  static std::optional<AvailabilityDomain>
  forTargetVariantPlatform(const ASTContext &ctx);

  /// Returns the built-in availability domain identified by the given string.
  static std::optional<AvailabilityDomain>
  builtinDomainForString(StringRef string, const DeclContext *declContext);

  static AvailabilityDomain fromOpaque(void *opaque) {
    return AvailabilityDomain(Storage::getFromOpaqueValue(opaque));
  }

  void *getOpaqueValue() const { return storage.getOpaqueValue(); }

  Kind getKind() const {
    if (auto inlineDomain = getInlineDomain())
      return inlineDomain->getKind();

    return Kind::Custom;
  }

  bool isUniversal() const { return getKind() == Kind::Universal; }

  bool isPlatform() const { return getKind() == Kind::Platform; }

  bool isSwiftLanguage() const { return getKind() == Kind::SwiftLanguage; }

  bool isPackageDescription() const {
    return getKind() == Kind::PackageDescription;
  }

  bool isEmbedded() const { return getKind() == Kind::Embedded; }

  bool isCustom() const { return getKind() == Kind::Custom; }

  /// Returns the platform kind for this domain if applicable.
  PlatformKind getPlatformKind() const {
    if (auto inlineDomain = getInlineDomain())
      return inlineDomain->getPlatform();

    return PlatformKind::none;
  }

  /// Returns true if availability for this domain can be specified in terms of
  /// version ranges.
  bool isVersioned() const;

  /// Returns true if this domain is considered active in the current
  /// compilation context.
  bool isActive(const ASTContext &ctx) const;

  /// Returns the string to use in diagnostics to identify the domain. May
  /// return an empty string.
  llvm::StringRef getNameForDiagnostics() const;

  /// Returns the string to use when printing an `@available` attribute.
  llvm::StringRef getNameForAttributePrinting() const;

  /// Returns the module that the domain belongs to, if it is a custom domain.
  ModuleDecl *getModule() const;

  /// Returns true if availability in `other` is a subset of availability in
  /// this domain. The set of all availability domains form a lattice where the
  /// universal domain (`*`) is the bottom element.
  bool contains(const AvailabilityDomain &other) const;

  /// Returns the root availability domain that this domain must be compatible
  /// with. For example, macCatalyst and visionOS must both be ABI compatible
  /// with iOS. The compatible domain must contain this domain.
  AvailabilityDomain getABICompatibilityDomain() const;

  bool operator==(const AvailabilityDomain &other) const {
    return storage.getOpaqueValue() == other.storage.getOpaqueValue();
  }

  bool operator!=(const AvailabilityDomain &other) const {
    return !(*this == other);
  }

  friend bool operator<(const AvailabilityDomain &lhs,
                        const AvailabilityDomain &rhs) {
    return lhs.storage.getOpaqueValue() < rhs.storage.getOpaqueValue();
  }

  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.AddPointer(getOpaqueValue());
  }

  void print(llvm::raw_ostream &os) const;

private:
  friend class AvailabilityDomainOrIdentifier;

  AvailabilityDomain copy(ASTContext &ctx) const;
};

inline void simple_display(llvm::raw_ostream &os,
                           const AvailabilityDomain &domain) {
  domain.print(os);
}

/// Represents an availability domain that has been defined in a module.
class CustomAvailabilityDomain : public ASTAllocated<CustomAvailabilityDomain> {
public:
  enum class Kind {
    /// A domain that is known to be enabled at compile time.
    Enabled,
    /// A domain that is known to be disabled at compile time.
    Disabled,
    /// A domain with an enablement state that must be queried at runtime.
    Dynamic,
  };

private:
  Identifier name;
  Kind kind;
  ModuleDecl *mod;

  CustomAvailabilityDomain(Identifier name, ModuleDecl *mod, Kind kind);

public:
  static CustomAvailabilityDomain *create(const ASTContext &ctx, StringRef name,
                                          ModuleDecl *mod, Kind kind);

  Identifier getName() const { return name; }
  Kind getKind() const { return kind; }
  ModuleDecl *getModule() const { return mod; }
};

/// Represents either a resolved availability domain or an identifier written
/// in source that has not yet been resolved to a domain.
class AvailabilityDomainOrIdentifier {
  friend struct llvm::PointerLikeTypeTraits<AvailabilityDomainOrIdentifier>;

  using DomainOrIdentifier = llvm::PointerUnion<AvailabilityDomain, Identifier>;

  /// Stores an extra bit representing whether the domain has been resolved.
  using Storage = llvm::PointerIntPair<DomainOrIdentifier, 1, bool>;
  Storage storage;

  AvailabilityDomainOrIdentifier(Storage storage) : storage(storage) {}

  static AvailabilityDomainOrIdentifier fromOpaque(void *opaque) {
    return AvailabilityDomainOrIdentifier(Storage::getFromOpaqueValue(opaque));
  }

  std::optional<AvailabilityDomain>
  lookUpInDeclContext(SourceLoc loc, const DeclContext *declContext) const;

public:
  AvailabilityDomainOrIdentifier(Identifier identifier)
      : storage(identifier) {};
  AvailabilityDomainOrIdentifier(AvailabilityDomain domain)
      : storage(domain) {};

  bool isDomain() const {
    return storage.getPointer().is<AvailabilityDomain>();
  }
  bool isIdentifier() const { return storage.getPointer().is<Identifier>(); }

  /// Overwrites the existing domain or identifier with the given domain.
  void setDomain(AvailabilityDomain domain) { storage = Storage(domain); }

  /// Returns the resolved domain, or `std::nullopt` if there isn't one.
  std::optional<AvailabilityDomain> getAsDomain() const {
    if (isDomain())
      return storage.getPointer().get<AvailabilityDomain>();
    return std::nullopt;
  }

  /// Returns the unresolved identifier, or `std::nullopt` if the domain has
  /// been resolved.
  std::optional<Identifier> getAsIdentifier() const {
    if (isIdentifier())
      return storage.getPointer().get<Identifier>();
    return std::nullopt;
  }

  std::optional<AvailabilityDomain>
  resolveInDeclContext(SourceLoc loc, const DeclContext *declContext) {
    // Return the domain directly if already resolved.
    if (storage.getInt() || isDomain())
      return getAsDomain();

    // Look up the domain and cache the result.
    auto result = lookUpInDeclContext(loc, declContext);
    if (result)
      storage.setPointer(*result);
    storage.setInt(true);

    return result;
  }

  /// Creates a new `AvailabilityDomainOrIdentifier`, defensively copying
  /// members of the original into the given `ASTContext` in case it is
  /// different than the context that the original was created for.
  AvailabilityDomainOrIdentifier copy(ASTContext &ctx) const;

  void print(llvm::raw_ostream &os) const;
};

} // end namespace swift

namespace llvm {
using swift::AvailabilityDomain;
using swift::AvailabilityDomainOrIdentifier;

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
  static inline unsigned getHashValue(AvailabilityDomain domain) {
    return DenseMapInfo<AvailabilityDomain::Storage>::getHashValue(
        domain.storage);
  }
  static bool isEqual(const AvailabilityDomain LHS,
                      const AvailabilityDomain RHS) {
    return LHS == RHS;
  }
};

// An AvailabilityDomainOrIdentifier is "pointer like".
template <typename T>
struct PointerLikeTypeTraits;
template <>
struct PointerLikeTypeTraits<swift::AvailabilityDomainOrIdentifier> {
public:
  static inline void *getAsVoidPointer(AvailabilityDomainOrIdentifier value) {
    return value.storage.getOpaqueValue();
  }
  static inline swift::AvailabilityDomainOrIdentifier
  getFromVoidPointer(void *P) {
    return AvailabilityDomainOrIdentifier::fromOpaque(P);
  }
  enum {
    NumLowBitsAvailable = PointerLikeTypeTraits<
        AvailabilityDomainOrIdentifier::Storage>::NumLowBitsAvailable
  };
};

} // end namespace llvm

#endif
