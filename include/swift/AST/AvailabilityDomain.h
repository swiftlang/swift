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

    /// Represents availability for a specific operating system platform.
    Platform,

    /// Represents availability with respect to Swift language mode.
    SwiftLanguage,

    /// Represents PackageDescription availability.
    PackageDescription,
  };

private:
  Kind kind;
  PlatformKind platform;

  AvailabilityDomain(Kind kind) : kind(kind), platform(PlatformKind::none) {
    assert(kind != Kind::Platform);
  };

  AvailabilityDomain(PlatformKind platform)
      : kind(Kind::Platform), platform(platform) {
    assert(platform != PlatformKind::none);
  };

public:
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

  Kind getKind() const { return kind; }

  bool isUniversal() const { return kind == Kind::Universal; }

  bool isPlatform() const { return kind == Kind::Platform; }

  bool isSwiftLanguage() const { return kind == Kind::SwiftLanguage; }

  bool isPackageDescription() const { return kind == Kind::PackageDescription; }

  /// Returns the platform kind for this domain if applicable.
  PlatformKind getPlatformKind() const { return platform; }

  /// Returns true if this domain is considered active in the current
  /// compilation context.
  bool isActive(ASTContext &ctx) const;

  /// Returns the string to use in diagnostics to identify the domain. May
  /// return an empty string.
  llvm::StringRef getNameForDiagnostics() const;

  /// Returns the string to use when printing an `@available` attribute.
  llvm::StringRef getNameForAttributePrinting() const;
};

} // end namespace swift

#endif
