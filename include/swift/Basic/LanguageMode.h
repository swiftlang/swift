//===--- Basic/LanguageMode.h -----------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// A representation for Swift language modes, aka Swift compatibility versions.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_LANGUAGE_MODE_H
#define SWIFT_BASIC_LANGUAGE_MODE_H

#include "swift/Basic/Debug.h"
#include "swift/Basic/Version.h"
#include "llvm/ADT/StringRef.h"
#include <array>
#include <utility>

namespace swift {

struct LanguageMode {
  enum class AnyLanguageMode : unsigned {
#define LANGUAGE_MODE(NAME, ...) NAME,
#include "swift/Basic/LanguageModes.def"

    // The abstract future language mode.
    future,
  };

private:
  AnyLanguageMode mode;

  constexpr LanguageMode(AnyLanguageMode mode) : mode(mode) {}

public:
  /// Implicit conversion to faciliate switching over objects of this type.
  constexpr operator AnyLanguageMode() const { return mode; }

  /// Declare a static constant per language mode for convenience:
  /// `LanguageMode::v6` vs. `LanguageMode(LanguageMode::v6)`.
#define LANGUAGE_MODE(NAME, ...) static const LanguageMode NAME;
#include "swift/Basic/LanguageModes.def"
  static const LanguageMode future;

  /// Returns a boolean value indicating whether this language mode is the
  /// abstract future mode.
  bool isFuture() const;

  /// Returns a boolean value indicating whether this language mode is effective
  /// (enabled) according to the given effective language version.
  bool isEffectiveIn(version::Version effectiveLanguageVersion) const;

  /// Returns the array of supported language modes. This does not include the
  /// abstract future language mode.
  static auto allSupportedModes() {
    return std::array{
#define LANGUAGE_MODE(NAME, ...) LanguageMode::NAME,
#include "swift/Basic/LanguageModes.def"
    };
  }

  /// Returns a pair containing the major and minor version associated with
  /// this language mode.
  std::pair<unsigned, unsigned> version() const;

  /// Returns a string representation of this language mode that can be used
  /// with `-swift-version`.
  std::string versionString() const;

  SWIFT_DEBUG_DUMP;
};

#define LANGUAGE_MODE(NAME, ...)                                               \
  constexpr inline LanguageMode LanguageMode::NAME =                           \
      LanguageMode::AnyLanguageMode::NAME;
#include "swift/Basic/LanguageModes.def"
constexpr inline LanguageMode LanguageMode::future =
    LanguageMode::AnyLanguageMode::future;

} // end namespace swift

#endif // SWIFT_BASIC_LANGUAGE_MODE_H
