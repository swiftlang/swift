//===--- Feature.h - Helpers related to Swift features ----------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_FEATURE_H
#define SWIFT_BASIC_FEATURE_H

#include "swift/Basic/LLVM.h"

#include "llvm/ADT/StringRef.h"
#include <optional>

namespace swift {

class LangOptions;

/// Enumeration describing all of the named features.
struct Feature {
  enum class InnerKind : uint16_t {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description) FeatureName,
#include "swift/Basic/Features.def"
  };

  InnerKind kind;

  constexpr Feature(InnerKind kind) : kind(kind) {}
  constexpr Feature(unsigned inputKind) : kind(InnerKind(inputKind)) {}

  constexpr operator InnerKind() const { return kind; }
  constexpr explicit operator unsigned() const { return unsigned(kind); }
  constexpr explicit operator size_t() const { return size_t(kind); }

  static constexpr unsigned getNumFeatures() {
    enum Features {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description) FeatureName,
#include "swift/Basic/Features.def"
      NumFeatures
    };
    return NumFeatures;
  }

#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  static const Feature FeatureName;
#include "swift/Basic/Features.def"

  /// Check whether the given feature is available in production compilers.
  bool isAvailableInProduction() const;

  /// Determine the in-source name of the given feature.
  llvm::StringRef getName() const;

  /// Determine whether the given feature supports migration mode.
  bool isMigratable() const;

  /// Determine whether this feature should be included in the
  /// module interface
  bool includeInModuleInterface() const;

  /// Determine whether the first feature is more recent (and thus implies
  /// the existence of) the second feature.  Only meaningful for suppressible
  /// features.
  constexpr bool featureImpliesFeature(Feature implied) const {
    // Suppressible features are expected to be listed in order of
    // addition in Features.def.
    return (unsigned)kind < (unsigned)implied.kind;
  }

  /// Get the feature corresponding to this "future" feature, if there is one.
  static std::optional<Feature> getUpcomingFeature(StringRef name);

  /// Get the feature corresponding to this "experimental" feature, if there is
  /// one.
  static std::optional<Feature> getExperimentalFeature(StringRef name);

  /// Get the major language version in which this feature was introduced, or
  /// \c None if it does not have such a version.
  std::optional<unsigned> getLanguageVersion() const;
};

#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  constexpr inline Feature Feature::FeatureName =                              \
      Feature::InnerKind::FeatureName;
#include "swift/Basic/Features.def"
}

#endif // SWIFT_BASIC_FEATURES_H
