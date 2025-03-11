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

#ifndef SWIFT_BASIC_FEATURES_H
#define SWIFT_BASIC_FEATURES_H

#include "llvm/ADT/StringRef.h"
#include <optional>

namespace swift {

class LangOptions;

/// Enumeration describing all of the named features.
enum class Feature : uint16_t {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description) FeatureName,
#include "swift/Basic/Features.def"
};

constexpr unsigned numFeatures() {
  enum Features {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description) FeatureName,
#include "swift/Basic/Features.def"
    NumFeatures
  };
  return NumFeatures;
}

/// Check whether the given feature is available in production compilers.
bool isFeatureAvailableInProduction(Feature feature);

/// Determine the in-source name of the given feature.
llvm::StringRef getFeatureName(Feature feature);

/// Determine whether the first feature is more recent (and thus implies
/// the existence of) the second feature.  Only meaningful for suppressible
/// features.
inline bool featureImpliesFeature(Feature feature, Feature implied) {
  // Suppressible features are expected to be listed in order of
  // addition in Features.def.
  return (unsigned) feature < (unsigned) implied;
}

/// Get the feature corresponding to this "future" feature, if there is one.
std::optional<Feature> getUpcomingFeature(llvm::StringRef name);

/// Get the feature corresponding to this "experimental" feature, if there is
/// one.
std::optional<Feature> getExperimentalFeature(llvm::StringRef name);

/// Get the major language version in which this feature was introduced, or
/// \c None if it does not have such a version.
std::optional<unsigned> getFeatureLanguageVersion(Feature feature);

/// Determine whether the given feature supports adoption mode.
bool isFeatureAdoptable(Feature feature);

/// Determine whether this feature should be included in the
/// module interface
bool includeInModuleInterface(Feature feature);

}

#endif // SWIFT_BASIC_FEATURES_H
