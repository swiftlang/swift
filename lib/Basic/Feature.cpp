//===--- Feature.cpp --------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Feature.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;

bool swift::isFeatureAvailableInProduction(Feature feature) {
  switch (feature) {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  case Feature::FeatureName:                                                   \
    return true;
#define EXPERIMENTAL_FEATURE(FeatureName, AvailableInProd)                     \
  case Feature::FeatureName:                                                   \
    return AvailableInProd;
#include "swift/Basic/Features.def"
  }
  llvm_unreachable("covered switch");
}

llvm::StringRef swift::getFeatureName(Feature feature) {
  switch (feature) {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  case Feature::FeatureName:                                                   \
    return #FeatureName;
#include "swift/Basic/Features.def"
  }
  llvm_unreachable("covered switch");
}

std::optional<Feature> swift::getUpcomingFeature(llvm::StringRef name) {
  return llvm::StringSwitch<std::optional<Feature>>(name)
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#define UPCOMING_FEATURE(FeatureName, SENumber, Version)                       \
  .Case(#FeatureName, Feature::FeatureName)
#include "swift/Basic/Features.def"
      .Default(std::nullopt);
}

std::optional<Feature> swift::getExperimentalFeature(llvm::StringRef name) {
  return llvm::StringSwitch<std::optional<Feature>>(name)
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#define EXPERIMENTAL_FEATURE(FeatureName, AvailableInProd)                     \
  .Case(#FeatureName, Feature::FeatureName)
#include "swift/Basic/Features.def"
      .Default(std::nullopt);
}

std::optional<unsigned> swift::getFeatureLanguageVersion(Feature feature) {
  switch (feature) {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#define UPCOMING_FEATURE(FeatureName, SENumber, Version)                       \
  case Feature::FeatureName:                                                   \
    return Version;
#include "swift/Basic/Features.def"
  default:
    return std::nullopt;
  }
}

bool swift::includeInModuleInterface(Feature feature) {
  switch (feature) {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  case Feature::FeatureName:                                                   \
    return true;
#define EXPERIMENTAL_FEATURE_EXCLUDED_FROM_MODULE_INTERFACE(FeatureName,       \
                                                            AvailableInProd)   \
  case Feature::FeatureName:                                                   \
    return false;
#include "swift/Basic/Features.def"
  }
  llvm_unreachable("covered switch");
}
