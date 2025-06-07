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

bool Feature::isAvailableInProduction() const {
  switch (kind) {
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

llvm::StringRef Feature::getName() const {
  switch (kind) {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  case Feature::FeatureName:                                                   \
    return #FeatureName;
#include "swift/Basic/Features.def"
  }
  llvm_unreachable("covered switch");
}

std::optional<Feature> Feature::getUpcomingFeature(llvm::StringRef name) {
  return llvm::StringSwitch<std::optional<Feature>>(name)
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#define UPCOMING_FEATURE(FeatureName, SENumber, Version)                       \
  .Case(#FeatureName, Feature::FeatureName)
#include "swift/Basic/Features.def"
      .Default(std::nullopt);
}

std::optional<Feature> Feature::getExperimentalFeature(llvm::StringRef name) {
  return llvm::StringSwitch<std::optional<Feature>>(name)
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#define EXPERIMENTAL_FEATURE(FeatureName, AvailableInProd)                     \
  .Case(#FeatureName, Feature::FeatureName)
#include "swift/Basic/Features.def"
      .Default(std::nullopt);
}

std::optional<unsigned> Feature::getLanguageVersion() const {
  switch (kind) {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#define UPCOMING_FEATURE(FeatureName, SENumber, Version)                       \
  case Feature::FeatureName:                                                   \
    return Version;
#include "swift/Basic/Features.def"
  default:
    return std::nullopt;
  }
}

bool Feature::isMigratable() const {
  switch (kind) {
#define MIGRATABLE_UPCOMING_FEATURE(FeatureName, SENumber, Version)
#define MIGRATABLE_EXPERIMENTAL_FEATURE(FeatureName, AvailableInProd)
#define MIGRATABLE_OPTIONAL_LANGUAGE_FEATURE(FeatureName, SENumber, Name)
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  case Feature::FeatureName:
#include "swift/Basic/Features.def"
    return false;
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#define MIGRATABLE_UPCOMING_FEATURE(FeatureName, SENumber, Version)            \
  case Feature::FeatureName:
#define MIGRATABLE_EXPERIMENTAL_FEATURE(FeatureName, AvailableInProd)          \
  case Feature::FeatureName:
#define MIGRATABLE_OPTIONAL_LANGUAGE_FEATURE(FeatureName, SENumber, Name)      \
  case Feature::FeatureName:
#include "swift/Basic/Features.def"
    return true;
  }
}

bool Feature::includeInModuleInterface() const {
  switch (kind) {
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
