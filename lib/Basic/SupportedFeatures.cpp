//===--- SupportedFeatures.cpp - Supported features printing --------------===//
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

#include <array>
#include <vector>

#include "swift/Basic/Feature.h"
#include "swift/Frontend/Frontend.h"

#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace swift {
namespace features {
/// Print information about what features upcoming/experimental are
/// supported by the compiler.
/// The information includes whether a feature is adoptable and for
/// upcoming features - what is the first mode it's introduced.
void printSupportedFeatures(llvm::raw_ostream &out) {
  std::array upcoming{
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#define UPCOMING_FEATURE(FeatureName, SENumber, Version) Feature::FeatureName,
#include "swift/Basic/Features.def"
  };

  std::vector<swift::Feature> experimental{{
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#define EXPERIMENTAL_FEATURE(FeatureName, AvailableInProd) Feature::FeatureName,
#include "swift/Basic/Features.def"
  }};

  // Include only experimental features that are available in production.
  llvm::erase_if(experimental, [](auto &feature) {
    return feature.isAvailableInProduction();
  });

  out << "{\n";
  auto printFeature = [&out](const Feature &feature) {
    out << "      ";
    out << "{ \"name\": \"" << feature.getName() << "\"";
    if (feature.isMigratable()) {
      out << ", \"migratable\": true";
    }
    if (auto version = feature.getLanguageVersion()) {
      out << ", \"enabled_in\": \"" << *version << "\"";
    }
    out << " }";
  };

  out << "  \"features\": {\n";
  out << "    \"upcoming\": [\n";
  llvm::interleave(upcoming, printFeature, [&out] { out << ",\n"; });
  out << "\n    ],\n";

  out << "    \"experimental\": [\n";
  llvm::interleave(experimental, printFeature, [&out] { out << ",\n"; });
  out << "\n    ]\n";

  out << "  }\n";
  out << "}\n";
}

} // end namespace features
} // end namespace swift
