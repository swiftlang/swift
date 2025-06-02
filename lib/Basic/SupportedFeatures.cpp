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

#include "swift/AST/DiagnosticGroups.h"
#include "swift/Basic/Feature.h"
#include "swift/Frontend/Frontend.h"

#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace swift {
namespace features {

/// The subset of diagnostic groups (called categories by the diagnostic machinery) whose diagnostics should be
/// considered to be part of the migration for this feature.
///
///  When making a feature migratable, ensure that all of the warnings that are used to drive the migration are
///  part of a diagnostic group, and put that diagnostic group into the list for that feature here.
static std::vector<DiagGroupID> migratableCategories(Feature feature) {
  switch (feature) {
    case Feature::InnerKind::ExistentialAny:
      return { DiagGroupID::ExistentialAny };
    case Feature::InnerKind::InferIsolatedConformances:
      return { DiagGroupID::IsolatedConformances };
    case Feature::InnerKind::MemberImportVisibility:
      return { DiagGroupID::MemberImportVisibility };
    case Feature::InnerKind::NonisolatedNonsendingByDefault:
      return { DiagGroupID::NonisolatedNonsendingByDefault };
    case Feature::InnerKind::StrictMemorySafety:
      return { DiagGroupID::StrictMemorySafety };

    // Provide unreachable cases for all of the non-migratable features.
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description) case Feature::FeatureName:
#define MIGRATABLE_UPCOMING_FEATURE(FeatureName, SENumber, Version)
#define MIGRATABLE_EXPERIMENTAL_FEATURE(FeatureName, AvailableInProd)
#define MIGRATABLE_OPTIONAL_LANGUAGE_FEATURE(FeatureName, SENumber, Name)
#include "swift/Basic/Features.def"
    llvm_unreachable("Not a migratable feature");
  }
}

/// For optional language features, return the flag name used by the compiler to enable the feature. For all others,
/// returns an empty optional.
static std::optional<std::string_view> optionalFlagName(Feature feature) {
  switch (feature) {
  case Feature::StrictMemorySafety:
    return "-strict-memory-safety";

#define LANGUAGE_FEATURE(FeatureName, SENumber, Description) case Feature::FeatureName:
#define OPTIONAL_LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#include "swift/Basic/Features.def"
    return std::nullopt;
  }
}

/// Print information about what features upcoming/experimental are
/// supported by the compiler.
/// The information includes whether a feature is adoptable and for
/// upcoming features - what is the first mode it's introduced.
void printSupportedFeatures(llvm::raw_ostream &out) {
  std::array optional{
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)
#define OPTIONAL_LANGUAGE_FEATURE(FeatureName, SENumber, Description) Feature::FeatureName,
#include "swift/Basic/Features.def"
  };

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

      auto categories = migratableCategories(feature);
      out << ", \"categories\": [";
      llvm::interleave(categories, [&out](DiagGroupID diagGroupID) {
        out << "\"" << getDiagGroupInfoByID(diagGroupID).name << "\"";
      }, [&out] {
        out << ", ";
      });
      out << "]";
    }
    if (auto version = feature.getLanguageVersion()) {
      out << ", \"enabled_in\": \"" << *version << "\"";
    }

    if (auto flagName = optionalFlagName(feature)) {
      out << ", \"flag_name\": \"" << *flagName << "\"";
    }

    out << " }";
  };

  out << "  \"features\": {\n";
  out << "    \"optional\": [\n";
  llvm::interleave(optional, printFeature, [&out] { out << ",\n"; });
  out << "\n    ],\n";

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
