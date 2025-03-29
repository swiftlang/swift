/* lit.swift-feature.cfg.inc - Config for Swift features for 'lit' test runner */

/*
 * This source file is part of the Swift.org open source project
 *
 * Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
 * Licensed under Apache License v2.0 with Runtime Library Exception
 *
 * See https://swift.org/LICENSE.txt for license information
 * See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
 */

/*
 * This is a configuration file for the 'lit' test runner. It takes the
 * Features.def for the language and creates 'lit' features associated with
 * them when the Swift feature is available. It allows using
 * `REQUIRES: FeatureName` instead of `REQUIRES: asserts`, which tend to be
 * left behind when the Swift feature becomes non-experimental.
 *
 * This file is preprocessed the Clang preprocessor and generates Python files.
 * C comments are possible, but not Python comments (they look like preprocessor
 * statements).
 */

config.existing_swift_features = set()
def language_feature(feature_name, enabled):
    config.existing_swift_features.add(feature_name)
    if enabled or "asserts" in config.available_features:
        config.available_features.add("swift_feature_" + feature_name)

#define UPCOMING_FEATURE(FeatureName, SENumber, Version) language_feature(#FeatureName, True)
#define EXPERIMENTAL_FEATURE(FeatureName, AvailableInProd) language_feature(#FeatureName, #AvailableInProd == "true")
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description)                   \
  language_feature(#FeatureName, True)

#include <swift/Basic/Features.def>
