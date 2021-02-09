//===--- Feature.h - Helpers related to Swift features ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_FEATURES_H
#define SWIFT_BASIC_FEATURES_H

#include "llvm/ADT/StringRef.h"

namespace swift {

class LangOptions;
  
/// Enumeration describing all of the named features.
enum class Feature {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description, Option) \
  FeatureName,
  #include "swift/Basic/Features.def"
};

/// Determine the in-source name of the given feature.
llvm::StringRef getFeatureName(Feature feature);

}

#endif // SWIFT_BASIC_FEATURES_H
