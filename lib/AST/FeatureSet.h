//===--- FeatureSet.h - Language feature support ----------------*- C++ -*-===//
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

#ifndef SWIFT_AST_FEATURES_H
#define SWIFT_AST_FEATURES_H

#include "swift/AST/Decl.h"
#include "swift/Basic/Feature.h"
#include "swift/Basic/FixedBitSet.h"

namespace swift {

using BasicFeatureSet =
    FixedBitSet<Feature::getNumFeatures(), Feature::InnerKind>;

class FeatureSet {
  BasicFeatureSet required;

  // Stored inverted: index i actually represents
  // Feature(numFeatures() - i)
  //
  // This is the easiest way of letting us iterate from largest to
  // smallest, i.e. from the newest to the oldest feature, which is
  // the order in which we need to emit #if clauses.
  using SuppressibleFeatureSet = FixedBitSet<Feature::getNumFeatures(), size_t>;
  SuppressibleFeatureSet suppressible;

public:
  class SuppressibleGenerator {
    SuppressibleFeatureSet::iterator i, e;
    friend class FeatureSet;
    SuppressibleGenerator(const SuppressibleFeatureSet &set)
        : i(set.begin()), e(set.end()) {}

  public:
    bool empty() const { return i == e; }
    Feature next() { return Feature(Feature::getNumFeatures() - *i++); }
  };

  bool empty() const { return required.empty() && suppressible.empty(); }

  bool hasAnyRequired() const { return !required.empty(); }
  const BasicFeatureSet &requiredFeatures() const { return required; }

  bool hasAnySuppressible() const { return !suppressible.empty(); }
  SuppressibleGenerator generateSuppressibleFeatures() const {
    return SuppressibleGenerator(suppressible);
  }

  enum InsertOrRemove : bool { Insert = true, Remove = false };

  void collectFeaturesUsed(Decl *decl, InsertOrRemove operation);

private:
  void collectRequiredFeature(Feature feature, InsertOrRemove operation);
  void collectSuppressibleFeature(Feature feature, InsertOrRemove operation);
};

/// Get the set of features that are uniquely used by this declaration, and are
/// not part of the enclosing context.
FeatureSet getUniqueFeaturesUsed(Decl *decl);

bool usesFeatureIsolatedDeinit(const Decl *decl);

} // end namespace swift

#endif /* SWIFT_AST_FEATURES_H */
