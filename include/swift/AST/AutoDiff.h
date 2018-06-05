//===--- AutoDiff.h - Swift Automatic Differentiation ---------------------===//
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
//
//  SWIFT_ENABLE_TENSORFLOW
//  This file defines AST support for automatic differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AUTODIFF_H
#define SWIFT_AST_AUTODIFF_H

#include "ASTContext.h"

namespace swift {

enum class AutoDiffMode {
  Forward, Reverse
};

struct AutoDiffIndexParameter {
  SourceLoc loc;
  unsigned index;
};

class AutoDiffParameter {
public:
  enum class Kind { Index, Self };

private:
  SourceLoc Loc;
  Kind Kind;
  union Value {
    struct { unsigned Index; }; // Index
    struct {};                  // Self
    Value(unsigned index) : Index(index) {}
    Value() {}
  } V;

public:
  AutoDiffParameter(SourceLoc loc, enum Kind kind, Value value)
    : Loc(loc), Kind(kind), V(value) {}

  static AutoDiffParameter getIndexParameter(SourceLoc loc, unsigned index) {
    return { loc, Kind::Index, index };
  }

  static AutoDiffParameter getSelfParameter(SourceLoc loc) {
    return { loc, Kind::Self, {} };
  }

  unsigned getIndex() const {
    assert(Kind == Kind::Index);
    return V.Index;
  }

  enum Kind getKind() const {
    return Kind;
  }

  SourceLoc getLoc() const {
    return Loc;
  }

  bool isEqual(AutoDiffParameter other) const {
    if (getKind() == other.getKind() && getKind() == Kind::Index)
      return getIndex() == other.getIndex();
    return getKind() == other.getKind() && getKind() == Kind::Self;
  }
};

/// SIL-level automatic differentiation indices. Consists of a source index,
/// i.e. index of the dependent result to differentiate from, and parameter
/// indices, i.e. index of an independent parameter to differentiate with
/// respect to.
struct SILReverseAutoDiffIndices {
  unsigned source;
  SmallVector<unsigned, 8> parameters;

  /*implicit*/ SILReverseAutoDiffIndices(unsigned source,
                                         ArrayRef<unsigned> parameters)
    : source(source), parameters(parameters.begin(), parameters.end()) {}

  bool isEqual(const SILReverseAutoDiffIndices &other) const {
    return source == other.source &&
           ArrayRef<unsigned>(parameters).equals(other.parameters);
  }

  // Returns true if it's a valid list of indices. To be valid, parameters
  // indices need to be ascending.
  bool isValid() const {
    int last = -1;
    for (auto paramIdx : parameters) {
      if ((int)paramIdx <= last)
        return false;
      last = paramIdx;
    }
    return true;
  }
};

/// Flags to define the semantics and the type signature of a gradient function.
enum class SILGradientFlags : unsigned {
  /// The gradient function is seedable, i.e. able to take a back-propagated
  /// adjoint value as the last parameter.
  Seedable = 1 << 0,
  
  /// The gradient function is preserving the result of the original function.
  PreservingResult = 1 << 1,
  
  /// The adjoint computation is "delayed". We say that the adjoint computation
  /// is delayed when when it's returned as a thunk.
  Delayed = 1 << 2
};
using SILGradientOptions = OptionSet<SILGradientFlags>;
static inline SILGradientOptions operator|(SILGradientFlags lhs,
                                           SILGradientFlags rhs) {
  return SILGradientOptions(unsigned(lhs) | unsigned(rhs));
}

/// SIL-level automatic differentiation configuration.
struct SILReverseAutoDiffConfiguration {
  SILReverseAutoDiffIndices indices;
  SILGradientOptions options;

  /*implicit*/
  SILReverseAutoDiffConfiguration(SILReverseAutoDiffIndices indices,
                                  bool seedable, bool preservingResult)
    : indices(indices), options(getCanonicalGradientOptions()) {}

  /*implicit*/
  SILReverseAutoDiffConfiguration(SILReverseAutoDiffIndices indices,
                                  SILGradientOptions options)
    : indices(indices), options(options) {}

  unsigned getSourceIndex() const {
    return indices.source;
  }

  void setSourceIndex(unsigned index) {
    indices.source = index;
  }

  ArrayRef<unsigned> getParameterIndices() const {
    return ArrayRef<unsigned>(indices.parameters);
  }

  bool isSeedable() const {
    return options.contains(SILGradientFlags::Seedable);
  }

  bool isPreservingResult() const {
    return options.contains(SILGradientFlags::PreservingResult);
  }

  bool isDelayed() const {
    return options.contains(SILGradientFlags::Delayed);
  }

  // FIXME: The master configuration should have all three gradient options
  // enabled, that is, the canonical gradient should return a delayed gradient
  // function. We need to handle this here as well as within the
  // differentiation pass.
  static SILGradientOptions getCanonicalGradientOptions() {
    return SILGradientFlags::Seedable | SILGradientFlags::PreservingResult;
  }

  /// Returns the "master" configuration, which all variants with the same
  /// parameter indices can derive from.
  static
  SILReverseAutoDiffConfiguration getMaster(SILReverseAutoDiffIndices indices) {
    return {
      indices,
      getCanonicalGradientOptions()
    };
  }

  SILReverseAutoDiffConfiguration getWithCanonicalOptions() const {
    return getMaster(indices);
  }

  bool isMaster() const {
    return options.toRaw() == getCanonicalGradientOptions().toRaw();
  }

  bool isEqual(const SILReverseAutoDiffConfiguration &other) const {
    return indices.isEqual(other.indices) &&
           options.toRaw() == other.options.toRaw();
  }
};

} // end namespace swift

namespace llvm {

using swift::SILReverseAutoDiffIndices;
using swift::SILReverseAutoDiffConfiguration;
using swift::SILGradientFlags;
using swift::OptionSet;

template<typename T> struct DenseMapInfo;

template<> struct DenseMapInfo<SILReverseAutoDiffIndices> {
  static SILReverseAutoDiffIndices getEmptyKey() {
    return { DenseMapInfo<unsigned>::getEmptyKey(), { ~0U } };
  }

  static SILReverseAutoDiffIndices getTombstoneKey() {
    return { DenseMapInfo<unsigned>::getTombstoneKey(), { ~0U - 1 } };
  }

  static unsigned getHashValue(const SILReverseAutoDiffIndices &Val) {
    unsigned combinedHash =
      hash_combine(~1U, DenseMapInfo<unsigned>::getHashValue(Val.source));
    for (auto i : Val.parameters)
      combinedHash = hash_combine(combinedHash,
        DenseMapInfo<unsigned>::getHashValue(i));
    return combinedHash;
  }

  static bool isEqual(const SILReverseAutoDiffIndices &LHS,
                      const SILReverseAutoDiffIndices &RHS) {
    return LHS.source == RHS.source &&
           LHS.parameters.size() == RHS.parameters.size() &&
           all_of(zip(LHS.parameters, RHS.parameters),
                  [&](std::pair<unsigned, unsigned> pair) -> bool {
                    return pair.first == pair.second;
                  });
  }
};

template<> struct DenseMapInfo<SILReverseAutoDiffConfiguration> {
  static SILReverseAutoDiffConfiguration getEmptyKey() {
    return { DenseMapInfo<SILReverseAutoDiffIndices>::getEmptyKey(), None };
  }

  static SILReverseAutoDiffConfiguration getTombstoneKey() {
    return {
      DenseMapInfo<SILReverseAutoDiffIndices>::getTombstoneKey(),
      SILGradientFlags::Delayed
    };
  }

  static unsigned getHashValue(const SILReverseAutoDiffConfiguration &Val) {
    return hash_combine(
      DenseMapInfo<SILReverseAutoDiffIndices>::getHashValue(Val.indices),
      DenseMapInfo<unsigned>::getHashValue(Val.options.toRaw())
    );
  }

  static bool isEqual(const SILReverseAutoDiffConfiguration &LHS,
                      const SILReverseAutoDiffConfiguration &RHS) {
    return DenseMapInfo<SILReverseAutoDiffIndices>
             ::isEqual(LHS.indices, RHS.indices) &&
           LHS.options.toRaw() == RHS.options.toRaw();
  }
};

} // end namespace llvm

#endif // SWIFT_AST_AUTODIFF_H
