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
#include "llvm/ADT/SmallBitVector.h"

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

  bool isEqual(const AutoDiffParameter &other) const {
    if (getKind() == other.getKind() && getKind() == Kind::Index)
      return getIndex() == other.getIndex();
    return getKind() == other.getKind() && getKind() == Kind::Self;
  }
};

/// SIL-level automatic differentiation indices. Consists of a source index,
/// i.e. index of the dependent result to differentiate from, and parameter
/// indices, i.e. index of independent parameters to differentiate with
/// respect to.
struct SILReverseAutoDiffIndices {
  /// The index of the dependent result to differentiate from.
  unsigned source;
  /// Indices of independent parameters to differentiate with respect to.
  llvm::SmallBitVector parameters;
  
  /// Creates a set of AD indices from the given source index and a bit vector
  /// representing parameter indices.
  /*implicit*/ SILReverseAutoDiffIndices(unsigned source,
                                         llvm::SmallBitVector parameters)
    : source(source), parameters(parameters) {}
  
  /// Creates a set of AD indices from the given source index and an array of
  /// parameter indices. Elements in `parameters` must be acending integers.
  /*implicit*/ SILReverseAutoDiffIndices(unsigned source,
                                         ArrayRef<unsigned> parameters);

  bool operator==(const SILReverseAutoDiffIndices &other) const {
    return source == other.source && !parameters.test(other.parameters);
  }

  void print(llvm::raw_ostream &s = llvm::outs()) const {
    s << "(source=" << source << " parameters=(";
    interleave(parameters.set_bits(),
               [&s](unsigned p) { s << p; }, [&s]{ s << ' '; });
    s << "))";
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &s,
                                     const SILReverseAutoDiffIndices &indices) {
  indices.print(s);
  return s;
}

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
                                  SILGradientOptions options)
    : indices(indices), options(options) {}

  /*implicit*/
  SILReverseAutoDiffConfiguration(SILReverseAutoDiffIndices indices,
                                  bool seedable, bool preservingResult)
    : SILReverseAutoDiffConfiguration(indices, getCanonicalGradientOptions()) {}

  unsigned getSourceIndex() const {
    return indices.source;
  }

  llvm::SmallBitVector getParameterIndices() const {
    return indices.parameters;
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

  bool operator==(const SILReverseAutoDiffConfiguration &other) const {
    return indices == other.indices &&
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
    return { DenseMapInfo<unsigned>::getEmptyKey(), SmallBitVector() };
  }

  static SILReverseAutoDiffIndices getTombstoneKey() {
    return { DenseMapInfo<unsigned>::getTombstoneKey(),
             SmallBitVector(sizeof(intptr_t), true) };
  }

  static unsigned getHashValue(const SILReverseAutoDiffIndices &Val) {
    auto params = Val.parameters.set_bits();
    unsigned combinedHash =
      hash_combine(~1U, DenseMapInfo<unsigned>::getHashValue(Val.source),
                   hash_combine_range(params.begin(), params.end()));
    return combinedHash;
  }

  static bool isEqual(const SILReverseAutoDiffIndices &LHS,
                      const SILReverseAutoDiffIndices &RHS) {
    return LHS == RHS;
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
