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

class AutoDiffArgument {
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
  AutoDiffArgument(SourceLoc loc, enum Kind kind, Value value)
    : Loc(loc), Kind(kind), V(value) {}

  static AutoDiffArgument getIndexArgument(SourceLoc loc, unsigned index) {
    return { loc, Kind::Index, { index } };
  }

  static AutoDiffArgument getSelfArgument(SourceLoc loc) {
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

  bool isEqual(AutoDiffArgument other) const {
    if (getKind() == other.getKind() && getKind() == Kind::Index)
      return getIndex() == other.getIndex();
    return getKind() == other.getKind() && getKind() == Kind::Self;
  }
};

/// SIL-level automatic differentiation configuration.
struct SILAutoDiffConfiguration {
  ArrayRef<unsigned> argumentIndices;
  bool seedable;
  bool preservingResult;
};

} // end namespace swift

namespace llvm {

using swift::SILAutoDiffConfiguration;

template<typename T> struct DenseMapInfo;

template<> struct DenseMapInfo<SILAutoDiffConfiguration> {
  static SILAutoDiffConfiguration getEmptyKey() {
    return { {}, false, false };
  }

  static SILAutoDiffConfiguration getTombstoneKey() {
    return { {}, true, true };
  }

  static unsigned getHashValue(SILAutoDiffConfiguration Val) {
    unsigned argHash = ~1U;
    for (auto i : Val.argumentIndices)
      argHash = hash_combine(argHash, DenseMapInfo<unsigned>::getHashValue(i));
    return hash_combine(
      argHash,
      DenseMapInfo<unsigned>::getHashValue(Val.seedable),
      DenseMapInfo<unsigned>::getHashValue(Val.preservingResult)
    );
  }

  static bool isEqual(SILAutoDiffConfiguration LHS,
                      SILAutoDiffConfiguration RHS) {
    auto numArgs = LHS.argumentIndices.size();
    if (numArgs != RHS.argumentIndices.size())
      return false;
    for (unsigned i = 0; i < numArgs; i++)
      if (LHS.argumentIndices[i] != RHS.argumentIndices[i])
        return false;
    return LHS.seedable == RHS.seedable &&
           LHS.preservingResult == LHS.preservingResult;
  }
};

} // end namespace llvm

#endif // SWIFT_AST_AUTODIFF_H
