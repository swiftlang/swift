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
//  This file defines AST support for reverse-mode automatic differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AUTODIFF_H
#define SWIFT_AST_AUTODIFF_H

#include "ASTContext.h"
#include "DeclNameLoc.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/DenseMap.h"
#include <utility>

namespace swift {

struct AutoDiffConfiguration;
struct SILAutoDiffConfiguration;

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

/// Swift-level automatic differentiation configuration.
struct AutoDiffConfiguration {
  ArrayRef<AutoDiffArgument> arguments;
  bool seedable;
  bool preservingResult;

  SILAutoDiffConfiguration getLoweredConfiguration() const;
};

/// SIL-level automatic differentiation configuration.
struct SILAutoDiffConfiguration {
  ArrayRef<unsigned> argumentIndices;
  bool seedable;
  bool preservingResult;

  std::string getEncoding() const;
};

} // end namespace swift

namespace llvm {

template<> struct DenseMapInfo<swift::AutoDiffArgument> {
  static swift::AutoDiffArgument getEmptyKey() {
    return swift::AutoDiffArgument::getIndexArgument(swift::SourceLoc(), 0);
  }

  static swift::AutoDiffArgument getTombstoneKey() {
    return swift::AutoDiffArgument::getIndexArgument(swift::SourceLoc(), ~1U);
  }

  static unsigned getHashValue(swift::AutoDiffArgument Val) {
    switch (Val.getKind()) {
    case swift::AutoDiffArgument::Kind::Self:
      return DenseMapInfo<unsigned>::getHashValue(UINT_MAX);
    case swift::AutoDiffArgument::Kind::Index:
      return DenseMapInfo<unsigned>::getHashValue(Val.getIndex());
    }
  }

  static bool isEqual(swift::AutoDiffArgument LHS,
                      swift::AutoDiffArgument RHS) {
    return LHS.isEqual(RHS);
  }
};

template<> struct DenseMapInfo<swift::AutoDiffConfiguration> {
  static swift::AutoDiffConfiguration getEmptyKey() {
    return { {}, false };
  }

  static swift::AutoDiffConfiguration getTombstoneKey() {
    return { {}, true };
  }

  static unsigned getHashValue(swift::AutoDiffConfiguration Val) {
    unsigned argHash = ~1U;
    for (auto arg : Val.arguments)
      argHash = hash_combine(
        argHash, DenseMapInfo<swift::AutoDiffArgument>::getHashValue(arg));
    return hash_combine(
      argHash,
      DenseMapInfo<unsigned>::getHashValue(Val.seedable)
    );
  }

  static bool isEqual(swift::AutoDiffConfiguration LHS,
                      swift::AutoDiffConfiguration RHS) {
    if (LHS.arguments.size() != RHS.arguments.size())
      return false;
    for (unsigned i = 0; i < LHS.arguments.size(); i++)
      if (!LHS.arguments[i].isEqual(RHS.arguments[i]))
        return false;
    return LHS.seedable == RHS.seedable;
  }
};

template<> struct DenseMapInfo<swift::SILAutoDiffConfiguration> {
  static swift::SILAutoDiffConfiguration getEmptyKey() {
    return { {}, false, false };
  }

  static swift::SILAutoDiffConfiguration getTombstoneKey() {
    return { {}, true, true };
  }

  static unsigned getHashValue(swift::SILAutoDiffConfiguration Val) {
    unsigned argHash = ~1U;
    for (auto i : Val.argumentIndices)
      argHash = hash_combine(argHash, DenseMapInfo<unsigned>::getHashValue(i));
    return hash_combine(
      argHash,
      DenseMapInfo<unsigned>::getHashValue(Val.seedable),
      DenseMapInfo<unsigned>::getHashValue(Val.preservingResult)
    );
  }

  static bool isEqual(swift::SILAutoDiffConfiguration LHS,
                      swift::SILAutoDiffConfiguration RHS) {
    if (LHS.argumentIndices.size() != RHS.argumentIndices.size())
      return false;
    for (unsigned i = 0; i < LHS.argumentIndices.size(); i++)
      if (LHS.argumentIndices[i] != RHS.argumentIndices[i])
        return false;
    return LHS.seedable == RHS.seedable &&
           LHS.preservingResult == LHS.preservingResult;
  }
};

} // end namespace llvm

#endif // SWIFT_AST_AUTODIFF_H
