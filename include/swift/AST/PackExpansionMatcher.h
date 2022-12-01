//===--- PackExpansionMatcher.h - Matching pack expansions ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Utilities for structural matching of sequences of types containing pack
// expansions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PACK_EXPANSION_MATCHER_H
#define SWIFT_AST_PACK_EXPANSION_MATCHER_H

#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class ASTContext;

/// The result of a match. An important invariant is that either both types
/// are PackExpansionTypes, or both types are scalars. In particular, any
/// PackTypes are always wrapped in a PackExpansionType.
struct MatchedPair {
  Type lhs;
  Type rhs;

  unsigned lhsIdx;
  unsigned rhsIdx;

  MatchedPair(Type lhs, Type rhs, unsigned lhsIdx, unsigned rhsIdx)
    : lhs(lhs), rhs(rhs), lhsIdx(lhsIdx), rhsIdx(rhsIdx) {}
};

/// Performs a structural match of two lists of tuple elements. The invariant
/// is that a pack expansion type must not be followed by an unlabeled
/// element, that is, it is either the last element or the next element has
/// a label.
///
/// In this manner, an element with a pack expansion type "absorbs" all
/// unlabeled elements up to the next label. An element with any other type
/// matches exactly one element on the other side.
class TuplePackMatcher {
  ArrayRef<TupleTypeElt> lhsElts;
  ArrayRef<TupleTypeElt> rhsElts;

  ASTContext &ctx;

public:
  SmallVector<MatchedPair, 4> pairs;

  TuplePackMatcher(TupleType *lhsTuple, TupleType *rhsTuple);

  bool match();
};

/// Performs a structural match of two lists of (unlabeled) function
/// parameters.
///
/// The invariant is that each list must only contain at most one pack
/// expansion type. After collecting a common prefix and suffix, the
/// pack expansion on either side asborbs the remaining elements on the
/// other side.
class ParamPackMatcher {
  ArrayRef<AnyFunctionType::Param> lhsParams;
  ArrayRef<AnyFunctionType::Param> rhsParams;

  ASTContext &ctx;

public:
  SmallVector<MatchedPair, 4> pairs;

  ParamPackMatcher(ArrayRef<AnyFunctionType::Param> lhsParams,
                   ArrayRef<AnyFunctionType::Param> rhsParams,
                   ASTContext &ctx);

  bool match();
};

/// Performs a structural match of two lists of types.
///
/// The invariant is that each list must only contain at most one pack
/// expansion type. After collecting a common prefix and suffix, the
/// pack expansion on either side asborbs the remaining elements on the
/// other side.
class PackMatcher {
  ArrayRef<Type> lhsTypes;
  ArrayRef<Type> rhsTypes;

  ASTContext &ctx;

public:
  SmallVector<MatchedPair, 4> pairs;

  PackMatcher(ArrayRef<Type> lhsTypes,
              ArrayRef<Type> rhsTypes,
              ASTContext &ctx);

  bool match();
};

} // end namespace swift

#endif // SWIFT_AST_PACK_EXPANSION_MATCHER_H
