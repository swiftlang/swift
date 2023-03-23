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

/// Performs a structural match of two lists of types.
///
/// The invariant is that each list must only contain at most one pack
/// expansion type. After collecting a common prefix and suffix, the
/// pack expansion on either side asborbs the remaining elements on the
/// other side.
class TypeListPackMatcher {
  struct Element {
  private:
    Identifier label;
    Type type;
    ParameterTypeFlags flags;

  public:
    Element(Identifier label, Type type,
            ParameterTypeFlags flags = ParameterTypeFlags())
        : label(label), type(type), flags(flags) {}

    bool hasLabel() const { return !label.empty(); }
    Identifier getLabel() const { return label; }

    Type getType() const { return type; }

    static Element from(const TupleTypeElt &tupleElt);
    static Element from(const AnyFunctionType::Param &funcParam);
    static Element from(Type type);
  };

  ASTContext &ctx;

  SmallVector<Element> lhsElements;
  SmallVector<Element> rhsElements;

protected:
  TypeListPackMatcher(ASTContext &ctx, ArrayRef<TupleTypeElt> lhs,
                      ArrayRef<TupleTypeElt> rhs);

  TypeListPackMatcher(ASTContext &ctx, ArrayRef<AnyFunctionType::Param> lhs,
                      ArrayRef<AnyFunctionType::Param> rhs);

  TypeListPackMatcher(ASTContext &ctx, ArrayRef<Type> lhs, ArrayRef<Type> rhs);

public:
  SmallVector<MatchedPair, 4> pairs;

  bool match();
};

/// Performs a structural match of two lists of (unlabeled) function
/// parameters.
///
/// The invariant is that each list must only contain at most one pack
/// expansion type. After collecting a common prefix and suffix, the
/// pack expansion on either side asborbs the remaining elements on the
/// other side.
class ParamPackMatcher : public TypeListPackMatcher {
public:
  ParamPackMatcher(ArrayRef<AnyFunctionType::Param> lhsParams,
                   ArrayRef<AnyFunctionType::Param> rhsParams, ASTContext &ctx)
      : TypeListPackMatcher(ctx, lhsParams, rhsParams) {}
};

/// Performs a structural match of two lists of types.
///
/// The invariant is that each list must only contain at most one pack
/// expansion type. After collecting a common prefix and suffix, the
/// pack expansion on either side asborbs the remaining elements on the
/// other side.
class PackMatcher : public TypeListPackMatcher {
public:
  PackMatcher(ArrayRef<Type> lhsTypes, ArrayRef<Type> rhsTypes, ASTContext &ctx)
      : TypeListPackMatcher(ctx, lhsTypes, rhsTypes) {}
};

} // end namespace swift

#endif // SWIFT_AST_PACK_EXPANSION_MATCHER_H
