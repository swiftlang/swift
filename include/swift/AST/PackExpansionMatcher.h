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

    Element(Identifier label, Type type,
            ParameterTypeFlags flags = ParameterTypeFlags())
        : label(label), type(type), flags(flags) {}

  public:
    bool hasLabel() const { return !label.empty(); }
    Identifier getLabel() const { return label; }

    Type getType() const { return type; }

    ParameterTypeFlags getFlags() const { return flags; }

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

  [[nodiscard]] bool match();
};

/// Performs a structural match of two lists of tuple elements.
///
/// The invariant is that each list must only contain at most one pack
/// expansion type. After collecting a common prefix and suffix, the
/// pack expansion on either side asborbs the remaining elements on the
/// other side.
class TuplePackMatcher : public TypeListPackMatcher {
public:
  TuplePackMatcher(TupleType *lhsTuple, TupleType *rhsTuple)
      : TypeListPackMatcher(lhsTuple->getASTContext(),
                            lhsTuple->getElements(),
                            rhsTuple->getElements()) {}
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
