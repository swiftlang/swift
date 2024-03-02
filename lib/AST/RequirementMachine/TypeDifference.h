//===--- TypeDifference.h - Utility for concrete type unification ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "Symbol.h"
#include "Term.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include <optional>

#ifndef TYPE_DIFFERENCE_H_
#define TYPE_DIFFERENCE_H_

namespace llvm {

class raw_ostream;

} // end namespace llvm

namespace swift {

namespace rewriting {

class RewriteContext;

/// Describes transformations that turn LHS into RHS, given that there are a
/// pair of rules (BaseTerm.[LHS] => BaseTerm) and (BaseTerm.[RHS] => BaseTerm).
///
/// There are two kinds of transformations:
///
/// - Replacing a type term T1 with another type term T2, where T2 < T1.
/// - Replacing a type term T1 with a concrete type C2.
struct TypeDifference {
  Term BaseTerm;
  Symbol LHS;
  Symbol RHS;

  /// A pair (N1, N2) where N1 is an index into LHS.getSubstitutions() and
  /// N2 is an index into RHS.getSubstitutions().
  SmallVector<std::pair<unsigned, Term>, 1> SameTypes;

  /// A pair (N1, C2) where N1 is an index into LHS.getSubstitutions() and
  /// C2 is a concrete type symbol.
  SmallVector<std::pair<unsigned, Symbol>, 1> ConcreteTypes;

  TypeDifference(Term baseTerm, Symbol lhs, Symbol rhs,
                 SmallVector<std::pair<unsigned, Term>, 1> sameTypes,
                 SmallVector<std::pair<unsigned, Symbol>, 1> concreteTypes)
    : BaseTerm(baseTerm), LHS(lhs), RHS(rhs),
      SameTypes(sameTypes), ConcreteTypes(concreteTypes) {}

  MutableTerm getOriginalSubstitution(unsigned index) const;
  MutableTerm getReplacementSubstitution(unsigned index) const;

  void dump(llvm::raw_ostream &out) const;
  void verify(RewriteContext &ctx) const;
};

TypeDifference
buildTypeDifference(
    Term baseTerm, Symbol symbol,
    const llvm::SmallVector<std::pair<unsigned, Term>, 1> &sameTypes,
    const llvm::SmallVector<std::pair<unsigned, Symbol>, 1> &concreteTypes,
    RewriteContext &ctx);

} // end namespace rewriting

} // end namespace swift

#endif