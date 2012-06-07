//===--- Attr.h - Swift Language Attribute ASTs -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines classes related to declaration attributes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ATTR_H
#define SWIFT_ATTR_H

#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

/// The associativity of a binary operator.
enum class Associativity {
  /// Non-associative operators cannot be written next to other
  /// operators with the same precedence.  Relational operators are
  /// typically non-associative.
  None,

  /// Left-associative operators associate to the left if written next
  /// to other left-associative operators of the same precedence.
  Left,

  /// Right-associative operators associate to the right if written
  /// next to other right-associative operators of the same precedence.
  Right
};

class InfixData {
  unsigned Precedence : 8;

  /// Zero if invalid, or else an Associativity+1.
  unsigned InvalidOrAssoc : 8;
public:
  InfixData() : Precedence(0), InvalidOrAssoc(0) {}
  InfixData(unsigned prec, Associativity assoc)
    : Precedence(prec), InvalidOrAssoc(unsigned(assoc) + 1) {}

  bool isValid() const { return InvalidOrAssoc != 0; }

  Associativity getAssociativity() const {
    assert(isValid());
    return Associativity(InvalidOrAssoc - 1);
  }
  bool isLeftAssociative() const {
    return getAssociativity() == Associativity::Left;
  }
  bool isRightAssociative() const {
    return getAssociativity() == Associativity::Right;
  }
  bool isNonAssociative() const {
    return getAssociativity() == Associativity::None;
  }

  unsigned getPrecedence() const {
    assert(isValid());
    return Precedence;
  }

  friend bool operator==(InfixData L, InfixData R) {
    return L.Precedence == R.Precedence
        && L.InvalidOrAssoc == R.InvalidOrAssoc;
  }
  friend bool operator!=(InfixData L, InfixData R) {
    return !operator==(L, R);
  }
};

/// ABI resilience.  Language structures are resilient if the details
/// of their implementation may be changed without requiring
/// associated code to be reprocessed.  Different structures are resilient
/// in different ways.  For example:
///   - A resilient type does not have a statically fixed size or layout.
///   - A resilient field must be accessed with getters and setters, even if
///     none are defined for it now.
///   - A resilient function may not be inlined.
///
/// In general, resilience is inherited from the lexical context.  For
/// example, a field declared in a fragile struct is implicitly fragile.
///
/// Some language structures, like tuples, are never themselves
/// resilient (although they may be defined in terms of resilient
/// types).  Additionally, code distributed with the component
/// defining a resilient structure need not actually use resilience
/// boundaries.
enum class Resilience : unsigned char {
  /// Inherently fragile language structures are not only resilient,
  /// but they have never been exposed as resilient.  This permits
  /// certain kinds of optimizations that are not otherwise possible
  /// because of the need for backward compatibility.
  InherentlyFragile,

  /// Fragile language structures are non-resilient.  They may have
  /// been resilient at some point in the past, however.
  Fragile,

  /// Everything else is resilient.  Resilience means different things
  /// on different kinds of objects.
  Resilient
};

class ResilienceData {
  unsigned Valid : 1;
  unsigned Kind : 2;

public:
  ResilienceData() : Valid(false) {}
  ResilienceData(Resilience resil) : Valid(true), Kind(unsigned(resil)) {}

  bool isValid() const { return Valid; }
  Resilience getResilience() const {
    assert(Valid);
    return Resilience(Kind);
  }
};
  
/// DeclAttributes - These are attributes that may be applied to declarations.
class DeclAttributes {
public:
  /// LSquareLoc/RSquareLoc - This is the location of the '[' and ']' in the
  /// attribute specifier.  If this is an empty attribute specifier, then these
  /// will be invalid locs.
  SourceLoc LSquareLoc, RSquareLoc;

  InfixData Infix;
  ResilienceData Resilience;
  StringRef AsmName;
  bool Byref;
  bool ByrefHeap;
  bool AutoClosure;
  bool Assignment;
  bool Conversion;
  bool Postfix;
  
  DeclAttributes() : Byref(false), ByrefHeap(false),
                     AutoClosure(false), Assignment(false),
                     Conversion(false), Postfix(false) { }

  bool isValid() const { return LSquareLoc.isValid(); }

  bool isInfix() const { return Infix.isValid(); }
  InfixData getInfixData() const { return Infix; }
  ResilienceData getResilienceData() const { return Resilience; }
  bool isByref() const { return Byref; }
  bool isByrefHeap() const { assert(isByref()); return ByrefHeap; }
  bool isAutoClosure() const { return AutoClosure; }
  bool isAssignment() const { return Assignment; }
  bool isConversion() const { return Conversion; }
  bool isPostfix() const { return Postfix; }
  
  bool empty() const {
    return !isInfix() && !getResilienceData().isValid() && !isByref() &&
           !isAutoClosure() && !isAssignment() && !isConversion() &&
           !isPostfix();
  }
};
  
} // end namespace swift

#endif
