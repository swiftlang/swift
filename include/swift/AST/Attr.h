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

#include "swift/Basic/Optional.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/AST/Ownership.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
class ASTPrinter;

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
  InfixData(unsigned char prec, Associativity assoc)
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
///   - A resilient variable must be accessed with getters and setters, even if
///     none are defined for it now.
///   - A resilient function may not be inlined.
///
/// In general, resilience is inherited from the lexical context.  For
/// example, a variable declared in a fragile struct is implicitly fragile.
///
/// Some language structures, like tuples, are never themselves
/// resilient (although they may be defined in terms of resilient
/// types).  Additionally, code distributed with the component
/// defining a resilient structure need not actually use resilience
/// boundaries.
enum class Resilience : unsigned char {
  Default,
  
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

  
enum class AbstractCC : unsigned char;

// Define enumerators for each attribute, e.g. AK_weak.
enum AttrKind {
#define ATTR(X) AK_##X,
#include "swift/AST/Attr.def"
  AK_Count
};

// Define enumerators for each type attribute, e.g. TAK_weak.
enum TypeAttrKind {
#define ATTR(X)
#define TYPE_ATTR(X) TAK_##X,
#include "swift/AST/Attr.def"
  TAK_Count
};

  
  
/// TypeAttributes - These are attributes that may be applied to types.
class TypeAttributes {
  // Get a SourceLoc for every possible attribute that can be parsed in source.
  // the presence of the attribute is indicated by its location being set.
  SourceLoc AttrLocs[TAK_Count];
public:
  /// AtLoc - This is the location of the first '@' in the attribute specifier.
  /// If this is an empty attribute specifier, then this will be an invalid loc.
  SourceLoc AtLoc;
  Optional<AbstractCC> cc = Nothing;

  // For an opened existential type, the known ID.
  Optional<unsigned> OpenedID;

  TypeAttributes() {}
  
  bool isValid() const { return AtLoc.isValid(); }
  
  void clearAttribute(TypeAttrKind A) {
    AttrLocs[A] = SourceLoc();
  }
  
  bool has(TypeAttrKind A) const {
    return getLoc(A).isValid();
  }
  
  SourceLoc getLoc(TypeAttrKind A) const {
    return AttrLocs[A];
  }
  
  void setAttr(TypeAttrKind A, SourceLoc L) {
    assert(!L.isInvalid() && "Cannot clear attribute with this method");
    AttrLocs[A] = L;
  }

  void getAttrLocs(SmallVectorImpl<SourceLoc> &Locs) const {
    for (auto Loc : AttrLocs) {
      if (Loc.isValid())
        Locs.push_back(Loc);
    }
  }

  // This attribute list is empty if no attributes are specified.  Note that
  // the presence of the leading @ is not enough to tell, because we want
  // clients to be able to remove attributes they process until they get to
  // an empty list.
  bool empty() const {
    for (SourceLoc elt : AttrLocs)
      if (elt.isValid()) return false;
    
    return true;
  }
  
  bool hasCC() const { return cc.hasValue(); }
  AbstractCC getAbstractCC() const { return *cc; }
  
  bool hasOwnership() const { return getOwnership() != Ownership::Strong; }
  Ownership getOwnership() const {
    if (has(TAK_sil_weak)) return Ownership::Weak;
    if (has(TAK_sil_unowned)) return Ownership::Unowned;
    return Ownership::Strong;
  }
  
  void clearOwnership() {
    clearAttribute(TAK_sil_weak);
    clearAttribute(TAK_sil_unowned);
  }

  bool hasOpenedID() const { return OpenedID.hasValue(); }
  unsigned getOpenedID() const { return *OpenedID; }
};

  
/// \brief Attributes that may be applied to declarations.
class DeclAttributes {
  /// Source locations for every possible attribute that can be parsed in
  /// source.
  SourceLoc AttrLocs[AK_Count];
  bool HasAttr[AK_Count] = { false };

  unsigned NumAttrsSet : 8;
  unsigned NumVirtualAttrsSet : 8;

public:
  /// The location of the first '@' in the attribute specifier.
  ///
  /// This is an invalid location if the declaration does not have any or has
  /// only virtual attributes.
  ///
  /// This could be a valid location even if none of the attributes are set.
  /// This can happen when the attributes were parsed, but then cleared because
  /// they are not allowed in that context.
  SourceLoc AtLoc;

  StringRef AsmName;

  /// When the mutating attribute is present (i.e., we have a location for it),
  /// this indicates whether it was inverted (@!mutating) or not (@mutating).
  /// Clients should generally use the getMutating() accessor.
  bool MutatingInverted = false;

  /// Source range of the attached comment.  The comment could be located
  /// before or after the declaration).
  CharSourceRange CommentRange;

  DeclAttributes() : NumAttrsSet(0), NumVirtualAttrsSet(0) {}

  bool shouldSaveInAST() const {
    return AtLoc.isValid() || NumAttrsSet != 0;
  }

  bool isEmpty() const {
    return NumAttrsSet == 0;
  }

  bool hasNonVirtualAttributes() const {
    return NumAttrsSet - NumVirtualAttrsSet != 0;
  }

  void clearAttribute(AttrKind A) {
    if (!has(A))
      return;

    AttrLocs[A] = SourceLoc();
    HasAttr[A] = false;

    // Update counters.
    NumAttrsSet--;
    switch (A) {
#define ATTR(X)
#define VIRTUAL_ATTR(X) case AK_ ## X:
      NumVirtualAttrsSet--;
      break;
#include "swift/AST/Attr.def"

    default:
      break;
    }
  }
  
  bool has(AttrKind A) const {
    return HasAttr[A];
  }
  
  SourceLoc getLoc(AttrKind A) const {
    return AttrLocs[A];
  }
  
  void setAttr(AttrKind A, SourceLoc L) {
    bool HadAttribute = has(A);

    AttrLocs[A] = L;
    HasAttr[A] = true;

    if (HadAttribute)
      return;

    // Update counters.
    NumAttrsSet++;
    switch (A) {
#define ATTR(X)
#define VIRTUAL_ATTR(X) case AK_ ## X:
#include "swift/AST/Attr.def"
      NumVirtualAttrsSet++;
      break;

    default:
      break;
    }
  }

  void getAttrLocs(SmallVectorImpl<SourceLoc> &Locs) const {
    for (auto Loc : AttrLocs) {
      if (Loc.isValid())
        Locs.push_back(Loc);
    }
  }

  bool isNoReturn() const { return has(AK_noreturn); }
  bool isAssignment() const { return has(AK_assignment); }
  bool isConversion() const { return has(AK_conversion); }
  bool isTransparent() const {return has(AK_transparent);}
  bool isPrefix() const { return has(AK_prefix); }
  bool isPostfix() const { return has(AK_postfix); }
  bool isInfix() const { return has(AK_infix); }
  bool isObjC() const { return has(AK_objc); }
  bool isIBOutlet() const { return has(AK_IBOutlet); }
  bool isIBAction() const { return has(AK_IBAction); }
  bool isIBLiveView() const { return has(AK_IBLiveView); }
  bool isIBInspectable() const { return has(AK_IBInspectable); }
  bool isClassProtocol() const { return has(AK_class_protocol); }
  bool isWeak() const { return has(AK_weak); }
  bool isUnowned() const { return has(AK_unowned); }
  bool isExported() const { return has(AK_exported); }
  bool isOptional() const { return has(AK_optional); }
  bool requiresStoredPropertyInits() const {
    return has(AK_requires_stored_property_inits);
  }

  bool hasMutating() const { return has(AK_mutating); }
  Optional<bool> getMutating() const {
    if (hasMutating())
      return !MutatingInverted;
    return Nothing;
  }

  Resilience getResilienceKind() const {
    if (has(AK_resilient))
      return Resilience::Resilient;
    if (has(AK_fragile))
      return Resilience::Fragile;
    if (has(AK_born_fragile))
      return Resilience::InherentlyFragile;
    return Resilience::Default;
  }

  
  bool hasOwnership() const { return isWeak() || isUnowned(); }
  Ownership getOwnership() const {
    if (isWeak()) return Ownership::Weak;
    if (isUnowned()) return Ownership::Unowned;
    return Ownership::Strong;
  }
  
  void clearOwnership() {
    clearAttribute(AK_weak);
    clearAttribute(AK_unowned);
  }

  bool hasRawDocComment() const {
    return CommentRange.isValid();
  }

  void print(llvm::raw_ostream &OS) const;
  void print(ASTPrinter &Printer) const;
};
  
} // end namespace swift

#endif
