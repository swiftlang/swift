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
  
enum class AbstractCC : unsigned char;

/// Marks if a function is a compute kernel, vertex shader, or fragment shader.
enum class KernelOrShaderKind : unsigned char {
  /// Not a kernel or shader.
  Default = 0,
  /// A compute kernel.
  Kernel,
  /// A vertex shader.
  Vertex,
  /// A fragment shader.
  Fragment
};

/// DeclAttributes - These are attributes that may be applied to declarations.
class DeclAttributes {
public:
  /// LSquareLoc/RSquareLoc - This is the location of the '[' and ']' in the
  /// attribute specifier.  If this is an empty attribute specifier, then these
  /// will be invalid locs.
  SourceLoc LSquareLoc, RSquareLoc;

  ResilienceData Resilience;
  StringRef AsmName;
  bool InOut = false;
  bool AutoClosure = false;
  bool Thin = false;
  bool NoReturn = false;
  bool Assignment = false;
  bool Conversion = false;
  bool Transparent = false;
  bool ObjC = false;
  bool ObjCBlock = false;
  bool ExplicitPrefix = false;
  bool ExplicitPostfix = false;
  bool ExplicitInfix = false;
  bool IBOutlet = false;
  bool IBAction = false;
  bool ClassProtocol = false;
  bool Weak = false;
  bool Unowned = false;
  bool LocalStorage = false;
  bool Exported = false;
  Optional<AbstractCC> cc = Nothing;
  KernelOrShaderKind KernelOrShader = KernelOrShaderKind::Default;

  DeclAttributes() {}

  bool isValid() const { return LSquareLoc.isValid(); }

  ResilienceData getResilienceData() const { return Resilience; }
  bool isInOut() const { return InOut; }
  bool isAutoClosure() const { return AutoClosure; }
  bool isThin() const { return Thin; }
  bool isNoReturn() const { return NoReturn; }
  bool isAssignment() const { return Assignment; }
  bool isConversion() const { return Conversion; }
  bool isTransparent() const { return Transparent; }
  bool isPrefix() const { return ExplicitPrefix; }
  bool isPostfix() const { return ExplicitPostfix; }
  bool isInfix() const { return ExplicitInfix; }
  bool isObjC() const { return ObjC; }
  bool isObjCBlock() const { return ObjCBlock; }
  bool isIBOutlet() const { return IBOutlet; }
  bool isIBAction() const { return IBAction; }
  bool isClassProtocol() const { return ClassProtocol; }
  bool isLocalStorage() const { return LocalStorage; }
  bool isWeak() const { return Weak; }
  bool isUnowned() const { return Unowned; }
  bool hasOwnership() const { return Weak || Unowned; }
  Ownership getOwnership() const {
    if (Weak) return Ownership::Weak;
    if (Unowned) return Ownership::Unowned;
    return Ownership::Strong;
  }
  bool isExported() const { return Exported; }
  bool hasCC() const { return cc.hasValue(); }
  AbstractCC getAbstractCC() const { return *cc; }
  KernelOrShaderKind getKernelOrShaderKind() const {
    return KernelOrShader;
  }
  bool isKernel() const {
    return KernelOrShader == KernelOrShaderKind::Kernel;
  }
  bool isVertex() const {
    return KernelOrShader == KernelOrShaderKind::Vertex;
  }
  bool isFragment() const {
    return KernelOrShader == KernelOrShaderKind::Fragment;
  }

  bool empty() const {
    return !isInfix() && !getResilienceData().isValid() && !isInOut() &&
           !isAutoClosure() && !isThin() && !isNoReturn() && !isAssignment() &&
           !isConversion() && !isTransparent() && !isPostfix() && !isPrefix() &&
           !isObjC() && !isObjCBlock() && !isIBOutlet() && !isIBAction() &&
           !isClassProtocol() && !hasCC() && !hasOwnership() &&
           !isLocalStorage() && !isExported() && AsmName.empty() &&
           getKernelOrShaderKind() == KernelOrShaderKind::Default;
  }

  void clearOwnership() {
    Weak = Unowned = false;
  }
};
  
} // end namespace swift

#endif
