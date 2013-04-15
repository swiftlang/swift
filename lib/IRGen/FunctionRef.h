//===--- FunctionRef.h - A function declaration reference -------*- C++ -*-===//
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
// This file defines a simple structure encapsulating a reference to a
// specific potentially-globally-accessible Swift function-like entity.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_FUNCTIONREF_H
#define SWIFT_IRGEN_FUNCTIONREF_H

#include "swift/AST/Decl.h"
#include "IRGen.h"

namespace swift {
namespace irgen {

/// A ValueDecl --- either a function, a getter, or a setter --- at a
/// specific explosion and uncurrying level.
class CodeRef {
public:
  enum class Kind {
    Function,
    Getter,
    Setter
  };

private:
  ValueDecl *TheDecl;
  unsigned TheKind : 2;
  unsigned ExplosionLevel : 2;
  unsigned UncurryLevel : 28;

  CodeRef(Kind kind, ValueDecl *theDecl, ExplosionKind explosionLevel,
          unsigned uncurryLevel)
    : TheDecl(theDecl),
      TheKind(unsigned(kind)),
      ExplosionLevel(unsigned(explosionLevel)),
      UncurryLevel(uncurryLevel) {
  }

public:
  CodeRef() = default;

  static CodeRef forFunction(FuncDecl *fn,
                             ExplosionKind explosionLevel,
                             unsigned uncurryLevel) {
    assert(!fn->isGetterOrSetter());
    return CodeRef(Kind::Function, fn, explosionLevel, uncurryLevel);
  }

  static CodeRef forOneOfElement(OneOfElementDecl *fn,
                                 ExplosionKind explosionLevel,
                                 unsigned uncurryLevel) {
    return CodeRef(Kind::Function, fn, explosionLevel, uncurryLevel);
  }

  static CodeRef forConstructor(ConstructorDecl *fn,
                                ExplosionKind explosionLevel,
                                unsigned uncurryLevel) {
    return CodeRef(Kind::Function, fn, explosionLevel, uncurryLevel);
  }

  static CodeRef forGetter(ValueDecl *value,
                           ExplosionKind explosionLevel,
                           unsigned uncurryLevel) {
    assert(isa<VarDecl>(value) || isa<SubscriptDecl>(value));
    return CodeRef(Kind::Getter, value, explosionLevel, uncurryLevel);
  }

  static CodeRef forSetter(ValueDecl *value,
                           ExplosionKind explosionLevel,
                           unsigned uncurryLevel) {
    assert(isa<VarDecl>(value) || isa<SubscriptDecl>(value));
    return CodeRef(Kind::Setter, value, explosionLevel, uncurryLevel);
  }

  ValueDecl *getDecl() const { return TheDecl; }
  Kind getKind() const { return Kind(TheKind); }
  unsigned getUncurryLevel() const { return UncurryLevel; }
  ExplosionKind getExplosionLevel() const {
    return ExplosionKind(ExplosionLevel);
  }

  friend bool operator==(CodeRef left, CodeRef right) {
    return left.getDecl() == right.getDecl() &&
           left.getKind() == right.getKind() &&
           left.getUncurryLevel() == right.getUncurryLevel() &&
           left.getExplosionLevel() == right.getExplosionLevel();
  }
  friend bool operator!=(CodeRef left, CodeRef right) {
    return !(left == right);
  }
};

/// A specialization of CodeRef which asserts that its input is a FuncDecl.
class FunctionRef : public CodeRef {
public:
  FunctionRef() = default;
  FunctionRef(FuncDecl *fn, ExplosionKind explosionLevel, unsigned uncurryLevel)
    : CodeRef(CodeRef::forFunction(fn, explosionLevel, uncurryLevel)) {}

  FuncDecl *getDecl() const { return cast<FuncDecl>(CodeRef::getDecl()); }
};

} // end namespace irgen
} // end namespace swift

#endif
