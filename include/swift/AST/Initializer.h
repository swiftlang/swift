//===--- Initializer.h - Initializer DeclContext ----------------*- C++ -*-===//
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
// This file defines the Initializer class, which is a kind of
// DeclContext used for expressions that are not part of a normal
// code-evaluation context, such as a global initializer or a default
// argument.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_INITIALIZER_H
#define SWIFT_INITIALIZER_H

#include "swift/AST/DeclContext.h"

namespace swift {
class PatternBindingDecl;

enum class InitializerKind : uint8_t {
  /// The initializer expression of a PatternBindingDecl that declares
  /// a global variable or type member.
  PatternBinding,

  /// A function's default argument expression.
  DefaultArgument,
};

/// An Initializer is a kind of DeclContext used for expressions that
/// aren't potentially evaluated as part of some function.
///
/// Generally, Initializers are created lazily, as most initializers
/// don't really require DeclContexts.
class Initializer : public DeclContext {
  unsigned Kind : 1;
protected:
  unsigned SpareBits : 31;
  
  Initializer(InitializerKind kind, DeclContext *parent)
    : DeclContext(DeclContextKind::Initializer, parent),
      Kind(unsigned(kind)) {
  }

public:
  /// Returns the kind of initializer this is.
  InitializerKind getInitializerKind() const {
    return InitializerKind(Kind);
  }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::Initializer;
  }
  static bool classof(const Initializer *I) { return true; }
};

/// The initializer expression of a non-local pattern binding
/// declaration, such as a field or global variable.
class PatternBindingInitializer : public Initializer {
  PatternBindingDecl *Binding;
public:
  explicit PatternBindingInitializer(PatternBindingDecl *binding)
    : Initializer(InitializerKind::PatternBinding,
                  binding->getDeclContext()),
      Binding(binding) {}

  PatternBindingDecl *getBinding() const { return Binding; }

  static bool classof(const DeclContext *DC) {
    if (auto init = dyn_cast<Initializer>(DC))
      return classof(init);
    return false;
  }
  static bool classof(const Initializer *I) {
    return I->getInitializerKind() == InitializerKind::PatternBinding;
  }
};

/// A default argument expression.  The parent context is the function
/// (possibly a closure) for which this is a default argument.
class DefaultArgumentInitializer : public Initializer {
public:
  explicit DefaultArgumentInitializer(DeclContext *parent, unsigned index)
      : Initializer(InitializerKind::DefaultArgument, parent) {
    SpareBits = index;
  }

  unsigned getIndex() const { return SpareBits; }

  static bool classof(const DeclContext *DC) {
    if (auto init = dyn_cast<Initializer>(DC))
      return classof(init);
    return false;
  }
  static bool classof(const Initializer *I) {
    return I->getInitializerKind() == InitializerKind::DefaultArgument;
  }
};
  
} // end namespace swift

#endif
