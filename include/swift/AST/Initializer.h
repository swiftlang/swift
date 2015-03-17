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

  // Expose this to subclasses.
  using DeclContext::setParent;

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

  friend class ASTContext; // calls reset on unused contexts

  void reset(DeclContext *parent) {
    setParent(parent);
    Binding = nullptr;
  }

public:
  explicit PatternBindingInitializer(DeclContext *parent)
    : Initializer(InitializerKind::PatternBinding, parent),
      Binding(nullptr) {
  }
 

  void setBinding(PatternBindingDecl *binding) {
    setParent(binding->getDeclContext());
    Binding = binding;
  }
  
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

/// SerializedPatternBindingInitializer - This represents what was originally a
/// PatternBindingInitializer during serialization. It is preserved as a special
/// class only to maintain the correct AST structure and remangling after
/// deserialization.
class SerializedPatternBindingInitializer : public SerializedLocalDeclContext {
  PatternBindingDecl *Binding;

public:
  SerializedPatternBindingInitializer(PatternBindingDecl *Binding)
    : SerializedLocalDeclContext(LocalDeclContextKind::PatternBindingInitializer,
                                 Binding->getDeclContext()),
      Binding(Binding) {}

  PatternBindingDecl *getBinding() const {
    return Binding;
  }

  static bool classof(const DeclContext *DC) {
    if (auto LDC = dyn_cast<SerializedLocalDeclContext>(DC))
      return LDC->getLocalDeclContextKind() ==
      LocalDeclContextKind::PatternBindingInitializer;
    return false;
  }
};

/// A default argument expression.  The parent context is the function
/// (possibly a closure) for which this is a default argument.
class DefaultArgumentInitializer : public Initializer {
  friend class ASTContext; // calls reset on unused contexts
  void reset(DeclContext *parent, unsigned index) {
    setParent(parent);
    SpareBits = index;
  }

public:
  explicit DefaultArgumentInitializer(DeclContext *parent, unsigned index)
      : Initializer(InitializerKind::DefaultArgument, parent) {
    SpareBits = index;
  }

  unsigned getIndex() const { return SpareBits; }

  /// Change the parent of this context.  This is necessary because
  /// the function signature is parsed before the function
  /// declaration/expression itself is built.
  void changeFunction(DeclContext *parent) {
    assert(parent->isLocalContext());
    setParent(parent);
  }

  static bool classof(const DeclContext *DC) {
    if (auto init = dyn_cast<Initializer>(DC))
      return classof(init);
    return false;
  }
  static bool classof(const Initializer *I) {
    return I->getInitializerKind() == InitializerKind::DefaultArgument;
  }
};

/// SerializedDefaultArgumentInitializer - This represents what was originally a
/// DefaultArgumentInitializer during serialization. It is preserved only to
/// maintain the correct AST structure and remangling after deserialization.
class SerializedDefaultArgumentInitializer : public SerializedLocalDeclContext {
  const unsigned Index;
public:
  SerializedDefaultArgumentInitializer(unsigned Index, DeclContext *Parent)
    : SerializedLocalDeclContext(LocalDeclContextKind::DefaultArgumentInitializer,
                                 Parent),
      Index(Index) {}

  unsigned getIndex() const {
    return Index;
  }

  static bool classof(const DeclContext *DC) {
    if (auto LDC = dyn_cast<SerializedLocalDeclContext>(DC))
      return LDC->getLocalDeclContextKind() ==
        LocalDeclContextKind::DefaultArgumentInitializer;
    return false;
  }
};
  
} // end namespace swift

#endif
