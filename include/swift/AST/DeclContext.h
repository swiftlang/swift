//===--- DeclContext.h - Swift Language Context ASTs ------------*- C++ -*-===//
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
// This file defines the DeclContext class.  A DeclContext is the semantic
// construct that a declaration belongs to, such as the enclosing
// FuncExpr or declaration.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DECLCONTEXT_H
#define SWIFT_DECLCONTEXT_H

#include "llvm/ADT/PointerIntPair.h"
#include <cassert>

namespace swift {
  class ASTContext;
  class DeclContext;
  class GenericParamList;
  class Type;
}

namespace llvm {
  template<>
  class PointerLikeTypeTraits<swift::DeclContext*> {
  public:
    static void *getAsVoidPointer(swift::DeclContext* P) {
      return (void*)P;
    }
    static swift::DeclContext *getFromVoidPointer(void *P) {
      return (swift::DeclContext*)P;
    }
    enum { NumLowBitsAvailable = 4 };
  };
}

namespace swift {

// The indentation of the members of this enum describe the inheritance
// hierarchy.  Commented out members are abstract classes.  This formation
// allows for range checks in classof.
enum class DeclContextKind {
  //Module,
    TranslationUnit,
    BuiltinModule,
    ClangModule,
  CapturingExpr,
  NominalTypeDecl,
  ExtensionDecl,
  TopLevelCodeDecl,
  ConstructorDecl,
  DestructorDecl,
  
  First_Module = TranslationUnit, Last_Module = ClangModule,
};
  
/// A DeclContext is an AST object which acts as a semantic container
/// for declarations.  As a policy matter, we currently define
/// contexts broadly: a lambda expression in a function is a new
/// DeclContext, but a new brace statement is not.  There's no
/// particular mandate for this, though.
class DeclContext  {
  // FIXME: Can we use the low bits of Parent for the Kind?

  /// \brief The parent of this context.
  DeclContext *Parent;

  DeclContextKind Kind;

public:
  DeclContext(DeclContextKind Kind, DeclContext *Parent)
    : Parent(Parent), Kind(Kind)
  {
    assert((Parent != 0 || isModuleContext()) &&
           "DeclContext must have a parent unless it is a module!");
  }

  /// Returns the kind of context this is.
  DeclContextKind getContextKind() const {
    return Kind;
  }

  /// Determines whether this context is itself a local scope in a
  /// code block.  A context that appears in such a scope, like a
  /// local type declaration, does not itself become a local context.
  bool isLocalContext() const {
    return getContextKind() == DeclContextKind::CapturingExpr ||
           getContextKind() == DeclContextKind::TopLevelCodeDecl ||
           getContextKind() == DeclContextKind::ConstructorDecl ||
           getContextKind() == DeclContextKind::DestructorDecl;
  }
  
  /// isModuleContext - Return true if this is a subclass of Module.
  bool isModuleContext() const {
    return getContextKind() >= DeclContextKind::First_Module &&
           getContextKind() <= DeclContextKind::Last_Module;
  }

  /// isTypeContext - Return true if this is a type context, e.g., a oneof,
  /// an extension, or a protocol.
  bool isTypeContext() const {
    return getContextKind() == DeclContextKind::NominalTypeDecl ||
           getContextKind() == DeclContextKind::ExtensionDecl;
  }
  
  /// getDeclaredTypeOfContext - For a type context, retrieves the declared
  /// type of the context. Returns a null type for non-type contexts.
  Type getDeclaredTypeOfContext() const;

  /// getDeclaredTypeInContext - For a type context, retrieves the declared
  /// type of the context as visible from within the context. Returns a null
  /// type for non-type contexts.
  Type getDeclaredTypeInContext();

  /// \brief Retrieve the innermost generic parameters introduced by this
  /// context or one of its parent contexts, or null if this context is not
  /// directly dependent on any generic parameters.
  GenericParamList *getGenericParamsOfContext() const;

  /// Returns the semantic parent of this context.  A context has a
  /// parent if and only if it is not a module context.
  DeclContext *getParent() const {
    return Parent;
  }
  
  /// getASTContext - Return the ASTContext for a specified DeclContext by
  /// walking up to the enclosing module and returning its ASTContext.
  ASTContext &getASTContext();
};
  
} // end namespace swift

#endif
