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
  class FuncExpr;
  class TranslationUnitDecl;

enum class DeclContextKind {
  TranslationUnitDecl,
  FuncExpr,
  OneOfType,
  ProtocolType
};

/// A DeclContext is an AST object which acts as a semantic container
/// for declarations.  As a policy matter, we currently define
/// contexts broadly: a lambda expression in a function is a new
/// DeclContext, but a new brace statement is not.  There's no
/// particular mandate for this, though.
class DeclContext {
  llvm::PointerIntPair<DeclContext*, 2, unsigned> ParentAndKind;

public:
  DeclContext(DeclContextKind Kind, DeclContext *Parent)
    : ParentAndKind(Parent, static_cast<unsigned>(Kind)) {
    assert(Parent || Kind == DeclContextKind::TranslationUnitDecl);
  }

  /// Returns the kind of context this is.
  DeclContextKind getContextKind() const {
    return DeclContextKind(ParentAndKind.getInt());
  }

  /// Determines whether this context is itself a local scope in a
  /// code block.  A context that appears in such a scope, like a
  /// local type declaration, does not itself become a local context.
  bool isLocalContext() const {
    return getContextKind() == DeclContextKind::FuncExpr;
  }

  /// Returns the semantic parent of this context.  A context has a
  /// parent if and only if it is not a translation unit context.
  DeclContext *getParent() const {
    return ParentAndKind.getPointer();
  }
  
  /// getASTContext - Return the ASTContext for a specified DeclContetx by
  /// walking up to the translation unit and returning its ASTContext.
  ASTContext &getASTContext();
};
  
} // end namespace swift

#endif
