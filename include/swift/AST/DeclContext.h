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
    enum { NumLowBitsAvailable = 3 };
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
  //CapturingExpr,
    FuncExpr,
    //ClosureExpr,
      ExplicitClosureExpr,
      ImplicitClosureExpr,
  OneOfType,
  ExtensionDecl,
  ProtocolType,
  
  First_Module = TranslationUnit, Last_Module = BuiltinModule,
  First_Capturing = FuncExpr, Last_Capturing = ImplicitClosureExpr,
  First_Closure = ExplicitClosureExpr, Last_Closure = ImplicitClosureExpr
};
  
/// A DeclContext is an AST object which acts as a semantic container
/// for declarations.  As a policy matter, we currently define
/// contexts broadly: a lambda expression in a function is a new
/// DeclContext, but a new brace statement is not.  There's no
/// particular mandate for this, though.
class DeclContext {
  llvm::PointerIntPair<DeclContext*, 3, unsigned> ParentAndKind;

public:
  DeclContext(DeclContextKind Kind, DeclContext *Parent)
    : ParentAndKind(Parent, static_cast<unsigned>(Kind)) {
    assert((Parent != 0 || isModuleContext()) &&
           "DeclContext must have a parent unless it is a module!");
  }

  /// Returns the kind of context this is.
  DeclContextKind getContextKind() const {
    return DeclContextKind(ParentAndKind.getInt());
  }

  /// Determines whether this context is itself a local scope in a
  /// code block.  A context that appears in such a scope, like a
  /// local type declaration, does not itself become a local context.
  bool isLocalContext() const {
    return getContextKind() >= DeclContextKind::First_Capturing &&
           getContextKind() <= DeclContextKind::Last_Capturing;
  }
  
  /// isModuleContext - Return true if this is a subclass of Module.
  bool isModuleContext() const {
    return getContextKind() >= DeclContextKind::First_Module &&
           getContextKind() <= DeclContextKind::Last_Module;
  }

  /// Returns the semantic parent of this context.  A context has a
  /// parent if and only if it is not a module context.
  DeclContext *getParent() const {
    return ParentAndKind.getPointer();
  }
  
  /// getASTContext - Return the ASTContext for a specified DeclContext by
  /// walking up to the enclosing module and returning its ASTContext.
  ASTContext &getASTContext();
};
  
} // end namespace swift

#endif
