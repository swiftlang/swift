//===--- ASTWalker.h - Class for walking the AST ----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTWALKER_H
#define SWIFT_AST_ASTWALKER_H

#include "llvm/ADT/PointerUnion.h"
#include <utility>

namespace swift {

class Decl;
class Expr;
class ModuleDecl;
class Stmt;
class Pattern;
class TypeRepr;
struct TypeLoc;
class ParameterList;
enum class AccessKind: unsigned char;

enum class SemaReferenceKind : uint8_t {
  ModuleRef = 0,
  DeclRef,
  DeclMemberRef,
  DeclConstructorRef,
  TypeRef,
  EnumElementRef,
  SubscriptRef,
};

struct ReferenceMetaData {
  SemaReferenceKind Kind;
  llvm::Optional<AccessKind> AccKind;
  bool isImplicit = false;
  ReferenceMetaData(SemaReferenceKind Kind, llvm::Optional<AccessKind> AccKind,
                    bool isImplicit = false)
      : Kind(Kind), AccKind(AccKind), isImplicit(isImplicit) {}
};

/// An abstract class used to traverse an AST.
class ASTWalker {
public:
  enum class ParentKind {
    Module, Decl, Stmt, Expr, Pattern, TypeRepr
  };

  class ParentTy {
    ParentKind Kind;
    void *Ptr = nullptr;

  public:
    ParentTy(ModuleDecl *Mod) : Kind(ParentKind::Module), Ptr(Mod) {}
    ParentTy(Decl *D) : Kind(ParentKind::Decl), Ptr(D) {}
    ParentTy(Stmt *S) : Kind(ParentKind::Stmt), Ptr(S) {}
    ParentTy(Expr *E) : Kind(ParentKind::Expr), Ptr(E) {}
    ParentTy(Pattern *P) : Kind(ParentKind::Pattern), Ptr(P) {}
    ParentTy(TypeRepr *T) : Kind(ParentKind::TypeRepr), Ptr(T) {}
    ParentTy() : Kind(ParentKind::Module), Ptr(nullptr) { }

    bool isNull() const { return Ptr == nullptr; }
    ParentKind getKind() const {
      assert(!isNull());
      return Kind;
    }

    ModuleDecl *getAsModule() const {
      return Kind == ParentKind::Module ? static_cast<ModuleDecl*>(Ptr)
                                        : nullptr;
    }
    Decl *getAsDecl() const {
      return Kind == ParentKind::Decl ? static_cast<Decl*>(Ptr) : nullptr;
    }
    Stmt *getAsStmt() const {
      return Kind == ParentKind::Stmt ? static_cast<Stmt*>(Ptr) : nullptr;
    }
    Expr *getAsExpr() const {
      return Kind == ParentKind::Expr ? static_cast<Expr*>(Ptr) : nullptr;
    }
    Pattern *getAsPattern() const {
      return Kind == ParentKind::Pattern ? static_cast<Pattern*>(Ptr) : nullptr;
    }
    TypeRepr *getAsTypeRepr() const {
      return Kind==ParentKind::TypeRepr ? static_cast<TypeRepr*>(Ptr) : nullptr;
    }
  };

  /// The parent of the node we are visiting.
  ParentTy Parent;

  /// This method is called when first visiting an expression
  /// before walking into its children.
  ///
  /// \param E The expression to check.
  ///
  /// \returns a pair indicating whether to visit the children along with
  /// the expression that should replace this expression in the tree. If the
  /// latter is null, the traversal will be terminated.
  ///
  /// The default implementation returns \c {true, E}.
  virtual std::pair<bool, Expr *> walkToExprPre(Expr *E) {
    return { true, E };
  }

  /// This method is called after visiting an expression's children.
  /// If it returns null, the walk is terminated; otherwise, the
  /// returned expression is spliced in where the old expression
  /// previously appeared.
  ///
  /// The default implementation always returns its argument.
  virtual Expr *walkToExprPost(Expr *E) { return E; }

  /// This method is called when first visiting a statement before
  /// walking into its children.
  ///
  /// \param S The statement to check.
  ///
  /// \returns a pair indicating whether to visit the children along with
  /// the statement that should replace this statement in the tree. If the
  /// latter is null, the traversal will be terminated.
  ///
  /// The default implementation returns \c {true, S}.
  virtual std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) {
    return { true, S };
  }

  /// This method is called after visiting a statement's children.  If
  /// it returns null, the walk is terminated; otherwise, the returned
  /// statement is spliced in where the old statement previously
  /// appeared.
  ///
  /// The default implementation always returns its argument.
  virtual Stmt *walkToStmtPost(Stmt *S) { return S; }

  /// This method is called when first visiting a pattern before walking into
  /// its children.
  ///
  /// \param P The statement to check.
  ///
  /// \returns a pair indicating whether to visit the children along with
  /// the statement that should replace this statement in the tree. If the
  /// latter is null, the traversal will be terminated.
  ///
  /// The default implementation returns \c {true, P}.
  virtual std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) {
    return { true, P };
  }

  /// This method is called after visiting a pattern's children.  If
  /// it returns null, the walk is terminated; otherwise, the returned
  /// pattern is spliced in where the old statement previously
  /// appeared.
  ///
  /// The default implementation always returns its argument.
  virtual Pattern *walkToPatternPost(Pattern *P) { return P; }

  /// walkToDeclPre - This method is called when first visiting a decl, before
  /// walking into its children.  If it returns false, the subtree is skipped.
  ///
  /// \param D The declaration to check. The callee may update this declaration
  /// in-place.
  virtual bool walkToDeclPre(Decl *D) { return true; }

  /// walkToDeclPost - This method is called after visiting the children of a
  /// decl.  If it returns false, the remaining traversal is terminated and
  /// returns failure.
  virtual bool walkToDeclPost(Decl *D) { return true; }

  /// This method is called when first visiting a TypeLoc, before
  /// walking into its TypeRepr children.  If it returns false, the subtree is
  /// skipped.
  ///
  /// \param TL The TypeLoc to check.
  virtual bool walkToTypeLocPre(TypeLoc &TL) { return true; }

  /// This method is called after visiting the children of a TypeLoc.
  /// If it returns false, the remaining traversal is terminated and returns
  /// failure.
  virtual bool walkToTypeLocPost(TypeLoc &TL) { return true; }


  /// This method is called when first visiting a TypeRepr, before
  /// walking into its children.  If it returns false, the subtree is skipped.
  ///
  /// \param T The TypeRepr to check.
  virtual bool walkToTypeReprPre(TypeRepr *T) { return true; }

  /// This method is called after visiting the children of a TypeRepr.
  /// If it returns false, the remaining traversal is terminated and returns
  /// failure.
  virtual bool walkToTypeReprPost(TypeRepr *T) { return true; }

  /// This method configures whether the walker should explore into the generic
  /// params in AbstractFunctionDecl and NominalTypeDecl.
  virtual bool shouldWalkIntoGenericParams() { return false; }

  /// This method configures whether the walker should walk into the
  /// initializers of lazy variables.  These initializers are semantically
  /// different from other initializers in their context and so sometimes
  /// should not be visited.
  ///
  /// Note that visiting the body of the lazy getter will find a
  /// LazyInitializerExpr with the initializer as its sub-expression.
  /// However, ASTWalker does not walk into LazyInitializerExprs on its own.
  virtual bool shouldWalkIntoLazyInitializers() { return true; }

  /// This method configures whether the walker should visit the body of a
  /// non-single expression closure.
  ///
  /// For work that is performed for every top-level expression, this should
  /// be overridden to return false, to avoid duplicating work or visiting
  /// bodies of closures that have not yet been type checked.
  virtual bool shouldWalkIntoNonSingleExpressionClosure() { return true; }

  /// This method configures whether the walker should exhibit the legacy
  /// behavior where accessors appear as peers of their storage, rather
  /// than children nested inside of it.
  ///
  /// Please don't write new ASTWalker implementations that override this
  /// method to return true; instead, refactor existing code as needed
  /// until eventually we can remove this altogether.
  virtual bool shouldWalkAccessorsTheOldWay() { return false; }

  /// walkToParameterListPre - This method is called when first visiting a
  /// ParameterList, before walking into its parameters.  If it returns false,
  /// the subtree is skipped.
  ///
  virtual bool walkToParameterListPre(ParameterList *PL) { return true; }

  /// walkToParameterListPost - This method is called after visiting the
  /// children of a parameter list.  If it returns false, the remaining
  /// traversal is terminated and returns failure.
  virtual bool walkToParameterListPost(ParameterList *PL) { return true; }


protected:
  ASTWalker() = default;
  ASTWalker(const ASTWalker &) = default;
  virtual ~ASTWalker() = default;

  virtual void anchor();
};

} // end namespace swift

#endif
