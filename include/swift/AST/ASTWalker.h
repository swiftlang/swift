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

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerUnion.h"
#include <utility>

namespace swift {

class ArgumentList;
class Decl;
class Expr;
class ClosureExpr;
class ModuleDecl;
class Stmt;
class Pattern;
class TypeRepr;
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
  DynamicMemberRef,
};

struct ReferenceMetaData {
  SemaReferenceKind Kind;
  llvm::Optional<AccessKind> AccKind;
  bool isImplicit = false;
  ReferenceMetaData(SemaReferenceKind Kind, llvm::Optional<AccessKind> AccKind,
                    bool isImplicit = false)
      : Kind(Kind), AccKind(AccKind), isImplicit(isImplicit) {}
};

/// Specifies how the initialization expression of a \c lazy variable should be
/// walked by the ASTWalker.
enum class LazyInitializerWalking {
  /// No lazy initialization expressions will be walked.
  None,

  /// The lazy initialization expression will only be walked as a part of
  /// the variable's pattern binding decl. This is the default behavior, and is
  /// consistent with the initializer being syntactically part of the pattern
  /// binding.
  InPatternBinding,

  /// The lazy initialization expression will only be walked as part of the
  /// body of the synthesized accessor for the lazy variable. In such an
  /// accessor, the expression is denoted by LazyInitializerExpr. This is mainly
  /// useful for code emission.
  InAccessor
};

/// An abstract class used to traverse an AST.
class ASTWalker {
public:
  struct Action {
    struct _ContinueWalkAction {};
    struct _SkipChildrenWalkAction {};
    struct _SkipChildrenIfWalkAction { bool Cond; };
    struct _StopIfWalkAction { bool Cond; };
    struct _StopWalkAction {};

    template<typename T> struct _ContinueWalkResult { T Value; };
    template<typename T> struct _SkipChildrenWalkResult { T Value; };
    template<typename T> struct _SkipChildrenIfWalkResult { bool Cond; T Value; };
    template<typename T> struct _StopIfWalkResult { bool Cond; T Value; };

    template<typename T>
    static _ContinueWalkResult<T> Continue(T value) {
      return { std::move(value) };
    }
    template<typename T>
    static _SkipChildrenWalkResult<T> SkipChildren(T value) {
      return { std::move(value) };
    }
    template<typename T>
    static _SkipChildrenIfWalkResult<T> SkipChildrenIf(bool cond, T value) {
      return { cond, std::move(value) };
    }
    template<typename T>
    static _SkipChildrenIfWalkResult<T> VisitChildrenIf(bool cond, T value) {
      return { !cond, std::move(value) };
    }
    template<typename T>
    static _StopIfWalkResult<T> StopIf(bool cond, T value) {
      return { cond, std::move(value) };
    }

    static _ContinueWalkAction Continue() {
      return {};
    }
    static _SkipChildrenWalkAction SkipChildren() {
      return {};
    }
    static _SkipChildrenIfWalkAction SkipChildrenIf(bool cond) {
      return { cond };
    }
    static _SkipChildrenIfWalkAction VisitChildrenIf(bool cond) {
      return { !cond };
    }
    static _StopWalkAction Stop() {
      return {};
    }
    static _StopIfWalkAction StopIf(bool cond) {
      return { cond };
    }
  };

  /// Do not construct directly, use \c Action::<action> instead.
  struct PreWalkAction {
    enum Kind {
      Stop,
      SkipChildren,
      Continue
    };
    Kind Action;

    PreWalkAction(Kind action) : Action(action) {}

    PreWalkAction(Action::_ContinueWalkAction) : Action(Continue) {}
    PreWalkAction(Action::_SkipChildrenWalkAction) : Action(SkipChildren) {}
    PreWalkAction(Action::_StopWalkAction) : Action(Stop) {}

    PreWalkAction(Action::_SkipChildrenIfWalkAction action)
      : Action(action.Cond ? SkipChildren : Continue) {}

    PreWalkAction(Action::_StopIfWalkAction action)
      : Action(action.Cond ? Stop : Continue) {}
  };

  /// Do not construct directly, use \c Action::<action> instead.
  struct PostWalkAction {
    enum Kind {
      Stop,
      Continue
    };
    Kind Action;

    PostWalkAction(Kind action) : Action(action) {}

    PostWalkAction(Action::_ContinueWalkAction) : Action(Continue) {}
    PostWalkAction(Action::_StopWalkAction) : Action(Stop) {}

    PostWalkAction(Action::_StopIfWalkAction action)
      : Action(action.Cond ? Stop : Continue) {}
  };

  template<typename T>
  struct PreWalkResult {
    PreWalkAction Action;
    Optional<T> Value;

    template<typename U, typename std::enable_if<std::is_convertible<U, T>::value>::type * = nullptr>
    PreWalkResult(const PreWalkResult<U> &Other)
      : Action(Other.Action) {
        if (Other.Value)
          Value = *Other.Value;
      }

    template<typename U>
    PreWalkResult(Action::_ContinueWalkResult<U> Result)
      : Action(PreWalkAction::Continue), Value(std::move(Result.Value)) {}

    PreWalkResult(Action::_StopWalkAction)
      : Action(PreWalkAction::Stop), Value(None) {}

    template<typename U>
    PreWalkResult(Action::_SkipChildrenWalkResult<U> Result)
      : Action(PreWalkAction::SkipChildren), Value(std::move(Result.Value)) {}

    template<typename U>
    PreWalkResult(Action::_SkipChildrenIfWalkResult<U> Result)
      : Action(Result.Cond ? PreWalkAction::SkipChildren
                           : PreWalkAction::Continue),
        Value(std::move(Result.Value)) {}

    template<typename U>
    PreWalkResult(Action::_StopIfWalkResult<U> Result)
      : Action(Result.Cond ? PreWalkAction::Stop
                           : PreWalkAction::Continue),
        Value(std::move(Result.Value)) {}
  };
  template<typename T>
  struct PostWalkResult {
    PostWalkAction Action;
    Optional<T> Value;

    template<typename U, typename std::enable_if<std::is_convertible<U, T>::value>::type * = nullptr>
    PostWalkResult(const PostWalkResult<U> &Other)
      : Action(Other.Action) {
        if (Other.Value)
          Value = *Other.Value;
      }

    template<typename U>
    PostWalkResult(Action::_ContinueWalkResult<U> Result)
      : Action(PostWalkAction::Continue), Value(std::move(Result.Value)) {}

    PostWalkResult(Action::_StopWalkAction)
      : Action(PostWalkAction::Stop), Value(None) {}

    template<typename U>
    PostWalkResult(Action::_StopIfWalkResult<U> Result)
      : Action(Result.Cond ? PostWalkAction::Stop
                           : PostWalkAction::Continue),
        Value(std::move(Result.Value)) {}
  };

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
  virtual PreWalkResult<Expr *> walkToExprPre(Expr *E) {
    return Action::Continue(E);
  }

  /// This method is called after visiting an expression's children.
  /// If it returns null, the walk is terminated; otherwise, the
  /// returned expression is spliced in where the old expression
  /// previously appeared.
  ///
  /// The default implementation always returns its argument.
  virtual PostWalkResult<Expr *> walkToExprPost(Expr *E) {
    return Action::Continue(E);
  }

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
  virtual PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) {
    return Action::Continue(S);
  }

  /// This method is called after visiting a statement's children.  If
  /// it returns null, the walk is terminated; otherwise, the returned
  /// statement is spliced in where the old statement previously
  /// appeared.
  ///
  /// The default implementation always returns its argument.
  virtual PostWalkResult<Stmt *> walkToStmtPost(Stmt *S) {
    return Action::Continue(S);
  }

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
  virtual PreWalkResult<Pattern *> walkToPatternPre(Pattern *P) {
    return Action::Continue(P);
  }

  /// This method is called after visiting a pattern's children.  If
  /// it returns null, the walk is terminated; otherwise, the returned
  /// pattern is spliced in where the old statement previously
  /// appeared.
  ///
  /// The default implementation always returns its argument.
  virtual PostWalkResult<Pattern *> walkToPatternPost(Pattern *P) {
    return Action::Continue(P);
  }

  /// walkToDeclPre - This method is called when first visiting a decl, before
  /// walking into its children.  If it returns false, the subtree is skipped.
  ///
  /// \param D The declaration to check. The callee may update this declaration
  /// in-place.
  virtual PreWalkAction walkToDeclPre(Decl *D) {
    return Action::Continue();
  }

  /// walkToDeclPost - This method is called after visiting the children of a
  /// decl.  If it returns false, the remaining traversal is terminated and
  /// returns failure.
  virtual PostWalkAction walkToDeclPost(Decl *D) {
    return Action::Continue();
  }

  /// This method is called when first visiting a TypeRepr, before
  /// walking into its children.  If it returns false, the subtree is skipped.
  ///
  /// \param T The TypeRepr to check.
  virtual PreWalkAction walkToTypeReprPre(TypeRepr *T) {
    return Action::Continue();
  }

  /// This method is called after visiting the children of a TypeRepr.
  /// If it returns false, the remaining traversal is terminated and returns
  /// failure.
  virtual PostWalkAction walkToTypeReprPost(TypeRepr *T) {
    return Action::Continue();
  }

  /// This method configures whether the walker should explore into the generic
  /// params in AbstractFunctionDecl and NominalTypeDecl.
  virtual bool shouldWalkIntoGenericParams() { return false; }

  /// This method configures how the walker should walk the initializers of
  /// lazy variables. These initializers are semantically different from other
  /// initializers in their context and so sometimes should be visited as part
  /// of the synthesized getter, or should not be visited at all.
  virtual LazyInitializerWalking getLazyInitializerWalkingBehavior() {
    return LazyInitializerWalking::InPatternBinding;
  }

  /// This method configures whether the walker should visit the body of a
  /// closure that was checked separately from its enclosing expression.
  ///
  /// For work that is performed for every top-level expression, this should
  /// be overridden to return false, to avoid duplicating work or visiting
  /// bodies of closures that have not yet been type checked.
  virtual bool shouldWalkIntoSeparatelyCheckedClosure(ClosureExpr *) {
    return true;
  }

  /// This method configures whether the walker should visit the body of a
  /// TapExpr.
  virtual bool shouldWalkIntoTapExpression() { return true; }

  /// This method configures whether the walker should visit the underlying
  /// value of a property wrapper placeholder.
  virtual bool shouldWalkIntoPropertyWrapperPlaceholderValue() { return true; }

  /// This method configures whether the walker should visit the capture
  /// initializer expressions within a capture list directly, rather than
  /// walking the declarations.
  virtual bool shouldWalkCaptureInitializerExpressions() { return false; }

  /// This method configures whether the walker should exhibit the legacy
  /// behavior where accessors appear as peers of their storage, rather
  /// than children nested inside of it.
  ///
  /// Please don't write new ASTWalker implementations that override this
  /// method to return true; instead, refactor existing code as needed
  /// until eventually we can remove this altogether.
  virtual bool shouldWalkAccessorsTheOldWay() { return false; }

  /// Whether to walk internal top level decls in serialized modules.
  ///
  /// TODO: Consider changing this to false by default.
  virtual bool shouldWalkSerializedTopLevelInternalDecls() { return true; }

  /// walkToParameterListPre - This method is called when first visiting a
  /// ParameterList, before walking into its parameters.  If it returns false,
  /// the subtree is skipped.
  ///
  virtual PreWalkAction walkToParameterListPre(ParameterList *PL) {
    return Action::Continue();
  }

  /// walkToParameterListPost - This method is called after visiting the
  /// children of a parameter list.  If it returns false, the remaining
  /// traversal is terminated and returns failure.
  virtual PostWalkAction walkToParameterListPost(ParameterList *PL) {
    return Action::Continue();
  }

  /// This method is called when first visiting an argument list before walking
  /// into its arguments.
  ///
  /// \param ArgList The argument list to walk.
  ///
  /// \returns a pair indicating whether to visit the arguments, along with
  /// the argument list that should replace this argument list in the tree. If
  /// the latter is null, the traversal will be terminated.
  ///
  /// The default implementation returns \c {true, ArgList}.
  virtual PreWalkResult<ArgumentList *>
  walkToArgumentListPre(ArgumentList *ArgList) {
    return Action::Continue(ArgList);
  }

  /// This method is called after visiting the arguments in an argument list.
  /// If it returns null, the walk is terminated; otherwise, the
  /// returned argument list is spliced in where the old argument list
  /// previously appeared.
  ///
  /// The default implementation always returns the argument list.
  virtual PostWalkResult<ArgumentList *>
  walkToArgumentListPost(ArgumentList *ArgList) {
    return Action::Continue(ArgList);
  }

protected:
  ASTWalker() = default;
  ASTWalker(const ASTWalker &) = default;
  virtual ~ASTWalker() = default;

  virtual void anchor();
};

} // end namespace swift

#endif
