//===--- CodeCompletionTypeChecking.h  --------------------------*- C++ -*-===//
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
/// \file
/// Provides TypeCheckCompletionCallback implementations for the various kinds
/// of code completion. These extract and persist information needed to compute
/// completion results from the solutions formed during expression typechecking.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_CODECOMPLETIONTYPECHECKING_H
#define SWIFT_SEMA_CODECOMPLETIONTYPECHECKING_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
  class Decl;
  class DeclContext;
  class Type;
  class ValueDecl;
  class CodeCompletionExpr;

  namespace constraints {
    class Solution;
  }

  class TypeCheckCompletionCallback {
    bool GotCallback = false;
  public:
    virtual ~TypeCheckCompletionCallback() {}

    /// Called for each solution produced while  type-checking an expression
    /// that the code completion expression participates in.
    virtual void sawSolution(const constraints::Solution &solution) {
      GotCallback = true;
    };

    /// True if at least one solution was passed via the \c sawSolution
    /// callback.
    bool gotCallback() const { return GotCallback; }

    /// Typecheck the code completion expression in its outermost expression
    /// context, calling \c sawSolution for each solution formed.
    virtual void fallbackTypeCheck(DeclContext *DC);
  };


  /// Used to collect and store information needed to perform member completion
  /// (\c CompletionKind::DotExpr ) from the solutions formed during expression
  /// type-checking.
  class DotExprTypeCheckCompletionCallback: public TypeCheckCompletionCallback {
  public:
    struct Result {
      Type BaseTy;
      ValueDecl* BaseDecl;
      SmallVector<Type, 4> ExpectedTypes;
      bool ExpectsNonVoid;
      bool BaseIsStaticMetaType;
      bool IsImplicitSingleExpressionReturn;
    };

  private:
    CodeCompletionExpr *CompletionExpr;
    SmallVector<Result, 4> Results;
    llvm::DenseMap<std::pair<Type, Decl*>, size_t> BaseToSolutionIdx;

  public:
    DotExprTypeCheckCompletionCallback(CodeCompletionExpr *CompletionExpr)
      : CompletionExpr(CompletionExpr) {}

    /// Get the results collected from any sawSolutions() callbacks recevied so
    /// far.
    ArrayRef<Result> getResults() const { return Results; }

    /// Typecheck the code completion expression in isolation, calling
    /// \c sawSolution for each solution formed.
    void fallbackTypeCheck(DeclContext *DC) override;

    void sawSolution(const constraints::Solution &solution) override;
  };

  /// Used to collect and store information needed to perform unresolved member
  /// completion (\c CompletionKind::UnresolvedMember ) from the solutions
  /// formed during expression type-checking.
  class UnresolvedMemberTypeCheckCompletionCallback: public TypeCheckCompletionCallback {
  public:
    struct Result {
      Type ExpectedTy;
      bool IsImplicitSingleExpressionReturn;
    };

  private:
    CodeCompletionExpr *CompletionExpr;
    SmallVector<Result, 4> Results;

  public:
    UnresolvedMemberTypeCheckCompletionCallback(CodeCompletionExpr *CompletionExpr)
    : CompletionExpr(CompletionExpr) {}

    ArrayRef<Result> getResults() const { return Results; }

    void sawSolution(const constraints::Solution &solution) override;
  };

  class KeyPathTypeCheckCompletionCallback
      : public TypeCheckCompletionCallback {
  public:
    struct Result {
      /// The type on which completion should occur, i.e. a result type of the
      /// previous component.
      Type BaseType;
      /// Whether code completion happens on the key path's root.
      bool OnRoot;
    };

  private:
    KeyPathExpr *KeyPath;
    SmallVector<Result, 4> Results;

  public:
    KeyPathTypeCheckCompletionCallback(KeyPathExpr *KeyPath)
        : KeyPath(KeyPath) {}

    ArrayRef<Result> getResults() const { return Results; }

    void sawSolution(const constraints::Solution &solution) override;
  };

class ArgumentTypeCheckCompletionCallback: public TypeCheckCompletionCallback {
public:
  struct Result {
    /// The type associated with the code completion expression itself.
    Type ExpectedType;
    /// True if this is a subscript rather than a function call.
    bool IsSubscript;
    /// The FuncDecl or SubscriptDecl associated with the call.
    ValueDecl *FuncD;
    /// The type of the function being called.
    Type FuncTy;
    /// The index of the argument containing the completion location
    unsigned ArgIdx;
    /// The index of the parameter corresponding to the completion argument.
    Optional<unsigned> ParamIdx;
    /// The indices of all params that were bound to non-synthesized arguments.
    SmallVector<unsigned, 16> ClaimedParamIndices;
    /// True if the completion is a noninitial term in a variadic argument.
    bool IsNoninitialVariadic;
    /// The base type of the call/subscript (null for free functions).
    Type BaseType;
    /// True if an argument label precedes the completion location.
    bool HasLabel;
  };

private:
  CodeCompletionExpr *CompletionExpr;
  SmallVector<Result, 4> Results;

public:
  ArgumentTypeCheckCompletionCallback(CodeCompletionExpr *CompletionExpr)
  : CompletionExpr(CompletionExpr) {}

  ArrayRef<Result> getResults() const { return Results; }

  void sawSolution(const constraints::Solution &solution) override;
};

}

#endif
