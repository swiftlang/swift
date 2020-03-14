//===--- ExprContextAnalysis.h - Expession context analysis ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_EXPRCONTEXTANALYSIS_H
#define SWIFT_IDE_EXPRCONTEXTANALYSIS_H

#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {
class DeclContext;
class Expr;
class ValueDecl;

namespace ide {
enum class SemanticContextKind;

/// Type check parent contexts of the given decl context, and the body of the
/// given context until \c Loc if the context is a function body.
void typeCheckContextUntil(DeclContext *DC, SourceLoc Loc);

/// From \p DC, find and returns the outer most expression which source range is
/// exact the same as \p TargetRange. Returns \c nullptr if not found.
Expr *findParsedExpr(const DeclContext *DC, SourceRange TargetRange);

/// Returns expected return type of the given decl context.
/// \p DC should be an \c AbstractFunctionDecl or an \c AbstractClosureExpr.
Type getReturnTypeFromContext(const DeclContext *DC);

struct FunctionTypeAndDecl {
  AnyFunctionType *Type;
  ValueDecl *Decl;
  Optional<SemanticContextKind> SemanticContext;

  FunctionTypeAndDecl(AnyFunctionType *Type, ValueDecl *Decl)
      : Type(Type), Decl(Decl) {}
  FunctionTypeAndDecl(AnyFunctionType *Type, ValueDecl *Decl,
                      SemanticContextKind SemanticContext)
      : Type(Type), Decl(Decl), SemanticContext(SemanticContext) {}
};

/// Given an expression and its decl context, the analyzer tries to figure out
/// the expected type of the expression by analyzing its context.
class ExprContextInfo {
  SmallVector<Type, 2> PossibleTypes;
  SmallVector<const AnyFunctionType::Param *, 2> PossibleParams;
  SmallVector<FunctionTypeAndDecl, 2> PossibleCallees;
  bool singleExpressionBody = false;

public:
  ExprContextInfo(DeclContext *DC, Expr *TargetExpr);

  // Returns a list of possible context types.
  ArrayRef<Type> getPossibleTypes() const { return PossibleTypes; }

  /// Whether the type context comes from a single-expression body, e.g.
  /// `foo({ here })`.
  ///
  /// If the input may be incomplete, such as in code-completion, take into
  /// account that the types returned by `getPossibleTypes()` are only a hint.
  bool isSingleExpressionBody() const { return singleExpressionBody; }

  // Returns a list of possible argument label names.
  // Valid only if \c getKind() is \c CallArgument.
  ArrayRef<const AnyFunctionType::Param *> getPossibleParams() const {
    return PossibleParams;
  }

  // Returns a list of possible callee
  // Valid only if \c getKind() is \c CallArgument.
  ArrayRef<FunctionTypeAndDecl> getPossibleCallees() const {
    return PossibleCallees;
  }
};

/// Returns whether \p VD is referenceable with implicit member expression.
bool isReferenceableByImplicitMemberExpr(
    ModuleDecl *CurrModule, DeclContext *DC, Type T, ValueDecl *VD);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_EXPRCONTEXTANALYSIS_H
