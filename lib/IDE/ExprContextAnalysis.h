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
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {
class DeclContext;
class Expr;
class ValueDecl;
class AnyFunctionType;

namespace ide {

/// Type check parent contexts of the given decl context, and the body of the
/// given context until \c Loc if the context is a function body.
void typeCheckContextUntil(DeclContext *DC, SourceLoc Loc);

/// Returns expected return type of the given decl context.
/// \p DC should be an \c AbstractFunctionDecl or an \c AbstractClosureExpr.
Type getReturnTypeFromContext(const DeclContext *DC);

using FunctionTypeAndDecl = std::pair<AnyFunctionType *, ValueDecl *>;

/// Given an expression and its decl context, the analyzer tries to figure out
/// the expected type of the expression by analyzing its context.
class ExprContextInfo {
  SmallVector<Type, 2> PossibleTypes;
  SmallVector<StringRef, 2> PossibleNames;
  SmallVector<FunctionTypeAndDecl, 2> PossibleCallees;

public:
  ExprContextInfo(DeclContext *DC, Expr *TargetExpr);

  // Returns a list of possible context types.
  ArrayRef<Type> getPossibleTypes() const { return PossibleTypes; }

  // Returns a list of possible argument label names.
  // Valid only if \c getKind() is \c CallArgument.
  ArrayRef<StringRef> getPossibleNames() const { return PossibleNames; }

  // Returns a list of possible callee
  // Valid only if \c getKind() is \c CallArgument.
  ArrayRef<FunctionTypeAndDecl> getPossibleCallees() const {
    return PossibleCallees;
  }
};

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_EXPRCONTEXTANALYSIS_H
