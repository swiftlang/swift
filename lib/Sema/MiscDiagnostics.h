//===--- MiscDiagnostics.h - AST-Level Diagnostics ------------------------===//
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

#ifndef SWIFT_SEMA_MISC_DIAGNOSTICS_H
#define SWIFT_SEMA_MISC_DIAGNOSTICS_H

#include "swift/AST/Attr.h"

namespace swift {
  class AbstractFunctionDecl;
  class DeclContext;
  class Expr;
  class InFlightDiagnostic;
  class Stmt;
  class TypeChecker;
  class ValueDecl;

/// \brief Emit diagnostics for a given expression.
void performExprDiagnostics(TypeChecker &TC, const Expr *E,
                            const DeclContext *DC);
/// \brief Emit diagnostics for a given statement.
void performStmtDiagnostics(TypeChecker &TC, const Stmt *S);

void performAbstractFuncDeclDiagnostics(TypeChecker &TC,
                                        AbstractFunctionDecl *AFD);
  
/// Emit a fix-it to set the accessibility of \p VD to \p desiredAccess.
///
/// This actually updates \p VD as well.
void fixItAccessibility(InFlightDiagnostic &diag, ValueDecl *VD,
                        Accessibility desiredAccess, bool isForSetter = false);

} // namespace swift

#endif // SWIFT_SEMA_MISC_DIAGNOSTICS_H

