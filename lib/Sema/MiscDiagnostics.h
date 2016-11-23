//===--- MiscDiagnostics.h - AST-Level Diagnostics --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_MISC_DIAGNOSTICS_H
#define SWIFT_SEMA_MISC_DIAGNOSTICS_H

#include "swift/AST/AttrKind.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"

namespace swift {
  class AbstractFunctionDecl;
  class ApplyExpr;
  class AvailableAttr;
  class CallExpr;
  class DeclContext;
  class Expr;
  class InFlightDiagnostic;
  class Stmt;
  class TypeChecker;
  class ValueDecl;

/// \brief Emit diagnostics for syntactic restrictions on a given expression.
void performSyntacticExprDiagnostics(TypeChecker &TC, const Expr *E,
                                     const DeclContext *DC,
                                     bool isExprStmt);

/// \brief Emit diagnostics for a given statement.
void performStmtDiagnostics(TypeChecker &TC, const Stmt *S);

void performAbstractFuncDeclDiagnostics(TypeChecker &TC,
                                        AbstractFunctionDecl *AFD);
  
/// Emit a fix-it to set the accessibility of \p VD to \p desiredAccess.
///
/// This actually updates \p VD as well.
void fixItAccessibility(InFlightDiagnostic &diag, ValueDecl *VD,
                        Accessibility desiredAccess, bool isForSetter = false);

/// Emit fix-its to correct the argument labels in \p expr, which is the
/// argument tuple or single argument of a call.
///
/// If \p existingDiag is null, the fix-its will be attached to an appropriate
/// error diagnostic.
///
/// \returns true if the issue was diagnosed
bool diagnoseArgumentLabelError(TypeChecker &TC, const Expr *expr,
                                ArrayRef<Identifier> newNames,
                                bool isSubscript,
                                InFlightDiagnostic *existingDiag = nullptr);

/// Emit fix-its to rename the base name at \p referenceRange based on the
/// "renamed" argument in \p attr. If \p call is provided, the argument labels
/// will also be updated.
void fixItAvailableAttrRename(TypeChecker &TC,
                              InFlightDiagnostic &diag,
                              SourceRange referenceRange,
                              const ValueDecl *renamedDecl,
                              const AvailableAttr *attr,
                              const ApplyExpr *call);

/// Attempt to fix the type of \p decl so that it's a valid override for
/// \p base...but only if we're highly confident that we know what the user
/// should have written.
///
/// \returns true iff any fix-its were attached to \p diag.
bool fixItOverrideDeclarationTypes(TypeChecker &TC,
                                   InFlightDiagnostic &diag,
                                   ValueDecl *decl,
                                   const ValueDecl *base);

/// Emit fix-its to enclose trailing closure in argument parens.
void fixItEncloseTrailingClosure(TypeChecker &TC,
                                 InFlightDiagnostic &diag,
                                 const CallExpr *call,
                                 Identifier closureLabel);

/// Run the Availability-diagnostics algorithm otherwise used in an expr
/// context, but for non-expr contexts such as TypeDecls referenced from
/// TypeReprs.
bool diagnoseDeclAvailability(const ValueDecl *Decl,
                              TypeChecker &TC,
                              DeclContext *DC,
                              SourceRange R,
                              bool AllowPotentiallyUnavailableProtocol,
                              bool SignalOnPotentialUnavailability);

} // namespace swift

#endif // SWIFT_SEMA_MISC_DIAGNOSTICS_H

