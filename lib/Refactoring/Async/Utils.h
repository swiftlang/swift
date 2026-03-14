//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFACTORING_ASYNCUTILS_H
#define SWIFT_REFACTORING_ASYNCUTILS_H

#include "swift/AST/Stmt.h"

namespace swift {
namespace refactoring {
namespace asyncrefactorings {

/// Whether or not the given statement starts a new scope. Note that most
/// statements are handled by the \c BraceStmt check. The others listed are
/// a somewhat special case since they can also declare variables in their
/// condition.
bool startsNewScope(Stmt *S);

/// A more aggressive variant of \c Expr::getReferencedDecl that also looks
/// through autoclosures created to pass the \c self parameter to a member funcs
ValueDecl *getReferencedDeclLookingThroughAutoclosures(const Expr *Fn);

FuncDecl *getUnderlyingFunc(const Expr *Fn);

} // namespace asyncrefactorings
} // namespace refactoring
} // namespace swift

#endif
