//===--- CatchNode.h - An AST node that catches errors -----------*- C++-*-===//
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

#ifndef SWIFT_AST_CATCHNODE_H
#define SWIFT_AST_CATCHNODE_H

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerUnion.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"

namespace swift {

/// An AST node that represents a point where a thrown error can be caught and
/// or rethrown, which includes functions do...catch statements.
class CatchNode: public llvm::PointerUnion<
    AbstractFunctionDecl *, AbstractClosureExpr *, DoCatchStmt *
  > {
public:
  using PointerUnion::PointerUnion;

  /// Determine the thrown error type within the region of this catch node
  /// where it will catch (and possibly rethrow) errors. All of the errors
  /// thrown from within that region will be converted to this error type.
  ///
  /// Returns the thrown error type for a throwing context, or \c llvm::None
  /// if this is a non-throwing context.
  llvm::Optional<Type> getThrownErrorTypeInContext(ASTContext &ctx) const;
};

} // end namespace swift

#endif // SWIFT_AST_CATCHNODE_H
