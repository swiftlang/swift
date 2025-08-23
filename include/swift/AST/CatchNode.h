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

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/PointerUnion.h"
#include <optional>

namespace swift {

namespace detail {
using CatchNodeBase = llvm::PointerUnion<AbstractFunctionDecl *, ClosureExpr *,
                                         DoCatchStmt *, AnyTryExpr *>;
} // end namespace detail

/// An AST node that represents a point where a thrown error can be caught and
/// or rethrown, which includes functions do...catch statements.
class CatchNode : public detail::CatchNodeBase {
public:
  using Base = detail::CatchNodeBase;
  using PointerUnion::PointerUnion;

  /// Determine the thrown error type within the region of this catch node
  /// where it will catch (and possibly rethrow) errors. All of the errors
  /// thrown from within that region will be converted to this error type.
  ///
  /// Returns the thrown error type for a throwing context, or \c std::nullopt
  /// if this is a non-throwing context.
  std::optional<Type> getThrownErrorTypeInContext(ASTContext &ctx) const;

  /// Determines the explicitly-specified type error that will be caught by
  /// this catch node.
  ///
  /// Returns the explicitly-caught type, or a NULL type if the caught type
  /// needs to be inferred.
  Type getExplicitCaughtType(ASTContext &ctx) const;

  /// Returns the explicitly-specified type error that will be caught by this
  /// catch node, or `nullopt` if it has not yet been computed. This should only
  /// be used for dumping.
  std::optional<Type> getCachedExplicitCaughtType(ASTContext &ctx) const;

  friend llvm::hash_code hash_value(CatchNode catchNode) {
    using llvm::hash_value;
    return hash_value(catchNode.getOpaqueValue());
  }
};

void simple_display(llvm::raw_ostream &out, CatchNode catchNode);

SourceLoc extractNearestSourceLoc(CatchNode catchNode);

} // end namespace swift

namespace llvm {

using swift::CatchNode;

/// `isa`, `dyn_cast`, `cast` for `CatchNode`.
template <typename To>
struct CastInfo<To, CatchNode> : public CastInfo<To, CatchNode::Base> {};
template <typename To>
struct CastInfo<To, const CatchNode>
    : public CastInfo<To, const CatchNode::Base> {};

} // end namespace llvm

#endif // SWIFT_AST_CATCHNODE_H
