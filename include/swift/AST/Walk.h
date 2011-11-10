//===--- Walk.h - Support structures for walking the AST --------*- C++ -*-===//
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

#ifndef SWIFT_AST_WALK_H
#define SWIFT_AST_WALK_H

#include "llvm/ADT/PointerUnion.h"

namespace swift {

class Expr;
class Stmt;
  
/// This enum is used in AST traversals.
enum class WalkOrder {
  PreOrder,
  PostOrder
};

/// \brief Provides additional information about the context of an AST
/// walk.
struct WalkContext {
  /// \brief The parent of the node we are visiting.
  llvm::PointerUnion<Expr *, Stmt *> Parent;
};

/// \brief Function type used to describe a callback for walking an expression.
typedef Expr *WalkExprType(Expr *E, WalkOrder Order, 
                           WalkContext const& WalkCtx);

/// \brief Function type used to describe a callback for walking a statement.
typedef Stmt *WalkStmtType(Stmt *S, WalkOrder Order,
                           WalkContext const &WalkCtx);

} // end namespace swift

#endif
