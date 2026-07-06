//===--- BodyASTSerializer.h - Bitstream body AST serialization -*- C++ -*-===//
//
// This source code is part of the Swift.org open source project
//
// Copyright (c) 2024-2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements BodyASTSerializer, which writes type-checked expression and
// statement AST nodes to an ASTCACHE_BODY_BLOCK_ID block within the bitstream.
// Unlike BodySerializer (which writes a flat string format), this writes
// proper bitstream records using BCRecordLayout, sharing the Serializer's
// TypeID/DeclID/IdentifierID tables.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_BODY_AST_SERIALIZER_H
#define SWIFT_SERIALIZATION_BODY_AST_SERIALIZER_H

#include "swift/AST/ASTNode.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Type.h"
#include "ModuleFormat.h"
#include "llvm/Bitstream/BitstreamWriter.h"
#include <map>

namespace swift {

namespace serialization {

class Serializer;

/// Serializes expression/statement AST nodes to an ASTCACHE_BODY_BLOCK_ID
/// block within the bitstream. Uses the existing Serializer infrastructure
/// (addTypeRef, addDeclRef, addDeclBaseNameRef) for type/decl/identifier
/// references.
class BodyASTSerializer {
  Serializer &S;

  /// Sequential ExprID counter (starts at 1; 0 = null).
  uint32_t NextExprID = 1;

  /// Sequential StmtID counter (starts at 1; 0 = null).
  uint32_t NextStmtID = 1;

  /// Map from Expr* to its assigned ExprID (for dedup / back-references).
  llvm::DenseMap<Expr *, uint32_t> ExprIDs;

  /// Map from Stmt* to its assigned StmtID.
  llvm::DenseMap<Stmt *, uint32_t> StmtIDs;

  /// Assigns (or reuses) an ExprID for the given expression.
  uint32_t assignExprID(Expr *E);

  /// Assigns (or reuses) a StmtID for the given statement.
  uint32_t assignStmtID(Stmt *S);

  /// Serializes an expression node, writing its EXPR_NODE record.
  void serializeExpr(Expr *E);

  /// Serializes a statement node, writing its STMT_NODE record.
  void serializeStmt(Stmt *S);

public:
  BodyASTSerializer(Serializer &S) : S(S) {}

  /// Serializes a function body (BraceStmt) to the body block.
  /// Writes a BODY record {DeclID, rootStmtID} followed by STMT_NODE and
  /// EXPR_NODE records for the body tree.
  void serializeBody(DeclID funcDeclID, BraceStmt *body);

  /// Returns the number of EXPR_NODE records written.
  uint32_t getNumExprs() const { return NextExprID - 1; }

  /// Returns the number of STMT_NODE records written.
  uint32_t getNumStmts() const { return NextStmtID - 1; }
};

} // namespace serialization

} // namespace swift

#endif // SWIFT_SERIALIZATION_BODY_AST_SERIALIZER_H
