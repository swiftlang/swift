//===--- BodyASTDeserializer.h - Bitstream body AST deserialization -*- C++ -*-===//
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
// Implements BodyASTDeserializer, which reads ASTCACHE_BODY_BLOCK_ID records
// from a bitstream and reconstructs expression/statement AST nodes.
//
// The deserializer operates directly on a bitstream buffer. Type and decl
// references are resolved via callbacks, allowing use without a full
// ModuleFile in unit tests. When integrated with the AST cache, the
// callbacks wrap ModuleFile::getType(typeID) and getDeclChecked(declID).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_BODY_AST_DESERIALIZER_H
#define SWIFT_SERIALIZATION_BODY_AST_DESERIALIZER_H

#include "swift/AST/ASTNode.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Type.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/SubstitutionMap.h"
#include "ModuleFormat.h"
#include <map>

namespace swift {

namespace serialization {

/// Reads body block records from a bitstream and reconstructs AST nodes.
class BodyASTDeserializer {
  ASTContext &Ctx;

  /// Callback: resolve a TypeID to a Type.
  std::function<Type(serialization::TypeID)> ResolveType;

  /// Callback: resolve a DeclID to a Decl*.
  std::function<Decl *(serialization::DeclID)> ResolveDecl;

  /// Callback: resolve an IdentifierID to an Identifier.
  std::function<Identifier(serialization::IdentifierID)> ResolveIdentifier;

  /// Callback: resolve a ProtocolConformanceID to a ProtocolConformanceRef.
  std::function<ProtocolConformanceRef(serialization::ProtocolConformanceID)>
      ResolveConformance;

  /// Callback: resolve a SubstitutionMapID to a SubstitutionMap.
  std::function<SubstitutionMap(serialization::SubstitutionMapID)>
      ResolveSubstitutionMap;
  /// Maps ExprID → reconstructed Expr*.
  std::vector<Expr *> ExprTable;

  /// Maps StmtID → reconstructed Stmt*.
  std::vector<Stmt *> StmtTable;

  /// Reconstructs an expression from a record.
  Expr *deserializeExpr(ArrayRef<uint64_t> record, uint32_t exprID);

  /// Reconstructs a statement from a record.
  Stmt *deserializeStmt(ArrayRef<uint64_t> record, uint32_t stmtID);
  /// Look up a previously deserialized expression by ID.
  Expr *lookupExpr(uint32_t exprID) {
    if (exprID > 0 && exprID <= ExprTable.size())
      return ExprTable[exprID - 1];
    return nullptr;
  }
  /// Look up a previously deserialized statement by ID.
  Stmt *lookupStmt(uint32_t stmtID) {
    if (stmtID > 0 && stmtID <= StmtTable.size())
      return StmtTable[stmtID - 1];
    return nullptr;
  }

public:
  BodyASTDeserializer(
      ASTContext &ctx,
      std::function<Type(serialization::TypeID)> resolveType,
      std::function<Decl *(serialization::DeclID)> resolveDecl,
      std::function<Identifier(serialization::IdentifierID)> resolveIdentifier,
      std::function<ProtocolConformanceRef(serialization::ProtocolConformanceID)>
          resolveConformance = nullptr,
      std::function<SubstitutionMap(serialization::SubstitutionMapID)>
          resolveSubstitutionMap = nullptr)
      : Ctx(ctx), ResolveType(std::move(resolveType)),
        ResolveDecl(std::move(resolveDecl)),
        ResolveIdentifier(std::move(resolveIdentifier)),
        ResolveConformance(std::move(resolveConformance)),
        ResolveSubstitutionMap(std::move(resolveSubstitutionMap)) {}

  /// Deserializes a body block from the given bitstream data.
  /// Returns the root BraceStmt (or nullptr on error).
  BraceStmt *deserializeBody(ArrayRef<uint8_t> bitstreamData);

  /// Deserializes ALL body blocks from the given bitstream data.
  /// Returns a map from DeclID → root BraceStmt.
  /// Each body block has its own ExprID/StmtID space (cleared per block).
  std::vector<std::pair<serialization::DeclID, BraceStmt *>>
  deserializeAllBodies(ArrayRef<uint8_t> bitstreamData);

  /// Returns the number of EXPR_NODE records deserialized.
  size_t getNumExprs() const { return ExprTable.size(); }

  /// Returns the number of STMT_NODE records deserialized.
  size_t getNumStmts() const { return StmtTable.size(); }
};

} // namespace serialization

} // namespace swift

#endif // SWIFT_SERIALIZATION_BODY_AST_DESERIALIZER_H
