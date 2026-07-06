//===--- BodyASTSerializer.cpp - Bitstream body AST serialization --------===//
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

#include "BodyASTSerializer.h"
#include "Serialization.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Type.h"
#include "swift/AST/FunctionRefInfo.h"
#include "swift/AST/Identifier.h"
#include "llvm/Bitcode/BitcodeConvenience.h"

using namespace swift;
using namespace swift::serialization;

uint32_t BodyASTSerializer::assignExprID(Expr *E) {
  if (!E)
    return 0;
  auto it = ExprIDs.find(E);
  if (it != ExprIDs.end())
    return it->second;
  uint32_t id = NextExprID++;
  ExprIDs[E] = id;
  return id;
}

uint32_t BodyASTSerializer::assignStmtID(Stmt *stmt) {
  if (!stmt)
    return 0;
  auto it = StmtIDs.find(stmt);
  if (it != StmtIDs.end())
    return it->second;
  uint32_t id = NextStmtID++;
  StmtIDs[stmt] = id;
  return id;
}

void BodyASTSerializer::serializeExpr(Expr *E) {
  if (!E)
    return;

  uint32_t exprID = assignExprID(E);

  TypeID typeID = 0;
  if (E->getType())
    typeID = S.addTypeRef(E->getType());

  bool implicit = E->isImplicit();

  auto &Out = S.getWriter();
  auto &Scratch = S.getScratchRecord();
  Scratch.clear();

  using namespace astcache_body_block;

  switch (E->getKind()) {
  case ExprKind::DeclRef: {
    auto *DRE = cast<DeclRefExpr>(E);
    DeclID declID = S.addDeclRef(DRE->getDecl());
    uint8_t fri = DRE->getFunctionRefInfo().getOpaqueValue();
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_DeclRef);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(declID);
    Scratch.push_back(fri);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::IntegerLiteral: {
    auto *ILE = cast<IntegerLiteralExpr>(E);
    IdentifierID iid = S.addDeclBaseNameRef(
        DeclBaseName(S.getASTContext().getIdentifier(ILE->getDigitsText())));
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_IntegerLiteral);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(iid);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::MemberRef: {
    auto *MRE = cast<MemberRefExpr>(E);
    // Serialize the base expression first (depth-first).
    serializeExpr(MRE->getBase());
    uint32_t baseExprID = assignExprID(MRE->getBase());
    DeclID memberDeclID = S.addDeclRef(MRE->getMember().getDecl());
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_MemberRef);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Scratch.push_back(baseExprID);
    Scratch.push_back(memberDeclID);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  case ExprKind::Type: {
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Type);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
  default:
    Scratch.push_back(exprID);
    Scratch.push_back(Expr_Error);
    Scratch.push_back(typeID);
    Scratch.push_back(implicit ? 1u : 0u);
    Out.EmitRecord(EXPR_NODE, Scratch);
    break;
  }
}

void BodyASTSerializer::serializeStmt(Stmt *stmt) {
  if (!stmt)
    return;

  uint32_t stmtID = assignStmtID(stmt);

  auto &Out = S.getWriter();
  auto &Scratch = S.getScratchRecord();
  Scratch.clear();

  using namespace astcache_body_block;

  switch (stmt->getKind()) {
  case StmtKind::Brace: {
    auto *BS = cast<BraceStmt>(stmt);
    auto elements = BS->getElements();
    // Serialize sub-expressions/sub-statements FIRST, collecting their IDs.
    // We can't interleave serializeExpr() calls with Scratch building because
    // serializeExpr() clears and overwrites Scratch.
    SmallVector<std::pair<uint32_t, uint32_t>, 8> elementIDs;
    for (auto &elt : elements) {
      if (isa<Expr *>(elt)) {
        Expr *E = cast<Expr *>(elt);
        uint32_t eid = assignExprID(E);
        serializeExpr(E);
        elementIDs.push_back({0u, eid}); // elementKind = expr
      } else if (isa<Stmt *>(elt)) {
        Stmt *sub = cast<Stmt *>(elt);
        uint32_t sid = assignStmtID(sub);
        serializeStmt(sub);
        elementIDs.push_back({1u, sid}); // elementKind = stmt
      } else {
        elementIDs.push_back({2u, 0u}); // elementKind = decl/other
      }
    }
    // NOW build the BraceStmt record.
    Scratch.clear();
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Brace);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(static_cast<uint32_t>(elementIDs.size()));
    for (auto &pair : elementIDs) {
      Scratch.push_back(pair.first);
      Scratch.push_back(pair.second);
    }
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  case StmtKind::Return: {
    auto *RS = cast<ReturnStmt>(stmt);
    bool hasResult = RS->hasResult();
    uint32_t resultExprID = 0;
    if (hasResult) {
      resultExprID = assignExprID(RS->getResult());
      serializeExpr(RS->getResult());
    }
    Scratch.clear(); // Clear after serializeExpr which may have used Scratch
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Return);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Scratch.push_back(hasResult ? 1u : 0u);
    if (hasResult)
      Scratch.push_back(resultExprID);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
  default:
    Scratch.push_back(stmtID);
    Scratch.push_back(Stmt_Error);
    Scratch.push_back(stmt->isImplicit() ? 1u : 0u);
    Out.EmitRecord(STMT_NODE, Scratch);
    break;
  }
}

void BodyASTSerializer::serializeBody(DeclID funcDeclID, BraceStmt *body) {
  auto &Out = S.getWriter();

  // Enter the body block.
  llvm::BCBlockRAII block(Out, ASTCACHE_BODY_BLOCK_ID, 4);

  uint32_t rootStmtID = 0;
  if (body) {
    rootStmtID = assignStmtID(body);
    serializeStmt(body);
  }

  // Write the BODY record: {DeclID, rootStmtID}
  auto &Scratch = S.getScratchRecord();
  Scratch.clear();
  Scratch.push_back(funcDeclID);
  Scratch.push_back(rootStmtID);
  Out.EmitRecord(astcache_body_block::BODY, Scratch);
}
