//===--- BodyASTDeserializer.cpp - Bitstream body AST deserialization ----===//
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

#include "BodyASTDeserializer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ArgumentList.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Type.h"
#include "swift/AST/FunctionRefInfo.h"
#include "swift/AST/Identifier.h"
#include "ModuleFormat.h"
#include "llvm/Bitstream/BitstreamReader.h"

using namespace swift;
using namespace swift::serialization;

Expr *BodyASTDeserializer::deserializeExpr(ArrayRef<uint64_t> record,
                                            uint32_t exprID) {
  using namespace astcache_body_block;

  if (record.size() < 4)
    return nullptr;

  uint8_t kind = static_cast<uint8_t>(record[1]);
  TypeID typeID = static_cast<TypeID>(record[2]);
  bool implicit = record[3] != 0;

  Type ty;
  if (typeID != 0 && ResolveType)
    ty = ResolveType(typeID);

  Expr *result = nullptr;

  switch (kind) {
  case Expr_DeclRef: {
    // {ExprID, ExprKind, TypeID, implicit, DeclID, FunctionRefInfo}
    if (record.size() < 6)
      return nullptr;
    DeclID declID = static_cast<DeclID>(record[4]);
    uint8_t fri = static_cast<uint8_t>(record[5]);
    Decl *D = ResolveDecl ? ResolveDecl(declID) : nullptr;
    auto *VD = dyn_cast_or_null<ValueDecl>(D);
    if (!VD)
      return nullptr;
    result = new (Ctx) DeclRefExpr(VD, DeclNameLoc(SourceLoc()), implicit,
                                   AccessSemantics::Ordinary, ty);
    if (auto *DRE = dyn_cast<DeclRefExpr>(result))
      DRE->setFunctionRefInfo(FunctionRefInfo::fromOpaque(fri));
    break;
  }
  case Expr_IntegerLiteral: {
    // {ExprID, ExprKind, TypeID, implicit, IdentifierID}
    if (record.size() < 5)
      return nullptr;
    IdentifierID iid = static_cast<IdentifierID>(record[4]);
    std::string digitsStr;
    if (ResolveIdentifier)
      digitsStr = ResolveIdentifier(iid).str().str();
    result = new (Ctx) IntegerLiteralExpr(digitsStr, SourceLoc(), implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_MemberRef: {
    // {ExprID, ExprKind, TypeID, implicit, baseExprID, memberDeclID}
    if (record.size() < 6)
      return nullptr;
    uint32_t baseExprID = static_cast<uint32_t>(record[4]);
    DeclID memberDeclID = static_cast<DeclID>(record[5]);
    Expr *base = (baseExprID > 0 && baseExprID <= ExprTable.size())
                     ? ExprTable[baseExprID - 1]
                     : nullptr;
    if (!base)
      return nullptr;
    Decl *D = ResolveDecl ? ResolveDecl(memberDeclID) : nullptr;
    auto *VD = dyn_cast_or_null<ValueDecl>(D);
    if (!VD)
      return nullptr;
    result = new (Ctx) MemberRefExpr(base, SourceLoc(), VD, DeclNameLoc(),
                                     implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_Type: {
    // {ExprID, ExprKind, TypeID, implicit}
    if (!ty) {
      result = new (Ctx) ErrorExpr(SourceRange());
      break;
    }
    result = TypeExpr::createImplicit(ty, Ctx);
    if (!implicit)
      result->setImplicit(false);
    break;
  }
  case Expr_Binary: {
    // {ExprID, ExprKind, TypeID, implicit, lhsExprID, rhsExprID, fnExprID}
    if (record.size() < 7)
      return nullptr;
    uint32_t lhsID = static_cast<uint32_t>(record[4]);
    uint32_t rhsID = static_cast<uint32_t>(record[5]);
    uint32_t fnID = static_cast<uint32_t>(record[6]);
    Expr *lhs = lookupExpr(lhsID);
    Expr *rhs = lookupExpr(rhsID);
    Expr *fn = (fnID > 0) ? lookupExpr(fnID) : nullptr;
    if (!lhs || !rhs)
      return nullptr;
    result = BinaryExpr::create(Ctx, lhs, fn, rhs, implicit, ty);
    break;
  }
  case Expr_Call: {
    // {ExprID, ExprKind, TypeID, implicit, fnExprID, numArgs, [argExprIDs...]}
    if (record.size() < 6)
      return nullptr;
    uint32_t fnID = static_cast<uint32_t>(record[4]);
    uint32_t numArgs = static_cast<uint32_t>(record[5]);
    Expr *fn = lookupExpr(fnID);
    if (!fn)
      return nullptr;
    SmallVector<Expr*, 4> argExprs;
    for (uint32_t i = 0; i < numArgs; i++) {
      if (record.size() <= 6 + i)
        return nullptr;
      Expr *arg = lookupExpr(static_cast<uint32_t>(record[6 + i]));
      if (!arg)
        return nullptr;
      argExprs.push_back(arg);
    }
    auto *argList = ArgumentList::forImplicitUnlabeled(Ctx, argExprs);
    result = CallExpr::createImplicit(Ctx, fn, argList);
    result->setImplicit(implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_Assign: {
    // {ExprID, ExprKind, TypeID, implicit, destExprID, srcExprID}
    if (record.size() < 6)
      return nullptr;
    Expr *dest = lookupExpr(static_cast<uint32_t>(record[4]));
    Expr *src = lookupExpr(static_cast<uint32_t>(record[5]));
    if (!dest || !src)
      return nullptr;
    result = new (Ctx) AssignExpr(dest, SourceLoc(), src, implicit);
    if (ty)
      result->setType(ty);
    break;
  }
  case Expr_InOut: {
    // {ExprID, ExprKind, TypeID, implicit, subExprID}
    if (record.size() < 5)
      return nullptr;
    Expr *sub = lookupExpr(static_cast<uint32_t>(record[4]));
    if (!sub)
      return nullptr;
    result = new (Ctx) InOutExpr(SourceLoc(), sub, Type(), implicit);
    break;
  }
  case Expr_DotSyntaxCall: {
    // {ExprID, ExprKind, TypeID, implicit, fnExprID, baseExprID}
    if (record.size() < 6)
      return nullptr;
    Expr *fn = lookupExpr(static_cast<uint32_t>(record[4]));
    Expr *base = lookupExpr(static_cast<uint32_t>(record[5]));
    if (!fn || !base)
      return nullptr;
    result = DotSyntaxCallExpr::create(Ctx, fn, SourceLoc(),
                                       Argument::unlabeled(base), ty);
    result->setImplicit(implicit);
    break;
  }
  default:
    result = new (Ctx) ErrorExpr(SourceRange());
    break;
  }

  if (result && exprID > 0) {
    if (exprID > ExprTable.size())
      ExprTable.resize(exprID);
    ExprTable[exprID - 1] = result;
  }

  return result;
}

Stmt *BodyASTDeserializer::deserializeStmt(ArrayRef<uint64_t> record,
                                            uint32_t stmtID) {
  using namespace astcache_body_block;

  if (record.size() < 3)
    return nullptr;

  uint8_t kind = static_cast<uint8_t>(record[1]);
  bool implicit = record[2] != 0;

  Stmt *result = nullptr;

  switch (kind) {
  case Stmt_Brace: {
    // {StmtID, StmtKind, implicit, numElements, [elementKind + elementID...]}
    if (record.size() < 4)
      return nullptr;
    uint32_t numElements = static_cast<uint32_t>(record[3]);
    SmallVector<ASTNode, 8> elements;
    unsigned idx = 4;
    for (uint32_t i = 0; i < numElements && idx + 1 < record.size(); ++i) {
      uint32_t eltKind = static_cast<uint32_t>(record[idx]);
      uint32_t eltID = static_cast<uint32_t>(record[idx + 1]);
      idx += 2;
      if (eltKind == 0) {
        Expr *E = (eltID > 0 && eltID <= ExprTable.size())
                     ? ExprTable[eltID - 1]
                     : nullptr;
        if (E)
          elements.push_back(ASTNode(E));
      } else if (eltKind == 1) {
        Stmt *S = (eltID > 0 && eltID <= StmtTable.size())
                      ? StmtTable[eltID - 1]
                      : nullptr;
        if (S)
          elements.push_back(ASTNode(S));
      }
      // eltKind == 2 (decl/other): skip
    }
    result = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                               implicit ? std::optional<bool>(true)
                                        : std::nullopt);
    break;
  }
  case Stmt_Return: {
    // {StmtID, StmtKind, implicit, hasResult, [resultExprID]}
    if (record.size() < 4)
      return nullptr;
    bool hasResult = record[3] != 0;
    Expr *resultExpr = nullptr;
    if (hasResult && record.size() >= 5) {
      uint32_t resultExprID = static_cast<uint32_t>(record[4]);
      resultExpr = (resultExprID > 0 && resultExprID <= ExprTable.size())
                       ? ExprTable[resultExprID - 1]
                       : nullptr;
    }
    if (implicit)
      result = ReturnStmt::createImplicit(Ctx, resultExpr);
    else
      result = ReturnStmt::createParsed(Ctx, SourceLoc(), resultExpr);
    break;
  }
  default:
    return nullptr;
  }

  if (result && stmtID > 0) {
    if (stmtID > StmtTable.size())
      StmtTable.resize(stmtID);
    StmtTable[stmtID - 1] = result;
  }

  return result;
}

BraceStmt *BodyASTDeserializer::deserializeBody(ArrayRef<uint8_t> bitstreamData) {
  // Create a bitstream cursor from the data.
  // The data may start with a 4-byte SWIFTMODULE_SIGNATURE.
  // Skip it if present.
  ArrayRef<uint8_t> data = bitstreamData;
  if (data.size() >= 4 && data[0] == 0xE2 && data[1] == 0x9C &&
      data[2] == 0xA8 && data[3] == 0x0E) {
    data = data.slice(4);
  }

  llvm::BitstreamCursor cursor(data);

  // Advance to find the ASTCACHE_BODY_BLOCK_ID sub-block.
  while (!cursor.AtEndOfStream()) {
    llvm::Expected<llvm::BitstreamEntry> maybeEntry = cursor.advance();
    if (!maybeEntry) {
      consumeError(maybeEntry.takeError());
      return nullptr;
    }
    llvm::BitstreamEntry entry = maybeEntry.get();

    if (entry.Kind == llvm::BitstreamEntry::SubBlock) {
      if (entry.ID == ASTCACHE_BODY_BLOCK_ID) {
        if (llvm::Error Err = cursor.EnterSubBlock(ASTCACHE_BODY_BLOCK_ID)) {
          consumeError(std::move(Err));
          return nullptr;
        }
        break;
      }
      // Skip other sub-blocks.
      if (llvm::Error Err = cursor.SkipBlock()) {
        consumeError(std::move(Err));
        return nullptr;
      }
      continue;
    }
    if (entry.Kind == llvm::BitstreamEntry::Error)
      return nullptr;
    // Skip records and end blocks.
  }

  if (cursor.AtEndOfStream())
    return nullptr;

  // Read records within the body block.
  SmallVector<uint64_t, 64> scratch;
  uint32_t rootStmtID = 0;

  while (true) {
    llvm::Expected<llvm::BitstreamEntry> maybeEntry =
        cursor.advance(llvm::BitstreamCursor::AF_DontPopBlockAtEnd);
    if (!maybeEntry) {
      consumeError(maybeEntry.takeError());
      break;
    }
    llvm::BitstreamEntry entry = maybeEntry.get();

    if (entry.Kind == llvm::BitstreamEntry::EndBlock)
      break;
    if (entry.Kind != llvm::BitstreamEntry::Record)
      break;

    scratch.clear();
    llvm::Expected<unsigned> maybeRecordID =
        cursor.readRecord(entry.ID, scratch);
    if (!maybeRecordID) {
      consumeError(maybeRecordID.takeError());
      break;
    }
    unsigned recordID = maybeRecordID.get();

    if (recordID == astcache_body_block::BODY) {
      if (scratch.size() >= 2)
        rootStmtID = static_cast<uint32_t>(scratch[1]);
    } else if (recordID == astcache_body_block::EXPR_NODE) {
      if (scratch.size() >= 1) {
        uint32_t exprID = static_cast<uint32_t>(scratch[0]);
        deserializeExpr(scratch, exprID);
      }
    } else if (recordID == astcache_body_block::STMT_NODE) {
      if (scratch.size() >= 1) {
        uint32_t stmtID = static_cast<uint32_t>(scratch[0]);
        deserializeStmt(scratch, stmtID);
      }
    }
  }

  // Look up the root BraceStmt.
  if (rootStmtID > 0 && rootStmtID <= StmtTable.size()) {
    if (auto *BS = dyn_cast_or_null<BraceStmt>(StmtTable[rootStmtID - 1]))
      return BS;
  }
  return nullptr;
}
