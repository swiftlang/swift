//===--- BodySerializer.cpp - AST body serialization for cache --*- C++-*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
#include "BodySerializer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTDemangler.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ArgumentList.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/AST/Type.h"

using namespace swift;
using namespace swift::serialization;

//===----------------------------------------------------------------------===//
// Node kind tags
//===----------------------------------------------------------------------===//

enum class ExprKindTag : uint8_t {
  DeclRef = 1,
  MemberRef = 2,
  IntegerLiteral = 3,
  Binary = 4,
  Call = 5,
  Assign = 6,
  InOut = 7,
  DotSyntaxCall = 8,
  Type = 9,
  Error = 255,
};

enum class StmtKindTag : uint8_t {
  Return = 1,
  Brace = 2,
};

//===----------------------------------------------------------------------===//
// BodySerializer
//===----------------------------------------------------------------------===//

void BodySerializer::writeUInt32(uint32_t val) {
  Output.append(reinterpret_cast<const char *>(&val), 4);
}

void BodySerializer::writeUInt8(uint8_t val) {
  Output.append(reinterpret_cast<const char *>(&val), 1);
}

void BodySerializer::writeString(StringRef str) {
  writeUInt32(static_cast<uint32_t>(str.size()));
  Output.append(str.data(), str.size());
}

void BodySerializer::writeType(Type ty) {
  if (ty) {
    writeString(ty->getString());
  } else {
    writeString("");
  }
}

void BodySerializer::writeDeclRef(const ValueDecl *D) {
  if (!D) {
    writeString("");
    return;
  }
  writeString(D->getBaseIdentifier().str());
}

void BodySerializer::serializeExpr(Expr *E) {
  if (!E) {
    writeType(Type());
    writeUInt8(static_cast<uint8_t>(ExprKindTag::Error));
    return;
  }

  writeType(E->getType());

  switch (E->getKind()) {
  case ExprKind::DeclRef: {
    writeUInt8(static_cast<uint8_t>(ExprKindTag::DeclRef));
    writeDeclRef(cast<DeclRefExpr>(E)->getDecl());
    break;
  }
  case ExprKind::MemberRef: {
    writeUInt8(static_cast<uint8_t>(ExprKindTag::MemberRef));
    auto *MRE = cast<MemberRefExpr>(E);
    serializeExpr(MRE->getBase());
    writeDeclRef(MRE->getMember().getDecl());
    break;
  }
  case ExprKind::IntegerLiteral: {
    writeUInt8(static_cast<uint8_t>(ExprKindTag::IntegerLiteral));
    writeString(cast<IntegerLiteralExpr>(E)->getDigitsText());
    break;
  }
  case ExprKind::Binary: {
    writeUInt8(static_cast<uint8_t>(ExprKindTag::Binary));
    auto *argList = cast<BinaryExpr>(E)->getArgs();
    if (argList && argList->size() == 2) {
      serializeExpr(argList->get(0).getExpr());
      serializeExpr(argList->get(1).getExpr());
    } else {
      serializeExpr(nullptr);
      serializeExpr(nullptr);
    }
    break;
  }
  case ExprKind::Call: {
    writeUInt8(static_cast<uint8_t>(ExprKindTag::Call));
    auto *CE = cast<CallExpr>(E);
    serializeExpr(CE->getFn());
    if (auto *argList = CE->getArgs()) {
      writeUInt32(argList->size());
      for (auto arg : *argList)
        serializeExpr(arg.getExpr());
    } else {
      writeUInt32(0);
    }
    break;
  }
  case ExprKind::Assign: {
    writeUInt8(static_cast<uint8_t>(ExprKindTag::Assign));
    auto *AE = cast<AssignExpr>(E);
    serializeExpr(AE->getDest());
    serializeExpr(AE->getSrc());
    break;
  }
  case ExprKind::InOut: {
    writeUInt8(static_cast<uint8_t>(ExprKindTag::InOut));
    serializeExpr(cast<InOutExpr>(E)->getSubExpr());
    break;
  }
  case ExprKind::DotSyntaxCall: {
    writeUInt8(static_cast<uint8_t>(ExprKindTag::DotSyntaxCall));
    auto *DSCE = cast<DotSyntaxCallExpr>(E);
    serializeExpr(DSCE->getFn());
    serializeExpr(DSCE->getBase());
    break;
  }
  case ExprKind::Type:
    writeUInt8(static_cast<uint8_t>(ExprKindTag::Type));
    break;
  default:
    writeUInt8(static_cast<uint8_t>(ExprKindTag::Error));
    break;
  }
}

void BodySerializer::serializeStmt(Stmt *S) {
  if (!S) {
    writeUInt8(0);
    return;
  }

  switch (S->getKind()) {
  case StmtKind::Return: {
    writeUInt8(static_cast<uint8_t>(StmtKindTag::Return));
    auto *RS = cast<ReturnStmt>(S);
    writeUInt8(RS->isImplicit() ? 1 : 0);
    if (RS->hasResult()) {
      writeUInt8(1);
      serializeExpr(RS->getResult());
    } else {
      writeUInt8(0);
    }
    break;
  }
  case StmtKind::Brace: {
    writeUInt8(static_cast<uint8_t>(StmtKindTag::Brace));
    auto *BS = cast<BraceStmt>(S);
    writeUInt32(BS->getNumElements());
    for (auto &elem : BS->getElements())
      serializeASTNode(elem);
    break;
  }
  default:
    writeUInt8(0);
    break;
  }
}

void BodySerializer::serializeDecl(Decl *D) {
  writeUInt8(0); // TODO: handle local decls
}

void BodySerializer::serializeASTNode(ASTNode node) {
  if (auto *S = node.dyn_cast<Stmt *>()) {
    writeUInt8(0);
    serializeStmt(S);
  } else if (auto *E = node.dyn_cast<Expr *>()) {
    writeUInt8(1);
    serializeExpr(E);
  } else if (auto *D = node.dyn_cast<Decl *>()) {
    writeUInt8(2);
    serializeDecl(D);
  } else {
    writeUInt8(3);
  }
}

void BodySerializer::serializeBody(BraceStmt *body) {
  if (!body) {
    writeUInt32(0);
    return;
  }
  writeUInt32(body->getNumElements());
  for (auto &elem : body->getElements())
    serializeASTNode(elem);
}

//===----------------------------------------------------------------------===//
// BodyDeserializer
//===----------------------------------------------------------------------===//

uint32_t BodyDeserializer::readUInt32() {
  if (Remaining < 4) return 0;
  uint32_t val;
  memcpy(&val, Data, 4);
  Data += 4; Remaining -= 4;
  return val;
}

uint8_t BodyDeserializer::readUInt8() {
  if (Remaining < 1) return 0;
  uint8_t val = *Data;
  Data += 1; Remaining -= 1;
  return val;
}

StringRef BodyDeserializer::readString() {
  uint32_t len = readUInt32();
  if (len == 0 || Remaining < len) return {};
  StringRef str(Data, len);
  Data += len; Remaining -= len;
  return str;
}

Type BodyDeserializer::readType() {
  StringRef mangled = readString();
  if (mangled.empty()) return Type();
  return Demangle::getTypeForMangling(Ctx, mangled);
}

ValueDecl *BodyDeserializer::readDeclRef() {
  StringRef name = readString();
  if (name.empty()) return nullptr;
  auto ident = Ctx.getIdentifier(name);
  if (!DC) return nullptr;
  // Check if it's a parameter of the function.
  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
    if (auto *params = AFD->getParameters()) {
      for (auto *param : *params) {
        if (param->getBaseIdentifier() == ident)
          return param;
      }
    }
  }
  // Check if it's a member of the enclosing nominal type.
  auto *parentDC = DC->getSelfNominalTypeDecl();
  if (parentDC) {
    for (auto *member : parentDC->getMembers()) {
      if (auto *VD = dyn_cast<ValueDecl>(member)) {
        if (VD->getBaseIdentifier() == ident)
          return VD;
      }
    }
  }
  return nullptr;
}

Expr *BodyDeserializer::deserializeExpr() {
  Type ty = readType();
  uint8_t tag = readUInt8();

  switch (static_cast<ExprKindTag>(tag)) {
  case ExprKindTag::DeclRef: {
    auto *decl = readDeclRef();
    if (!decl)
      return new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    return new (Ctx) DeclRefExpr(ConcreteDeclRef(decl), DeclNameLoc(),
                                /*Implicit=*/true, AccessSemantics::Ordinary,
                                ty);
  }
  case ExprKindTag::MemberRef: {
    auto *base = deserializeExpr();
    auto *member = readDeclRef();
    if (!member)
      return new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    if (!base)
      base = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    return new (Ctx) MemberRefExpr(base, SourceLoc(),
                                   ConcreteDeclRef(member), DeclNameLoc(),
                                   /*Implicit=*/true);
  }
  case ExprKindTag::IntegerLiteral: {
    StringRef digits = readString();
    return new (Ctx) IntegerLiteralExpr(digits, SourceLoc(),
                                        /*Implicit=*/true);
  }
  case ExprKindTag::Binary: {
    auto *lhs = deserializeExpr();
    auto *rhs = deserializeExpr();
    if (!lhs) lhs = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    if (!rhs) rhs = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    return BinaryExpr::create(Ctx, lhs, nullptr, rhs, /*implicit=*/true, ty);
  }
  case ExprKindTag::Call: {
    auto *fn = deserializeExpr();
    if (!fn) fn = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    uint32_t numArgs = readUInt32();
    SmallVector<Expr*, 4> argExprs;
    for (uint32_t i = 0; i < numArgs; i++) {
      auto *argExpr = deserializeExpr();
      if (!argExpr)
        argExpr = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
      argExprs.push_back(argExpr);
    }
    auto *argList = ArgumentList::forImplicitUnlabeled(Ctx, argExprs);
    return CallExpr::createImplicit(Ctx, fn, argList);
  }
  case ExprKindTag::Assign: {
    auto *dest = deserializeExpr();
    auto *src = deserializeExpr();
    if (!dest) dest = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    if (!src) src = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    return new (Ctx) AssignExpr(dest, SourceLoc(), src, /*Implicit=*/true);
  }
  case ExprKindTag::InOut: {
    auto *sub = deserializeExpr();
    if (!sub)
      sub = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    return new (Ctx) InOutExpr(SourceLoc(), sub, Type(), /*isImplicit=*/true);
  }
  case ExprKindTag::DotSyntaxCall: {
    auto *fn = deserializeExpr();
    auto *base = deserializeExpr();
    if (!fn) fn = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    if (!base) base = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    return DotSyntaxCallExpr::create(Ctx, fn, SourceLoc(),
                                     Argument::unlabeled(base), ty);
  }
  case ExprKindTag::Type:
    if (!ty || ty->hasError())
      return new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    return TypeExpr::createImplicit(ty, Ctx);
  default:
    return new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
  }
}

Stmt *BodyDeserializer::deserializeStmt() {
  uint8_t tag = readUInt8();

  switch (static_cast<StmtKindTag>(tag)) {
  case StmtKindTag::Return: {
    readUInt8(); // isImplicit
    bool hasResult = readUInt8() != 0;
    Expr *result = hasResult ? deserializeExpr() : nullptr;
    if (result)
      return ReturnStmt::createImplied(Ctx, result);
    return ReturnStmt::createImplicit(Ctx, SourceLoc(), nullptr);
  }
  case StmtKindTag::Brace: {
    uint32_t numElements = readUInt32();
    SmallVector<ASTNode, 4> elements;
    for (uint32_t i = 0; i < numElements; i++)
      elements.push_back(deserializeASTNode());
    return BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(), false);
  }
  default:
    return nullptr;
  }
}

Decl *BodyDeserializer::deserializeDecl() {
  readUInt8();
  return nullptr;
}

ASTNode BodyDeserializer::deserializeASTNode() {
  uint8_t kind = readUInt8();
  switch (kind) {
  case 0: return ASTNode(deserializeStmt());
  case 1: return ASTNode(deserializeExpr());
  case 2: return ASTNode(deserializeDecl());
  default: return ASTNode();
  }
}

BraceStmt *BodyDeserializer::deserializeBody() {
  uint32_t numElements = readUInt32();
  if (numElements == 0) return nullptr;

  SmallVector<ASTNode, 4> elements;
  for (uint32_t i = 0; i < numElements; i++) {
    auto node = deserializeASTNode();
    if (node.isNull()) return nullptr;
    elements.push_back(node);
  }
  return BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(), false);
}
