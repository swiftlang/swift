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
#include "swift/AST/Pattern.h"
#include "swift/AST/ArgumentList.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/AST/Type.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Demangling/Demangle.h"

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
  Yield = 3,
  Switch = 4,
  Case = 5,
};

enum class DeclKindTag : uint8_t {
  PatternBinding = 1,
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
    // Map archetypes out of their generic environment so they can be
    // mangled. This matches what the ASTDumper does (typeUSR).
    if (ty->hasArchetype())
      ty = ty->mapTypeOutOfEnvironment();
    std::string usr;
    llvm::raw_string_ostream os(usr);
    if (ide::printTypeUSR(ty, os))
      writeString("");
    else
      writeString(os.str());
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
    writeUInt8(0); // implicit = false
    writeUInt8(static_cast<uint8_t>(ExprKindTag::Error));
    return;
  }

  writeType(E->getType());
  writeUInt8(E->isImplicit() ? 1 : 0);

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
  case StmtKind::Yield: {
    writeUInt8(static_cast<uint8_t>(StmtKindTag::Yield));
    auto *YS = cast<YieldStmt>(S);
    writeUInt8(YS->isImplicit() ? 1 : 0);
    auto yields = YS->getYields();
    writeUInt32(static_cast<uint32_t>(yields.size()));
    for (auto *yield : yields)
      serializeExpr(yield);
    break;
  }
  case StmtKind::Switch: {
    writeUInt8(static_cast<uint8_t>(StmtKindTag::Switch));
    auto *SS = cast<SwitchStmt>(S);
    writeUInt8(SS->isImplicit() ? 1 : 0);
    serializeExpr(SS->getSubjectExpr());
    auto cases = SS->getCases();
    writeUInt32(static_cast<uint32_t>(cases.size()));
    for (auto *CS : cases)
      serializeStmt(CS);
    break;
  }
  case StmtKind::Case: {
    writeUInt8(static_cast<uint8_t>(StmtKindTag::Case));
    auto *CS = cast<CaseStmt>(S);
    writeUInt8(CS->isImplicit() ? 1 : 0);
    // Serialize label items: just the guard expr (if any). Patterns are
    // skipped — they produce ErrorExpr fallbacks on deserialization but
    // won't crash.
    auto labels = CS->getCaseLabelItems();
    writeUInt8(static_cast<uint8_t>(labels.size()));
    for (auto &label : labels) {
      auto *guard = const_cast<CaseLabelItem &>(label).getGuardExpr();
      writeUInt8(guard ? 1 : 0);
      if (guard)
        serializeExpr(guard);
    }
    // Body
    if (auto *body = CS->getBody()) {
      writeUInt8(1);
      serializeStmt(body);
    } else {
      writeUInt8(0);
    }
    break;
  }
  default:
    writeUInt8(0);
    break;
  }
}

void BodySerializer::serializeDecl(Decl *D) {
  if (!D) {
    writeUInt8(0);
    return;
  }

  switch (D->getKind()) {
  case DeclKind::PatternBinding: {
    writeUInt8(static_cast<uint8_t>(DeclKindTag::PatternBinding));
    auto *PBD = cast<PatternBindingDecl>(D);
    auto numEntries = PBD->getNumPatternEntries();
    writeUInt32(static_cast<uint32_t>(numEntries));
    for (unsigned i = 0; i < numEntries; i++)
      serializeExpr(PBD->getInit(i));
    break;
  }
  default:
    writeUInt8(0);
    break;
  }
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
    writeUInt8(0); // hasBody = false
    return;
  }
  writeUInt8(1); // hasBody = true
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
  // Check if it's a parameter of the function (including implicit self).
  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
    if (auto *selfDecl = AFD->getImplicitSelfDecl(/*createIfNeeded=*/false)) {
      if (!selfDecl->getBaseName().isSpecial() &&
          selfDecl->getBaseIdentifier() == ident)
        return selfDecl;
    }
    if (auto *params = AFD->getParameters()) {
      for (auto *param : *params) {
        if (!param->getBaseName().isSpecial() &&
            param->getBaseIdentifier() == ident)
          return param;
      }
    }
  }
  // Check if it's a member of the enclosing nominal type.
  auto *parentDC = DC->getSelfNominalTypeDecl();
  if (parentDC) {
    for (auto *member : parentDC->getMembers()) {
      if (auto *VD = dyn_cast<ValueDecl>(member)) {
        if (!VD->getBaseName().isSpecial() &&
            VD->getBaseIdentifier() == ident)
          return VD;
      }
    }
  }
  // Check top-level decls in the source file's module.
  if (auto *SF = dyn_cast<SourceFile>(DC->getModuleScopeContext())) {
    for (auto item : SF->getTopLevelItems()) {
      if (auto *D = item.dyn_cast<Decl *>()) {
        if (auto *VD = dyn_cast<ValueDecl>(D)) {
          if (!VD->getBaseName().isSpecial() &&
              VD->getBaseIdentifier() == ident)
            return VD;
        }
        if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
          for (auto *member : IDC->getMembers()) {
            if (auto *VD = dyn_cast<ValueDecl>(member)) {
              if (!VD->getBaseName().isSpecial() &&
                  VD->getBaseIdentifier() == ident)
                return VD;
            }
          }
        }
      }
    }
  }
  return nullptr;
}

Expr *BodyDeserializer::deserializeExpr() {
  Type ty = readType();
  bool isImplicit = readUInt8() != 0;
  uint8_t tag = readUInt8();

  switch (static_cast<ExprKindTag>(tag)) {
  case ExprKindTag::DeclRef: {
    auto *decl = readDeclRef();
    if (!decl)
      return new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    auto *dre = new (Ctx) DeclRefExpr(ConcreteDeclRef(decl), DeclNameLoc(),
                                /*Implicit=*/isImplicit, AccessSemantics::Ordinary,
                                ty);
    return dre;
  }
  case ExprKindTag::MemberRef: {
    auto *base = deserializeExpr();
    auto *member = readDeclRef();
    // Always read both fields before checking errors to stay aligned.
    if (!member || !base || isa<ErrorExpr>(base))
      return new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    auto *mre = new (Ctx) MemberRefExpr(base, SourceLoc(),
                                   ConcreteDeclRef(member), DeclNameLoc(),
                                   /*Implicit=*/isImplicit);
    if (ty)
      mre->setType(ty);
    return mre;
  }
  case ExprKindTag::IntegerLiteral: {
    StringRef digits = readString();
    auto *lit = new (Ctx) IntegerLiteralExpr(digits, SourceLoc(),
                                        /*Implicit=*/isImplicit);
    return lit;
  }
  case ExprKindTag::Binary: {
    auto *lhs = deserializeExpr();
    auto *rhs = deserializeExpr();
    if (!lhs || !rhs)
      return new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    // BinaryExpr needs a non-null fn (the operator declref) for getLoc().
    // Use an ErrorExpr as a placeholder to avoid crashes.
    auto *fn = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    auto *bin = BinaryExpr::create(Ctx, lhs, fn, rhs, /*implicit=*/isImplicit, ty);
    return bin;
  }
  case ExprKindTag::Call: {
    auto *fn = deserializeExpr();
    uint32_t numArgs = readUInt32();
    SmallVector<Expr*, 4> argExprs;
    for (uint32_t i = 0; i < numArgs; i++) {
      auto *argExpr = deserializeExpr();
      if (!argExpr)
        argExpr = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
      argExprs.push_back(argExpr);
    }
    // Now check for errors (after reading all fields to stay aligned).
    if (!fn || isa<ErrorExpr>(fn))
      return new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    auto *argList = ArgumentList::forImplicitUnlabeled(Ctx, argExprs);
    auto *call = CallExpr::createImplicit(Ctx, fn, argList);
    call->setImplicit(isImplicit);
    if (ty)
      call->setType(ty);
    return call;
  }
  case ExprKindTag::Assign: {
    auto *dest = deserializeExpr();
    auto *src = deserializeExpr();
    if (!dest) dest = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    if (!src) src = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    auto *assign = new (Ctx) AssignExpr(dest, SourceLoc(), src, /*Implicit=*/isImplicit);
    if (ty)
      assign->setType(ty);
    return assign;
  }
  case ExprKindTag::InOut: {
    auto *sub = deserializeExpr();
    if (!sub)
      sub = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    auto *io = new (Ctx) InOutExpr(SourceLoc(), sub, Type(), /*isImplicit=*/isImplicit);
    return io;
  }
  case ExprKindTag::DotSyntaxCall: {
    auto *fn = deserializeExpr();
    auto *base = deserializeExpr();
    if (!fn || isa<ErrorExpr>(fn) || !base || isa<ErrorExpr>(base))
      return new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    auto *dsc = DotSyntaxCallExpr::create(Ctx, fn, SourceLoc(),
                                     Argument::unlabeled(base), ty);
    dsc->setImplicit(isImplicit);
    return dsc;
  }
  case ExprKindTag::Type: {
    if (!ty || ty->hasError())
      return new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    // TypeExpr::createImplicit always creates implicit; clear if needed.
    auto *te = TypeExpr::createImplicit(ty, Ctx);
    if (!isImplicit)
      te->setImplicit(false);
    return te;
  }
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
    for (uint32_t i = 0; i < numElements; i++) {
      auto node = deserializeASTNode();
      if (node.isNull()) {
        auto *err = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
        elements.push_back(ASTNode(err));
      } else {
        elements.push_back(node);
      }
    }
    return BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(), false);
  }
  case StmtKindTag::Yield: {
    readUInt8(); // isImplicit
    uint32_t numYields = readUInt32();
    SmallVector<Expr*, 4> yields;
    for (uint32_t i = 0; i < numYields; i++) {
      auto *yield = deserializeExpr();
      if (!yield)
        yield = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
      yields.push_back(yield);
    }
    return YieldStmt::create(Ctx, SourceLoc(), SourceLoc(), yields,
                             SourceLoc(), /*implicit=*/true);
  }
  case StmtKindTag::Switch: {
    readUInt8(); // isImplicit
    Expr *subject = deserializeExpr();
    if (!subject)
      subject = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
    uint32_t numCases = readUInt32();
    SmallVector<CaseStmt*, 4> cases;
    for (uint32_t i = 0; i < numCases; i++) {
      auto *CS = dyn_cast_or_null<CaseStmt>(deserializeStmt());
      if (CS)
        cases.push_back(CS);
    }
    auto *SS = SwitchStmt::createImplicit(LabeledStmtInfo(), subject, cases,
                                          Ctx);
    for (auto *CS : cases)
      CS->setParentStmt(SS);
    return SS;
  }
  case StmtKindTag::Case: {
    readUInt8(); // isImplicit
    uint8_t numLabels = readUInt8();
    SmallVector<CaseLabelItem, 2> labelItems;
    for (uint8_t i = 0; i < numLabels; i++) {
      bool hasGuard = readUInt8() != 0;
      Expr *guard = hasGuard ? deserializeExpr() : nullptr;
      labelItems.push_back(
          CaseLabelItem(AnyPattern::createImplicit(Ctx), SourceLoc(), guard));
    }
    // CaseStmt requires at least one label item.
    if (labelItems.empty()) {
      labelItems.push_back(CaseLabelItem::getDefault(AnyPattern::createImplicit(Ctx)));
    }
    bool hasBody = readUInt8() != 0;
    BraceStmt *body = hasBody
                           ? dyn_cast_or_null<BraceStmt>(deserializeStmt())
                           : nullptr;
    if (!body) {
      body = BraceStmt::create(Ctx, SourceLoc(), {}, SourceLoc(), false);
    }
    auto *CS = CaseStmt::createImplicit(Ctx, CaseParentKind::Switch,
                                        labelItems, body);
    return CS;
  }
  default:
    return nullptr;
  }
}

Decl *BodyDeserializer::deserializeDecl() {
  uint8_t tag = readUInt8();

  switch (static_cast<DeclKindTag>(tag)) {
  case DeclKindTag::PatternBinding: {
    uint32_t numEntries = readUInt32();
    for (uint32_t i = 0; i < numEntries; i++)
      deserializeExpr(); // read and discard init exprs
    // Return nullptr — the body will have an ErrorExpr fallback but won't
    // crash. Creating a real PatternBindingDecl requires patterns and a
    // DeclContext that we don't have here.
    return nullptr;
  }
  default:
    return nullptr;
  }
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
  uint8_t hasBody = readUInt8();
  if (!hasBody) return nullptr;

  uint32_t numElements = readUInt32();

  SmallVector<ASTNode, 4> elements;
  for (uint32_t i = 0; i < numElements; i++) {
    auto node = deserializeASTNode();
    if (node.isNull()) {
      // Push an ErrorExpr to maintain element count; the body still
      // has the correct structure even if some nodes failed to deserialize.
      auto *err = new (Ctx) ErrorExpr(SourceRange(), ErrorType::get(Ctx));
      elements.push_back(ASTNode(err));
    } else {
      elements.push_back(node);
    }
  }
  return BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(), false);
}
