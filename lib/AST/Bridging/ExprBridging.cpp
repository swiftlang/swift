//===--- Bridging/ExprBridging.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: Exprs
//===----------------------------------------------------------------------===//

// Define `.asExpr` on each BridgedXXXExpr type.
#define EXPR(Id, Parent)                                                       \
  BridgedExpr Bridged##Id##Expr_asExpr(Bridged##Id##Expr expr) {               \
    return static_cast<Expr *>(expr.unbridged());                              \
  }
#define ABSTRACT_EXPR(Id, Parent) EXPR(Id, Parent)
#include "swift/AST/ExprNodes.def"

BridgedArgumentList
BridgedArgumentList_createImplicitUnlabeled(BridgedASTContext cContext,
                                            BridgedArrayRef cExprs) {
  return ArgumentList::forImplicitUnlabeled(cContext.unbridged(),
                                            cExprs.unbridged<Expr *>());
}

BridgedArgumentList BridgedArgumentList_createParsed(
    BridgedASTContext cContext, SourceLoc lParenLoc, BridgedArrayRef cArgs,
    SourceLoc rParenLoc, size_t cFirstTrailingClosureIndex) {
  SmallVector<Argument> arguments;
  arguments.reserve(cArgs.unbridged<BridgedCallArgument>().size());
  for (auto &arg : cArgs.unbridged<BridgedCallArgument>()) {
    arguments.push_back(arg.unbridged());
  }

  std::optional<unsigned int> firstTrailingClosureIndex;
  if (cFirstTrailingClosureIndex < arguments.size())
    firstTrailingClosureIndex = cFirstTrailingClosureIndex;

  return ArgumentList::createParsed(cContext.unbridged(), lParenLoc, arguments,
                                    rParenLoc, firstTrailingClosureIndex);
}

BridgedArrayExpr BridgedArrayExpr_createParsed(BridgedASTContext cContext,
                                               SourceLoc lLoc,
                                               BridgedArrayRef elements,
                                               BridgedArrayRef commas,
                                               SourceLoc rLoc) {
  ASTContext &context = cContext.unbridged();
  return ArrayExpr::create(context, lLoc, elements.unbridged<Expr *>(),
                           commas.unbridged<SourceLoc>(), rLoc);
}

BridgedArrowExpr BridgedArrowExpr_createParsed(BridgedASTContext cContext,
                                               SourceLoc asyncLoc,
                                               SourceLoc throwsLoc,
                                               BridgedNullableExpr cThrownType,
                                               SourceLoc arrowLoc) {
  return new (cContext.unbridged())
      ArrowExpr(asyncLoc, throwsLoc, cThrownType.unbridged(), arrowLoc);
}

BridgedAssignExpr BridgedAssignExpr_createParsed(BridgedASTContext cContext,
                                                 SourceLoc equalsLoc) {
  return new (cContext.unbridged()) AssignExpr(equalsLoc);
}

BridgedAwaitExpr BridgedAwaitExpr_createParsed(BridgedASTContext cContext,
                                               SourceLoc awaitLoc,
                                               BridgedExpr cSubExpr) {
  return new (cContext.unbridged()) AwaitExpr(awaitLoc, cSubExpr.unbridged());
}

BridgedBindOptionalExpr BridgedBindOptionalExpr_createParsed(
    BridgedASTContext cContext, BridgedExpr cSubExpr, SourceLoc questionLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      BindOptionalExpr(cSubExpr.unbridged(), questionLoc, /*depth=*/0);
}

BridgedBooleanLiteralExpr
BridgedBooleanLiteralExpr_createParsed(BridgedASTContext cContext, bool value,
                                       SourceLoc tokenLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) BooleanLiteralExpr(value, tokenLoc);
}

BridgedBorrowExpr BridgedBorrowExpr_createParsed(BridgedASTContext cContext,
                                                 SourceLoc borrowLoc,
                                                 BridgedExpr cSubExpr) {
  return new (cContext.unbridged()) BorrowExpr(borrowLoc, cSubExpr.unbridged());
}

BridgedCallExpr BridgedCallExpr_createParsed(BridgedASTContext cContext,
                                             BridgedExpr fn,
                                             BridgedArgumentList cArguments) {
  return CallExpr::create(cContext.unbridged(), fn.unbridged(),
                          cArguments.unbridged(),
                          /*implicit*/ false);
}

BridgedCaptureListEntry BridegedCaptureListEntry_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedReferenceOwnership cOwnershipKind,
    BridgedSourceRange cOwnershipRange, Identifier name, SourceLoc nameLoc,
    SourceLoc equalLoc, BridgedExpr cInitializer) {
  return CaptureListEntry::createParsed(
      cContext.unbridged(), unbridged(cOwnershipKind),
      cOwnershipRange.unbridged(), name, nameLoc, equalLoc,
      cInitializer.unbridged(), cDeclContext.unbridged());
}

BridgedCaptureListExpr
BridgedCaptureListExpr_createParsed(BridgedASTContext cContext,
                                    BridgedArrayRef cCaptureList,
                                    BridgedClosureExpr cClosure) {
  SmallVector<CaptureListEntry, 4> captureList;
  for (auto e : cCaptureList.unbridged<BridgedCaptureListEntry>())
    captureList.push_back(e.unbridged());

  return CaptureListExpr::create(cContext.unbridged(), captureList,
                                 cClosure.unbridged());
}

BridgedClosureExpr BridgedClosureExpr_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedDeclAttributes cAttributes, BridgedSourceRange cBracketRange,
    BridgedNullableVarDecl cCapturedSelfDecl,
    BridgedNullableParameterList cParameterList, SourceLoc asyncLoc,
    SourceLoc throwsLoc, BridgedNullableTypeRepr cThrownType,
    SourceLoc arrowLoc, BridgedNullableTypeRepr cExplicitResultType,
    SourceLoc inLoc) {
  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();
  TypeExpr *throwsType = nullptr;
  if (auto *tyR = cThrownType.unbridged())
    throwsType = new (context) TypeExpr(tyR);
  TypeExpr *explicitResultType = nullptr;
  if (auto *tyR = cExplicitResultType.unbridged())
    explicitResultType = new (context) TypeExpr(tyR);

  return new (context) ClosureExpr(
      cAttributes.unbridged(), cBracketRange.unbridged(),
      cCapturedSelfDecl.unbridged(), cParameterList.unbridged(), asyncLoc,
      throwsLoc, throwsType, arrowLoc, inLoc, explicitResultType, declContext);
}

BridgedParameterList
BridgedClosureExpr_getParameterList(BridgedClosureExpr cClosure) {
  return cClosure.unbridged()->getParameters();
}

void BridgedClosureExpr_setParameterList(BridgedClosureExpr cClosure,
                                         BridgedParameterList cParams) {
  cClosure.unbridged()->setParameterList(cParams.unbridged());
}

bool BridgedClosureExpr_hasAnonymousClosureVars(BridgedClosureExpr cClosure) {
  return cClosure.unbridged()->hasAnonymousClosureVars();
}

void BridgedClosureExpr_setHasAnonymousClosureVars(
    BridgedClosureExpr cClosure) {
  cClosure.unbridged()->setHasAnonymousClosureVars();
}

void BridgedClosureExpr_setBody(BridgedClosureExpr cClosure,
                                BridgedBraceStmt cBody) {
  cClosure.unbridged()->setBody(cBody.unbridged());
}

BridgedCoerceExpr BridgedCoerceExpr_createParsed(BridgedASTContext cContext,
                                                 SourceLoc asLoc,
                                                 BridgedTypeRepr cType) {
  return CoerceExpr::create(cContext.unbridged(), asLoc, cType.unbridged());
}

BridgedConditionalCheckedCastExpr
BridgedConditionalCheckedCastExpr_createParsed(BridgedASTContext cContext,
                                               SourceLoc asLoc,
                                               SourceLoc questionLoc,
                                               BridgedTypeRepr cType) {
  return ConditionalCheckedCastExpr::create(cContext.unbridged(), asLoc,
                                            questionLoc, cType.unbridged());
}

BridgedConsumeExpr BridgedConsumeExpr_createParsed(BridgedASTContext cContext,
                                                   SourceLoc consumeLoc,
                                                   BridgedExpr cSubExpr) {
  return new (cContext.unbridged())
      ConsumeExpr(consumeLoc, cSubExpr.unbridged());
}

BridgedCopyExpr BridgedCopyExpr_createParsed(BridgedASTContext cContext,
                                             SourceLoc copyLoc,
                                             BridgedExpr cSubExpr) {
  return new (cContext.unbridged()) CopyExpr(copyLoc, cSubExpr.unbridged());
}

BridgedDeclRefExpr BridgedDeclRefExpr_create(BridgedASTContext cContext,
                                             BridgedDecl cDecl,
                                             BridgedDeclNameLoc cLoc,
                                             bool IsImplicit) {
  return new (cContext.unbridged()) DeclRefExpr(
      cast<ValueDecl>(cDecl.unbridged()), cLoc.unbridged(), IsImplicit);
}

BridgedDictionaryExpr BridgedDictionaryExpr_createParsed(
    BridgedASTContext cContext, SourceLoc lBracketLoc,
    BridgedArrayRef cElements, BridgedArrayRef cCommaLocs,
    SourceLoc rBracketLoc) {
  return DictionaryExpr::create(cContext.unbridged(), lBracketLoc,
                                cElements.unbridged<Expr *>(),
                                cCommaLocs.unbridged<SourceLoc>(), rBracketLoc);
}

BridgedDiscardAssignmentExpr
BridgedDiscardAssignmentExpr_createParsed(BridgedASTContext cContext,
                                          SourceLoc loc) {
  return new (cContext.unbridged())
      DiscardAssignmentExpr(loc, /*Implicit=*/false);
}

BridgedDotSelfExpr BridgedDotSelfExpr_createParsed(BridgedASTContext cContext,
                                                   BridgedExpr cSubExpr,
                                                   SourceLoc dotLoc,
                                                   SourceLoc selfLoc) {

  return new (cContext.unbridged())
      DotSelfExpr(cSubExpr.unbridged(), dotLoc, selfLoc);
}

BridgedEditorPlaceholderExpr BridgedEditorPlaceholderExpr_createParsed(
    BridgedASTContext cContext, Identifier placeholderId, SourceLoc loc,
    BridgedNullableTypeRepr cPlaceholderTyR,
    BridgedNullableTypeRepr cExpansionTyR) {
  return new (cContext.unbridged())
      EditorPlaceholderExpr(placeholderId, loc, cPlaceholderTyR.unbridged(),
                            cExpansionTyR.unbridged());
}

BridgedErrorExpr BridgedErrorExpr_create(BridgedASTContext cContext,
                                         BridgedSourceRange cRange) {
  return new (cContext.unbridged()) ErrorExpr(cRange.unbridged());
}

BridgedForceTryExpr BridgedForceTryExpr_createParsed(BridgedASTContext cContext,
                                                     SourceLoc tryLoc,
                                                     BridgedExpr cSubExpr,
                                                     SourceLoc exclaimLoc) {
  return new (cContext.unbridged())
      ForceTryExpr(tryLoc, cSubExpr.unbridged(), exclaimLoc);
}

BridgedForceValueExpr
BridgedForceValueExpr_createParsed(BridgedASTContext cContext,
                                   BridgedExpr cSubExpr, SourceLoc exclaimLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) ForceValueExpr(cSubExpr.unbridged(), exclaimLoc);
}

BridgedFloatLiteralExpr BridgedFloatLiteralExpr_createParsed(
    BridgedASTContext cContext, BridgedStringRef cStr, SourceLoc tokenLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) FloatLiteralExpr(cStr.unbridged(), tokenLoc);
}

BridgedForcedCheckedCastExpr
BridgedForcedCheckedCastExpr_createParsed(BridgedASTContext cContext,
                                          SourceLoc asLoc, SourceLoc exclaimLoc,
                                          BridgedTypeRepr cType) {
  return ForcedCheckedCastExpr::create(cContext.unbridged(), asLoc, exclaimLoc,
                                       cType.unbridged());
}

BridgedUnresolvedSpecializeExpr BridgedUnresolvedSpecializeExpr_createParsed(
    BridgedASTContext cContext, BridgedExpr cSubExpr, SourceLoc lAngleLoc,
    BridgedArrayRef cArguments, SourceLoc rAngleLoc) {
  return UnresolvedSpecializeExpr::create(
      cContext.unbridged(), cSubExpr.unbridged(), lAngleLoc,
      cArguments.unbridged<TypeRepr *>(), rAngleLoc);
}

BridgedUnsafeExpr BridgedUnsafeExpr_createParsed(BridgedASTContext cContext,
                                                 SourceLoc unsafeLoc,
                                                 BridgedExpr cSubExpr) {
  return new (cContext.unbridged()) UnsafeExpr(unsafeLoc, cSubExpr.unbridged());
}

BridgedInOutExpr BridgedInOutExpr_createParsed(BridgedASTContext cContext,
                                               SourceLoc loc,
                                               BridgedExpr cSubExpr) {
  ASTContext &context = cContext.unbridged();
  return new (context) InOutExpr(loc, cSubExpr.unbridged(), Type());
}

BridgedIntegerLiteralExpr BridgedIntegerLiteralExpr_createParsed(
    BridgedASTContext cContext, BridgedStringRef cStr, SourceLoc tokenLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) IntegerLiteralExpr(cStr.unbridged(), tokenLoc);
}

BridgedKeyPathDotExpr
BridgedKeyPathDotExpr_createParsed(BridgedASTContext cContext, SourceLoc loc) {
  return new (cContext.unbridged()) KeyPathDotExpr(loc);
}

BridgedKeyPathExpr BridgedKeyPathExpr_createParsed(
    BridgedASTContext cContext, SourceLoc backslashLoc,
    BridgedNullableExpr cParsedRoot, BridgedNullableExpr cParsedPath,
    bool hasLeadingDot) {
  return KeyPathExpr::createParsed(cContext.unbridged(), backslashLoc,
                                   cParsedRoot.unbridged(),
                                   cParsedPath.unbridged(), hasLeadingDot);
}

BridgedKeyPathExpr BridgedKeyPathExpr_createParsedPoundKeyPath(
    BridgedASTContext cContext, SourceLoc poundLoc, SourceLoc lParenLoc,
    BridgedArrayRef cNames, BridgedArrayRef cNameLocs, SourceLoc rParenLoc) {

  SmallVector<KeyPathExpr::Component> components;
  auto cNameArr = cNames.unbridged<BridgedDeclNameRef>();
  auto cNameLocArr = cNameLocs.unbridged<BridgedDeclNameLoc>();
  for (size_t i = 0, e = cNameArr.size(); i != e; ++i) {
    auto name = cNameArr[i].unbridged();
    auto loc = cNameLocArr[i].unbridged().getBaseNameLoc();
    components.push_back(KeyPathExpr::Component::forUnresolvedMember(
        name, FunctionRefInfo::unappliedBaseName(), loc));
  }

  return KeyPathExpr::createParsedPoundKeyPath(
      cContext.unbridged(), poundLoc, lParenLoc, components, rParenLoc);
}

BridgedSuperRefExpr BridgedSuperRefExpr_createParsed(BridgedASTContext cContext,
                                                     SourceLoc superLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      SuperRefExpr(/*Self=*/nullptr, superLoc, /*Implicit=*/false);
}

BridgedSubscriptExpr
BridgedSubscriptExpr_createParsed(BridgedASTContext cContext,
                                  BridgedExpr cBaseExpr,
                                  BridgedArgumentList cArgs) {
  return SubscriptExpr::create(cContext.unbridged(), cBaseExpr.unbridged(),
                               cArgs.unbridged());
}

BridgedInterpolatedStringLiteralExpr
BridgedInterpolatedStringLiteralExpr_createParsed(
    BridgedASTContext cContext, SourceLoc loc, size_t literalCapacity,
    size_t interpolationCount, BridgedTapExpr cAppendingExpr) {
  return new (cContext.unbridged()) InterpolatedStringLiteralExpr(
      loc, literalCapacity, interpolationCount, cAppendingExpr.unbridged());
}

BridgedIsExpr BridgedIsExpr_createParsed(BridgedASTContext cContext,
                                         SourceLoc isLoc,
                                         BridgedTypeRepr cType) {
  return IsExpr::create(cContext.unbridged(), isLoc, cType.unbridged());
}

BridgedMacroExpansionExpr BridgedMacroExpansionExpr_createParsed(
    BridgedDeclContext cDeclContext, SourceLoc poundLoc,
    BridgedDeclNameRef cMacroNameRef, BridgedDeclNameLoc cMacroNameLoc,
    SourceLoc leftAngleLoc, BridgedArrayRef cGenericArgs,
    SourceLoc rightAngleLoc, BridgedNullableArgumentList cArgList) {
  auto *DC = cDeclContext.unbridged();
  auto &Context = DC->getASTContext();
  return MacroExpansionExpr::create(
      cDeclContext.unbridged(), poundLoc,
      /*module name=*/DeclNameRef(), /*module name loc=*/DeclNameLoc(),
      cMacroNameRef.unbridged(), cMacroNameLoc.unbridged(), leftAngleLoc,
      Context.AllocateCopy(cGenericArgs.unbridged<TypeRepr *>()), rightAngleLoc,
      cArgList.unbridged(),
      DC->isTypeContext() ? MacroRole::Declaration
                          : getFreestandingMacroRoles());
}

BridgedMagicIdentifierLiteralKind
BridgedMagicIdentifierLiteralKind_fromString(BridgedStringRef cStr) {
  StringRef str = cStr.unbridged();

  // Note: STRING includes '#' e.g. '#fileID'.
#define MAGIC_IDENTIFIER(NAME, STRING)                                         \
  if (str == StringRef(STRING).drop_front())                                   \
    return BridgedMagicIdentifierLiteralKind##NAME;
#include "swift/AST/MagicIdentifierKinds.def"
  return BridgedMagicIdentifierLiteralKindNone;
}

static std::optional<MagicIdentifierLiteralExpr::Kind>
unbridge(BridgedMagicIdentifierLiteralKind cKind) {
  switch (cKind) {
#define MAGIC_IDENTIFIER(NAME, STRING)                                         \
  case BridgedMagicIdentifierLiteralKind##NAME:                                \
    return MagicIdentifierLiteralExpr::Kind::NAME;
#include "swift/AST/MagicIdentifierKinds.def"
  case BridgedMagicIdentifierLiteralKindNone:
    return std::nullopt;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedMagicIdentifierLiteralExpr
BridgedMagicIdentifierLiteralExpr_createParsed(
    BridgedASTContext cContext, BridgedMagicIdentifierLiteralKind cKind,
    SourceLoc loc) {
  return new (cContext.unbridged())
      MagicIdentifierLiteralExpr(*unbridge(cKind), loc);
}

BridgedNilLiteralExpr
BridgedNilLiteralExpr_createParsed(BridgedASTContext cContext,
                                   SourceLoc nilKeywordLoc) {
  return new (cContext.unbridged()) NilLiteralExpr(nilKeywordLoc);
}

BridgedObjCSelectorExpr BridgedObjCSelectorExpr_createParsed(
    BridgedASTContext cContext, BridgedObjCSelectorKind cKind,
    SourceLoc keywordLoc, SourceLoc lParenLoc, SourceLoc modifierLoc,
    BridgedExpr cSubExpr, SourceLoc rParenLoc) {
  ObjCSelectorExpr::ObjCSelectorKind kind;
  switch (cKind) {
  case BridgedObjCSelectorKindMethod:
    kind = ObjCSelectorExpr::Method;
    break;
  case BridgedObjCSelectorKindGetter:
    kind = ObjCSelectorExpr::Getter;
    break;
  case BridgedObjCSelectorKindSetter:
    kind = ObjCSelectorExpr::Setter;
    break;
  }
  return new (cContext.unbridged())
      ObjCSelectorExpr(kind, keywordLoc, lParenLoc, modifierLoc,
                       cSubExpr.unbridged(), rParenLoc);
}

SWIFT_NAME("BridgedObjectLiteralKind.init(from:)")
BridgedObjectLiteralKind
BridgedObjectLiteralKind_fromString(BridgedStringRef cStr) {
  return llvm::StringSwitch<BridgedObjectLiteralKind>(cStr.unbridged())
#define POUND_OBJECT_LITERAL(Name, Desc, Proto)                                \
  .Case(#Name, BridgedObjectLiteralKind_##Name)
#include "swift/AST/TokenKinds.def"
      .Default(BridgedObjectLiteralKind_none);
}

static std::optional<ObjectLiteralExpr::LiteralKind>
unbridge(BridgedObjectLiteralKind kind) {
  switch (kind) {
#define POUND_OBJECT_LITERAL(Name, Desc, Proto)                                \
  case BridgedObjectLiteralKind_##Name:                                        \
    return ObjectLiteralExpr::LiteralKind::Name;
#include "swift/AST/TokenKinds.def"
  case BridgedObjectLiteralKind_none:
    return std::nullopt;
  }
  llvm_unreachable("unhandled enum value");
}

SWIFT_NAME("BridgedObjectLiteralExpr.createParsed(_:poundLoc:kind:args:)")
BridgedObjectLiteralExpr BridgedObjectLiteralExpr_createParsed(
    BridgedASTContext cContext, SourceLoc poundLoc,
    BridgedObjectLiteralKind cKind, BridgedArgumentList cArgs) {
  return ObjectLiteralExpr::create(cContext.unbridged(), poundLoc,
                                   *unbridge(cKind), cArgs.unbridged(),
                                   /*implicit=*/false);
}

BridgedOptionalTryExpr
BridgedOptionalTryExpr_createParsed(BridgedASTContext cContext,
                                    SourceLoc tryLoc, BridgedExpr cSubExpr,
                                    SourceLoc questionLoc) {
  return new (cContext.unbridged())
      OptionalTryExpr(tryLoc, cSubExpr.unbridged(), questionLoc);
}

BridgedPackElementExpr BridgedPackElementExpr_createParsed(
    BridgedASTContext cContext, SourceLoc eachLoc, BridgedExpr cPackRefExpr) {
  return PackElementExpr::create(cContext.unbridged(), eachLoc,
                                 cPackRefExpr.unbridged());
}

BridgedPackExpansionExpr BridgedPackExpansionExpr_createParsed(
    BridgedASTContext cContext, SourceLoc repeatLoc, BridgedExpr cPatternExpr) {
  return PackExpansionExpr::create(cContext.unbridged(), repeatLoc,
                                   cPatternExpr.unbridged(),
                                   /*genericEnv=*/nullptr);
}

BridgedPostfixUnaryExpr
BridgedPostfixUnaryExpr_createParsed(BridgedASTContext cContext,
                                     BridgedExpr oper, BridgedExpr operand) {
  return PostfixUnaryExpr::create(cContext.unbridged(), oper.unbridged(),
                                  operand.unbridged());
}

BridgedPrefixUnaryExpr
BridgedPrefixUnaryExpr_createParsed(BridgedASTContext cContext,
                                    BridgedExpr oper, BridgedExpr operand) {
  return PrefixUnaryExpr::create(cContext.unbridged(), oper.unbridged(),
                                 operand.unbridged());
}

BridgedRegexLiteralExpr
BridgedRegexLiteralExpr_createParsed(BridgedASTContext cContext, SourceLoc loc,
                                     BridgedStringRef cRegexText) {
  return RegexLiteralExpr::createParsed(cContext.unbridged(), loc,
                                        cRegexText.unbridged());
}

BridgedParenExpr BridgedParenExpr_createParsed(BridgedASTContext cContext,
                                               SourceLoc lParen,
                                               BridgedExpr cExpr,
                                               SourceLoc rParen) {
  ASTContext &context = cContext.unbridged();
  return new (context) ParenExpr(lParen, cExpr.unbridged(), rParen);
}
BridgedSequenceExpr BridgedSequenceExpr_createParsed(BridgedASTContext cContext,
                                                     BridgedArrayRef exprs) {
  return SequenceExpr::create(cContext.unbridged(), exprs.unbridged<Expr *>());
}

BridgedSingleValueStmtExpr BridgedSingleValueStmtExpr_createWithWrappedBranches(
    BridgedASTContext cContext, BridgedStmt S, BridgedDeclContext cDeclContext,
    bool mustBeExpr) {
  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();
  return SingleValueStmtExpr::createWithWrappedBranches(
      context, S.unbridged(), declContext, mustBeExpr);
}

BridgedStringLiteralExpr BridgedStringLiteralExpr_createParsed(
    BridgedASTContext cContext, BridgedStringRef cStr, SourceLoc tokenLoc) {
  ASTContext &context = cContext.unbridged();
  auto str = context.AllocateCopy(cStr.unbridged());
  return new (context) StringLiteralExpr(str, tokenLoc);
}

BridgedTapExpr BridgedTapExpr_create(BridgedASTContext cContext,
                                     BridgedBraceStmt cBody) {
  return new (cContext.unbridged()) TapExpr(nullptr, cBody.unbridged());
}

BridgedTernaryExpr BridgedTernaryExpr_createParsed(BridgedASTContext cContext,
                                                   SourceLoc questionLoc,
                                                   BridgedExpr cThenExpr,
                                                   SourceLoc colonLoc) {
  return new (cContext.unbridged())
      TernaryExpr(questionLoc, cThenExpr.unbridged(), colonLoc);
}

BridgedTryExpr BridgedTryExpr_createParsed(BridgedASTContext cContext,
                                           SourceLoc tryLoc,
                                           BridgedExpr cSubExpr) {
  return new (cContext.unbridged()) TryExpr(tryLoc, cSubExpr.unbridged());
}

BridgedTupleExpr
BridgedTupleExpr_createParsed(BridgedASTContext cContext, SourceLoc lParen,
                              BridgedArrayRef subs, BridgedArrayRef names,
                              BridgedArrayRef cNameLocs, SourceLoc rParen) {
  ASTContext &context = cContext.unbridged();
  return TupleExpr::create(
      context, lParen, subs.unbridged<Expr *>(), names.unbridged<Identifier>(),
      cNameLocs.unbridged<SourceLoc>(), rParen, /*Implicit*/ false);
}

BridgedTupleExpr BridgedTupleExpr_createParsedDictionaryElement(
    BridgedASTContext cContext, BridgedExpr cKeyExpr, BridgedExpr cValueExpr) {
  return TupleExpr::createImplicit(
      cContext.unbridged(), {cKeyExpr.unbridged(), cValueExpr.unbridged()}, {});
}

BridgedTypeExpr BridgedTypeExpr_createParsed(BridgedASTContext cContext,
                                             BridgedTypeRepr cType) {
  ASTContext &context = cContext.unbridged();
  return new (context) TypeExpr(cType.unbridged());
}

BridgedUnresolvedDeclRefExpr BridgedUnresolvedDeclRefExpr_createParsed(
    BridgedASTContext cContext, BridgedDeclNameRef cName,
    BridgedDeclRefKind cKind, BridgedDeclNameLoc cLoc) {
  DeclRefKind kind;
  switch (cKind) {
  case BridgedDeclRefKindOrdinary:
    kind = DeclRefKind::Ordinary;
    break;
  case BridgedDeclRefKindBinaryOperator:
    kind = DeclRefKind::BinaryOperator;
    break;
  case BridgedDeclRefKindPostfixOperator:
    kind = DeclRefKind::PostfixOperator;
    break;
  case BridgedDeclRefKindPrefixOperator:
    kind = DeclRefKind::PrefixOperator;
    break;
  }
  return new (cContext.unbridged())
      UnresolvedDeclRefExpr(cName.unbridged(), kind, cLoc.unbridged());
}

BridgedUnresolvedDotExpr BridgedUnresolvedDotExpr_createParsed(
    BridgedASTContext cContext, BridgedExpr base, SourceLoc dotLoc,
    BridgedDeclNameRef cName, BridgedDeclNameLoc cNameLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      UnresolvedDotExpr(base.unbridged(), dotLoc, cName.unbridged(),
                        cNameLoc.unbridged(), /*isImplicit=*/false);
}

BridgedUnresolvedMemberExpr BridgedUnresolvedMemberExpr_createParsed(
    BridgedASTContext cContext, SourceLoc dotLoc, BridgedDeclNameRef cName,
    BridgedDeclNameLoc cNameLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) UnresolvedMemberExpr(
      dotLoc, cNameLoc.unbridged(), cName.unbridged(), /*isImplicit=*/false);
}

BridgedUnresolvedPatternExpr
BridgedUnresolvedPatternExpr_createParsed(BridgedASTContext cContext,
                                          BridgedPattern cPattern) {
  return new (cContext.unbridged()) UnresolvedPatternExpr(cPattern.unbridged());
}

void BridgedExpr_setImplicit(BridgedExpr cExpr) {
  cExpr.unbridged()->setImplicit();
}
