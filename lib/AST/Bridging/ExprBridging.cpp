//===--- Bridging/ExprBridging.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2024 Apple Inc. and the Swift project authors
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
    BridgedASTContext cContext, BridgedSourceLoc cLParenLoc,
    BridgedArrayRef cArgs, BridgedSourceLoc cRParenLoc,
    size_t cFirstTrailingClosureIndex) {
  SmallVector<Argument> arguments;
  arguments.reserve(cArgs.unbridged<BridgedCallArgument>().size());
  for (auto &arg : cArgs.unbridged<BridgedCallArgument>()) {
    arguments.push_back(arg.unbridged());
  }

  std::optional<unsigned int> firstTrailingClosureIndex;
  if (cFirstTrailingClosureIndex < arguments.size())
    firstTrailingClosureIndex = cFirstTrailingClosureIndex;

  return ArgumentList::createParsed(
      cContext.unbridged(), cLParenLoc.unbridged(), arguments,
      cRParenLoc.unbridged(), firstTrailingClosureIndex);
}

BridgedArrayExpr BridgedArrayExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLLoc,
                                               BridgedArrayRef elements,
                                               BridgedArrayRef commas,
                                               BridgedSourceLoc cRLoc) {
  ASTContext &context = cContext.unbridged();
  return ArrayExpr::create(context, cLLoc.unbridged(),
                           elements.unbridged<Expr *>(),
                           commas.unbridged<SourceLoc>(), cRLoc.unbridged());
}

BridgedArrowExpr BridgedArrowExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cAsyncLoc,
                                               BridgedSourceLoc cThrowsLoc,
                                               BridgedNullableExpr cThrownType,
                                               BridgedSourceLoc cArrowLoc) {
  return new (cContext.unbridged())
      ArrowExpr(cAsyncLoc.unbridged(), cThrowsLoc.unbridged(),
                cThrownType.unbridged(), cArrowLoc.unbridged());
}

BridgedAssignExpr BridgedAssignExpr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cEqualsLoc) {
  return new (cContext.unbridged()) AssignExpr(cEqualsLoc.unbridged());
}

BridgedAwaitExpr BridgedAwaitExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cAwaitLoc,
                                               BridgedExpr cSubExpr) {
  return new (cContext.unbridged())
      AwaitExpr(cAwaitLoc.unbridged(), cSubExpr.unbridged());
}

BridgedBooleanLiteralExpr
BridgedBooleanLiteralExpr_createParsed(BridgedASTContext cContext, bool value,
                                       BridgedSourceLoc cTokenLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) BooleanLiteralExpr(value, cTokenLoc.unbridged());
}

BridgedBorrowExpr BridgedBorrowExpr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cBorrowLoc,
                                                 BridgedExpr cSubExpr) {
  return new (cContext.unbridged())
      BorrowExpr(cBorrowLoc.unbridged(), cSubExpr.unbridged());
}

BridgedCallExpr BridgedCallExpr_createParsed(BridgedASTContext cContext,
                                             BridgedExpr fn,
                                             BridgedArgumentList cArguments) {
  return CallExpr::create(cContext.unbridged(), fn.unbridged(),
                          cArguments.unbridged(),
                          /*implicit*/ false);
}

BridgedClosureExpr BridgedClosureExpr_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedParameterList cParamList, BridgedBraceStmt body) {
  DeclAttributes attributes;
  SourceRange bracketRange;
  SourceLoc asyncLoc;
  SourceLoc throwsLoc;
  SourceLoc arrowLoc;
  SourceLoc inLoc;

  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();

  auto *out = new (context) ClosureExpr(
      attributes, bracketRange, nullptr, cParamList.unbridged(), asyncLoc,
      throwsLoc,
      /*FIXME:thrownType=*/nullptr, arrowLoc, inLoc, nullptr, declContext);
  out->setBody(body.unbridged());
  return out;
}

BridgedCoerceExpr BridgedCoerceExpr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cAsLoc,
                                                 BridgedTypeRepr cType) {
  return CoerceExpr::create(cContext.unbridged(), cAsLoc.unbridged(),
                            cType.unbridged());
}

BridgedConditionalCheckedCastExpr
BridgedConditionalCheckedCastExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cAsLoc,
                                               BridgedSourceLoc cQuestionLoc,
                                               BridgedTypeRepr cType) {
  return ConditionalCheckedCastExpr::create(
      cContext.unbridged(), cAsLoc.unbridged(), cQuestionLoc.unbridged(),
      cType.unbridged());
}

BridgedConsumeExpr BridgedConsumeExpr_createParsed(BridgedASTContext cContext,
                                                   BridgedSourceLoc cConsumeLoc,
                                                   BridgedExpr cSubExpr) {
  return new (cContext.unbridged())
      ConsumeExpr(cConsumeLoc.unbridged(), cSubExpr.unbridged());
}

BridgedCopyExpr BridgedCopyExpr_createParsed(BridgedASTContext cContext,
                                             BridgedSourceLoc cCopyLoc,
                                             BridgedExpr cSubExpr) {
  return new (cContext.unbridged())
      CopyExpr(cCopyLoc.unbridged(), cSubExpr.unbridged());
}

BridgedDeclRefExpr BridgedDeclRefExpr_create(BridgedASTContext cContext,
                                             BridgedDecl cDecl,
                                             BridgedDeclNameLoc cLoc,
                                             bool IsImplicit) {
  return new (cContext.unbridged()) DeclRefExpr(
      cast<ValueDecl>(cDecl.unbridged()), cLoc.unbridged(), IsImplicit);
}

BridgedDictionaryExpr BridgedDictionaryExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLBracketLoc,
    BridgedArrayRef cElements, BridgedArrayRef cCommaLocs,
    BridgedSourceLoc cRBracketLoc) {
  return DictionaryExpr::create(cContext.unbridged(), cLBracketLoc.unbridged(),
                                cElements.unbridged<Expr *>(),
                                cCommaLocs.unbridged<SourceLoc>(),
                                cRBracketLoc.unbridged());
}

BridgedDiscardAssignmentExpr
BridgedDiscardAssignmentExpr_createParsed(BridgedASTContext cContext,
                                          BridgedSourceLoc cLoc) {
  return new (cContext.unbridged())
      DiscardAssignmentExpr(cLoc.unbridged(), /*Implicit=*/false);
}

BridgedDotSelfExpr BridgedDotSelfExpr_createParsed(BridgedASTContext cContext,
                                                   BridgedExpr cSubExpr,
                                                   BridgedSourceLoc cDotLoc,
                                                   BridgedSourceLoc cSelfLoc) {

  return new (cContext.unbridged()) DotSelfExpr(
      cSubExpr.unbridged(), cDotLoc.unbridged(), cSelfLoc.unbridged());
}

BridgedErrorExpr BridgedErrorExpr_create(BridgedASTContext cContext,
                                         BridgedSourceRange cRange) {
  return new (cContext.unbridged()) ErrorExpr(cRange.unbridged());
}

BridgedForceTryExpr
BridgedForceTryExpr_createParsed(BridgedASTContext cContext,
                                 BridgedSourceLoc cTryLoc, BridgedExpr cSubExpr,
                                 BridgedSourceLoc cExclaimLoc) {
  return new (cContext.unbridged()) ForceTryExpr(
      cTryLoc.unbridged(), cSubExpr.unbridged(), cExclaimLoc.unbridged());
}

BridgedFloatLiteralExpr
BridgedFloatLiteralExpr_createParsed(BridgedASTContext cContext,
                                     BridgedStringRef cStr,
                                     BridgedSourceLoc cTokenLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      FloatLiteralExpr(cStr.unbridged(), cTokenLoc.unbridged());
}

BridgedForcedCheckedCastExpr BridgedForcedCheckedCastExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAsLoc,
    BridgedSourceLoc cExclaimLoc, BridgedTypeRepr cType) {
  return ForcedCheckedCastExpr::create(cContext.unbridged(), cAsLoc.unbridged(),
                                       cExclaimLoc.unbridged(),
                                       cType.unbridged());
}

BridgedUnresolvedSpecializeExpr BridgedUnresolvedSpecializeExpr_createParsed(
    BridgedASTContext cContext, BridgedExpr cSubExpr,
    BridgedSourceLoc cLAngleLoc, BridgedArrayRef cArguments,
    BridgedSourceLoc cRAngleLoc) {

  ASTContext &context = cContext.unbridged();
  return UnresolvedSpecializeExpr::create(
      cContext.unbridged(), cSubExpr.unbridged(), cLAngleLoc.unbridged(),
      cArguments.unbridged<TypeRepr *>(), cRAngleLoc.unbridged());
}

BridgedInOutExpr BridgedInOutExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLoc,
                                               BridgedExpr cSubExpr) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      InOutExpr(cLoc.unbridged(), cSubExpr.unbridged(), Type());
}

BridgedIntegerLiteralExpr
BridgedIntegerLiteralExpr_createParsed(BridgedASTContext cContext,
                                       BridgedStringRef cStr,
                                       BridgedSourceLoc cTokenLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      IntegerLiteralExpr(cStr.unbridged(), cTokenLoc.unbridged());
}

BridgedSuperRefExpr
BridgedSuperRefExpr_createParsed(BridgedASTContext cContext,
                                 BridgedSourceLoc cSuperLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      SuperRefExpr(/*Self=*/nullptr, cSuperLoc.unbridged(), /*Implicit=*/false);
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
    BridgedASTContext cContext, BridgedSourceLoc cLoc, size_t literalCapacity,
    size_t interpolationCount, BridgedTapExpr cAppendingExpr) {
  return new (cContext.unbridged()) InterpolatedStringLiteralExpr(
      cLoc.unbridged(), literalCapacity, interpolationCount,
      cAppendingExpr.unbridged());
}

BridgedIsExpr BridgedIsExpr_createParsed(BridgedASTContext cContext,
                                         BridgedSourceLoc cIsLoc,
                                         BridgedTypeRepr cType) {
  return IsExpr::create(cContext.unbridged(), cIsLoc.unbridged(),
                        cType.unbridged());
}

BridgedNilLiteralExpr
BridgedNilLiteralExpr_createParsed(BridgedASTContext cContext,
                                   BridgedSourceLoc cNilKeywordLoc) {
  return new (cContext.unbridged()) NilLiteralExpr(cNilKeywordLoc.unbridged());
}

BridgedOptionalTryExpr BridgedOptionalTryExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cTryLoc, BridgedExpr cSubExpr,
    BridgedSourceLoc cQuestionLoc) {
  return new (cContext.unbridged()) OptionalTryExpr(
      cTryLoc.unbridged(), cSubExpr.unbridged(), cQuestionLoc.unbridged());
}

BridgedPackElementExpr
BridgedPackElementExpr_createParsed(BridgedASTContext cContext,
                                    BridgedSourceLoc cEachLoc,
                                    BridgedExpr cPackRefExpr) {
  return PackElementExpr::create(cContext.unbridged(), cEachLoc.unbridged(),
                                 cPackRefExpr.unbridged());
}

BridgedPackExpansionExpr
BridgedPackExpansionExpr_createParsed(BridgedASTContext cContext,
                                      BridgedSourceLoc cRepeatLoc,
                                      BridgedExpr cPatternExpr) {
  return PackExpansionExpr::create(cContext.unbridged(), cRepeatLoc.unbridged(),
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

BridgedData BridgedRegexLiteralExpr_allocateCaptureStructureSerializationBuffer(
    BridgedASTContext cContext, SwiftInt size) {
  auto buf = cContext.unbridged().AllocateUninitialized<uint8_t>(
      RegexLiteralExpr::getCaptureStructureSerializationAllocationSize(
          unsigned(size)));
  return BridgedData(reinterpret_cast<const char *>(buf.data()), buf.size());
}

BridgedRegexLiteralExpr BridgedRegexLiteralExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLoc,
    BridgedStringRef cRegexText, SwiftInt version,
    BridgedData cCaptureStructure) {
  ArrayRef<uint8_t> captures(
      reinterpret_cast<const uint8_t *>(cCaptureStructure.BaseAddress),
      cCaptureStructure.Length);

  return RegexLiteralExpr::createParsed(cContext.unbridged(), cLoc.unbridged(),
                                        cRegexText.unbridged(),
                                        unsigned(version), captures);
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

BridgedStringLiteralExpr
BridgedStringLiteralExpr_createParsed(BridgedASTContext cContext,
                                      BridgedStringRef cStr,
                                      BridgedSourceLoc cTokenLoc) {
  ASTContext &context = cContext.unbridged();
  auto str = context.AllocateCopy(cStr.unbridged());
  return new (context) StringLiteralExpr(str, cTokenLoc.unbridged());
}

BridgedTapExpr BridgedTapExpr_create(BridgedASTContext cContext,
                                     BridgedBraceStmt cBody) {
  return new (cContext.unbridged()) TapExpr(nullptr, cBody.unbridged());
}

BridgedTernaryExpr BridgedTernaryExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cQuestionLoc,
    BridgedExpr cThenExpr, BridgedSourceLoc cColonLoc) {
  return new (cContext.unbridged()) TernaryExpr(
      cQuestionLoc.unbridged(), cThenExpr.unbridged(), cColonLoc.unbridged());
}

BridgedTryExpr BridgedTryExpr_createParsed(BridgedASTContext cContext,
                                           BridgedSourceLoc cTryLoc,
                                           BridgedExpr cSubExpr) {
  return new (cContext.unbridged())
      TryExpr(cTryLoc.unbridged(), cSubExpr.unbridged());
}

BridgedTupleExpr BridgedTupleExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLParen,
                                               BridgedArrayRef subs,
                                               BridgedArrayRef names,
                                               BridgedArrayRef cNameLocs,
                                               BridgedSourceLoc cRParen) {
  ASTContext &context = cContext.unbridged();
  return TupleExpr::create(
      context, cLParen.unbridged(), subs.unbridged<Expr *>(),
      names.unbridged<Identifier>(), cNameLocs.unbridged<SourceLoc>(),
      cRParen.unbridged(), /*Implicit*/ false);
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
    BridgedASTContext cContext, BridgedExpr base, BridgedSourceLoc cDotLoc,
    BridgedDeclNameRef cName, BridgedDeclNameLoc cNameLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) UnresolvedDotExpr(
      base.unbridged(), cDotLoc.unbridged(), cName.unbridged(),
      cNameLoc.unbridged(), /*isImplicit=*/false);
}

BridgedUnresolvedMemberExpr BridgedUnresolvedMemberExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cDotLoc,
    BridgedDeclNameRef cName, BridgedDeclNameLoc cNameLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      UnresolvedMemberExpr(cDotLoc.unbridged(), cNameLoc.unbridged(),
                           cName.unbridged(), /*isImplicit=*/false);
}

BridgedUnresolvedPatternExpr
BridgedUnresolvedPatternExpr_createParsed(BridgedASTContext cContext,
                                          BridgedPattern cPattern) {
  return new (cContext.unbridged()) UnresolvedPatternExpr(cPattern.unbridged());
}

void BridgedExpr_setImplicit(BridgedExpr cExpr) {
  cExpr.unbridged()->setImplicit();
}
