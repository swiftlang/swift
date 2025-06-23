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

BridgedBindOptionalExpr
BridgedBindOptionalExpr_createParsed(BridgedASTContext cContext,
                                     BridgedExpr cSubExpr,
                                     BridgedSourceLoc cQuestionLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) BindOptionalExpr(cSubExpr.unbridged(),
                                        cQuestionLoc.unbridged(), /*depth=*/0);
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

BridgedCaptureListEntry BridegedCaptureListEntry_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedReferenceOwnership cOwnershipKind,
    BridgedSourceRange cOwnershipRange, Identifier name,
    BridgedSourceLoc cNameLoc, BridgedSourceLoc cEqualLoc,
    BridgedExpr cInitializer) {
  return CaptureListEntry::createParsed(
      cContext.unbridged(), unbridged(cOwnershipKind),
      cOwnershipRange.unbridged(), name, cNameLoc.unbridged(),
      cEqualLoc.unbridged(), cInitializer.unbridged(),
      cDeclContext.unbridged());
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
    BridgedNullableParameterList cParameterList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr cThrownType,
    BridgedSourceLoc cArrowLoc, BridgedNullableTypeRepr cExplicitResultType,
    BridgedSourceLoc cInLoc) {
  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();
  TypeExpr *throwsType = nullptr;
  if (auto *tyR = cThrownType.unbridged())
    throwsType = new (context) TypeExpr(tyR);
  TypeExpr *explicitResultType = nullptr;
  if (auto *tyR = cExplicitResultType.unbridged())
    explicitResultType = new (context) TypeExpr(tyR);

  return new (context)
      ClosureExpr(cAttributes.unbridged(), cBracketRange.unbridged(),
                  cCapturedSelfDecl.unbridged(), cParameterList.unbridged(),
                  cAsyncLoc.unbridged(), cThrowsLoc.unbridged(), throwsType,
                  cArrowLoc.unbridged(), cInLoc.unbridged(), explicitResultType,
                  declContext);
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

BridgedEditorPlaceholderExpr BridgedEditorPlaceholderExpr_createParsed(
    BridgedASTContext cContext, Identifier placeholderId, BridgedSourceLoc cLoc,
    BridgedNullableTypeRepr cPlaceholderTyR,
    BridgedNullableTypeRepr cExpansionTyR) {
  return new (cContext.unbridged()) EditorPlaceholderExpr(
      placeholderId, cLoc.unbridged(), cPlaceholderTyR.unbridged(),
      cExpansionTyR.unbridged());
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

BridgedForceValueExpr
BridgedForceValueExpr_createParsed(BridgedASTContext cContext,
                                   BridgedExpr cSubExpr,
                                   BridgedSourceLoc cExclaimLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      ForceValueExpr(cSubExpr.unbridged(), cExclaimLoc.unbridged());
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
  return UnresolvedSpecializeExpr::create(
      cContext.unbridged(), cSubExpr.unbridged(), cLAngleLoc.unbridged(),
      cArguments.unbridged<TypeRepr *>(), cRAngleLoc.unbridged());
}

BridgedUnsafeExpr BridgedUnsafeExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cUnsafeLoc,
                                               BridgedExpr cSubExpr) {
  return new (cContext.unbridged())
      UnsafeExpr(cUnsafeLoc.unbridged(), cSubExpr.unbridged());
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

BridgedKeyPathDotExpr
BridgedKeyPathDotExpr_createParsed(BridgedASTContext cContext,
                                   BridgedSourceLoc cLoc) {
  return new (cContext.unbridged()) KeyPathDotExpr(cLoc.unbridged());
}

BridgedKeyPathExpr BridgedKeyPathExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cBackslashLoc,
    BridgedNullableExpr cParsedRoot, BridgedNullableExpr cParsedPath,
    bool hasLeadingDot) {
  return KeyPathExpr::createParsed(
      cContext.unbridged(), cBackslashLoc.unbridged(), cParsedRoot.unbridged(),
      cParsedPath.unbridged(), hasLeadingDot);
}

BridgedKeyPathExpr BridgedKeyPathExpr_createParsedPoundKeyPath(
    BridgedASTContext cContext, BridgedSourceLoc cPoundLoc,
    BridgedSourceLoc cLParenLoc, BridgedArrayRef cNames,
    BridgedArrayRef cNameLocs, BridgedSourceLoc cRParenLoc) {

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
      cContext.unbridged(), cPoundLoc.unbridged(), cLParenLoc.unbridged(),
      components, cRParenLoc.unbridged());
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

BridgedMacroExpansionExpr BridgedMacroExpansionExpr_createParsed(
    BridgedDeclContext cDeclContext, BridgedSourceLoc cPoundLoc,
    BridgedDeclNameRef cMacroNameRef, BridgedDeclNameLoc cMacroNameLoc,
    BridgedSourceLoc cLeftAngleLoc, BridgedArrayRef cGenericArgs,
    BridgedSourceLoc cRightAngleLoc, BridgedNullableArgumentList cArgList) {
  auto *DC = cDeclContext.unbridged();
  auto &Context = DC->getASTContext();
  return MacroExpansionExpr::create(
      cDeclContext.unbridged(), cPoundLoc.unbridged(),
      /*module name=*/DeclNameRef(), /*module name loc=*/DeclNameLoc(),
      cMacroNameRef.unbridged(), cMacroNameLoc.unbridged(),
      cLeftAngleLoc.unbridged(),
      Context.AllocateCopy(cGenericArgs.unbridged<TypeRepr *>()),
      cRightAngleLoc.unbridged(), cArgList.unbridged(),
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
    BridgedSourceLoc cLoc) {
  return new (cContext.unbridged())
      MagicIdentifierLiteralExpr(*unbridge(cKind), cLoc.unbridged());
}

BridgedNilLiteralExpr
BridgedNilLiteralExpr_createParsed(BridgedASTContext cContext,
                                   BridgedSourceLoc cNilKeywordLoc) {
  return new (cContext.unbridged()) NilLiteralExpr(cNilKeywordLoc.unbridged());
}

BridgedObjCSelectorExpr BridgedObjCSelectorExpr_createParsed(
    BridgedASTContext cContext, BridgedObjCSelectorKind cKind,
    BridgedSourceLoc cKeywordLoc, BridgedSourceLoc cLParenLoc,
    BridgedSourceLoc cModifierLoc, BridgedExpr cSubExpr,
    BridgedSourceLoc cRParenLoc) {
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
  return new (cContext.unbridged()) ObjCSelectorExpr(
      kind, cKeywordLoc.unbridged(), cLParenLoc.unbridged(),
      cModifierLoc.unbridged(), cSubExpr.unbridged(), cRParenLoc.unbridged());
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
    BridgedASTContext cContext, BridgedSourceLoc cPoundLoc,
    BridgedObjectLiteralKind cKind, BridgedArgumentList cArgs) {
  return ObjectLiteralExpr::create(cContext.unbridged(), cPoundLoc.unbridged(),
                                   *unbridge(cKind), cArgs.unbridged(),
                                   /*implicit=*/false);
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

BridgedRegexLiteralExpr
BridgedRegexLiteralExpr_createParsed(BridgedASTContext cContext,
                                     BridgedSourceLoc cLoc,
                                     BridgedStringRef cRegexText) {
  return RegexLiteralExpr::createParsed(cContext.unbridged(), cLoc.unbridged(),
                                        cRegexText.unbridged());
}

BridgedParenExpr BridgedParenExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLParen,
                                               BridgedExpr cExpr,
                                               BridgedSourceLoc cRParen) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      ParenExpr(cLParen.unbridged(), cExpr.unbridged(), cRParen.unbridged());
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
