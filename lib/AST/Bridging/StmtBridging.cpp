//===--- Bridging/StmtBridging.cpp ----------------------------------------===//
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
#include "swift/AST/ASTNode.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ParseRequests.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: Stmts
//===----------------------------------------------------------------------===//

// Define `.asStmt` on each BridgedXXXStmt type.
#define STMT(Id, Parent)                                                       \
  BridgedStmt Bridged##Id##Stmt_asStmt(Bridged##Id##Stmt stmt) {               \
    return static_cast<Stmt *>(stmt.unbridged());                              \
  }
#define ABSTRACT_STMT(Id, Parent) STMT(Id, Parent)
#include "swift/AST/StmtNodes.def"

BridgedStmtConditionElement
BridgedStmtConditionElement_createBoolean(BridgedExpr expr) {
  return StmtConditionElement(expr.unbridged());
}

BridgedStmtConditionElement BridgedStmtConditionElement_createPatternBinding(
    BridgedASTContext cContext, SourceLoc introducerLoc,
    BridgedPattern cPattern, BridgedExpr cInitializer) {
  return StmtConditionElement(ConditionalPatternBindingInfo::create(
      cContext.unbridged(), introducerLoc, cPattern.unbridged(),
      cInitializer.unbridged()));
}

BridgedStmtConditionElement BridgedStmtConditionElement_createPoundAvailable(
    BridgedPoundAvailableInfo info) {
  return StmtConditionElement(info.unbridged());
}

BridgedPoundAvailableInfo BridgedPoundAvailableInfo_createParsed(
    BridgedASTContext cContext, SourceLoc poundLoc, SourceLoc lParenLoc,
    BridgedArrayRef cSpecs, SourceLoc rParenLoc, bool isUnavailability) {
  SmallVector<AvailabilitySpec *, 4> specs;
  for (auto cSpec : cSpecs.unbridged<BridgedAvailabilitySpec>())
    specs.push_back(cSpec.unbridged());
  return PoundAvailableInfo::create(cContext.unbridged(), poundLoc, lParenLoc,
                                    specs, rParenLoc, isUnavailability);
}

BridgedStmtConditionElement BridgedStmtConditionElement_createHasSymbol(
    BridgedASTContext cContext, SourceLoc poundLoc, SourceLoc lParenLoc,
    BridgedNullableExpr cSymbolExpr, SourceLoc rParenLoc) {
  return StmtConditionElement(
      PoundHasSymbolInfo::create(cContext.unbridged(), poundLoc, lParenLoc,
                                 cSymbolExpr.unbridged(), rParenLoc));
}

BridgedBraceStmt BridgedBraceStmt_createParsed(BridgedASTContext cContext,
                                               SourceLoc lBLoc,
                                               BridgedArrayRef elements,
                                               SourceLoc rBLoc) {
  llvm::SmallVector<ASTNode, 16> nodes;
  for (auto node : elements.unbridged<BridgedASTNode>())
    nodes.push_back(node.unbridged());

  ASTContext &context = cContext.unbridged();
  return BraceStmt::create(context, lBLoc, nodes, rBLoc);
}

BridgedBraceStmt BridgedBraceStmt_createImplicit(BridgedASTContext cContext,
                                                 SourceLoc lBLoc,
                                                 BridgedASTNode element,
                                                 SourceLoc rBLoc) {
  return BraceStmt::create(cContext.unbridged(), lBLoc, {element.unbridged()},
                           rBLoc,
                           /*Implicit=*/true);
}

BridgedBreakStmt BridgedBreakStmt_createParsed(BridgedDeclContext cDeclContext,
                                               SourceLoc loc,
                                               Identifier targetName,
                                               SourceLoc targetLoc) {
  return new (cDeclContext.unbridged()->getASTContext())
      BreakStmt(loc, targetName, targetLoc, cDeclContext.unbridged());
}

static void getCaseLabelItems(BridgedArrayRef cItems,
                       SmallVectorImpl<CaseLabelItem> &output) {
  for (auto &elem : cItems.unbridged<BridgedCaseLabelItemInfo>()) {
    if (!elem.IsDefault) {
      output.emplace_back(elem.ThePattern.unbridged(), elem.WhereLoc,
                          elem.GuardExpr.unbridged());
    } else {
      output.push_back(CaseLabelItem::getDefault(
          cast<AnyPattern>(elem.ThePattern.unbridged()), elem.WhereLoc,
          elem.GuardExpr.unbridged()));
    }
  }
}

BridgedCaseStmt BridgedCaseStmt_createParsedSwitchCase(
    BridgedASTContext cContext, SourceLoc introducerLoc,
    BridgedArrayRef cCaseLabelItems, SourceLoc unknownAttrLoc,
    SourceLoc terminatorLoc, BridgedBraceStmt cBody) {
  SmallVector<CaseLabelItem, 1> labelItems;
  getCaseLabelItems(cCaseLabelItems, labelItems);

  return CaseStmt::createParsedSwitchCase(cContext.unbridged(), introducerLoc,
                                          labelItems, unknownAttrLoc,
                                          terminatorLoc, cBody.unbridged());
}

BridgedCaseStmt BridgedCaseStmt_createParsedDoCatch(
    BridgedASTContext cContext, SourceLoc catchLoc,
    BridgedArrayRef cCaseLabelItems, BridgedBraceStmt cBody) {
  SmallVector<CaseLabelItem, 1> labelItems;
  getCaseLabelItems(cCaseLabelItems, labelItems);

  return CaseStmt::createParsedDoCatch(cContext.unbridged(), catchLoc,
                                       labelItems, cBody.unbridged());
}

BridgedContinueStmt
BridgedContinueStmt_createParsed(BridgedDeclContext cDeclContext, SourceLoc loc,
                                 Identifier targetName, SourceLoc targetLoc) {
  return new (cDeclContext.unbridged()->getASTContext())
      ContinueStmt(loc, targetName, targetLoc, cDeclContext.unbridged());
}

BridgedDeferStmt BridgedDeferStmt_createParsed(BridgedDeclContext cDeclContext,
                                               SourceLoc deferLoc) {
  return DeferStmt::create(cDeclContext.unbridged(), deferLoc);
}

BridgedFuncDecl BridgedDeferStmt_getTempDecl(BridgedDeferStmt bridged) {
  return bridged.unbridged()->getTempDecl();
}

BridgedDiscardStmt BridgedDiscardStmt_createParsed(BridgedASTContext cContext,
                                                   SourceLoc discardLoc,
                                                   BridgedExpr cSubExpr) {
  return new (cContext.unbridged())
      DiscardStmt(discardLoc, cSubExpr.unbridged());
}

BridgedDoStmt BridgedDoStmt_createParsed(BridgedASTContext cContext,
                                         BridgedLabeledStmtInfo cLabelInfo,
                                         SourceLoc doLoc,
                                         BridgedBraceStmt cBody) {
  return new (cContext.unbridged())
      DoStmt(cLabelInfo.unbridged(), doLoc, cBody.unbridged());
}

BridgedDoCatchStmt BridgedDoCatchStmt_createParsed(
    BridgedDeclContext cDeclContext, BridgedLabeledStmtInfo cLabelInfo,
    SourceLoc doLoc, SourceLoc throwsLoc, BridgedNullableTypeRepr cThrownType,
    BridgedStmt cBody, BridgedArrayRef cCatches) {
  return DoCatchStmt::create(cDeclContext.unbridged(), cLabelInfo.unbridged(),
                             doLoc, throwsLoc, cThrownType.unbridged(),
                             cBody.unbridged(),
                             cCatches.unbridged<CaseStmt *>());
}

BridgedFallthroughStmt
BridgedFallthroughStmt_createParsed(SourceLoc loc, BridgedDeclContext cDC) {
  return FallthroughStmt::createParsed(loc, cDC.unbridged());
}

BridgedForEachStmt BridgedForEachStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    SourceLoc forLoc, SourceLoc tryLoc, SourceLoc awaitLoc, SourceLoc unsafeLoc,
    BridgedPattern cPat, SourceLoc inLoc, BridgedExpr cSequence,
    SourceLoc whereLoc, BridgedNullableExpr cWhereExpr,
    BridgedBraceStmt cBody) {
  return new (cContext.unbridged())
      ForEachStmt(cLabelInfo.unbridged(), forLoc, tryLoc, awaitLoc, unsafeLoc,
                  cPat.unbridged(), inLoc, cSequence.unbridged(), whereLoc,
                  cWhereExpr.unbridged(), cBody.unbridged());
}

BridgedGuardStmt BridgedGuardStmt_createParsed(BridgedASTContext cContext,
                                               SourceLoc guardLoc,
                                               BridgedArrayRef cConds,
                                               BridgedBraceStmt cBody) {
  auto &context = cContext.unbridged();
  StmtCondition cond = context.AllocateTransform<StmtConditionElement>(
      cConds.unbridged<BridgedStmtConditionElement>(),
      [](auto &e) { return e.unbridged(); });

  return new (context) GuardStmt(guardLoc, cond, cBody.unbridged());
}

BridgedIfStmt
BridgedIfStmt_createParsed(BridgedASTContext cContext,
                           BridgedLabeledStmtInfo cLabelInfo, SourceLoc ifLoc,
                           BridgedArrayRef cConds, BridgedBraceStmt cThen,
                           SourceLoc elseLoc, BridgedNullableStmt cElse) {
  auto &context = cContext.unbridged();
  StmtCondition cond = context.AllocateTransform<StmtConditionElement>(
      cConds.unbridged<BridgedStmtConditionElement>(),
      [](auto &e) { return e.unbridged(); });

  return new (context) IfStmt(cLabelInfo.unbridged(), ifLoc, cond,
                              cThen.unbridged(), elseLoc, cElse.unbridged());
}

BridgedPoundAssertStmt BridgedPoundAssertStmt_createParsed(
    BridgedASTContext cContext, SourceRange range, BridgedExpr cConditionExpr,
    BridgedStringRef cMessage) {
  return new (cContext.unbridged())
      PoundAssertStmt(range, cConditionExpr.unbridged(), cMessage.unbridged());
}

BridgedRepeatWhileStmt
BridgedRepeatWhileStmt_createParsed(BridgedASTContext cContext,
                                    BridgedLabeledStmtInfo cLabelInfo,
                                    SourceLoc repeatLoc, BridgedExpr cCond,
                                    SourceLoc whileLoc, BridgedStmt cBody) {
  return new (cContext.unbridged())
      RepeatWhileStmt(cLabelInfo.unbridged(), repeatLoc, cCond.unbridged(),
                      whileLoc, cBody.unbridged());
}

BridgedReturnStmt BridgedReturnStmt_createParsed(BridgedASTContext cContext,
                                                 SourceLoc loc,
                                                 BridgedNullableExpr expr) {
  ASTContext &context = cContext.unbridged();
  return ReturnStmt::createParsed(context, loc, expr.unbridged());
}

BridgedSwitchStmt BridgedSwitchStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    SourceLoc switchLoc, BridgedExpr cSubjectExpr, SourceLoc lBraceLoc,
    BridgedArrayRef cCases, SourceLoc rBraceLoc) {
  SmallVector<CaseStmt *, 16> cases;
  for (auto cCase : cCases.unbridged<BridgedCaseStmt>())
    cases.push_back(cCase.unbridged());
  return SwitchStmt::create(cLabelInfo.unbridged(), switchLoc,
                            cSubjectExpr.unbridged(), lBraceLoc, cases,
                            rBraceLoc, rBraceLoc, cContext.unbridged());
}

BridgedThenStmt BridgedThenStmt_createParsed(BridgedASTContext cContext,
                                             SourceLoc thenLoc,
                                             BridgedExpr cResult) {
  return ThenStmt::createParsed(cContext.unbridged(), thenLoc,
                                cResult.unbridged());
}

BridgedThrowStmt BridgedThrowStmt_createParsed(BridgedASTContext cContext,
                                               SourceLoc throwLoc,
                                               BridgedExpr cSubExpr) {
  return new (cContext.unbridged()) ThrowStmt(throwLoc, cSubExpr.unbridged());
}

BridgedWhileStmt BridgedWhileStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    SourceLoc whileLoc, BridgedArrayRef cCond, BridgedStmt cBody) {
  auto &context = cContext.unbridged();
  StmtCondition cond = context.AllocateTransform<StmtConditionElement>(
      cCond.unbridged<BridgedStmtConditionElement>(),
      [](auto &e) { return e.unbridged(); });

  return new (cContext.unbridged())
      WhileStmt(cLabelInfo.unbridged(), whileLoc, cond, cBody.unbridged());
}

BridgedYieldStmt BridgedYieldStmt_createParsed(BridgedASTContext cContext,
                                               SourceLoc yieldLoc,
                                               SourceLoc lParenLoc,
                                               BridgedArrayRef cYields,
                                               SourceLoc rParenLoc) {
  return YieldStmt::create(cContext.unbridged(), yieldLoc, lParenLoc,
                           cYields.unbridged<Expr *>(), rParenLoc);
}
