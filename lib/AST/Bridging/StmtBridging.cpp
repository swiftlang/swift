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
    BridgedASTContext cContext, BridgedSourceLoc cIntroducerLoc,
    BridgedPattern cPattern, BridgedExpr cInitializer) {
  return StmtConditionElement(ConditionalPatternBindingInfo::create(
      cContext.unbridged(), cIntroducerLoc.unbridged(), cPattern.unbridged(),
      cInitializer.unbridged()));
}

BridgedStmtConditionElement BridgedStmtConditionElement_createPoundAvailable(
    BridgedPoundAvailableInfo info) {
  return StmtConditionElement(info.unbridged());
}

BridgedPoundAvailableInfo BridgedPoundAvailableInfo_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cPoundLoc,
    BridgedSourceLoc cLParenLoc, BridgedArrayRef cSpecs,
    BridgedSourceLoc cRParenLoc, bool isUnavailability) {
  SmallVector<AvailabilitySpec *, 4> specs;
  for (auto cSpec : cSpecs.unbridged<BridgedAvailabilitySpec>())
    specs.push_back(cSpec.unbridged());
  return PoundAvailableInfo::create(cContext.unbridged(), cPoundLoc.unbridged(),
                                    cLParenLoc.unbridged(), specs,
                                    cRParenLoc.unbridged(), isUnavailability);
}

BridgedStmtConditionElement BridgedStmtConditionElement_createHasSymbol(
    BridgedASTContext cContext, BridgedSourceLoc cPoundLoc,
    BridgedSourceLoc cLParenLoc, BridgedNullableExpr cSymbolExpr,
    BridgedSourceLoc cRParenLoc) {
  return StmtConditionElement(PoundHasSymbolInfo::create(
      cContext.unbridged(), cPoundLoc.unbridged(), cLParenLoc.unbridged(),
      cSymbolExpr.unbridged(), cRParenLoc.unbridged()));
}

BridgedBraceStmt BridgedBraceStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLBLoc,
                                               BridgedArrayRef elements,
                                               BridgedSourceLoc cRBLoc) {
  llvm::SmallVector<ASTNode, 16> nodes;
  for (auto node : elements.unbridged<BridgedASTNode>())
    nodes.push_back(node.unbridged());

  ASTContext &context = cContext.unbridged();
  return BraceStmt::create(context, cLBLoc.unbridged(), nodes,
                           cRBLoc.unbridged());
}

BridgedBraceStmt BridgedBraceStmt_createImplicit(BridgedASTContext cContext,
                                                 BridgedSourceLoc cLBLoc,
                                                 BridgedASTNode element,
                                                 BridgedSourceLoc cRBLoc) {
  return BraceStmt::create(cContext.unbridged(), cLBLoc.unbridged(),
                           {element.unbridged()}, cRBLoc.unbridged(),
                           /*Implicit=*/true);
}

BridgedBreakStmt BridgedBreakStmt_createParsed(BridgedDeclContext cDeclContext,
                                               BridgedSourceLoc cLoc,
                                               Identifier targetName,
                                               BridgedSourceLoc cTargetLoc) {
  return new (cDeclContext.unbridged()->getASTContext())
      BreakStmt(cLoc.unbridged(), targetName, cTargetLoc.unbridged(),
                cDeclContext.unbridged());
}

static void getCaseLabelItems(BridgedArrayRef cItems,
                       SmallVectorImpl<CaseLabelItem> &output) {
  for (auto &elem : cItems.unbridged<BridgedCaseLabelItemInfo>()) {
    if (!elem.IsDefault) {
      output.emplace_back(elem.ThePattern.unbridged(),
                          elem.WhereLoc.unbridged(),
                          elem.GuardExpr.unbridged());
    } else {
      output.push_back(CaseLabelItem::getDefault(
          cast<AnyPattern>(elem.ThePattern.unbridged()),
          elem.WhereLoc.unbridged(), elem.GuardExpr.unbridged()));
    }
  }
}

BridgedCaseStmt BridgedCaseStmt_createParsedSwitchCase(
    BridgedASTContext cContext, BridgedSourceLoc cIntroducerLoc,
    BridgedArrayRef cCaseLabelItems, BridgedSourceLoc cUnknownAttrLoc,
    BridgedSourceLoc cTerminatorLoc, BridgedBraceStmt cBody) {
  SmallVector<CaseLabelItem, 1> labelItems;
  getCaseLabelItems(cCaseLabelItems, labelItems);

  return CaseStmt::createParsedSwitchCase(
      cContext.unbridged(), cIntroducerLoc.unbridged(), labelItems,
      cUnknownAttrLoc.unbridged(), cTerminatorLoc.unbridged(),
      cBody.unbridged());
}

BridgedCaseStmt BridgedCaseStmt_createParsedDoCatch(
    BridgedASTContext cContext, BridgedSourceLoc cCatchLoc,
    BridgedArrayRef cCaseLabelItems, BridgedBraceStmt cBody) {
  SmallVector<CaseLabelItem, 1> labelItems;
  getCaseLabelItems(cCaseLabelItems, labelItems);

  return CaseStmt::createParsedDoCatch(cContext.unbridged(),
                                       cCatchLoc.unbridged(), labelItems,
                                       cBody.unbridged());
}

BridgedContinueStmt
BridgedContinueStmt_createParsed(BridgedDeclContext cDeclContext,
                                 BridgedSourceLoc cLoc, Identifier targetName,
                                 BridgedSourceLoc cTargetLoc) {
  return new (cDeclContext.unbridged()->getASTContext())
      ContinueStmt(cLoc.unbridged(), targetName, cTargetLoc.unbridged(),
                   cDeclContext.unbridged());
}

BridgedDeferStmt BridgedDeferStmt_createParsed(BridgedDeclContext cDeclContext,
                                               BridgedSourceLoc cDeferLoc) {
  return DeferStmt::create(cDeclContext.unbridged(), cDeferLoc.unbridged());
}

BridgedFuncDecl BridgedDeferStmt_getTempDecl(BridgedDeferStmt bridged) {
  return bridged.unbridged()->getTempDecl();
}

BridgedDiscardStmt BridgedDiscardStmt_createParsed(BridgedASTContext cContext,
                                                   BridgedSourceLoc cDiscardLoc,
                                                   BridgedExpr cSubExpr) {
  return new (cContext.unbridged())
      DiscardStmt(cDiscardLoc.unbridged(), cSubExpr.unbridged());
}

BridgedDoStmt BridgedDoStmt_createParsed(BridgedASTContext cContext,
                                         BridgedLabeledStmtInfo cLabelInfo,
                                         BridgedSourceLoc cDoLoc,
                                         BridgedBraceStmt cBody) {
  return new (cContext.unbridged())
      DoStmt(cLabelInfo.unbridged(), cDoLoc.unbridged(), cBody.unbridged());
}

BridgedDoCatchStmt BridgedDoCatchStmt_createParsed(
    BridgedDeclContext cDeclContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cDoLoc, BridgedSourceLoc cThrowsLoc,
    BridgedNullableTypeRepr cThrownType, BridgedStmt cBody,
    BridgedArrayRef cCatches) {
  return DoCatchStmt::create(cDeclContext.unbridged(), cLabelInfo.unbridged(),
                             cDoLoc.unbridged(), cThrowsLoc.unbridged(),
                             cThrownType.unbridged(), cBody.unbridged(),
                             cCatches.unbridged<CaseStmt *>());
}

BridgedFallthroughStmt
BridgedFallthroughStmt_createParsed(BridgedSourceLoc cLoc,
                                    BridgedDeclContext cDC) {
  return FallthroughStmt::createParsed(cLoc.unbridged(), cDC.unbridged());
}

BridgedForEachStmt BridgedForEachStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cForLoc, BridgedSourceLoc cTryLoc,
    BridgedSourceLoc cAwaitLoc, BridgedSourceLoc cUnsafeLoc,
    BridgedPattern cPat, BridgedSourceLoc cInLoc,
    BridgedExpr cSequence, BridgedSourceLoc cWhereLoc,
    BridgedNullableExpr cWhereExpr, BridgedBraceStmt cBody) {
  return new (cContext.unbridged()) ForEachStmt(
      cLabelInfo.unbridged(), cForLoc.unbridged(), cTryLoc.unbridged(),
      cAwaitLoc.unbridged(), cUnsafeLoc.unbridged(), cPat.unbridged(),
      cInLoc.unbridged(), cSequence.unbridged(), cWhereLoc.unbridged(),
      cWhereExpr.unbridged(), cBody.unbridged());
}

BridgedGuardStmt BridgedGuardStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cGuardLoc,
                                               BridgedArrayRef cConds,
                                               BridgedBraceStmt cBody) {
  auto &context = cContext.unbridged();
  StmtCondition cond = context.AllocateTransform<StmtConditionElement>(
      cConds.unbridged<BridgedStmtConditionElement>(),
      [](auto &e) { return e.unbridged(); });

  return new (context)
      GuardStmt(cGuardLoc.unbridged(), cond, cBody.unbridged());
}

BridgedIfStmt BridgedIfStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cIfLoc, BridgedArrayRef cConds, BridgedBraceStmt cThen,
    BridgedSourceLoc cElseLoc, BridgedNullableStmt cElse) {
  auto &context = cContext.unbridged();
  StmtCondition cond = context.AllocateTransform<StmtConditionElement>(
      cConds.unbridged<BridgedStmtConditionElement>(),
      [](auto &e) { return e.unbridged(); });

  return new (context)
      IfStmt(cLabelInfo.unbridged(), cIfLoc.unbridged(), cond,
             cThen.unbridged(), cElseLoc.unbridged(), cElse.unbridged());
}

BridgedPoundAssertStmt BridgedPoundAssertStmt_createParsed(
    BridgedASTContext cContext, BridgedSourceRange cRange,
    BridgedExpr cConditionExpr, BridgedStringRef cMessage) {
  return new (cContext.unbridged()) PoundAssertStmt(
      cRange.unbridged(), cConditionExpr.unbridged(), cMessage.unbridged());
}

BridgedRepeatWhileStmt BridgedRepeatWhileStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cRepeatLoc, BridgedExpr cCond, BridgedSourceLoc cWhileLoc,
    BridgedStmt cBody) {
  return new (cContext.unbridged()) RepeatWhileStmt(
      cLabelInfo.unbridged(), cRepeatLoc.unbridged(), cCond.unbridged(),
      cWhileLoc.unbridged(), cBody.unbridged());
}

BridgedReturnStmt BridgedReturnStmt_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cLoc,
                                                 BridgedNullableExpr expr) {
  ASTContext &context = cContext.unbridged();
  return ReturnStmt::createParsed(context, cLoc.unbridged(), expr.unbridged());
}

BridgedSwitchStmt BridgedSwitchStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cSwitchLoc, BridgedExpr cSubjectExpr,
    BridgedSourceLoc cLBraceLoc, BridgedArrayRef cCases,
    BridgedSourceLoc cRBraceLoc) {
  SmallVector<CaseStmt *, 16> cases;
  for (auto cCase : cCases.unbridged<BridgedCaseStmt>())
    cases.push_back(cCase.unbridged());
  return SwitchStmt::create(cLabelInfo.unbridged(), cSwitchLoc.unbridged(),
                            cSubjectExpr.unbridged(), cLBraceLoc.unbridged(),
                            cases, cRBraceLoc.unbridged(),
                            cRBraceLoc.unbridged(), cContext.unbridged());
}

BridgedThenStmt BridgedThenStmt_createParsed(BridgedASTContext cContext,
                                             BridgedSourceLoc cThenLoc,
                                             BridgedExpr cResult) {
  return ThenStmt::createParsed(cContext.unbridged(), cThenLoc.unbridged(),
                                cResult.unbridged());
}

BridgedThrowStmt BridgedThrowStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cThrowLoc,
                                               BridgedExpr cSubExpr) {
  return new (cContext.unbridged())
      ThrowStmt(cThrowLoc.unbridged(), cSubExpr.unbridged());
}

BridgedWhileStmt BridgedWhileStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cWhileLoc, BridgedArrayRef cCond, BridgedStmt cBody) {
  auto &context = cContext.unbridged();
  StmtCondition cond = context.AllocateTransform<StmtConditionElement>(
      cCond.unbridged<BridgedStmtConditionElement>(),
      [](auto &e) { return e.unbridged(); });

  return new (cContext.unbridged()) WhileStmt(
      cLabelInfo.unbridged(), cWhileLoc.unbridged(), cond, cBody.unbridged());
}

BridgedYieldStmt BridgedYieldStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cYieldLoc,
                                               BridgedSourceLoc cLParenLoc,
                                               BridgedArrayRef cYields,
                                               BridgedSourceLoc cRParenLoc) {
  return YieldStmt::create(cContext.unbridged(), cYieldLoc.unbridged(),
                           cLParenLoc.unbridged(), cYields.unbridged<Expr *>(),
                           cRParenLoc.unbridged());
}
