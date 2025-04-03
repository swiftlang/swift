//===--- Bridging/PatternBridging.cpp -------------------------------------===//
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
#include "swift/AST/Pattern.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: Patterns
//===----------------------------------------------------------------------===//

// Define `.asPattern` on each BridgedXXXPattern type.
#define PATTERN(Id, Parent)                                                    \
  BridgedPattern Bridged##Id##Pattern_asPattern(                               \
      Bridged##Id##Pattern pattern) {                                          \
    return static_cast<Pattern *>(pattern.unbridged());                        \
  }
#include "swift/AST/PatternNodes.def"

BridgedNullableVarDecl BridgedPattern_getSingleVar(BridgedPattern cPattern) {
  return cPattern.unbridged()->getSingleVar();
}

BridgedAnyPattern BridgedAnyPattern_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cLoc) {
  return new (cContext.unbridged()) AnyPattern(cLoc.unbridged());
}

BridgedAnyPattern BridgedAnyPattern_createImplicit(BridgedASTContext cContext) {
  return AnyPattern::createImplicit(cContext.unbridged());
}

BridgedBindingPattern
BridgedBindingPattern_createParsed(BridgedASTContext cContext,
                                   BridgedSourceLoc cKeywordLoc, bool isLet,
                                   BridgedPattern cSubPattern) {
  VarDecl::Introducer introducer =
      isLet ? VarDecl::Introducer::Let : VarDecl::Introducer::Var;
  return BindingPattern::createParsed(cContext.unbridged(),
                                      cKeywordLoc.unbridged(), introducer,
                                      cSubPattern.unbridged());
}

BridgedBindingPattern
BridgedBindingPattern_createImplicitCatch(BridgedDeclContext cDeclContext,
                                          BridgedSourceLoc cLoc) {
  return BindingPattern::createImplicitCatch(cDeclContext.unbridged(),
                                             cLoc.unbridged());
}

BridgedExprPattern
BridgedExprPattern_createParsed(BridgedDeclContext cDeclContext,
                                BridgedExpr cExpr) {
  auto *DC = cDeclContext.unbridged();
  auto &context = DC->getASTContext();
  return ExprPattern::createParsed(context, cExpr.unbridged(), DC);
}

BridgedIsPattern BridgedIsPattern_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cIsLoc,
                                               BridgedTypeExpr cTypeExpr) {
  return new (cContext.unbridged())
      IsPattern(cIsLoc.unbridged(), cTypeExpr.unbridged(),
                /*subPattern=*/nullptr, CheckedCastKind::Unresolved);
}

BridgedNamedPattern
BridgedNamedPattern_createParsed(BridgedASTContext cContext,
                                 BridgedDeclContext cDeclContext,
                                 BridgedIdentifier name, BridgedSourceLoc loc) {
  auto &context = cContext.unbridged();
  auto *dc = cDeclContext.unbridged();

  // Note 'isStatic' and the introducer value are temporary.
  // The outer context should set the correct values.
  auto *varDecl = new (context) VarDecl(
      /*isStatic=*/false, VarDecl::Introducer::Let, loc.unbridged(),
      name.unbridged(), dc);
  auto *pattern = new (context) NamedPattern(varDecl);
  return pattern;
}

BridgedParenPattern BridgedParenPattern_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLParenLoc,
    BridgedPattern cSubPattern, BridgedSourceLoc cRParenLoc) {
  return new (cContext.unbridged()) ParenPattern(
      cLParenLoc.unbridged(), cSubPattern.unbridged(), cRParenLoc.unbridged());
}

BridgedTuplePattern BridgedTuplePattern_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLParenLoc,
    BridgedArrayRef cElements, BridgedSourceLoc cRParenLoc) {
  ASTContext &context = cContext.unbridged();
  llvm::SmallVector<TuplePatternElt, 4> elements;
  elements.reserve(cElements.Length);
  llvm::transform(cElements.unbridged<BridgedTuplePatternElt>(),
                  std::back_inserter(elements),
                  [](const BridgedTuplePatternElt &elt) {
                    return TuplePatternElt(elt.Label.unbridged(),
                                           elt.LabelLoc.unbridged(),
                                           elt.ThePattern.unbridged());
                  });

  return TuplePattern::create(context, cLParenLoc.unbridged(), elements,
                              cRParenLoc.unbridged());
}

BridgedTypedPattern BridgedTypedPattern_createParsed(BridgedASTContext cContext,
                                                     BridgedPattern cPattern,
                                                     BridgedTypeRepr cType) {
  return new (cContext.unbridged())
      TypedPattern(cPattern.unbridged(), cType.unbridged());
}

BridgedTypedPattern
BridgedTypedPattern_createPropagated(BridgedASTContext cContext,
                                     BridgedPattern cPattern,
                                     BridgedTypeRepr cType) {
  return TypedPattern::createPropagated(
      cContext.unbridged(), cPattern.unbridged(), cType.unbridged());
}

void BridgedPattern_setImplicit(BridgedPattern cPattern) {
  cPattern.unbridged()->setImplicit();
}

BridgedIdentifier BridgedPattern_getBoundName(BridgedPattern cPattern) {
  return cPattern.unbridged()->getBoundName();
}
