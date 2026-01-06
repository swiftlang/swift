//===--- Bridging/PatternBridging.cpp -------------------------------------===//
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
                                                 SourceLoc loc) {
  return new (cContext.unbridged()) AnyPattern(loc);
}

BridgedAnyPattern BridgedAnyPattern_createImplicit(BridgedASTContext cContext) {
  return AnyPattern::createImplicit(cContext.unbridged());
}

BridgedBindingPattern
BridgedBindingPattern_createParsed(BridgedASTContext cContext,
                                   SourceLoc keywordLoc, bool isLet,
                                   BridgedPattern cSubPattern) {
  VarDecl::Introducer introducer =
      isLet ? VarDecl::Introducer::Let : VarDecl::Introducer::Var;
  return BindingPattern::createParsed(cContext.unbridged(), keywordLoc,
                                      introducer, cSubPattern.unbridged());
}

BridgedBindingPattern
BridgedBindingPattern_createImplicitCatch(BridgedDeclContext cDeclContext,
                                          SourceLoc loc) {
  return BindingPattern::createImplicitCatch(cDeclContext.unbridged(), loc);
}

BridgedExprPattern
BridgedExprPattern_createParsed(BridgedDeclContext cDeclContext,
                                BridgedExpr cExpr) {
  auto *DC = cDeclContext.unbridged();
  auto &context = DC->getASTContext();
  return ExprPattern::createParsed(context, cExpr.unbridged(), DC);
}

BridgedIsPattern BridgedIsPattern_createParsed(BridgedASTContext cContext,
                                               SourceLoc isLoc,
                                               BridgedTypeExpr cTypeExpr) {
  return new (cContext.unbridged())
      IsPattern(isLoc, cTypeExpr.unbridged(),
                /*subPattern=*/nullptr, CheckedCastKind::Unresolved);
}

BridgedNamedPattern
BridgedNamedPattern_createParsed(BridgedASTContext cContext,
                                 BridgedDeclContext cDeclContext,
                                 Identifier name, SourceLoc loc) {
  auto &context = cContext.unbridged();
  auto *dc = cDeclContext.unbridged();

  // Note 'isStatic' and the introducer value are temporary.
  // The outer context should set the correct values.
  auto *varDecl = new (context) VarDecl(
      /*isStatic=*/false, VarDecl::Introducer::Let, loc, name, dc);
  auto *pattern = new (context) NamedPattern(varDecl);
  return pattern;
}

BridgedParenPattern BridgedParenPattern_createParsed(BridgedASTContext cContext,
                                                     SourceLoc lParenLoc,
                                                     BridgedPattern cSubPattern,
                                                     SourceLoc rParenLoc) {
  return new (cContext.unbridged())
      ParenPattern(lParenLoc, cSubPattern.unbridged(), rParenLoc);
}

BridgedTuplePattern BridgedTuplePattern_createParsed(BridgedASTContext cContext,
                                                     SourceLoc lParenLoc,
                                                     BridgedArrayRef cElements,
                                                     SourceLoc rParenLoc) {
  ASTContext &context = cContext.unbridged();
  llvm::SmallVector<TuplePatternElt, 4> elements;
  elements.reserve(cElements.Length);
  llvm::transform(cElements.unbridged<BridgedTuplePatternElt>(),
                  std::back_inserter(elements),
                  [](const BridgedTuplePatternElt &elt) {
                    return TuplePatternElt(elt.Label, elt.LabelLoc,
                                           elt.ThePattern.unbridged());
                  });

  return TuplePattern::create(context, lParenLoc, elements, rParenLoc);
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

Identifier BridgedPattern_getBoundName(BridgedPattern cPattern) {
  return cPattern.unbridged()->getBoundName();
}
