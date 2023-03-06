//===--- SolutionApplicationTarget.cpp - Solution Target ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements SolutionApplicationTarget
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/SolutionApplicationTarget.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/TypeRepr.h"
#include "TypeChecker.h"

using namespace swift;
using namespace constraints;

#define DEBUG_TYPE "SolutionApplicationTarget"

SolutionApplicationTarget::SolutionApplicationTarget(
    Expr *expr, DeclContext *dc, ContextualTypePurpose contextualPurpose,
    TypeLoc convertType, ConstraintLocator *convertTypeLocator,
    bool isDiscarded) {
  // Verify that a purpose was specified if a convertType was.  Note that it is
  // ok to have a purpose without a convertType (which is used for call
  // return types).
  assert((!convertType.getType() || contextualPurpose != CTP_Unused) &&
         "Purpose for conversion type was not specified");

  // Take a look at the conversion type to check to make sure it is sensible.
  if (auto type = convertType.getType()) {
    // If we're asked to convert to an UnresolvedType, then ignore the request.
    // This happens when CSDiags nukes a type.
    if (type->is<UnresolvedType>() ||
        (type->is<MetatypeType>() && type->hasUnresolvedType())) {
      convertType = TypeLoc();
      contextualPurpose = CTP_Unused;
    }
  }

  kind = Kind::expression;
  expression.expression = expr;
  expression.dc = dc;
  expression.contextualPurpose = contextualPurpose;
  expression.convertType = convertType;
  expression.convertTypeLocator = convertTypeLocator;
  expression.pattern = nullptr;
  expression.propertyWrapper.wrappedVar = nullptr;
  expression.propertyWrapper.innermostWrappedValueInit = nullptr;
  expression.propertyWrapper.hasInitialWrappedValue = false;
  expression.isDiscarded = isDiscarded;
  expression.bindPatternVarsOneWay = false;
  expression.initialization.patternBinding = nullptr;
  expression.initialization.patternBindingIndex = 0;
}

void SolutionApplicationTarget::maybeApplyPropertyWrapper() {
  assert(kind == Kind::expression);
  assert(expression.contextualPurpose == CTP_Initialization);

  VarDecl *singleVar;
  if (auto *pattern = expression.pattern) {
    singleVar = pattern->getSingleVar();
  } else {
    singleVar = expression.propertyWrapper.wrappedVar;
  }

  if (!singleVar)
    return;

  auto wrapperAttrs = singleVar->getAttachedPropertyWrappers();
  if (wrapperAttrs.empty())
    return;

  // If the outermost property wrapper is directly initialized, form the
  // call.
  auto &ctx = singleVar->getASTContext();
  auto outermostWrapperAttr = wrapperAttrs.front();
  Expr *backingInitializer;
  if (Expr *initializer = expression.expression) {
    if (!isa<PropertyWrapperValuePlaceholderExpr>(initializer)) {
      expression.propertyWrapper.hasInitialWrappedValue = true;
    }
    // Form init(wrappedValue:) call(s).
    Expr *wrappedInitializer = buildPropertyWrapperInitCall(
        singleVar, Type(), initializer, PropertyWrapperInitKind::WrappedValue,
        [&](ApplyExpr *innermostInit) {
          expression.propertyWrapper.innermostWrappedValueInit = innermostInit;
        });
    if (!wrappedInitializer)
      return;

    backingInitializer = wrappedInitializer;
  } else {
    Type outermostWrapperType =
        singleVar->getAttachedPropertyWrapperType(0);
    if (!outermostWrapperType)
      return;

    bool isImplicit = false;

    // Retrieve the outermost wrapper argument. If there isn't one, we're
    // performing default initialization.
    auto outermostArgs = outermostWrapperAttr->getArgs();
    if (!outermostArgs) {
      SourceLoc fakeParenLoc = outermostWrapperAttr->getRange().End;
      outermostArgs = ArgumentList::createImplicit(ctx, fakeParenLoc, {},
                                                   fakeParenLoc);
      isImplicit = true;
    }

    SourceLoc typeLoc;
    if (auto *repr = outermostWrapperAttr->getTypeRepr()) {
      typeLoc = repr->getLoc();
    }
    auto typeExpr =
        TypeExpr::createImplicitHack(typeLoc, outermostWrapperType, ctx);
    backingInitializer = CallExpr::create(ctx, typeExpr, outermostArgs,
                                          /*implicit*/ isImplicit);
  }
  wrapperAttrs[0]->setSemanticInit(backingInitializer);

  // Note that we have applied to property wrapper, so we can adjust
  // the initializer type later.
  expression.propertyWrapper.wrappedVar = singleVar;
  expression.expression = backingInitializer;
  expression.convertType = {outermostWrapperAttr->getTypeRepr(),
                            outermostWrapperAttr->getType()};
}

SolutionApplicationTarget SolutionApplicationTarget::forInitialization(
    Expr *initializer, DeclContext *dc, Type patternType, Pattern *pattern,
    bool bindPatternVarsOneWay) {
  // Determine the contextual type for the initialization.
  TypeLoc contextualType;
  if (!(isa<OptionalSomePattern>(pattern) && !pattern->isImplicit()) &&
      patternType && !patternType->is<UnresolvedType>()) {
    contextualType = TypeLoc::withoutLoc(patternType);

    // Only provide a TypeLoc if it makes sense to allow diagnostics.
    if (auto *typedPattern = dyn_cast<TypedPattern>(pattern)) {
      const Pattern *inner = typedPattern->getSemanticsProvidingPattern();
      if (isa<NamedPattern>(inner) || isa<AnyPattern>(inner)) {
        contextualType = TypeLoc(typedPattern->getTypeRepr());
        if (typedPattern->hasType())
          contextualType.setType(typedPattern->getType());
        else
          contextualType.setType(patternType);
      }
    }
  }

  SolutionApplicationTarget target(
      initializer, dc, CTP_Initialization, contextualType,
      /*convertTypeLocator*/ nullptr, /*isDiscarded=*/false);
  target.expression.pattern = pattern;
  target.expression.bindPatternVarsOneWay = bindPatternVarsOneWay;
  target.maybeApplyPropertyWrapper();
  return target;
}

SolutionApplicationTarget SolutionApplicationTarget::forInitialization(
    Expr *initializer, DeclContext *dc, Type patternType,
    PatternBindingDecl *patternBinding, unsigned patternBindingIndex,
    bool bindPatternVarsOneWay) {
    auto result = forInitialization(
        initializer, dc, patternType,
        patternBinding->getPattern(patternBindingIndex), bindPatternVarsOneWay);
    result.expression.initialization.patternBinding = patternBinding;
    result.expression.initialization.patternBindingIndex = patternBindingIndex;
    return result;
}

SolutionApplicationTarget
SolutionApplicationTarget::forForEachStmt(ForEachStmt *stmt, DeclContext *dc,
                                          bool bindPatternVarsOneWay) {
  SolutionApplicationTarget target(
      stmt, dc, bindPatternVarsOneWay || bool(stmt->getWhere()));
  return target;
}

SolutionApplicationTarget
SolutionApplicationTarget::forPropertyWrapperInitializer(
    VarDecl *wrappedVar, DeclContext *dc, Expr *initializer) {
  SolutionApplicationTarget target(
      initializer, dc, CTP_Initialization, wrappedVar->getType(),
      /*isDiscarded=*/false);
  target.expression.propertyWrapper.wrappedVar = wrappedVar;
  if (auto *patternBinding = wrappedVar->getParentPatternBinding()) {
    auto index = patternBinding->getPatternEntryIndexForVarDecl(wrappedVar);
    target.expression.initialization.patternBinding = patternBinding;
    target.expression.initialization.patternBindingIndex = index;
    target.expression.pattern = patternBinding->getPattern(index);
  }
  target.maybeApplyPropertyWrapper();
  return target;
}

ContextualPattern
SolutionApplicationTarget::getContextualPattern() const {
  if (kind == Kind::uninitializedVar) {
    assert(patternBinding);
    return ContextualPattern::forPatternBindingDecl(patternBinding,
                                                    uninitializedVar.index);
  }

  if (isForEachStmt()) {
    return ContextualPattern::forRawPattern(forEachStmt.pattern,
                                            forEachStmt.dc);
  }

  assert(kind == Kind::expression);
  assert(expression.contextualPurpose == CTP_Initialization);
  if (expression.contextualPurpose == CTP_Initialization &&
      expression.initialization.patternBinding) {
    return ContextualPattern::forPatternBindingDecl(
        expression.initialization.patternBinding,
        expression.initialization.patternBindingIndex);
  }

  return ContextualPattern::forRawPattern(expression.pattern, expression.dc);
}

bool SolutionApplicationTarget::infersOpaqueReturnType() const {
  assert(kind == Kind::expression);
  switch (expression.contextualPurpose) {
  case CTP_Initialization:
  case CTP_ReturnStmt:
  case CTP_ReturnSingleExpr:
    if (Type convertType = expression.convertType.getType())
      return convertType->hasOpaqueArchetype();
    return false;
  default:
    return false;
  }
}

bool SolutionApplicationTarget::contextualTypeIsOnlyAHint() const {
  assert(kind == Kind::expression);
  switch (expression.contextualPurpose) {
  case CTP_Initialization:
    return !infersOpaqueReturnType() && !isOptionalSomePatternInit();
  case CTP_ForEachStmt:
  case CTP_ForEachSequence:
    return true;
  case CTP_Unused:
  case CTP_ReturnStmt:
  case CTP_ReturnSingleExpr:
  case CTP_YieldByValue:
  case CTP_YieldByReference:
  case CTP_CaseStmt:
  case CTP_ThrowStmt:
  case CTP_ForgetStmt:
  case CTP_EnumCaseRawValue:
  case CTP_DefaultParameter:
  case CTP_AutoclosureDefaultParameter:
  case CTP_CalleeResult:
  case CTP_CallArgument:
  case CTP_ClosureResult:
  case CTP_ArrayElement:
  case CTP_DictionaryKey:
  case CTP_DictionaryValue:
  case CTP_CoerceOperand:
  case CTP_AssignSource:
  case CTP_SubscriptAssignSource:
  case CTP_Condition:
  case CTP_WrappedProperty:
  case CTP_ComposedPropertyWrapper:
  case CTP_CannotFail:
  case CTP_ExprPattern:
  case CTP_SingleValueStmtBranch:
    return false;
  }
  llvm_unreachable("invalid contextual type");
}

SolutionApplicationTarget SolutionApplicationTarget::walk(ASTWalker &walker) {
  switch (kind) {
  case Kind::expression: {
    SolutionApplicationTarget result = *this;
    result.setExpr(getAsExpr()->walk(walker));
    return result;
  }

  case Kind::closure:
    return *this;

  case Kind::function:
    return SolutionApplicationTarget(
        *getAsFunction(),
        cast_or_null<BraceStmt>(getFunctionBody()->walk(walker)));

  case Kind::stmtCondition:
    for (auto &condElement : stmtCondition.stmtCondition) {
      condElement = *condElement.walk(walker);
    }
    return *this;

  case Kind::caseLabelItem:
    if (auto newPattern =
            caseLabelItem.caseLabelItem->getPattern()->walk(walker)) {
      caseLabelItem.caseLabelItem->setPattern(
          newPattern, caseLabelItem.caseLabelItem->isPatternResolved());
    }
    if (auto guardExpr = caseLabelItem.caseLabelItem->getGuardExpr()) {
      if (auto newGuardExpr = guardExpr->walk(walker))
        caseLabelItem.caseLabelItem->setGuardExpr(newGuardExpr);
    }

    return *this;

  case Kind::patternBinding:
    return *this;

  case Kind::uninitializedVar:
    return *this;

  case Kind::forEachStmt:
    return *this;
  }

  llvm_unreachable("invalid target kind");
}
