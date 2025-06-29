//===--- Bridging/TypeReprBridging.cpp ------------------------------------===//
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
#include "swift/AST/Attr.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: TypeReprs
//===----------------------------------------------------------------------===//

// Define `.asTypeRepr` on each BridgedXXXTypeRepr type.
#define TYPEREPR(Id, Parent)                                                   \
  BridgedTypeRepr Bridged##Id##TypeRepr_asTypeRepr(                            \
      Bridged##Id##TypeRepr typeRepr) {                                        \
    return static_cast<TypeRepr *>(typeRepr.unbridged());                      \
  }
#define ABSTRACT_TYPEREPR(Id, Parent) TYPEREPR(Id, Parent)
#include "swift/AST/TypeReprNodes.def"

BridgedUnqualifiedIdentTypeRepr
BridgedUnqualifiedIdentTypeRepr_createParsed(BridgedASTContext cContext,
                                             SourceLoc loc, Identifier id) {
  return UnqualifiedIdentTypeRepr::create(cContext.unbridged(),
                                          DeclNameLoc(loc), DeclNameRef(id));
}

BridgedUnqualifiedIdentTypeRepr BridgedUnqualifiedIdentTypeRepr_createParsed(
    BridgedASTContext cContext, Identifier name, SourceLoc nameLoc,
    BridgedArrayRef genericArgs, SourceLoc lAngleLoc, SourceLoc rAngleLoc) {
  ASTContext &context = cContext.unbridged();
  auto Loc = DeclNameLoc(nameLoc);
  auto Name = DeclNameRef(name);
  return UnqualifiedIdentTypeRepr::create(context, Loc, Name,
                                          genericArgs.unbridged<TypeRepr *>(),
                                          SourceRange{lAngleLoc, rAngleLoc});
}

BridgedOptionalTypeRepr BridgedOptionalTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base, SourceLoc questionLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) OptionalTypeRepr(base.unbridged(), questionLoc);
}

BridgedImplicitlyUnwrappedOptionalTypeRepr
BridgedImplicitlyUnwrappedOptionalTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    SourceLoc exclamationLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      ImplicitlyUnwrappedOptionalTypeRepr(base.unbridged(), exclamationLoc);
}

BridgedArrayTypeRepr
BridgedArrayTypeRepr_createParsed(BridgedASTContext cContext,
                                  BridgedTypeRepr base, SourceLoc lSquareLoc,
                                  SourceLoc rSquareLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      ArrayTypeRepr(base.unbridged(), SourceRange{lSquareLoc, rSquareLoc});
}

BridgedDictionaryTypeRepr BridgedDictionaryTypeRepr_createParsed(
    BridgedASTContext cContext, SourceLoc lSquareLoc, BridgedTypeRepr keyType,
    SourceLoc colonLoc, BridgedTypeRepr valueType, SourceLoc rSquareLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      DictionaryTypeRepr(keyType.unbridged(), valueType.unbridged(), colonLoc,
                         SourceRange{lSquareLoc, rSquareLoc});
}

BridgedErrorTypeRepr BridgedErrorTypeRepr_create(BridgedASTContext cContext,
                                                 BridgedSourceRange cRange) {
  return ErrorTypeRepr::create(cContext.unbridged(), cRange.unbridged());
}

BridgedInlineArrayTypeRepr BridgedInlineArrayTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr cCountType,
    BridgedTypeRepr cElementType, BridgedSourceRange cBracketsRange) {
  return InlineArrayTypeRepr::create(
      cContext.unbridged(), cCountType.unbridged(), cElementType.unbridged(),
      cBracketsRange.unbridged());
}

BridgedInverseTypeRepr
BridgedInverseTypeRepr_createParsed(BridgedASTContext cContext,
                                    SourceLoc tildeLoc,
                                    BridgedTypeRepr cConstraint) {

  return new (cContext.unbridged())
      InverseTypeRepr(tildeLoc, cConstraint.unbridged());
}

BridgedIsolatedTypeRepr BridgedIsolatedTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base, SourceLoc specifierLoc) {
  return new (cContext.unbridged())
      IsolatedTypeRepr(base.unbridged(), specifierLoc);
}

BridgedLifetimeDependentTypeRepr
BridgedLifetimeDependentTypeRepr_createParsed(BridgedASTContext cContext,
                                              BridgedTypeRepr base,
                                              BridgedLifetimeEntry cEntry) {
  return new (cContext.unbridged())
      LifetimeDependentTypeRepr(base.unbridged(), cEntry.unbridged());
}

BridgedMetatypeTypeRepr BridgedMetatypeTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr baseType, SourceLoc typeLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc tyLoc = typeLoc;
  return new (context) MetatypeTypeRepr(baseType.unbridged(), tyLoc);
}

BridgedOwnershipTypeRepr BridgedOwnershipTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedParamSpecifier cSpecifier, SourceLoc specifierLoc) {
  return new (cContext.unbridged())
      OwnershipTypeRepr(base.unbridged(), unbridge(cSpecifier), specifierLoc);
}

BridgedPlaceholderTypeRepr
BridgedPlaceholderTypeRepr_createParsed(BridgedASTContext cContext,
                                        SourceLoc loc) {
  return new (cContext.unbridged()) PlaceholderTypeRepr(loc);
}

BridgedProtocolTypeRepr BridgedProtocolTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr baseType, SourceLoc protoLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) ProtocolTypeRepr(baseType.unbridged(), protoLoc);
}

BridgedPackElementTypeRepr BridgedPackElementTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base, SourceLoc eachLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) PackElementTypeRepr(eachLoc, base.unbridged());
}

BridgedPackExpansionTypeRepr BridgedPackExpansionTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base, SourceLoc repeatLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) PackExpansionTypeRepr(repeatLoc, base.unbridged());
}

BridgedAttributedTypeRepr
BridgedAttributedTypeRepr_createParsed(BridgedASTContext cContext,
                                       BridgedTypeRepr base,
                                       BridgedArrayRef cAttributes) {
  SmallVector<TypeOrCustomAttr, 2> attrs;
  for (auto elem : cAttributes.unbridged<BridgedTypeOrCustomAttr>()) {
    switch (elem.getKind()) {
    case BridgedTypeOrCustomAttr::TypeAttr:
      attrs.push_back(elem.castToTypeAttr().unbridged());
      break;
    case BridgedTypeOrCustomAttr::CustomAttr:
      attrs.push_back(elem.castToCustomAttr().unbridged());
      break;
    }
  }
  assert(!attrs.empty());

  return AttributedTypeRepr::create(cContext.unbridged(), attrs,
                                    base.unbridged());
}

BridgedSendingTypeRepr BridgedSendingTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base, SourceLoc specifierLoc) {
  return new (cContext.unbridged())
      SendingTypeRepr(base.unbridged(), specifierLoc);
}

BridgedCallerIsolatedTypeRepr BridgedCallerIsolatedTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base, SourceLoc specifierLoc) {
  return new (cContext.unbridged())
      CallerIsolatedTypeRepr(base.unbridged(), specifierLoc);
}

BridgedVarargTypeRepr BridgedVarargTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base, SourceLoc ellipsisLoc) {
  ASTContext &context = cContext.unbridged();
  TypeRepr *baseType = base.unbridged();
  return new (context) VarargTypeRepr(baseType, ellipsisLoc);
}

BridgedTupleTypeRepr
BridgedTupleTypeRepr_createParsed(BridgedASTContext cContext,
                                  BridgedArrayRef elements, SourceLoc lParenLoc,
                                  SourceLoc rParenLoc) {
  ASTContext &context = cContext.unbridged();

  SmallVector<TupleTypeReprElement, 8> tupleElements;
  for (auto element : elements.unbridged<BridgedTupleTypeElement>()) {
    TupleTypeReprElement elementRepr;
    elementRepr.Name = element.Name;
    elementRepr.NameLoc = element.NameLoc;
    elementRepr.SecondName = element.SecondName;
    elementRepr.SecondNameLoc = element.SecondNameLoc;
    elementRepr.UnderscoreLoc = element.UnderscoreLoc;
    elementRepr.ColonLoc = element.ColonLoc;
    elementRepr.Type = element.Type.unbridged();
    elementRepr.TrailingCommaLoc = element.TrailingCommaLoc;
    tupleElements.emplace_back(elementRepr);
  }

  return TupleTypeRepr::create(context, tupleElements,
                               SourceRange{lParenLoc, rParenLoc});
}

BridgedDeclRefTypeRepr BridgedDeclRefTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr cBase, Identifier name,
    SourceLoc loc, BridgedArrayRef cGenericArguments,
    BridgedSourceRange cAngleRange) {
  ASTContext &context = cContext.unbridged();
  auto genericArguments = cGenericArguments.unbridged<TypeRepr *>();
  auto angleRange = cAngleRange.unbridged();

  assert(angleRange.isValid() || genericArguments.empty());

  return DeclRefTypeRepr::create(context, cBase.unbridged(), DeclNameLoc(loc),
                                 DeclNameRef(name), genericArguments,
                                 angleRange);
}

BridgedCompositionTypeRepr
BridgedCompositionTypeRepr_createEmpty(BridgedASTContext cContext,
                                       SourceLoc anyLoc) {
  ASTContext &context = cContext.unbridged();
  return CompositionTypeRepr::createEmptyComposition(context, anyLoc);
}

BridgedCompositionTypeRepr BridgedCompositionTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedArrayRef cTypes, SourceLoc firstAmpLoc) {
  ASTContext &context = cContext.unbridged();
  auto types = cTypes.unbridged<TypeRepr *>();
  return CompositionTypeRepr::create(
      context, types, types.front()->getStartLoc(),
      SourceRange{firstAmpLoc, types.back()->getEndLoc()});
}

BridgedCompileTimeLiteralTypeRepr
BridgedCompileTimeLiteralTypeRepr_createParsed(BridgedASTContext cContext,
                                               BridgedTypeRepr base,
                                               SourceLoc specifierLoc) {
  return new (cContext.unbridged())
      CompileTimeLiteralTypeRepr(base.unbridged(), specifierLoc);
}

BridgedFunctionTypeRepr BridgedFunctionTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr argsTy, SourceLoc asyncLoc,
    SourceLoc throwsLoc, BridgedNullableTypeRepr thrownType, SourceLoc arrowLoc,
    BridgedTypeRepr resultType) {
  ASTContext &context = cContext.unbridged();
  return new (context) FunctionTypeRepr(
      nullptr, cast<TupleTypeRepr>(argsTy.unbridged()), asyncLoc, throwsLoc,
      thrownType.unbridged(), arrowLoc, resultType.unbridged());
}

BridgedNamedOpaqueReturnTypeRepr BridgedNamedOpaqueReturnTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr baseTy,
    BridgedGenericParamList genericParams) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      NamedOpaqueReturnTypeRepr(baseTy.unbridged(), genericParams.unbridged());
}

BridgedOpaqueReturnTypeRepr BridgedOpaqueReturnTypeRepr_createParsed(
    BridgedASTContext cContext, SourceLoc opaqueLoc, BridgedTypeRepr baseTy) {
  ASTContext &context = cContext.unbridged();
  return new (context) OpaqueReturnTypeRepr(opaqueLoc, baseTy.unbridged());
}

BridgedExistentialTypeRepr BridgedExistentialTypeRepr_createParsed(
    BridgedASTContext cContext, SourceLoc anyLoc, BridgedTypeRepr baseTy) {
  ASTContext &context = cContext.unbridged();
  return new (context) ExistentialTypeRepr(anyLoc, baseTy.unbridged());
}

BridgedIntegerTypeRepr
BridgedIntegerTypeRepr_createParsed(BridgedASTContext cContext,
                                    BridgedStringRef cString, SourceLoc loc,
                                    SourceLoc minusLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) IntegerTypeRepr(cString.unbridged(), loc, minusLoc);
}
