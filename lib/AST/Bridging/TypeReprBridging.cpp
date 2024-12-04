//===--- Bridging/TypeReprBridging.cpp ------------------------------------===//
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

BridgedUnqualifiedIdentTypeRepr BridgedUnqualifiedIdentTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLoc, BridgedIdentifier id) {
  return UnqualifiedIdentTypeRepr::create(cContext.unbridged(),
                                          DeclNameLoc(cLoc.unbridged()),
                                          DeclNameRef(id.unbridged()));
}

BridgedUnqualifiedIdentTypeRepr BridgedUnqualifiedIdentTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedIdentifier name,
    BridgedSourceLoc cNameLoc, BridgedArrayRef genericArgs,
    BridgedSourceLoc cLAngleLoc, BridgedSourceLoc cRAngleLoc) {
  ASTContext &context = cContext.unbridged();
  auto Loc = DeclNameLoc(cNameLoc.unbridged());
  auto Name = DeclNameRef(name.unbridged());
  SourceLoc lAngleLoc = cLAngleLoc.unbridged();
  SourceLoc rAngleLoc = cRAngleLoc.unbridged();
  return UnqualifiedIdentTypeRepr::create(context, Loc, Name,
                                          genericArgs.unbridged<TypeRepr *>(),
                                          SourceRange{lAngleLoc, rAngleLoc});
}

BridgedOptionalTypeRepr
BridgedOptionalTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr base,
                                     BridgedSourceLoc cQuestionLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      OptionalTypeRepr(base.unbridged(), cQuestionLoc.unbridged());
}

BridgedImplicitlyUnwrappedOptionalTypeRepr
BridgedImplicitlyUnwrappedOptionalTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedSourceLoc cExclamationLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) ImplicitlyUnwrappedOptionalTypeRepr(
      base.unbridged(), cExclamationLoc.unbridged());
}

BridgedArrayTypeRepr BridgedArrayTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedSourceLoc cLSquareLoc, BridgedSourceLoc cRSquareLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc lSquareLoc = cLSquareLoc.unbridged();
  SourceLoc rSquareLoc = cRSquareLoc.unbridged();
  return new (context)
      ArrayTypeRepr(base.unbridged(), SourceRange{lSquareLoc, rSquareLoc});
}

BridgedDictionaryTypeRepr BridgedDictionaryTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLSquareLoc,
    BridgedTypeRepr keyType, BridgedSourceLoc cColonloc,
    BridgedTypeRepr valueType, BridgedSourceLoc cRSquareLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc lSquareLoc = cLSquareLoc.unbridged();
  SourceLoc colonLoc = cColonloc.unbridged();
  SourceLoc rSquareLoc = cRSquareLoc.unbridged();
  return new (context)
      DictionaryTypeRepr(keyType.unbridged(), valueType.unbridged(), colonLoc,
                         SourceRange{lSquareLoc, rSquareLoc});
}

BridgedInverseTypeRepr
BridgedInverseTypeRepr_createParsed(BridgedASTContext cContext,
                                    BridgedSourceLoc cTildeLoc,
                                    BridgedTypeRepr cConstraint) {

  return new (cContext.unbridged())
      InverseTypeRepr(cTildeLoc.unbridged(), cConstraint.unbridged());
}

BridgedMetatypeTypeRepr
BridgedMetatypeTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr baseType,
                                     BridgedSourceLoc cTypeLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc tyLoc = cTypeLoc.unbridged();
  return new (context) MetatypeTypeRepr(baseType.unbridged(), tyLoc);
}

BridgedProtocolTypeRepr
BridgedProtocolTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr baseType,
                                     BridgedSourceLoc cProtoLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc protoLoc = cProtoLoc.unbridged();
  return new (context) ProtocolTypeRepr(baseType.unbridged(), protoLoc);
}

BridgedPackElementTypeRepr
BridgedPackElementTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedTypeRepr base,
                                        BridgedSourceLoc cEachLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      PackElementTypeRepr(cEachLoc.unbridged(), base.unbridged());
}

BridgedPackExpansionTypeRepr
BridgedPackExpansionTypeRepr_createParsed(BridgedASTContext cContext,
                                          BridgedTypeRepr base,
                                          BridgedSourceLoc cRepeatLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      PackExpansionTypeRepr(cRepeatLoc.unbridged(), base.unbridged());
}

BridgedAttributedTypeRepr
BridgedAttributedTypeRepr_createParsed(BridgedASTContext cContext,
                                       BridgedTypeRepr base,
                                       BridgedTypeAttributes cAttributes) {
  TypeAttributes *typeAttributes = cAttributes.unbridged();
  assert(!typeAttributes->attrs.empty());

  auto attributedType = AttributedTypeRepr::create(
      cContext.unbridged(), typeAttributes->attrs, base.unbridged());
  delete typeAttributes;
  return attributedType;
}

BridgedSpecifierTypeRepr BridgedSpecifierTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedAttributedTypeSpecifier specifier, BridgedSourceLoc cSpecifierLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc loc = cSpecifierLoc.unbridged();
  TypeRepr *baseType = base.unbridged();
  switch (specifier) {
  case BridgedAttributedTypeSpecifierInOut: {
    return new (context)
        OwnershipTypeRepr(baseType, ParamSpecifier::InOut, loc);
  }
  case BridgedAttributedTypeSpecifierBorrowing: {
    return new (context)
        OwnershipTypeRepr(baseType, ParamSpecifier::Borrowing, loc);
  }
  case BridgedAttributedTypeSpecifierConsuming: {
    return new (context)
        OwnershipTypeRepr(baseType, ParamSpecifier::Consuming, loc);
  }
  case BridgedAttributedTypeSpecifierLegacyShared: {
    return new (context)
        OwnershipTypeRepr(baseType, ParamSpecifier::LegacyShared, loc);
  }
  case BridgedAttributedTypeSpecifierLegacyOwned: {
    return new (context)
        OwnershipTypeRepr(baseType, ParamSpecifier::LegacyOwned, loc);
  }
  case BridgedAttributedTypeSpecifierSending: {
    return new (context) SendingTypeRepr(baseType, loc);
  }
  case BridgedAttributedTypeSpecifierConst: {
    return new (context) CompileTimeConstTypeRepr(baseType, loc);
  }
  case BridgedAttributedTypeSpecifierIsolated: {
    return new (context) IsolatedTypeRepr(baseType, loc);
  }
  }
}

BridgedVarargTypeRepr
BridgedVarargTypeRepr_createParsed(BridgedASTContext cContext,
                                   BridgedTypeRepr base,
                                   BridgedSourceLoc cEllipsisLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc ellipsisLoc = cEllipsisLoc.unbridged();
  TypeRepr *baseType = base.unbridged();
  return new (context) VarargTypeRepr(baseType, ellipsisLoc);
}

BridgedTupleTypeRepr BridgedTupleTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedArrayRef elements,
    BridgedSourceLoc cLParenLoc, BridgedSourceLoc cRParenLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc lParen = cLParenLoc.unbridged();
  SourceLoc rParen = cRParenLoc.unbridged();

  SmallVector<TupleTypeReprElement, 8> tupleElements;
  for (auto element : elements.unbridged<BridgedTupleTypeElement>()) {
    TupleTypeReprElement elementRepr;
    elementRepr.Name = element.Name.unbridged();
    elementRepr.NameLoc = element.NameLoc.unbridged();
    elementRepr.SecondName = element.SecondName.unbridged();
    elementRepr.SecondNameLoc = element.SecondNameLoc.unbridged();
    elementRepr.UnderscoreLoc = element.UnderscoreLoc.unbridged();
    elementRepr.ColonLoc = element.ColonLoc.unbridged();
    elementRepr.Type = element.Type.unbridged();
    elementRepr.TrailingCommaLoc = element.TrailingCommaLoc.unbridged();
    tupleElements.emplace_back(elementRepr);
  }

  return TupleTypeRepr::create(context, tupleElements,
                               SourceRange{lParen, rParen});
}

BridgedDeclRefTypeRepr BridgedDeclRefTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr cBase, BridgedIdentifier cName,
    BridgedSourceLoc cLoc, BridgedArrayRef cGenericArguments,
    BridgedSourceRange cAngleRange) {
  ASTContext &context = cContext.unbridged();
  auto genericArguments = cGenericArguments.unbridged<TypeRepr *>();
  auto angleRange = cAngleRange.unbridged();

  assert(angleRange.isValid() || genericArguments.empty());

  return DeclRefTypeRepr::create(
      context, cBase.unbridged(), DeclNameLoc(cLoc.unbridged()),
      DeclNameRef(cName.unbridged()), genericArguments, angleRange);
}

BridgedCompositionTypeRepr
BridgedCompositionTypeRepr_createEmpty(BridgedASTContext cContext,
                                       BridgedSourceLoc cAnyLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc anyLoc = cAnyLoc.unbridged();
  return CompositionTypeRepr::createEmptyComposition(context, anyLoc);
}

BridgedCompositionTypeRepr
BridgedCompositionTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedArrayRef cTypes,
                                        BridgedSourceLoc cFirstAmpLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc firstAmpLoc = cFirstAmpLoc.unbridged();
  auto types = cTypes.unbridged<TypeRepr *>();
  return CompositionTypeRepr::create(
      context, types, types.front()->getStartLoc(),
      SourceRange{firstAmpLoc, types.back()->getEndLoc()});
}

BridgedFunctionTypeRepr BridgedFunctionTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr argsTy,
    BridgedSourceLoc cAsyncLoc, BridgedSourceLoc cThrowsLoc,
    BridgedNullableTypeRepr thrownType, BridgedSourceLoc cArrowLoc,
    BridgedTypeRepr resultType) {
  ASTContext &context = cContext.unbridged();
  return new (context) FunctionTypeRepr(
      nullptr, cast<TupleTypeRepr>(argsTy.unbridged()), cAsyncLoc.unbridged(),
      cThrowsLoc.unbridged(), thrownType.unbridged(), cArrowLoc.unbridged(),
      resultType.unbridged());
}

BridgedNamedOpaqueReturnTypeRepr
BridgedNamedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                              BridgedTypeRepr baseTy) {
  ASTContext &context = cContext.unbridged();
  return new (context) NamedOpaqueReturnTypeRepr(baseTy.unbridged(), nullptr);
}

BridgedOpaqueReturnTypeRepr
BridgedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                         BridgedSourceLoc cOpaqueLoc,
                                         BridgedTypeRepr baseTy) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      OpaqueReturnTypeRepr(cOpaqueLoc.unbridged(), baseTy.unbridged());
}

BridgedExistentialTypeRepr
BridgedExistentialTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cAnyLoc,
                                        BridgedTypeRepr baseTy) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      ExistentialTypeRepr(cAnyLoc.unbridged(), baseTy.unbridged());
}

BridgedIntegerTypeRepr
BridgedIntegerTypeRepr_createParsed(BridgedASTContext cContext,
                                    BridgedStringRef cString,
                                    BridgedSourceLoc cLoc,
                                    BridgedSourceLoc cMinusLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) IntegerTypeRepr(cString.unbridged(), cLoc.unbridged(),
                                       cMinusLoc.unbridged());
}
