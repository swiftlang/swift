//===--- Bridging/TypeAttributeBridging.cpp -------------------------------===//
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

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: TypeAttributes
//===----------------------------------------------------------------------===//

// Define `.asTypeAttr` on each BridgedXXXTypeAttr type.
#define SIMPLE_TYPE_ATTR(...)
#define TYPE_ATTR(SPELLING, CLASS)                                             \
  SWIFT_NAME("getter:Bridged" #CLASS "TypeAttr.asTypeAttribute(self:)")        \
  BridgedTypeAttribute Bridged##CLASS##TypeAttr_asTypeAttribute(               \
      Bridged##CLASS##TypeAttr attr) {                                         \
    return attr.unbridged();                                                   \
  }
#include "swift/AST/TypeAttr.def"

BridgedOptionalTypeAttrKind
BridgedOptionalTypeAttrKind_fromString(BridgedStringRef cStr) {
  auto optKind = TypeAttribute::getAttrKindFromString(cStr.unbridged());
  if (!optKind) {
    return BridgedOptionalTypeAttrKind();
  }
  return *optKind;
}

BridgedTypeAttribute
BridgedTypeAttribute_createSimple(BridgedASTContext cContext,
                                  swift::TypeAttrKind kind, SourceLoc atLoc,
                                  SourceLoc nameLoc) {
  return TypeAttribute::createSimple(cContext.unbridged(), kind, atLoc,
                                     nameLoc);
}

BridgedConventionTypeAttr BridgedConventionTypeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc kwLoc,
    SourceRange parens, BridgedStringRef cName, SourceLoc nameLoc,
    BridgedDeclNameRef cWitnessMethodProtocol, BridgedStringRef cClangType,
    SourceLoc clangTypeLoc) {
  return new (cContext.unbridged())
      ConventionTypeAttr(atLoc, kwLoc, parens, {cName.unbridged(), nameLoc},
                         cWitnessMethodProtocol.unbridged(),
                         {cClangType.unbridged(), clangTypeLoc});
}

BridgedDifferentiableTypeAttr BridgedDifferentiableTypeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc nameLoc,
    SourceRange parensRange, BridgedDifferentiabilityKind cKind,
    SourceLoc kindLoc) {
  return new (cContext.unbridged()) DifferentiableTypeAttr(
      atLoc, nameLoc, parensRange, {unbridged(cKind), kindLoc});
}

BridgedLifetimeTypeAttr BridgedLifetimeTypeAttr_createParsed(
    BridgedASTContext cContext, swift::SourceLoc atLoc,
    swift::SourceLoc nameLoc, swift::SourceRange parensRange,
    BridgedLifetimeEntry entry) {
  return new (cContext.unbridged())
      LifetimeTypeAttr(atLoc, nameLoc, parensRange, entry.unbridged());
}

BridgedIsolatedTypeAttr BridgedIsolatedTypeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc nameLoc,
    SourceRange parensRange, BridgedIsolatedTypeAttrIsolationKind cIsolation,
    SourceLoc isolationLoc) {
  auto isolationKind = [=] {
    switch (cIsolation) {
    case BridgedIsolatedTypeAttrIsolationKind_DynamicIsolation:
      return IsolatedTypeAttr::IsolationKind::Dynamic;
    }
    llvm_unreachable("bad kind");
  }();
  return new (cContext.unbridged()) IsolatedTypeAttr(
      atLoc, nameLoc, parensRange, {isolationKind, isolationLoc});
}

BridgedOpaqueReturnTypeOfTypeAttr
BridgedOpaqueReturnTypeOfTypeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc kwLoc,
    SourceRange parens, BridgedStringRef cMangled, SourceLoc mangledLoc,
    size_t index, SourceLoc indexLoc) {
  return new (cContext.unbridged()) OpaqueReturnTypeOfTypeAttr(
      atLoc, kwLoc, parens, {cMangled.unbridged(), mangledLoc},
      {static_cast<unsigned int>(index), indexLoc});
}

BridgedCallableTypeAttr
BridgedCallableTypeAttr_createParsed(BridgedASTContext cContext,
                                     SourceLoc atLoc, SourceLoc nameLoc,
                                     SourceRange parensRange) {
  return new (cContext.unbridged())
      CallableTypeAttr(atLoc, nameLoc, parensRange);
}

ScopeRepr BridgedScopeRepr::unbridged() const {
  switch (kind) {
  case Kind::ScopeName:
    return ScopeRepr::forScopeName({ScopeName(name), loc});
  case Kind::AccessedValue:
    return ScopeRepr::forAccessedValue({name, loc});
  case Kind::Self:
    return ScopeRepr::forSelf(loc, /*isAccess=*/false);
  case Kind::AccessedSelf:
    return ScopeRepr::forSelf(loc, /*isAccess=*/true);
  case Kind::Immortal:
    return ScopeRepr::forImmortal(loc);
  }
  llvm_unreachable("bad kind");
}

ScopeRestrictionRepr BridgedScopeRestrictionRepr::unbridged() const {
  std::optional<Located<ScopeName>> unbridgedLabel;
  if (!label.empty())
    unbridgedLabel = {ScopeName(label), labelLoc};
  return ScopeRestrictionRepr(unbridgedLabel, scope.unbridged());
}

BridgedScopedTypeAttr
BridgedScopedTypeAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                   SourceLoc nameLoc, SourceRange parensRange,
                                   BridgedArrayRef cRestrictions) {
  SmallVector<ScopeRestrictionRepr, 2> restrictions;
  for (auto cRestriction :
       cRestrictions.unbridged<BridgedScopeRestrictionRepr>())
    restrictions.push_back(cRestriction.unbridged());
  return ScopedTypeAttr::create(cContext.unbridged(), atLoc, nameLoc,
                                parensRange, restrictions);
}

BridgedAccessScopeTypeAttr BridgedAccessScopeTypeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc nameLoc,
    SourceRange parensRange, BridgedScopeRepr scope) {
  return new (cContext.unbridged())
      AccessScopeTypeAttr(atLoc, nameLoc, parensRange, scope.unbridged());
}

BridgedScopeParamsTypeAttr BridgedScopeParamsTypeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc nameLoc,
    SourceRange parensRange, BridgedArrayRef cParams) {
  SmallVector<Located<ScopeName>, 2> params;
  for (auto cParam : cParams.unbridged<BridgedLocatedIdentifier>())
    params.push_back({ScopeName(cParam.Name), cParam.NameLoc});
  auto &ctx = cContext.unbridged();
  return new (ctx) ScopeParamsTypeAttr(atLoc, nameLoc, parensRange,
                                       ScopeParamListRepr::create(ctx, params));
}
