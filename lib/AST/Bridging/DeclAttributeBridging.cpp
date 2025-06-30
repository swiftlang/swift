//===--- Bridging/DeclAttributeBridging.cpp--------------------------------===//
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
#include "swift/AST/AutoDiff.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: AutoDiff
//===----------------------------------------------------------------------===//

DifferentiabilityKind unbridged(BridgedDifferentiabilityKind cKind) {
  switch (cKind) {
  case BridgedDifferentiabilityKindNonDifferentiable:
    return DifferentiabilityKind::NonDifferentiable;
  case BridgedDifferentiabilityKindForward:
    return DifferentiabilityKind::Forward;
  case BridgedDifferentiabilityKindReverse:
    return DifferentiabilityKind::Reverse;
  case BridgedDifferentiabilityKindNormal:
    return DifferentiabilityKind::Normal;
  case BridgedDifferentiabilityKindLinear:
    return DifferentiabilityKind::Linear;
  }
  llvm_unreachable("unhandled enum value");
}

ParsedAutoDiffParameter BridgedParsedAutoDiffParameter::unbridged() const {
  switch (kind) {
  case Kind::Named:
    return ParsedAutoDiffParameter::getNamedParameter(loc, value.name);
  case Kind::Ordered:
    return ParsedAutoDiffParameter::getOrderedParameter(loc, value.index);
  case Kind::Self:
    return ParsedAutoDiffParameter::getSelfParameter(loc);
  }
  llvm_unreachable("unhandled enum value");
}

//===----------------------------------------------------------------------===//
// MARK: DeclAttributes
//===----------------------------------------------------------------------===//

// Define `.asDeclAttribute` on each BridgedXXXAttr type.
#define SIMPLE_DECL_ATTR(...)
#define DECL_ATTR(_, CLASS, ...)                                               \
  BridgedDeclAttribute Bridged##CLASS##Attr_asDeclAttribute(                   \
      Bridged##CLASS##Attr attr) {                                             \
    return static_cast<DeclAttribute *>(attr.unbridged());                     \
  }
#include "swift/AST/DeclAttr.def"

BridgedOptionalDeclAttrKind
BridgedOptionalDeclAttrKind_fromString(BridgedStringRef cStr) {
  auto optKind = DeclAttribute::getAttrKindFromString(cStr.unbridged());
  if (!optKind) {
    return BridgedOptionalDeclAttrKind();
  }
  return *optKind;
}

BridgedDeclAttribute
BridgedDeclAttribute_createSimple(BridgedASTContext cContext,
                                  swift::DeclAttrKind kind, SourceLoc atLoc,
                                  SourceLoc attrLoc) {
  return DeclAttribute::createSimple(cContext.unbridged(), kind, atLoc,
                                     attrLoc);
}

bool BridgedDeclAttribute_shouldBeRejectedByParser(swift::DeclAttrKind kind) {
  return DeclAttribute::shouldBeRejectedByParser(kind);
}

bool BridgedDeclAttribute_isDeclModifier(swift::DeclAttrKind kind) {
  return DeclAttribute::isDeclModifier(kind);
}

void BridgedDeclAttributes_add(BridgedDeclAttributes *cAttrs,
                               BridgedDeclAttribute cAdd) {
  auto attrs = cAttrs->unbridged();
  attrs.add(cAdd.unbridged());
  *cAttrs = attrs;
}

static AvailableAttr::Kind unbridge(BridgedAvailableAttrKind value) {
  switch (value) {
  case BridgedAvailableAttrKindDefault:
    return AvailableAttr::Kind::Default;
  case BridgedAvailableAttrKindDeprecated:
    return AvailableAttr::Kind::Deprecated;
  case BridgedAvailableAttrKindUnavailable:
    return AvailableAttr::Kind::Unavailable;
  case BridgedAvailableAttrKindNoAsync:
    return AvailableAttr::Kind::NoAsync;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedAvailableAttr BridgedAvailableAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    Identifier domainIdentifier, SourceLoc domainLoc,
    BridgedAvailableAttrKind cKind, BridgedStringRef cMessage,
    BridgedStringRef cRenamed, BridgedVersionTuple cIntroduced,
    SourceRange introducedRange, BridgedVersionTuple cDeprecated,
    SourceRange deprecatedRange, BridgedVersionTuple cObsoleted,
    SourceRange obsoletedRange, bool isSPI) {
  return new (cContext.unbridged()) AvailableAttr(
      atLoc, range, domainIdentifier, domainLoc, unbridge(cKind),
      cMessage.unbridged(), cRenamed.unbridged(), cIntroduced.unbridged(),
      introducedRange, cDeprecated.unbridged(), deprecatedRange,
      cObsoleted.unbridged(), obsoletedRange,
      /*Implicit=*/false,
      /*IsSPI=*/isSPI);
}

BridgedAvailableAttr BridgedAvailableAttr_createUnavailableInEmbedded(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range) {
  return AvailableAttr::createUnavailableInEmbedded(cContext.unbridged(), atLoc,
                                                    range);
}

void BridgedAvailableAttr_setIsGroupMember(BridgedAvailableAttr cAttr) {
  cAttr.unbridged()->setIsGroupMember();
}
void BridgedAvailableAttr_setIsGroupedWithWildcard(BridgedAvailableAttr cAttr) {
  cAttr.unbridged()->setIsGroupedWithWildcard();
}
void BridgedAvailableAttr_setIsGroupTerminator(BridgedAvailableAttr cAttr) {
  cAttr.unbridged()->setIsGroupTerminator();
}

BridgedABIAttr BridgedABIAttr_createParsed(BridgedASTContext cContext,
                                           SourceLoc atLoc, SourceRange range,
                                           BridgedNullableDecl abiDecl) {
  return new (cContext.unbridged()) ABIAttr(abiDecl.unbridged(), atLoc, range,
                                            /*isImplicit=*/false);
}

BridgedAccessControlAttr
BridgedAccessControlAttr_createParsed(BridgedASTContext cContext,
                                      SourceRange range,
                                      swift::AccessLevel accessLevel) {
  return new (cContext.unbridged()) AccessControlAttr(
      /*atLoc=*/{}, range, accessLevel);
}

BridgedAlignmentAttr
BridgedAlignmentAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                  SourceRange range, size_t cValue) {
  return new (cContext.unbridged())
      AlignmentAttr(cValue, atLoc, range, /*Implicit=*/false);
}

BridgedAllowFeatureSuppressionAttr
BridgedAllowFeatureSuppressionAttr_createParsed(BridgedASTContext cContext,
                                                SourceLoc atLoc,
                                                SourceRange range,
                                                bool inverted,
                                                BridgedArrayRef cFeatures) {
  auto features = cFeatures.unbridged<Identifier>();
  return AllowFeatureSuppressionAttr::create(cContext.unbridged(), atLoc, range,
                                             /*implicit*/ false, inverted,
                                             features);
}

BridgedBackDeployedAttr BridgedBackDeployedAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    swift::PlatformKind platform, BridgedVersionTuple cVersion) {
  return new (cContext.unbridged()) BackDeployedAttr(
      atLoc, range, platform, cVersion.unbridged(), /*Implicit=*/false);
}

BridgedCDeclAttr BridgedCDeclAttr_createParsed(BridgedASTContext cContext,
                                               SourceLoc atLoc,
                                               SourceRange range,
                                               BridgedStringRef cName,
                                               bool underscored) {
  return new (cContext.unbridged())
      CDeclAttr(cName.unbridged(), atLoc, range,
                /*Implicit=*/false, /*Underscored*/ underscored);
}

BridgedCustomAttr BridgedCustomAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, BridgedTypeRepr cType,
    BridgedNullableCustomAttributeInitializer cInitContext,
    BridgedNullableArgumentList cArgumentList) {
  ASTContext &context = cContext.unbridged();
  return CustomAttr::create(
      context, atLoc, new (context) TypeExpr(cType.unbridged()),
      cInitContext.unbridged(), cArgumentList.unbridged());
}

BridgedDerivativeAttr BridgedDerivativeAttr_createParsedImpl(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedNullableTypeRepr cBaseType, BridgedDeclNameRef cOriginalName,
    BridgedDeclNameLoc cOriginalNameLoc,
    std::optional<swift::AccessorKind> AccessorKind, BridgedArrayRef cParams) {
  SmallVector<ParsedAutoDiffParameter, 2> params;
  for (auto &elem : cParams.unbridged<BridgedParsedAutoDiffParameter>())
    params.push_back(elem.unbridged());

  return DerivativeAttr::create(
      cContext.unbridged(),
      /*implicit=*/false, atLoc, range, cBaseType.unbridged(),
      DeclNameRefWithLoc{cOriginalName.unbridged(),
                         cOriginalNameLoc.unbridged(), AccessorKind},
      params);
}

BridgedDerivativeAttr BridgedDerivativeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedNullableTypeRepr cBaseType, BridgedDeclNameRef cOriginalName,
    BridgedDeclNameLoc cOriginalNameLoc, swift::AccessorKind AccessorKind,
    BridgedArrayRef cParams) {
  return BridgedDerivativeAttr_createParsedImpl(
      cContext, atLoc, range, cBaseType, cOriginalName, cOriginalNameLoc,
      AccessorKind, cParams);
}

BridgedDerivativeAttr BridgedDerivativeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedNullableTypeRepr cBaseType, BridgedDeclNameRef cOriginalName,
    BridgedDeclNameLoc cOriginalNameLoc, BridgedArrayRef cParams) {
  return BridgedDerivativeAttr_createParsedImpl(
      cContext, atLoc, range, cBaseType, cOriginalName, cOriginalNameLoc,
      /*cAccessorKind=*/std::nullopt, cParams);
}

BridgedDifferentiableAttr BridgedDifferentiableAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedDifferentiabilityKind cKind, BridgedArrayRef cParams,
    BridgedNullableTrailingWhereClause cGenericWhereClause) {
  SmallVector<ParsedAutoDiffParameter, 2> params;
  for (auto &elem : cParams.unbridged<BridgedParsedAutoDiffParameter>())
    params.push_back(elem.unbridged());

  return DifferentiableAttr::create(cContext.unbridged(), /*implicit=*/false,
                                    atLoc, range, unbridged(cKind), params,
                                    cGenericWhereClause.unbridged());
}

BridgedDynamicReplacementAttr BridgedDynamicReplacementAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc attrNameLoc,
    SourceLoc lParenLoc, BridgedDeclNameRef cReplacedFunction,
    SourceLoc rParenLoc) {
  return DynamicReplacementAttr::create(
      cContext.unbridged(), atLoc, attrNameLoc, lParenLoc,
      cReplacedFunction.unbridged(), rParenLoc);
}

BridgedDocumentationAttr BridgedDocumentationAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedStringRef cMetadata, BridgedOptionalAccessLevel accessLevel) {
  std::optional<swift::AccessLevel> optAccessLevel;
  if (accessLevel.getHasValue()) {
    optAccessLevel.emplace(accessLevel.getValue());
  }
  return new (cContext.unbridged()) DocumentationAttr(
      atLoc, range, cMetadata.unbridged(), optAccessLevel, /*implicit=*/false);
}

BridgedEffectsAttr
BridgedEffectsAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                SourceRange range,
                                swift::EffectsKind effectKind) {
  return new (cContext.unbridged()) EffectsAttr(atLoc, range, effectKind);
}

BridgedEffectsAttr BridgedEffectsAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedStringRef cCustomString, SourceLoc customStringLoc) {
  return new (cContext.unbridged())
      EffectsAttr(atLoc, range, cCustomString.unbridged(), customStringLoc);
}

static ExclusivityAttr::Mode unbridged(BridgedExclusivityAttrMode mode) {
  switch (mode) {
  case BridgedExclusivityAttrModeChecked:
    return ExclusivityAttr::Checked;
  case BridgedExclusivityAttrModeUnchecked:
    return ExclusivityAttr::Unchecked;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedExclusivityAttr
BridgedExclusivityAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                    SourceRange range,
                                    BridgedExclusivityAttrMode cMode) {
  return new (cContext.unbridged())
      ExclusivityAttr(atLoc, range, unbridged(cMode));
}

BridgedExposeAttr BridgedExposeAttr_createParsed(BridgedASTContext cContext,
                                                 SourceLoc atLoc,
                                                 SourceRange range,
                                                 BridgedStringRef cName,
                                                 swift::ExposureKind kind) {
  return new (cContext.unbridged())
      ExposeAttr(cName.unbridged(), atLoc, range, kind, /*Implicit=*/false);
}

BridgedExternAttr BridgedExternAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    SourceLoc lParenLoc, SourceLoc rParenLoc, swift::ExternKind kind,
    BridgedStringRef cModuleName, BridgedStringRef cName) {
  std::optional<StringRef> moduleName = cModuleName.unbridged();
  if (moduleName->empty())
    moduleName = std::nullopt;

  std::optional<StringRef> name = cName.unbridged();
  if (name->empty())
    name = std::nullopt;

  return new (cContext.unbridged())
      ExternAttr(moduleName, name, atLoc, lParenLoc, rParenLoc, range, kind,
                 /*Implicit=*/false);
}

BridgedImplementsAttr BridgedImplementsAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedTypeRepr cProtocolType, BridgedDeclNameRef cMemberName,
    BridgedDeclNameLoc cMemberNameLoc) {
  return ImplementsAttr::create(
      cContext.unbridged(), atLoc, range, cProtocolType.unbridged(),
      cMemberName.unbridged().getFullName(), cMemberNameLoc.unbridged());
}

BridgedInlineAttr BridgedInlineAttr_createParsed(BridgedASTContext cContext,
                                                 SourceLoc atLoc,
                                                 SourceRange range,
                                                 swift::InlineKind kind) {
  return new (cContext.unbridged()) InlineAttr(atLoc, range, kind);
}

static swift::ParsedLifetimeDependenceKind
unbridged(BridgedParsedLifetimeDependenceKind kind) {
  switch (kind) {
  case BridgedParsedLifetimeDependenceKindDefault:
    return swift::ParsedLifetimeDependenceKind::Default;
  case BridgedParsedLifetimeDependenceKindBorrow:
    return swift::ParsedLifetimeDependenceKind::Borrow;
  case BridgedParsedLifetimeDependenceKindInherit:
    return swift::ParsedLifetimeDependenceKind::Inherit;
  case BridgedParsedLifetimeDependenceKindInout:
    return swift::ParsedLifetimeDependenceKind::Inout;
  }
  llvm_unreachable("unhandled enum value");
}

swift::LifetimeDescriptor BridgedLifetimeDescriptor::unbridged() {
  switch (kind) {
  case DescriptorKind::Named:
    return LifetimeDescriptor::forNamed(value.name, ::unbridged(dependenceKind),
                                        loc);
  case DescriptorKind::Ordered:
    return LifetimeDescriptor::forOrdered(value.index,
                                          ::unbridged(dependenceKind), loc);
  case DescriptorKind::Self:
    return LifetimeDescriptor::forSelf(::unbridged(dependenceKind), loc);
  }
  llvm_unreachable("unhandled enum value");
}

static BridgedLifetimeEntry BridgedLifetimeEntry_createParsedImpl(
    BridgedASTContext cContext, SourceRange range, BridgedArrayRef cSources,
    std::optional<BridgedLifetimeDescriptor> cTarget) {
  SmallVector<LifetimeDescriptor> sources;
  for (auto cSource : cSources.unbridged<BridgedLifetimeDescriptor>())
    sources.push_back(cSource.unbridged());
  std::optional<LifetimeDescriptor> target;
  if (cTarget)
    target = cTarget->unbridged();

  return LifetimeEntry::create(cContext.unbridged(), range.Start, range.End,
                               sources, target);
}

BridgedLifetimeEntry
BridgedLifetimeEntry_createParsed(BridgedASTContext cContext, SourceRange range,
                                  BridgedArrayRef cSources) {
  return BridgedLifetimeEntry_createParsedImpl(cContext, range, cSources,
                                               std::nullopt);
}

BridgedLifetimeEntry
BridgedLifetimeEntry_createParsed(BridgedASTContext cContext, SourceRange range,
                                  BridgedArrayRef cSources,
                                  BridgedLifetimeDescriptor cTarget) {
  return BridgedLifetimeEntry_createParsedImpl(cContext, range, cSources,
                                               cTarget);
}

BridgedLifetimeAttr
BridgedLifetimeAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                 SourceRange range, BridgedLifetimeEntry cEntry,
                                 bool isUnderscored) {
  return LifetimeAttr::create(cContext.unbridged(), atLoc, range,
                              /*implicit=*/false, cEntry.unbridged(),
                              isUnderscored);
}

BridgedMacroRole BridgedMacroRole_fromString(BridgedStringRef str) {
  // Match the role string to the known set of roles.
  auto role =
      llvm::StringSwitch<std::optional<BridgedMacroRole>>(str.unbridged())
#define MACRO_ROLE(Name, Description) .Case(Description, BridgedMacroRole##Name)
#include "swift/Basic/MacroRoles.def"
          .Default(std::nullopt);
  return role.has_value() ? *role : BridgedMacroRoleNone;
}

MacroSyntax unbridge(BridgedMacroSyntax cSyntax) {
  switch (cSyntax) {
  case BridgedMacroSyntaxAttached:
    return MacroSyntax::Attached;
  case BridgedMacroSyntaxFreestanding:
    return MacroSyntax::Freestanding;
  }
}

BridgedMacroRoleAttr BridgedMacroRoleAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedMacroSyntax cSyntax, SourceLoc lParenLoc, BridgedMacroRole cRole,
    BridgedArrayRef cNames, BridgedArrayRef cConformances,
    SourceLoc rParenLoc) {
  SmallVector<MacroIntroducedDeclName, 2> names;
  for (auto &n : cNames.unbridged<BridgedMacroIntroducedDeclName>())
    names.push_back(n.unbridged());

  SmallVector<Expr *, 2> conformances;
  for (auto &t : cConformances.unbridged<BridgedExpr>())
    conformances.push_back(t.unbridged());

  return MacroRoleAttr::create(cContext.unbridged(), atLoc, range,
                               unbridge(cSyntax), lParenLoc, unbridge(cRole),
                               names, conformances, rParenLoc,
                               /*implicit=*/false);
}

BridgedOriginallyDefinedInAttr BridgedOriginallyDefinedInAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedStringRef cModuleName, swift::PlatformKind platform,
    BridgedVersionTuple cVersion) {
  return new (cContext.unbridged()) OriginallyDefinedInAttr(
      atLoc, range, cModuleName.unbridged(), platform, cVersion.unbridged(),
      /*Implicit=*/false);
}

BridgedStorageRestrictionsAttr BridgedStorageRestrictionsAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedArrayRef cInitializes, BridgedArrayRef cAccesses) {
  ASTContext &context = cContext.unbridged();

  auto initializes = cInitializes.unbridged<Identifier>();
  auto accesses = cAccesses.unbridged<Identifier>();
  return StorageRestrictionsAttr::create(context, atLoc, range, initializes,
                                         accesses);
}

BridgedSwiftNativeObjCRuntimeBaseAttr
BridgedSwiftNativeObjCRuntimeBaseAttr_createParsed(BridgedASTContext cContext,
                                                   SourceLoc atLoc,
                                                   SourceRange range,
                                                   Identifier name) {
  return new (cContext.unbridged())
      SwiftNativeObjCRuntimeBaseAttr(name, atLoc, range, /*Implicit=*/false);
}

static NonSendableKind unbridged(BridgedNonSendableKind kind) {
  switch (kind) {
  case BridgedNonSendableKindSpecific:
    return NonSendableKind::Specific;
  case BridgedNonSendableKindAssumed:
    return NonSendableKind::Assumed;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedNonSendableAttr
BridgedNonSendableAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                    SourceRange range,
                                    BridgedNonSendableKind cKind) {
  return new (cContext.unbridged())
      NonSendableAttr(atLoc, range, unbridged(cKind));
}

BridgedNonisolatedAttr
BridgedNonisolatedAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                    SourceRange range,
                                    swift::NonIsolatedModifier modifier) {
  return new (cContext.unbridged()) NonisolatedAttr(atLoc, range, modifier,
                                                    /*implicit=*/false);
}

BridgedInheritActorContextAttr BridgedInheritActorContextAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    swift::InheritActorContextModifier modifier) {
  return new (cContext.unbridged())
      InheritActorContextAttr(atLoc, range, modifier,
                              /*implicit=*/false);
}

BridgedObjCAttr BridgedObjCAttr_createParsedUnnamed(BridgedASTContext cContext,
                                                    SourceLoc atLoc,
                                                    SourceLoc attrNameLoc) {
  return ObjCAttr::createUnnamed(cContext.unbridged(), atLoc, attrNameLoc);
}

BridgedObjCAttr
BridgedObjCAttr_createParsedNullary(BridgedASTContext cContext, SourceLoc atLoc,
                                    SourceLoc attrNameLoc, SourceLoc lParenLoc,
                                    SourceLoc nameLoc, Identifier name,
                                    SourceLoc rParenLoc) {
  return ObjCAttr::createNullary(cContext.unbridged(), atLoc, attrNameLoc,
                                 lParenLoc, nameLoc, name, rParenLoc);
}

BridgedObjCAttr BridgedObjCAttr_createParsedSelector(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc attrNameLoc,
    SourceLoc lParenLoc, BridgedArrayRef cNameLocs, BridgedArrayRef cNames,
    SourceLoc rParenLoc) {
  auto nameLocs = cNameLocs.unbridged<SourceLoc>();
  auto names = cNames.unbridged<Identifier>();

  return ObjCAttr::createSelector(cContext.unbridged(), atLoc, attrNameLoc,
                                  lParenLoc, nameLocs, names, rParenLoc);
}

BridgedObjCImplementationAttr BridgedObjCImplementationAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    Identifier name, bool isEarlyAdopter) {
  return new (cContext.unbridged())
      ObjCImplementationAttr(name, atLoc, range, isEarlyAdopter);
}

BridgedObjCRuntimeNameAttr
BridgedObjCRuntimeNameAttr_createParsed(BridgedASTContext cContext,
                                        SourceLoc atLoc, SourceRange range,
                                        Identifier name) {
  return new (cContext.unbridged())
      ObjCRuntimeNameAttr(name.str(), atLoc, range, /*Implicit=*/false);
}

static OptimizationMode unbridged(BridgedOptimizationMode mode) {
  switch (mode) {
  case BridgedOptimizationModeForSpeed:
    return OptimizationMode::ForSpeed;
  case BridgedOptimizationModeForSize:
    return OptimizationMode::ForSize;
  case BridgedOptimizationModeNoOptimization:
    return OptimizationMode::NoOptimization;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedOptimizeAttr
BridgedOptimizeAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                 SourceRange range,
                                 BridgedOptimizationMode cMode) {
  return new (cContext.unbridged())
      OptimizeAttr(atLoc, range, unbridged(cMode));
}

BridgedPrivateImportAttr BridgedPrivateImportAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceLoc attrNameLoc,
    SourceLoc lParenLoc, BridgedStringRef cFileName, SourceLoc rParenLoc) {
  return PrivateImportAttr::create(cContext.unbridged(), atLoc, attrNameLoc,
                                   lParenLoc, cFileName.unbridged(), rParenLoc);
}

BridgedProjectedValuePropertyAttr
BridgedProjectedValuePropertyAttr_createParsed(BridgedASTContext cContext,
                                               SourceLoc atLoc,
                                               SourceRange range,
                                               Identifier name) {
  return new (cContext.unbridged())
      ProjectedValuePropertyAttr(name, atLoc, range, /*Implicit=*/false);
}

BridgedRawDocCommentAttr
BridgedRawDocCommentAttr_createParsed(BridgedASTContext cContext,
                                      BridgedCharSourceRange cRange) {
  return new (cContext.unbridged()) RawDocCommentAttr(cRange.unbridged());
}

BridgedRawLayoutAttr
BridgedStorageRestrictionsAttr_createParsed(BridgedASTContext cContext,
                                            SourceLoc atLoc, SourceRange range,
                                            size_t size, size_t alignment) {
  return new (cContext.unbridged())
      RawLayoutAttr(size, alignment, atLoc, range);
}

SWIFT_NAME("BridgedRawLayoutAttr.createParsed(_:atLoc:range:like:moveAsLike:)")
BridgedRawLayoutAttr BridgedStorageRestrictionsAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedTypeRepr cLikeType, bool moveAsLike) {
  return new (cContext.unbridged())
      RawLayoutAttr(cLikeType.unbridged(), moveAsLike, atLoc, range);
}

SWIFT_NAME("BridgedRawLayoutAttr.createParsed(_:atLoc:range:likeArrayOf:count:"
           "moveAsLike:)")
BridgedRawLayoutAttr BridgedStorageRestrictionsAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedTypeRepr cLikeType, BridgedTypeRepr cCountType, bool moveAsLike) {
  return new (cContext.unbridged()) RawLayoutAttr(
      cLikeType.unbridged(), cCountType.unbridged(), moveAsLike, atLoc, range);
}

ReferenceOwnership unbridged(BridgedReferenceOwnership kind) {
  switch (kind) {
  case BridgedReferenceOwnershipStrong:
    return ReferenceOwnership::Strong;
  case BridgedReferenceOwnershipWeak:
    return ReferenceOwnership::Weak;
  case BridgedReferenceOwnershipUnowned:
    return ReferenceOwnership::Unowned;
  case BridgedReferenceOwnershipUnmanaged:
    return ReferenceOwnership::Unmanaged;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedReferenceOwnershipAttr
BridgedReferenceOwnershipAttr_createParsed(BridgedASTContext cContext,
                                           SourceLoc atLoc, SourceRange range,
                                           BridgedReferenceOwnership cKind) {
  return new (cContext.unbridged())
      ReferenceOwnershipAttr(range, unbridged(cKind));
}

BridgedSectionAttr BridgedSectionAttr_createParsed(BridgedASTContext cContext,
                                                   SourceLoc atLoc,
                                                   SourceRange range,
                                                   BridgedStringRef cName) {
  return new (cContext.unbridged()) SectionAttr(cName.unbridged(), atLoc, range,
                                                /*Implicit=*/false);
}

BridgedSemanticsAttr
BridgedSemanticsAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                  SourceRange range, BridgedStringRef cValue) {
  return new (cContext.unbridged())
      SemanticsAttr(cValue.unbridged(), atLoc, range,
                    /*Implicit=*/false);
}

BridgedSetterAccessAttr
BridgedSetterAccessAttr_createParsed(BridgedASTContext cContext,
                                     SourceRange range,
                                     swift::AccessLevel accessLevel) {
  return new (cContext.unbridged()) SetterAccessAttr(
      /*atLoc=*/{}, range, accessLevel);
}

static SpecializeAttr::SpecializationKind
unbridge(BridgedSpecializationKind kind) {
  switch (kind) {
  case BridgedSpecializationKindFull:
    return AbstractSpecializeAttr::SpecializationKind::Full;
  case BridgedSpecializationKindPartial:
    return AbstractSpecializeAttr::SpecializationKind::Partial;
  }
  llvm_unreachable("unhandled kind");
}

BridgedSpecializeAttr BridgedSpecializeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedNullableTrailingWhereClause cWhereClause, bool exported,
    BridgedSpecializationKind cKind, BridgedDeclNameRef cTargetFunction,
    BridgedArrayRef cSPIGroups, BridgedArrayRef cAvailableAttrs) {
  auto spiGroups = cSPIGroups.unbridged<Identifier>();
  SmallVector<AvailableAttr *, 2> availableAttrs;
  for (auto bridging : cAvailableAttrs.unbridged<BridgedAvailableAttr>())
    availableAttrs.push_back(bridging.unbridged());

  return SpecializeAttr::create(
      cContext.unbridged(), atLoc, range, cWhereClause.unbridged(), exported,
      unbridge(cKind), cTargetFunction.unbridged(), spiGroups, availableAttrs);
}

BridgedSpecializedAttr BridgedSpecializedAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedNullableTrailingWhereClause cWhereClause, bool exported,
    BridgedSpecializationKind cKind, BridgedDeclNameRef cTargetFunction,
    BridgedArrayRef cSPIGroups, BridgedArrayRef cAvailableAttrs) {
  auto spiGroups = cSPIGroups.unbridged<Identifier>();
  SmallVector<AvailableAttr *, 2> availableAttrs;
  for (auto bridging : cAvailableAttrs.unbridged<BridgedAvailableAttr>())
    availableAttrs.push_back(bridging.unbridged());

  return SpecializedAttr::create(
      cContext.unbridged(), atLoc, range, cWhereClause.unbridged(), exported,
      unbridge(cKind), cTargetFunction.unbridged(), spiGroups, availableAttrs);
}

BridgedSPIAccessControlAttr
BridgedSPIAccessControlAttr_createParsed(BridgedASTContext cContext,
                                         SourceLoc atLoc, SourceRange range,
                                         Identifier SPIGroupName) {
  return SPIAccessControlAttr::create(cContext.unbridged(), atLoc, range,
                                      SPIGroupName);
}

BridgedSILGenNameAttr
BridgedSILGenNameAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                   SourceRange range, BridgedStringRef cName,
                                   bool isRaw) {
  return new (cContext.unbridged()) SILGenNameAttr(
      cName.unbridged(), isRaw, atLoc, range, /*Implicit=*/false);
}

BridgedTransposeAttr BridgedTransposeAttr_createParsed(
    BridgedASTContext cContext, SourceLoc atLoc, SourceRange range,
    BridgedNullableTypeRepr cBaseType, BridgedDeclNameRef cOriginalName,
    BridgedDeclNameLoc cOriginalNameLoc, BridgedArrayRef cParams) {
  SmallVector<ParsedAutoDiffParameter, 2> params;
  for (auto &elem : cParams.unbridged<BridgedParsedAutoDiffParameter>())
    params.push_back(elem.unbridged());

  return TransposeAttr::create(
      cContext.unbridged(),
      /*implicit=*/false, atLoc, range, cBaseType.unbridged(),
      DeclNameRefWithLoc{cOriginalName.unbridged(),
                         cOriginalNameLoc.unbridged(),
                         /*AccessorKind=*/std::nullopt},
      params);
}

BridgedTypeEraserAttr
BridgedTypeEraserAttr_createParsed(BridgedASTContext cContext, SourceLoc atLoc,
                                   SourceRange range,
                                   BridgedTypeExpr cTypeExpr) {
  return TypeEraserAttr::create(cContext.unbridged(), atLoc, range,
                                cTypeExpr.unbridged());
}

BridgedUnavailableFromAsyncAttr
BridgedUnavailableFromAsyncAttr_createParsed(BridgedASTContext cContext,
                                             SourceLoc atLoc, SourceRange range,
                                             BridgedStringRef cMessage) {
  return new (cContext.unbridged()) UnavailableFromAsyncAttr(
      cMessage.unbridged(), atLoc, range, /*implicit=*/false);
}
