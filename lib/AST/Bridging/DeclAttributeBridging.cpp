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
    return ParsedAutoDiffParameter::getNamedParameter(loc.unbridged(),
                                                      value.name.unbridged());
  case Kind::Ordered:
    return ParsedAutoDiffParameter::getOrderedParameter(loc.unbridged(),
                                                        value.index);
  case Kind::Self:
    return ParsedAutoDiffParameter::getSelfParameter(loc.unbridged());
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

BridgedDeclAttribute BridgedDeclAttribute_createSimple(
    BridgedASTContext cContext, swift::DeclAttrKind kind,
    BridgedSourceLoc cAtLoc, BridgedSourceLoc cAttrLoc) {
  return DeclAttribute::createSimple(cContext.unbridged(), kind,
                                     cAtLoc.unbridged(), cAttrLoc.unbridged());
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
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedIdentifier cDomainIdentifier,
    BridgedSourceLoc cDomainLoc, BridgedAvailableAttrKind cKind,
    BridgedStringRef cMessage, BridgedStringRef cRenamed,
    BridgedVersionTuple cIntroduced, BridgedSourceRange cIntroducedRange,
    BridgedVersionTuple cDeprecated, BridgedSourceRange cDeprecatedRange,
    BridgedVersionTuple cObsoleted, BridgedSourceRange cObsoletedRange,
    bool isSPI) {

  return new (cContext.unbridged())
      AvailableAttr(cAtLoc.unbridged(), cRange.unbridged(),
                    cDomainIdentifier.unbridged(), cDomainLoc.unbridged(),
                    unbridge(cKind), cMessage.unbridged(), cRenamed.unbridged(),
                    cIntroduced.unbridged(), cIntroducedRange.unbridged(),
                    cDeprecated.unbridged(), cDeprecatedRange.unbridged(),
                    cObsoleted.unbridged(), cObsoletedRange.unbridged(),
                    /*Implicit=*/false,
                    /*IsSPI=*/isSPI);
}

BridgedAvailableAttr
BridgedAvailableAttr_createUnavailableInEmbedded(BridgedASTContext cContext,
                                                 BridgedSourceLoc cAtLoc,
                                                 BridgedSourceRange cRange) {
  return AvailableAttr::createUnavailableInEmbedded(
      cContext.unbridged(), cAtLoc.unbridged(), cRange.unbridged());
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

static std::optional<AccessLevel> unbridge(BridgedAccessLevel level) {
  switch (level) {
  case BridgedAccessLevelPrivate:
    return AccessLevel::Private;
  case BridgedAccessLevelFilePrivate:
    return AccessLevel::FilePrivate;
  case BridgedAccessLevelInternal:
    return AccessLevel::Internal;
  case BridgedAccessLevelPackage:
    return AccessLevel::Package;
  case BridgedAccessLevelPublic:
    return AccessLevel::Public;
  case BridgedAccessLevelOpen:
    return AccessLevel::Open;
  case BridgedAccessLevelNone:
    return std::nullopt;
  }
  llvm_unreachable("unhandled BridgedAccessLevel");
}

BridgedABIAttr BridgedABIAttr_createParsed(BridgedASTContext cContext,
                                           BridgedSourceLoc atLoc,
                                           BridgedSourceRange range,
                                           BridgedNullableDecl abiDecl) {
  return new (cContext.unbridged()) ABIAttr(abiDecl.unbridged(),
                                            atLoc.unbridged(),
                                            range.unbridged(),
                                            /*isImplicit=*/false);
}

BridgedAccessControlAttr
BridgedAccessControlAttr_createParsed(BridgedASTContext cContext,
                                      BridgedSourceRange cRange,
                                      BridgedAccessLevel cAccessLevel) {
  return new (cContext.unbridged()) AccessControlAttr(
      /*atLoc=*/{}, cRange.unbridged(), unbridge(cAccessLevel).value());
}

BridgedAlignmentAttr
BridgedAlignmentAttr_createParsed(BridgedASTContext cContext,
                                  BridgedSourceLoc cAtLoc,
                                  BridgedSourceRange cRange, size_t cValue) {
  return new (cContext.unbridged()) AlignmentAttr(
      cValue, cAtLoc.unbridged(), cRange.unbridged(), /*Implicit=*/false);
}

BridgedAllowFeatureSuppressionAttr
BridgedAllowFeatureSuppressionAttr_createParsed(BridgedASTContext cContext,
                                                BridgedSourceLoc cAtLoc,
                                                BridgedSourceRange cRange,
                                                bool inverted,
                                                BridgedArrayRef cFeatures) {
  SmallVector<Identifier> features;
  for (auto elem : cFeatures.unbridged<BridgedIdentifier>())
    features.push_back(elem.unbridged());
  return AllowFeatureSuppressionAttr::create(
      cContext.unbridged(), cAtLoc.unbridged(), cRange.unbridged(),
      /*implicit*/ false, inverted, features);
}

BridgedBackDeployedAttr BridgedBackDeployedAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedPlatformKind cPlatform,
    BridgedVersionTuple cVersion) {
  return new (cContext.unbridged()) BackDeployedAttr(
      cAtLoc.unbridged(), cRange.unbridged(), unbridge(cPlatform),
      cVersion.unbridged(), /*Implicit=*/false);
}

BridgedCDeclAttr BridgedCDeclAttr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cAtLoc,
                                               BridgedSourceRange cRange,
                                               BridgedStringRef cName) {
  return new (cContext.unbridged())
      CDeclAttr(cName.unbridged(), cAtLoc.unbridged(), cRange.unbridged(),
                /*Implicit=*/false, /*Underscored*/true);
}

BridgedCustomAttr BridgedCustomAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc, BridgedTypeRepr cType,
    BridgedNullableCustomAttributeInitializer cInitContext,
    BridgedNullableArgumentList cArgumentList) {
  ASTContext &context = cContext.unbridged();
  return CustomAttr::create(
      context, cAtLoc.unbridged(), new (context) TypeExpr(cType.unbridged()),
      cInitContext.unbridged(), cArgumentList.unbridged());
}

BridgedDerivativeAttr BridgedDerivativeAttr_createParsedImpl(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedNullableTypeRepr cBaseType,
    BridgedDeclNameRef cOriginalName, BridgedDeclNameLoc cOriginalNameLoc,
    std::optional<swift::AccessorKind> AccessorKind, BridgedArrayRef cParams) {
  SmallVector<ParsedAutoDiffParameter, 2> params;
  for (auto &elem : cParams.unbridged<BridgedParsedAutoDiffParameter>())
    params.push_back(elem.unbridged());

  return DerivativeAttr::create(cContext.unbridged(),
                                /*implicit=*/false, cAtLoc.unbridged(),
                                cRange.unbridged(), cBaseType.unbridged(),
                                DeclNameRefWithLoc{cOriginalName.unbridged(),
                                                   cOriginalNameLoc.unbridged(),
                                                   AccessorKind},
                                params);
}

BridgedDerivativeAttr BridgedDerivativeAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedNullableTypeRepr cBaseType,
    BridgedDeclNameRef cOriginalName, BridgedDeclNameLoc cOriginalNameLoc,
    swift::AccessorKind AccessorKind, BridgedArrayRef cParams) {
  return BridgedDerivativeAttr_createParsedImpl(
      cContext, cAtLoc, cRange, cBaseType, cOriginalName, cOriginalNameLoc,
      AccessorKind, cParams);
}

BridgedDerivativeAttr BridgedDerivativeAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedNullableTypeRepr cBaseType,
    BridgedDeclNameRef cOriginalName, BridgedDeclNameLoc cOriginalNameLoc,
    BridgedArrayRef cParams) {
  return BridgedDerivativeAttr_createParsedImpl(
      cContext, cAtLoc, cRange, cBaseType, cOriginalName, cOriginalNameLoc,
      /*cAccessorKind=*/std::nullopt, cParams);
}

BridgedDifferentiableAttr BridgedDifferentiableAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedDifferentiabilityKind cKind,
    BridgedArrayRef cParams,
    BridgedNullableTrailingWhereClause cGenericWhereClause) {
  SmallVector<ParsedAutoDiffParameter, 2> params;
  for (auto &elem : cParams.unbridged<BridgedParsedAutoDiffParameter>())
    params.push_back(elem.unbridged());

  return DifferentiableAttr::create(cContext.unbridged(), /*implicit=*/false,
                                    cAtLoc.unbridged(), cRange.unbridged(),
                                    unbridged(cKind), params,
                                    cGenericWhereClause.unbridged());
}

BridgedDynamicReplacementAttr BridgedDynamicReplacementAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceLoc cAttrNameLoc, BridgedSourceLoc cLParenLoc,
    BridgedDeclNameRef cReplacedFunction, BridgedSourceLoc cRParenLoc) {
  return DynamicReplacementAttr::create(
      cContext.unbridged(), cAtLoc.unbridged(), cAttrNameLoc.unbridged(),
      cLParenLoc.unbridged(), cReplacedFunction.unbridged(),
      cRParenLoc.unbridged());
}

BridgedDocumentationAttr BridgedDocumentationAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cMetadata,
    BridgedAccessLevel cAccessLevel) {
  return new (cContext.unbridged()) DocumentationAttr(
      cAtLoc.unbridged(), cRange.unbridged(), cMetadata.unbridged(),
      unbridge(cAccessLevel), /*implicit=*/false);
}

static EffectsKind unbridged(BridgedEffectsKind kind) {
  switch (kind) {
  case BridgedEffectsKindReadNone:
    return EffectsKind::ReadNone;
  case BridgedEffectsKindReadOnly:
    return EffectsKind::ReadOnly;
  case BridgedEffectsKindReleaseNone:
    return EffectsKind::ReleaseNone;
  case BridgedEffectsKindReadWrite:
    return EffectsKind::ReadWrite;
  case BridgedEffectsKindUnspecified:
    return EffectsKind::Unspecified;
  case BridgedEffectsKindCustom:
    return EffectsKind::Custom;
  }
  llvm_unreachable("unhandled kind");
}

BridgedEffectsAttr BridgedEffectsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedEffectsKind cEffectKind) {
  return new (cContext.unbridged()) EffectsAttr(
      cAtLoc.unbridged(), cRange.unbridged(), unbridged(cEffectKind));
}

BridgedEffectsAttr BridgedEffectsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cCustomString,
    BridgedSourceLoc cCustomStringLoc) {
  return new (cContext.unbridged())
      EffectsAttr(cAtLoc.unbridged(), cRange.unbridged(),
                  cCustomString.unbridged(), cCustomStringLoc.unbridged());
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
BridgedExclusivityAttr BridgedExclusivityAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedExclusivityAttrMode cMode) {
  return new (cContext.unbridged())
      ExclusivityAttr(cAtLoc.unbridged(), cRange.unbridged(), unbridged(cMode));
}

static ExposureKind unbridged(BridgedExposureKind kind) {
  switch (kind) {
  case BridgedExposureKindCxx:
    return ExposureKind::Cxx;
  case BridgedExposureKindWasm:
    return ExposureKind::Wasm;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedExposeAttr BridgedExposeAttr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cAtLoc,
                                                 BridgedSourceRange cRange,
                                                 BridgedStringRef cName,
                                                 BridgedExposureKind cKind) {
  return new (cContext.unbridged())
      ExposeAttr(cName.unbridged(), cAtLoc.unbridged(), cRange.unbridged(),
                 unbridged(cKind), /*Implicit=*/false);
}

static ExternKind unbridged(BridgedExternKind kind) {
  switch (kind) {
  case BridgedExternKindC:
    return ExternKind::C;
  case BridgedExternKindWasm:
    return ExternKind::Wasm;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedExternAttr BridgedExternAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedSourceLoc cLParenLoc,
    BridgedSourceLoc cRParenLoc, BridgedExternKind cKind,
    BridgedStringRef cModuleName, BridgedStringRef cName) {
  std::optional<StringRef> moduleName = cModuleName.unbridged();
  if (moduleName->empty())
    moduleName = std::nullopt;

  std::optional<StringRef> name = cName.unbridged();
  if (name->empty())
    name = std::nullopt;

  return new (cContext.unbridged())
      ExternAttr(moduleName, name, cAtLoc.unbridged(), cLParenLoc.unbridged(),
                 cRParenLoc.unbridged(), cRange.unbridged(), unbridged(cKind),
                 /*Implicit=*/false);
}

BridgedImplementsAttr BridgedImplementsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedTypeRepr cProtocolType,
    BridgedDeclNameRef cMemberName, BridgedDeclNameLoc cMemberNameLoc) {
  return ImplementsAttr::create(cContext.unbridged(), cAtLoc.unbridged(),
                                cRange.unbridged(), cProtocolType.unbridged(),
                                cMemberName.unbridged().getFullName(),
                                cMemberNameLoc.unbridged());
}

static InlineKind unbridged(BridgedInlineKind kind) {
  switch (kind) {
  case BridgedInlineKindNever:
    return InlineKind::Never;
  case BridgedInlineKindAlways:
    return InlineKind::Always;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedInlineAttr BridgedInlineAttr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cAtLoc,
                                                 BridgedSourceRange cRange,
                                                 BridgedInlineKind cKind) {
  return new (cContext.unbridged())
      InlineAttr(cAtLoc.unbridged(), cRange.unbridged(), unbridged(cKind));
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
    return LifetimeDescriptor::forNamed(
        value.name.unbridged(), ::unbridged(dependenceKind), loc.unbridged());
  case DescriptorKind::Ordered:
    return LifetimeDescriptor::forOrdered(
        value.index, ::unbridged(dependenceKind), loc.unbridged());
  case DescriptorKind::Self:
    return LifetimeDescriptor::forSelf(::unbridged(dependenceKind),
                                       loc.unbridged());
  }
  llvm_unreachable("unhandled enum value");
}

static BridgedLifetimeEntry BridgedLifetimeEntry_createParsedImpl(
    BridgedASTContext cContext, BridgedSourceRange cRange,
    BridgedArrayRef cSources,
    std::optional<BridgedLifetimeDescriptor> cTarget) {
  SmallVector<LifetimeDescriptor> sources;
  for (auto cSource : cSources.unbridged<BridgedLifetimeDescriptor>())
    sources.push_back(cSource.unbridged());
  std::optional<LifetimeDescriptor> target;
  if (cTarget)
    target = cTarget->unbridged();

  return LifetimeEntry::create(cContext.unbridged(), cRange.Start.unbridged(),
                               cRange.End.unbridged(), sources, target);
}

BridgedLifetimeEntry
BridgedLifetimeEntry_createParsed(BridgedASTContext cContext,
                                  BridgedSourceRange cRange,
                                  BridgedArrayRef cSources) {
  return BridgedLifetimeEntry_createParsedImpl(cContext, cRange, cSources,
                                               std::nullopt);
}

BridgedLifetimeEntry BridgedLifetimeEntry_createParsed(
    BridgedASTContext cContext, BridgedSourceRange cRange,
    BridgedArrayRef cSources, BridgedLifetimeDescriptor cTarget) {
  return BridgedLifetimeEntry_createParsedImpl(cContext, cRange, cSources,
                                               cTarget);
}

BridgedLifetimeAttr BridgedLifetimeAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedLifetimeEntry cEntry,
    bool isUnderscored) {
  return LifetimeAttr::create(cContext.unbridged(), cAtLoc.unbridged(),
                              cRange.unbridged(), /*implicit=*/false,
                              cEntry.unbridged(), isUnderscored);
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
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedMacroSyntax cSyntax,
    BridgedSourceLoc cLParenLoc, BridgedMacroRole cRole, BridgedArrayRef cNames,
    BridgedArrayRef cConformances, BridgedSourceLoc cRParenLoc) {
  SmallVector<MacroIntroducedDeclName, 2> names;
  for (auto &n : cNames.unbridged<BridgedMacroIntroducedDeclName>())
    names.push_back(n.unbridged());

  SmallVector<Expr *, 2> conformances;
  for (auto &t : cConformances.unbridged<BridgedExpr>())
    conformances.push_back(t.unbridged());

  return MacroRoleAttr::create(
      cContext.unbridged(), cAtLoc.unbridged(), cRange.unbridged(),
      unbridge(cSyntax), cLParenLoc.unbridged(), unbridge(cRole), names,
      conformances, cRParenLoc.unbridged(), /*implicit=*/false);
}

BridgedOriginallyDefinedInAttr BridgedOriginallyDefinedInAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cModuleName,
    BridgedPlatformKind cPlatform, BridgedVersionTuple cVersion) {
  return new (cContext.unbridged()) OriginallyDefinedInAttr(
      cAtLoc.unbridged(), cRange.unbridged(), cModuleName.unbridged(),
      unbridge(cPlatform), cVersion.unbridged(),
      /*Implicit=*/false);
}

BridgedStorageRestrictionsAttr BridgedStorageRestrictionsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedArrayRef cInitializes,
    BridgedArrayRef cAccesses) {
  ASTContext &context = cContext.unbridged();

  ArrayRef<Identifier> initializes =
      cContext.unbridged().AllocateTransform<Identifier>(
          cInitializes.unbridged<BridgedIdentifier>(),
          [](auto &e) { return e.unbridged(); });
  ArrayRef<Identifier> accesses =
      cContext.unbridged().AllocateTransform<Identifier>(
          cAccesses.unbridged<BridgedIdentifier>(),
          [](auto &e) { return e.unbridged(); });

  return StorageRestrictionsAttr::create(
      context, cAtLoc.unbridged(), cRange.unbridged(), initializes, accesses);
}

BridgedSwiftNativeObjCRuntimeBaseAttr
BridgedSwiftNativeObjCRuntimeBaseAttr_createParsed(BridgedASTContext cContext,
                                                   BridgedSourceLoc cAtLoc,
                                                   BridgedSourceRange cRange,
                                                   BridgedIdentifier cName) {
  return new (cContext.unbridged())
      SwiftNativeObjCRuntimeBaseAttr(cName.unbridged(), cAtLoc.unbridged(),
                                     cRange.unbridged(), /*Implicit=*/false);
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

BridgedNonSendableAttr BridgedNonSendableAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedNonSendableKind cKind) {
  return new (cContext.unbridged())
      NonSendableAttr(cAtLoc.unbridged(), cRange.unbridged(), unbridged(cKind));
}

static NonIsolatedModifier unbridged(BridgedNonIsolatedModifier modifier) {
  switch (modifier) {
  case BridgedNonIsolatedModifierNone:
    return NonIsolatedModifier::None;
  case BridgedNonIsolatedModifierUnsafe:
    return NonIsolatedModifier::Unsafe;
  case BridgedNonIsolatedModifierNonSending:
    return NonIsolatedModifier::NonSending;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedNonisolatedAttr
BridgedNonisolatedAttr_createParsed(BridgedASTContext cContext,
                                    BridgedSourceLoc cAtLoc,
                                    BridgedSourceRange cRange,
                                    BridgedNonIsolatedModifier modifier) {
  return new (cContext.unbridged()) NonisolatedAttr(
      cAtLoc.unbridged(), cRange.unbridged(), unbridged(modifier),
      /*implicit=*/false);
}

static InheritActorContextModifier
unbridged(BridgedInheritActorContextModifier modifier) {
  switch (modifier) {
  case BridgedInheritActorContextModifierNone:
    return InheritActorContextModifier::None;
  case BridgedInheritActorContextModifierAlways:
    return InheritActorContextModifier::Always;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedInheritActorContextAttr BridgedInheritActorContextAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedInheritActorContextModifier modifier) {
  return new (cContext.unbridged()) InheritActorContextAttr(
      cAtLoc.unbridged(), cRange.unbridged(), unbridged(modifier),
      /*implicit=*/false);
}

BridgedObjCAttr
BridgedObjCAttr_createParsedUnnamed(BridgedASTContext cContext,
                                    BridgedSourceLoc cAtLoc,
                                    BridgedSourceLoc cAttrNameLoc) {
  return ObjCAttr::createUnnamed(cContext.unbridged(), cAtLoc.unbridged(),
                                 cAttrNameLoc.unbridged());
}

BridgedObjCAttr BridgedObjCAttr_createParsedNullary(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceLoc cAttrNameLoc, BridgedSourceLoc cLParenLoc,
    BridgedSourceLoc cNameLoc, BridgedIdentifier cName,
    BridgedSourceLoc cRParenLoc) {
  return ObjCAttr::createNullary(cContext.unbridged(), cAtLoc.unbridged(),
                                 cAttrNameLoc.unbridged(),
                                 cLParenLoc.unbridged(), cNameLoc.unbridged(),
                                 cName.unbridged(), cRParenLoc.unbridged());
}

BridgedObjCAttr BridgedObjCAttr_createParsedSelector(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceLoc cAttrNameLoc, BridgedSourceLoc cLParenLoc,
    BridgedArrayRef cNameLocs, BridgedArrayRef cNames,
    BridgedSourceLoc cRParenLoc) {
  SmallVector<SourceLoc> nameLocs;
  for (auto elem : cNameLocs.unbridged<BridgedSourceLoc>())
    nameLocs.push_back(elem.unbridged());
  SmallVector<Identifier> names;
  for (auto elem : cNames.unbridged<BridgedIdentifier>())
    names.push_back(elem.unbridged());

  return ObjCAttr::createSelector(
      cContext.unbridged(), cAtLoc.unbridged(), cAttrNameLoc.unbridged(),
      cLParenLoc.unbridged(), nameLocs, names, cRParenLoc.unbridged());
}

BridgedObjCImplementationAttr BridgedObjCImplementationAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedIdentifier cName, bool isEarlyAdopter) {
  return new (cContext.unbridged())
      ObjCImplementationAttr(cName.unbridged(), cAtLoc.unbridged(),
                             cRange.unbridged(), isEarlyAdopter);
}

BridgedObjCRuntimeNameAttr BridgedObjCRuntimeNameAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedIdentifier cName) {
  return new (cContext.unbridged())
      ObjCRuntimeNameAttr(cName.unbridged().str(), cAtLoc.unbridged(),
                          cRange.unbridged(), /*Implicit=*/false);
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

BridgedOptimizeAttr BridgedOptimizeAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedOptimizationMode cMode) {

  return new (cContext.unbridged())
      OptimizeAttr(cAtLoc.unbridged(), cRange.unbridged(), unbridged(cMode));
}

BridgedPrivateImportAttr BridgedPrivateImportAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceLoc cAttrNameLoc, BridgedSourceLoc cLParenLoc,
    BridgedStringRef cFileName, BridgedSourceLoc cRParenLoc) {
  return PrivateImportAttr::create(
      cContext.unbridged(), cAtLoc.unbridged(), cAttrNameLoc.unbridged(),
      cLParenLoc.unbridged(), cFileName.unbridged(), cRParenLoc.unbridged());
}

BridgedProjectedValuePropertyAttr
BridgedProjectedValuePropertyAttr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cAtLoc,
                                               BridgedSourceRange cRange,
                                               BridgedIdentifier cName) {
  return new (cContext.unbridged())
      ProjectedValuePropertyAttr(cName.unbridged(), cAtLoc.unbridged(),
                                 cRange.unbridged(), /*Implicit=*/false);
}

BridgedRawDocCommentAttr
BridgedRawDocCommentAttr_createParsed(BridgedASTContext cContext,
                                      BridgedCharSourceRange cRange) {
  return new (cContext.unbridged()) RawDocCommentAttr(cRange.unbridged());
}

BridgedRawLayoutAttr BridgedStorageRestrictionsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, size_t size, size_t alignment) {
  return new (cContext.unbridged())
      RawLayoutAttr(size, alignment, cAtLoc.unbridged(), cRange.unbridged());
}

SWIFT_NAME("BridgedRawLayoutAttr.createParsed(_:atLoc:range:like:moveAsLike:)")
BridgedRawLayoutAttr BridgedStorageRestrictionsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedTypeRepr cLikeType, bool moveAsLike) {
  return new (cContext.unbridged())
      RawLayoutAttr(cLikeType.unbridged(), moveAsLike, cAtLoc.unbridged(),
                    cRange.unbridged());
}

SWIFT_NAME("BridgedRawLayoutAttr.createParsed(_:atLoc:range:likeArrayOf:count:"
           "moveAsLike:)")
BridgedRawLayoutAttr BridgedStorageRestrictionsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedTypeRepr cLikeType,
    BridgedTypeRepr cCountType, bool moveAsLike) {
  return new (cContext.unbridged())
      RawLayoutAttr(cLikeType.unbridged(), cCountType.unbridged(), moveAsLike,
                    cAtLoc.unbridged(), cRange.unbridged());
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

BridgedReferenceOwnershipAttr BridgedReferenceOwnershipAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedReferenceOwnership cKind) {
  return new (cContext.unbridged())
      ReferenceOwnershipAttr(cRange.unbridged(), unbridged(cKind));
}

BridgedSectionAttr BridgedSectionAttr_createParsed(BridgedASTContext cContext,
                                                   BridgedSourceLoc cAtLoc,
                                                   BridgedSourceRange cRange,
                                                   BridgedStringRef cName) {
  return new (cContext.unbridged())
      SectionAttr(cName.unbridged(), cAtLoc.unbridged(), cRange.unbridged(),
                  /*Implicit=*/false);
}

BridgedSemanticsAttr BridgedSemanticsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cValue) {
  return new (cContext.unbridged())
      SemanticsAttr(cValue.unbridged(), cAtLoc.unbridged(), cRange.unbridged(),
                    /*Implicit=*/false);
}

BridgedSetterAccessAttr
BridgedSetterAccessAttr_createParsed(BridgedASTContext cContext,
                                     BridgedSourceRange cRange,
                                     BridgedAccessLevel cAccessLevel) {
  return new (cContext.unbridged()) SetterAccessAttr(
      /*atLoc=*/{}, cRange.unbridged(), unbridge(cAccessLevel).value());
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
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedNullableTrailingWhereClause cWhereClause,
    bool exported, BridgedSpecializationKind cKind,
    BridgedDeclNameRef cTargetFunction, BridgedArrayRef cSPIGroups,
    BridgedArrayRef cAvailableAttrs) {
  SmallVector<Identifier, 2> spiGroups;
  for (auto bridging : cSPIGroups.unbridged<BridgedIdentifier>())
    spiGroups.push_back(bridging.unbridged());
  SmallVector<AvailableAttr *, 2> availableAttrs;
  for (auto bridging : cAvailableAttrs.unbridged<BridgedAvailableAttr>())
    availableAttrs.push_back(bridging.unbridged());

  return SpecializeAttr::create(
      cContext.unbridged(), cAtLoc.unbridged(), cRange.unbridged(),
      cWhereClause.unbridged(), exported, unbridge(cKind),
      cTargetFunction.unbridged(), spiGroups, availableAttrs);
}

BridgedSpecializedAttr BridgedSpecializedAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedNullableTrailingWhereClause cWhereClause,
    bool exported, BridgedSpecializationKind cKind,
    BridgedDeclNameRef cTargetFunction, BridgedArrayRef cSPIGroups,
    BridgedArrayRef cAvailableAttrs) {
  SmallVector<Identifier, 2> spiGroups;
  for (auto bridging : cSPIGroups.unbridged<BridgedIdentifier>())
    spiGroups.push_back(bridging.unbridged());
  SmallVector<AvailableAttr *, 2> availableAttrs;
  for (auto bridging : cAvailableAttrs.unbridged<BridgedAvailableAttr>())
    availableAttrs.push_back(bridging.unbridged());

  return SpecializedAttr::create(
      cContext.unbridged(), cAtLoc.unbridged(), cRange.unbridged(),
      cWhereClause.unbridged(), exported, unbridge(cKind),
      cTargetFunction.unbridged(), spiGroups, availableAttrs);
}

BridgedSPIAccessControlAttr BridgedSPIAccessControlAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedIdentifier cSPIGroupName) {

  return SPIAccessControlAttr::create(cContext.unbridged(), cAtLoc.unbridged(),
                                      cRange.unbridged(),
                                      cSPIGroupName.unbridged());
}

BridgedSILGenNameAttr BridgedSILGenNameAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cName, bool isRaw) {
  return new (cContext.unbridged())
      SILGenNameAttr(cName.unbridged(), isRaw, cAtLoc.unbridged(),
                     cRange.unbridged(), /*Implicit=*/false);
}

BridgedTransposeAttr BridgedTransposeAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedNullableTypeRepr cBaseType,
    BridgedDeclNameRef cOriginalName, BridgedDeclNameLoc cOriginalNameLoc,
    BridgedArrayRef cParams) {
  SmallVector<ParsedAutoDiffParameter, 2> params;
  for (auto &elem : cParams.unbridged<BridgedParsedAutoDiffParameter>())
    params.push_back(elem.unbridged());

  return TransposeAttr::create(
      cContext.unbridged(),
      /*implicit=*/false, cAtLoc.unbridged(), cRange.unbridged(),
      cBaseType.unbridged(),
      DeclNameRefWithLoc{cOriginalName.unbridged(), cOriginalNameLoc.unbridged(),
                         /*AccessorKind=*/std::nullopt},
      params);
}

BridgedTypeEraserAttr BridgedTypeEraserAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedTypeExpr cTypeExpr

) {
  return TypeEraserAttr::create(cContext.unbridged(), cAtLoc.unbridged(),
                                cRange.unbridged(), cTypeExpr.unbridged());
}

BridgedUnavailableFromAsyncAttr BridgedUnavailableFromAsyncAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cMessage) {
  return new (cContext.unbridged())
      UnavailableFromAsyncAttr(cMessage.unbridged(), cAtLoc.unbridged(),
                               cRange.unbridged(), /*implicit=*/false);
}
