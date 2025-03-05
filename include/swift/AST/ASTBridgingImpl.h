//===--- ASTBridgingImpl.h - header for the swift ASTBridging module ------===//
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

#ifndef SWIFT_AST_ASTBRIDGINGIMPL_H
#define SWIFT_AST_ASTBRIDGINGIMPL_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/ArgumentList.h"
#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/IfConfigClauseRangeInfo.h"
#include "swift/AST/MacroDeclaration.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Fingerprint.h"

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

//===----------------------------------------------------------------------===//
// MARK: BridgedIdentifier
//===----------------------------------------------------------------------===//

BridgedIdentifier::BridgedIdentifier(swift::Identifier ident)
    : Raw(ident.getAsOpaquePointer()) {}

swift::Identifier BridgedIdentifier::unbridged() const {
  return swift::Identifier::getFromOpaquePointer(Raw);
}

bool BridgedIdentifier_isOperator(const BridgedIdentifier ident) {
  return ident.unbridged().isOperator();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedDeclBaseName
//===----------------------------------------------------------------------===//

BridgedDeclBaseName::BridgedDeclBaseName(swift::DeclBaseName baseName)
  : Ident(baseName.Ident) {}

swift::DeclBaseName BridgedDeclBaseName::unbridged() const {
  return swift::DeclBaseName(Ident.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: BridgedDeclBaseName
//===----------------------------------------------------------------------===//

BridgedConsumedLookupResult::BridgedConsumedLookupResult(
    swift::Identifier name, swift::SourceLoc sourceLoc, SwiftInt flag)
    : Name(BridgedIdentifier(name)), NameLoc(BridgedSourceLoc(sourceLoc)),
      Flag(flag) {}

//===----------------------------------------------------------------------===//
// MARK: BridgedDeclNameRef
//===----------------------------------------------------------------------===//

BridgedDeclNameRef::BridgedDeclNameRef()
    : BridgedDeclNameRef(swift::DeclNameRef()) {}

BridgedDeclNameRef::BridgedDeclNameRef(swift::DeclNameRef name)
  : opaque(name.getOpaqueValue()) {}

swift::DeclNameRef BridgedDeclNameRef::unbridged() const {
  return swift::DeclNameRef::getFromOpaqueValue(opaque);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedDeclNameLoc
//===----------------------------------------------------------------------===//

BridgedDeclNameLoc::BridgedDeclNameLoc(swift::DeclNameLoc loc)
    : LocationInfo(loc.LocationInfo),
      NumArgumentLabels(loc.NumArgumentLabels) {}

swift::DeclNameLoc BridgedDeclNameLoc::unbridged() const {
  return swift::DeclNameLoc(LocationInfo, NumArgumentLabels);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedASTContext
//===----------------------------------------------------------------------===//

BridgedASTContext::BridgedASTContext(swift::ASTContext &ctx) : Ctx(&ctx) {}

swift::ASTContext &BridgedASTContext::unbridged() const { return *Ctx; }

void * _Nonnull BridgedASTContext_raw(BridgedASTContext bridged) {
  return &bridged.unbridged();
}

BridgedASTContext BridgedASTContext_fromRaw(void * _Nonnull ptr) {
  return *static_cast<swift::ASTContext *>(ptr);
}

void *_Nullable BridgedASTContext_allocate(BridgedASTContext bridged,
                                           size_t size, size_t alignment) {
  return bridged.unbridged().Allocate(size, alignment);
}

BridgedStringRef BridgedASTContext_allocateCopyString(BridgedASTContext bridged,
                                                      BridgedStringRef cStr) {
  return bridged.unbridged().AllocateCopy(cStr.unbridged());
}

#define IDENTIFIER_WITH_NAME(Name, _) \
BridgedIdentifier BridgedASTContext_id_##Name(BridgedASTContext bridged) { \
return bridged.unbridged().Id_##Name; \
}
#include "swift/AST/KnownIdentifiers.def"

//===----------------------------------------------------------------------===//
// MARK: BridgedDeclContext
//===----------------------------------------------------------------------===//

bool BridgedDeclContext_isLocalContext(BridgedDeclContext dc) {
  return dc.unbridged()->isLocalContext();
}

bool BridgedDeclContext_isTypeContext(BridgedDeclContext dc) {
  return dc.unbridged()->isTypeContext();
}

bool BridgedDeclContext_isModuleScopeContext(BridgedDeclContext dc) {
  return dc.unbridged()->isModuleScopeContext();
}

bool BridgedDeclContext_isClosureExpr(BridgedDeclContext dc) {
  return llvm::isa_and_present<swift::ClosureExpr>(
      llvm::dyn_cast<swift::AbstractClosureExpr>(dc.unbridged()));
}

BridgedClosureExpr BridgedDeclContext_castToClosureExpr(BridgedDeclContext dc) {
  return llvm::cast<swift::ClosureExpr>(
      llvm::cast<swift::AbstractClosureExpr>(dc.unbridged()));
}

BridgedASTContext BridgedDeclContext_getASTContext(BridgedDeclContext dc) {
  return dc.unbridged()->getASTContext();
}

BridgedSourceFile
BridgedDeclContext_getParentSourceFile(BridgedDeclContext dc) {
  return dc.unbridged()->getParentSourceFile();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedSoureFile
//===----------------------------------------------------------------------===//

bool BridgedSourceFile_isScriptMode(BridgedSourceFile sf) {
  return sf.unbridged()->isScriptMode();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedDeclObj
//===----------------------------------------------------------------------===//

BridgedSourceLoc BridgedDeclObj::getLoc() const {
  swift::SourceLoc sourceLoc = unbridged()->getLoc();
  return BridgedSourceLoc(sourceLoc.getOpaquePointerValue());
}

BridgedStringRef BridgedDeclObj::Type_getName() const {
  return getAs<swift::TypeDecl>()->getName().str();
}

BridgedStringRef BridgedDeclObj::Value_getUserFacingName() const {
  return getAs<swift::ValueDecl>()->getBaseName().userFacingName();
}

BridgedSourceLoc BridgedDeclObj::Value_getNameLoc() const {
  return BridgedSourceLoc(getAs<swift::ValueDecl>()->getNameLoc().getOpaquePointerValue());
}

bool BridgedDeclObj::hasClangNode() const {
  return unbridged()->hasClangNode();
}

bool BridgedDeclObj::Value_isObjC() const {
  return getAs<swift::ValueDecl>()->isObjC();
}

bool BridgedDeclObj::GenericType_isGenericAtAnyLevel() const {
  return getAs<swift::GenericTypeDecl>()->isGenericContext();
}

bool BridgedDeclObj::NominalType_isGlobalActor() const {
  return getAs<swift::NominalTypeDecl>()->isGlobalActor();
}

OptionalBridgedDeclObj BridgedDeclObj::NominalType_getValueTypeDestructor() const {
  return {getAs<swift::NominalTypeDecl>()->getValueTypeDestructor()};
}

bool BridgedDeclObj::Struct_hasUnreferenceableStorage() const {
  return getAs<swift::StructDecl>()->hasUnreferenceableStorage();
}

BridgedASTType BridgedDeclObj::Class_getSuperclass() const {
  return {getAs<swift::ClassDecl>()->getSuperclass().getPointer()};
}

BridgedDeclObj BridgedDeclObj::Class_getDestructor() const {
  return {getAs<swift::ClassDecl>()->getDestructor()};
}

bool BridgedDeclObj::Destructor_isIsolated() const {
  auto dd = getAs<swift::DestructorDecl>();
  auto ai = swift::getActorIsolation(dd);
  return ai.isActorIsolated();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedASTNode
//===----------------------------------------------------------------------===//

BridgedASTNode::BridgedASTNode(void *_Nonnull pointer, BridgedASTNodeKind kind)
    : opaque(intptr_t(pointer) | kind) {
  assert(getPointer() == pointer && getKind() == kind);
}

BridgedExpr BridgedASTNode::castToExpr() const {
  assert(getKind() == BridgedASTNodeKindExpr);
  return static_cast<swift::Expr *>(getPointer());
}
BridgedStmt BridgedASTNode::castToStmt() const {
  assert(getKind() == BridgedASTNodeKindStmt);
  return static_cast<swift::Stmt *>(getPointer());
}
BridgedDecl BridgedASTNode::castToDecl() const {
  assert(getKind() == BridgedASTNodeKindDecl);
  return static_cast<swift::Decl *>(getPointer());
}

swift::ASTNode BridgedASTNode::unbridged() const {
  switch (getKind()) {
  case BridgedASTNodeKindExpr:
    return castToExpr().unbridged();
  case BridgedASTNodeKindStmt:
    return castToStmt().unbridged();
  case BridgedASTNodeKindDecl:
    return castToDecl().unbridged();
  }
}

//===----------------------------------------------------------------------===//
// MARK: Diagnostic Engine
//===----------------------------------------------------------------------===//

BridgedDiagnosticArgument::BridgedDiagnosticArgument(const swift::DiagnosticArgument &arg) {
  *reinterpret_cast<swift::DiagnosticArgument *>(&storage) = arg;
}

const swift::DiagnosticArgument &BridgedDiagnosticArgument::unbridged() const {
  return *reinterpret_cast<const swift::DiagnosticArgument *>(&storage);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedDeclAttributes
//===----------------------------------------------------------------------===//

BridgedDeclAttributes::BridgedDeclAttributes(swift::DeclAttributes attrs)
    : chain(attrs.getRawAttributeChain()) {}

swift::DeclAttributes BridgedDeclAttributes::unbridged() const {
  swift::DeclAttributes attrs;
  attrs.setRawAttributeChain(chain.unbridged());
  return attrs;
}

//===----------------------------------------------------------------------===//
// MARK: AvailabilityDomain
//===----------------------------------------------------------------------===//

BridgedAvailabilityDomain::BridgedAvailabilityDomain(
    swift::AvailabilityDomain domain)
    : opaque(domain.getOpaqueValue()) {}

swift::AvailabilityDomain BridgedAvailabilityDomain::unbridged() const {
  return swift::AvailabilityDomain::fromOpaque(opaque);
}

BridgedAvailabilityDomain BridgedAvailabilityDomain::forUniversal() {
  return swift::AvailabilityDomain::forUniversal();
}
BridgedAvailabilityDomain
BridgedAvailabilityDomain::forPlatform(BridgedPlatformKind platformKind) {
  return swift::AvailabilityDomain::forPlatform(unbridge(platformKind));
}
BridgedAvailabilityDomain BridgedAvailabilityDomain::forSwiftLanguage() {
  return swift::AvailabilityDomain::forSwiftLanguage();
}
BridgedAvailabilityDomain BridgedAvailabilityDomain::forPackageDescription() {
  return swift::AvailabilityDomain::forPackageDescription();
}
BridgedAvailabilityDomain BridgedAvailabilityDomain::forEmbedded() {
  return swift::AvailabilityDomain::forEmbedded();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedParamDecl
//===----------------------------------------------------------------------===//

swift::ParamSpecifier unbridge(BridgedParamSpecifier specifier) {
  switch (specifier) {
#define CASE(ID)                                                               \
  case BridgedParamSpecifier##ID:                                              \
    return swift::ParamSpecifier::ID;
    CASE(Default)
    CASE(InOut)
    CASE(Borrowing)
    CASE(Consuming)
    CASE(LegacyShared)
    CASE(LegacyOwned)
    CASE(ImplicitlyCopyableConsuming)
#undef CASE
  }
}

void BridgedParamDecl_setTypeRepr(BridgedParamDecl cDecl,
                                  BridgedTypeRepr cType) {
  cDecl.unbridged()->setTypeRepr(cType.unbridged());
}

void BridgedParamDecl_setSpecifier(BridgedParamDecl cDecl,
                                   BridgedParamSpecifier cSpecifier) {
  cDecl.unbridged()->setSpecifier(unbridge(cSpecifier));
}

void BridgedParamDecl_setImplicit(BridgedParamDecl cDecl) {
  cDecl.unbridged()->setImplicit();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedSubscriptDecl
//===----------------------------------------------------------------------===//

BridgedAbstractStorageDecl
BridgedSubscriptDecl_asAbstractStorageDecl(BridgedSubscriptDecl decl) {
  return decl.unbridged();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedVarDecl
//===----------------------------------------------------------------------===//

BridgedAbstractStorageDecl
BridgedVarDecl_asAbstractStorageDecl(BridgedVarDecl decl) {
  return decl.unbridged();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedCallArgument
//===----------------------------------------------------------------------===//

swift::Argument BridgedCallArgument::unbridged() const {
  return swift::Argument(labelLoc.unbridged(), label.unbridged(),
                         argExpr.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: BridgedLabeledStmtInfo
//===----------------------------------------------------------------------===//

swift::LabeledStmtInfo BridgedLabeledStmtInfo::unbridged() const {
  return {Name.unbridged(), Loc.unbridged()};
}

//===----------------------------------------------------------------------===//
// MARK: BridgedASTType
//===----------------------------------------------------------------------===//

swift::Type BridgedASTType::unbridged() const {
  return type;
}

BridgedOwnedString BridgedASTType::getDebugDescription() const {
  return BridgedOwnedString(unbridged().getString());
}

BridgedCanType BridgedASTType::getCanonicalType() const {
  return unbridged()->getCanonicalType();
}

bool BridgedASTType::hasTypeParameter() const {
  return unbridged()->hasTypeParameter();
}

bool BridgedASTType::isOpenedExistentialWithError() const {
  return unbridged()->isOpenedExistentialWithError();
}

bool BridgedASTType::isEscapable() const {
  return unbridged()->isEscapable();
}

bool BridgedASTType::isNoEscape() const {
  return unbridged()->isNoEscape();
}

bool BridgedASTType::isInteger() const {
  return unbridged()->is<swift::IntegerType>();
}

BridgedASTType BridgedASTType::subst(BridgedSubstitutionMap substMap) const {
  return {unbridged().subst(substMap.unbridged()).getPointer()};
}

//===----------------------------------------------------------------------===//
// MARK: BridgedCanType
//===----------------------------------------------------------------------===//

static_assert((int)BridgedCanType::TraitResult::IsNot == (int)swift::TypeTraitResult::IsNot);
static_assert((int)BridgedCanType::TraitResult::CanBe == (int)swift::TypeTraitResult::CanBe);
static_assert((int)BridgedCanType::TraitResult::Is == (int)swift::TypeTraitResult::Is);

BridgedCanType::BridgedCanType(swift::CanType ty) : type(ty.getPointer()) {
}

swift::CanType BridgedCanType::unbridged() const {
  return swift::CanType(type);
}

BridgedASTType BridgedCanType::getType() const {
  return {type};
}

BridgedCanType::TraitResult BridgedCanType::canBeClass() const {
  return (TraitResult)unbridged()->canBeClass();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedASTTypeArray
//===----------------------------------------------------------------------===//

BridgedASTType BridgedASTTypeArray::getAt(SwiftInt index) const {
  return {typeArray.unbridged<swift::Type>()[index].getPointer()};
}


//===----------------------------------------------------------------------===//
// MARK: BridgedConformance
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedConformance) == sizeof(swift::ProtocolConformanceRef));

BridgedConformance::BridgedConformance(swift::ProtocolConformanceRef conformance)
    : opaqueValue(conformance.getOpaqueValue()) {}

swift::ProtocolConformanceRef BridgedConformance::unbridged() const {
  return swift::ProtocolConformanceRef::getFromOpaqueValue(opaqueValue);
}

bool BridgedConformance::isConcrete() const {
  return unbridged().isConcrete();
}

bool BridgedConformance::isValid() const {
  return !unbridged().isInvalid();
}

bool BridgedConformance::isSpecializedConformance() const {
  return swift::isa<swift::SpecializedProtocolConformance>(unbridged().getConcrete());
}

bool BridgedConformance::isInheritedConformance() const {
  return swift::isa<swift::InheritedProtocolConformance>(unbridged().getConcrete());
}

BridgedASTType BridgedConformance::getType() const {
  return {unbridged().getConcrete()->getType().getPointer()};
}

BridgedConformance BridgedConformance::getGenericConformance() const {
  auto *specPC = swift::cast<swift::SpecializedProtocolConformance>(unbridged().getConcrete());
  return {swift::ProtocolConformanceRef(specPC->getGenericConformance())};
}

BridgedConformance BridgedConformance::getInheritedConformance() const {
  auto *inheritedConf = swift::cast<swift::InheritedProtocolConformance>(unbridged().getConcrete());
  return {swift::ProtocolConformanceRef(inheritedConf->getInheritedConformance())};
}

BridgedSubstitutionMap BridgedConformance::getSpecializedSubstitutions() const {
  auto *specPC = swift::cast<swift::SpecializedProtocolConformance>(unbridged().getConcrete());
  return {specPC->getSubstitutionMap()};
}

BridgedConformance BridgedConformanceArray::getAt(SwiftInt index) const {
  return pcArray.unbridged<swift::ProtocolConformanceRef>()[index];
}

//===----------------------------------------------------------------------===//
// MARK: BridgedLayoutConstraint
//===----------------------------------------------------------------------===//

BridgedLayoutConstraint::BridgedLayoutConstraint()
    : raw(swift::LayoutConstraint().getPointer()) {}

BridgedLayoutConstraint::BridgedLayoutConstraint(
    swift::LayoutConstraint constraint)
    : raw(constraint.getPointer()) {}

swift::LayoutConstraint BridgedLayoutConstraint::unbridged() const {
  return raw;
}

bool BridgedLayoutConstraint_isNull(BridgedLayoutConstraint cConstraint) {
  return cConstraint.unbridged().isNull();
}

bool BridgedLayoutConstraint_isKnownLayout(
    BridgedLayoutConstraint cConstraint) {
  return cConstraint.unbridged()->isKnownLayout();
}

bool BridgedLayoutConstraint_isTrivial(BridgedLayoutConstraint cConstraint) {
  return cConstraint.unbridged()->isTrivial();
}

//===----------------------------------------------------------------------===//
// MARK: Macros
//===----------------------------------------------------------------------===//

swift::MacroRole unbridge(BridgedMacroRole cRole) {
  switch (cRole) {
#define MACRO_ROLE(Name, Description)                                          \
  case BridgedMacroRole##Name:                                                 \
    return swift::MacroRole::Name;
#include "swift/Basic/MacroRoles.def"
  case BridgedMacroRoleNone:
    break;
  }
  llvm_unreachable("invalid macro role");
}

swift::MacroIntroducedDeclNameKind
unbridge(BridgedMacroIntroducedDeclNameKind kind) {
  switch (kind) {
#define CASE(ID)                                                               \
  case BridgedMacroIntroducedDeclNameKind##ID:                                 \
    return swift::MacroIntroducedDeclNameKind::ID;
    CASE(Named)
    CASE(Overloaded)
    CASE(Prefixed)
    CASE(Suffixed)
    CASE(Arbitrary)
#undef CASE
  }
}

bool BridgedMacroRole_isAttached(BridgedMacroRole role) {
  return isAttachedMacro(unbridge(role));
}

swift::MacroIntroducedDeclName
BridgedMacroIntroducedDeclName::unbridged() const {
  return swift::MacroIntroducedDeclName(unbridge(kind),
                                        name.unbridged().getFullName());
}

//===----------------------------------------------------------------------===//
// MARK: BridgedSubstitutionMap
//===----------------------------------------------------------------------===//

BridgedSubstitutionMap::BridgedSubstitutionMap(swift::SubstitutionMap map) {
  *reinterpret_cast<swift::SubstitutionMap *>(&storage) = map;
}

swift::SubstitutionMap BridgedSubstitutionMap::unbridged() const {
  return *reinterpret_cast<const swift::SubstitutionMap *>(&storage);
}

BridgedSubstitutionMap::BridgedSubstitutionMap()
  : BridgedSubstitutionMap(swift::SubstitutionMap()) {}

bool BridgedSubstitutionMap::isEmpty() const {
  return unbridged().empty();
}

bool BridgedSubstitutionMap::hasAnySubstitutableParams() const {
  return unbridged().hasAnySubstitutableParams();
}

SwiftInt BridgedSubstitutionMap::getNumConformances() const {
  return (SwiftInt)unbridged().getConformances().size();
}

BridgedConformance BridgedSubstitutionMap::getConformance(SwiftInt index) const {
  return unbridged().getConformances()[index];
}

BridgedASTTypeArray BridgedSubstitutionMap::getReplacementTypes() const {
  return {unbridged().getReplacementTypes()};
}

//===----------------------------------------------------------------------===//
// MARK: BridgedFingerprint
//===----------------------------------------------------------------------===//

swift::Fingerprint BridgedFingerprint::unbridged() const {
  return swift::Fingerprint(swift::Fingerprint::Core{this->v1, this->v2});
}

//===----------------------------------------------------------------------===//
// MARK: BridgedIfConfigClauseRangeInfo
//===----------------------------------------------------------------------===//

swift::IfConfigClauseRangeInfo BridgedIfConfigClauseRangeInfo::unbridged() const {
  swift::IfConfigClauseRangeInfo::ClauseKind clauseKind;
  switch (kind) {
  case IfConfigActive:
    clauseKind = swift::IfConfigClauseRangeInfo::ActiveClause;
    break;

  case IfConfigInactive:
    clauseKind = swift::IfConfigClauseRangeInfo::InactiveClause;
    break;

  case IfConfigEnd:
    clauseKind = swift::IfConfigClauseRangeInfo::EndDirective;
    break;
  }

  return swift::IfConfigClauseRangeInfo(directiveLoc.unbridged(),
                                        bodyLoc.unbridged(),
                                        endLoc.unbridged(),
                                        clauseKind);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedRegexLiteralPatternFeature
//===----------------------------------------------------------------------===//

BridgedRegexLiteralPatternFeatureKind::BridgedRegexLiteralPatternFeatureKind(
    SwiftInt rawValue)
    : RawValue(rawValue) {
  ASSERT(rawValue >= 0);
  ASSERT(rawValue == RawValue);
}

BridgedRegexLiteralPatternFeatureKind::BridgedRegexLiteralPatternFeatureKind(
    UnbridgedTy kind)
    : RawValue(kind.getRawValue()) {}

BridgedRegexLiteralPatternFeatureKind::UnbridgedTy
BridgedRegexLiteralPatternFeatureKind::unbridged() const {
  return UnbridgedTy(RawValue);
}

BridgedRegexLiteralPatternFeature::BridgedRegexLiteralPatternFeature(
    UnbridgedTy feature)
    : Range(feature.getRange()), Kind(feature.getKind()) {}

BridgedRegexLiteralPatternFeature::UnbridgedTy
BridgedRegexLiteralPatternFeature::unbridged() const {
  return UnbridgedTy(Kind.unbridged(), Range.unbridged());
}

BridgedRegexLiteralPatternFeatures::UnbridgedTy
BridgedRegexLiteralPatternFeatures::unbridged() const {
  return UnbridgedTy(Data, Count);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedStmtConditionElement
//===----------------------------------------------------------------------===//

BridgedStmtConditionElement::BridgedStmtConditionElement(swift::StmtConditionElement elem)
    : Raw(elem.getOpaqueValue()) {}

swift::StmtConditionElement BridgedStmtConditionElement::unbridged() const {
  return swift::StmtConditionElement::fromOpaqueValue(Raw);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedCaptureListEntry
//===----------------------------------------------------------------------===//

BridgedCaptureListEntry::BridgedCaptureListEntry(swift::CaptureListEntry CLE)
    : PBD(CLE.PBD) {}

swift::CaptureListEntry BridgedCaptureListEntry::unbridged() const {
  return swift::CaptureListEntry(PBD);
}

BridgedVarDecl BridegedCaptureListEntry_getVar(BridgedCaptureListEntry entry) {
  return entry.unbridged().getVar();
}

//===----------------------------------------------------------------------===//
// MARK: NumberLiteralExpr
//===----------------------------------------------------------------------===//

void BridgedFloatLiteralExpr_setNegative(BridgedFloatLiteralExpr cExpr,
                                         BridgedSourceLoc cLoc) {
  cExpr.unbridged()->setNegative(cLoc.unbridged());
}

void BridgedIntegerLiteralExpr_setNegative(BridgedIntegerLiteralExpr cExpr,
                                           BridgedSourceLoc cLoc) {
  cExpr.unbridged()->setNegative(cLoc.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: BridgedTypeOrCustomAttr
//===----------------------------------------------------------------------===//

BridgedTypeOrCustomAttr::BridgedTypeOrCustomAttr(void *_Nonnull pointer,
                                                 Kind kind)
    : opaque(intptr_t(pointer) | kind) {
  assert(getPointer() == pointer && getKind() == kind);
}

BridgedTypeAttribute BridgedTypeOrCustomAttr::castToTypeAttr() const {
  assert(getKind() == Kind::TypeAttr);
  return static_cast<swift::TypeAttribute *>(getPointer());
}

BridgedCustomAttr BridgedTypeOrCustomAttr::castToCustomAttr() const {
  assert(getKind() == Kind::CustomAttr);
  return static_cast<swift::CustomAttr *>(getPointer());
}

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_AST_ASTBRIDGINGIMPL_H
