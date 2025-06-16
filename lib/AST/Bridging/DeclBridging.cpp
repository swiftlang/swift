//===--- Bridging/DeclBridging.cpp ----------------------------------------===//
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
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ParseRequests.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: DeclName
//===----------------------------------------------------------------------===//

BridgedDeclBaseName BridgedDeclBaseName_createConstructor() {
  return DeclBaseName::createConstructor();
}

BridgedDeclBaseName BridgedDeclBaseName_createDestructor() {
  return DeclBaseName::createDestructor();
}

BridgedDeclBaseName BridgedDeclBaseName_createSubscript() {
  return DeclBaseName::createSubscript();
}

BridgedDeclBaseName
BridgedDeclBaseName_createIdentifier(BridgedIdentifier identifier) {
  return DeclBaseName(identifier.unbridged());
}

BridgedDeclNameRef
BridgedDeclNameRef_createParsed(BridgedASTContext cContext,
                                BridgedDeclBaseName cBaseName,
                                BridgedArrayRef cLabels) {
  ASTContext &context = cContext.unbridged();
  SmallVector<Identifier, 4> labels;
  for (auto &cLabel : cLabels.unbridged<BridgedIdentifier>()) {
    labels.push_back(cLabel.unbridged());
  }
  return DeclNameRef(DeclName(context, cBaseName.unbridged(), labels));
}

BridgedDeclNameRef
BridgedDeclNameRef_createParsed(BridgedDeclBaseName cBaseName) {
  return DeclNameRef(cBaseName.unbridged());
}

BridgedDeclNameLoc BridgedDeclNameLoc_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cBaseNameLoc,
    BridgedSourceLoc cLParenLoc, BridgedArrayRef cLabelLocs,
    BridgedSourceLoc cRParenLoc) {
  return BridgedDeclNameLoc_createParsed(
       cContext, BridgedSourceLoc(), cBaseNameLoc, cLParenLoc, cLabelLocs,
       cRParenLoc);
}

BridgedDeclNameLoc BridgedDeclNameLoc_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cModuleSelectorLoc,
    BridgedSourceLoc cBaseNameLoc, BridgedSourceLoc cLParenLoc,
    BridgedArrayRef cLabelLocs, BridgedSourceLoc cRParenLoc) {

  ASTContext &context = cContext.unbridged();
  SmallVector<SourceLoc, 4> labelLocs;
  for (auto &cLabelLoc : cLabelLocs.unbridged<BridgedSourceLoc>())
    labelLocs.push_back(cLabelLoc.unbridged());

  return DeclNameLoc(context, cModuleSelectorLoc.unbridged(),
                     cBaseNameLoc.unbridged(), cLParenLoc.unbridged(),
                     labelLocs, cRParenLoc.unbridged());
}

BridgedDeclNameLoc
BridgedDeclNameLoc_createParsed(BridgedSourceLoc cBaseNameLoc) {
  return DeclNameLoc(cBaseNameLoc.unbridged());
}

BridgedDeclNameLoc
BridgedDeclNameLoc_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cModuleSelectorLoc,
    BridgedSourceLoc cBaseNameLoc) {
  return DeclNameLoc(cContext.unbridged(), cModuleSelectorLoc.unbridged(),
                     cBaseNameLoc.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: Decls
//===----------------------------------------------------------------------===//

// Define `.asDecl` on each BridgedXXXDecl type.
#define DECL(Id, Parent)                                                       \
  BridgedDecl Bridged##Id##Decl_asDecl(Bridged##Id##Decl decl) {               \
    return static_cast<Decl *>(decl.unbridged());                              \
  }
#define ABSTRACT_DECL(Id, Parent) DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

// Define `.asDeclContext` on each BridgedXXXDecl type that's also a
// DeclContext.
#define DECL(Id, Parent)
#define CONTEXT_DECL(Id, Parent)                                               \
  BridgedDeclContext Bridged##Id##Decl_asDeclContext(Bridged##Id##Decl decl) { \
    return static_cast<DeclContext *>(decl.unbridged());                       \
  }
#define ABSTRACT_CONTEXT_DECL(Id, Parent) CONTEXT_DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

static StaticSpellingKind unbridged(BridgedStaticSpelling kind) {
  return static_cast<StaticSpellingKind>(kind);
}

void BridgedDecl_attachParsedAttrs(BridgedDecl decl,
                                   BridgedDeclAttributes attrs) {
  decl.unbridged()->attachParsedAttrs(attrs.unbridged());
}

void BridgedDecl_forEachDeclToHoist(BridgedDecl cDecl,
                                    BridgedSwiftClosure closure) {
  cDecl.unbridged()->forEachDeclToHoist([&](Decl *D) {
    BridgedDecl bridged(D);
    closure(&bridged);
  });
}

BridgedAccessorDecl BridgedAccessorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    swift::AccessorKind Kind, BridgedAbstractStorageDecl cStorage,
    BridgedSourceLoc cDeclLoc, BridgedSourceLoc cAccessorKeywordLoc,
    BridgedNullableParameterList cParamList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr cThrownType) {
  return AccessorDecl::createParsed(
      cContext.unbridged(), Kind, cStorage.unbridged(), cDeclLoc.unbridged(),
      cAccessorKeywordLoc.unbridged(), cParamList.unbridged(),
      cAsyncLoc.unbridged(), cThrowsLoc.unbridged(), cThrownType.unbridged(),
      cDeclContext.unbridged());
}

static VarDecl::Introducer unbridged(BridgedVarDeclIntroducer introducer) {
  switch (introducer) {
  case BridgedVarDeclIntroducerLet:
    return swift::VarDecl::Introducer::Let;
  case BridgedVarDeclIntroducerVar:
    return swift::VarDecl::Introducer::Var;
  case BridgedVarDeclIntroducerInOut:
    return swift::VarDecl::Introducer::InOut;
  case BridgedVarDeclIntroducerBorrowing:
    return swift::VarDecl::Introducer::Borrowing;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedPatternBindingDecl BridgedPatternBindingDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedDeclAttributes cAttrs, BridgedSourceLoc cStaticLoc,
    BridgedStaticSpelling cStaticSpelling, BridgedSourceLoc cIntroducerLoc,
    BridgedVarDeclIntroducer cIntroducer, BridgedArrayRef cBindingEntries) {

  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();

  auto introducer = unbridged(cIntroducer);
  auto introducerLoc = cIntroducerLoc.unbridged();
  auto staticSpelling = unbridged(cStaticSpelling);
  auto staticLoc = cStaticLoc.unbridged();

  SmallVector<PatternBindingEntry, 4> entries;
  for (auto &entry : cBindingEntries.unbridged<BridgedPatternBindingEntry>()) {
    auto *pattern = entry.pattern.unbridged();

    // Configure all vars.
    pattern->forEachVariable([&](VarDecl *VD) {
      VD->attachParsedAttrs(cAttrs.unbridged());
      VD->setStatic(staticLoc.isValid());
      VD->setIntroducer(introducer);
      VD->setTopLevelGlobal(isa<TopLevelCodeDecl>(declContext));
    });

    entries.emplace_back(pattern, entry.equalLoc.unbridged(),
                         entry.init.unbridged(), entry.initContext.unbridged());
  }

  return PatternBindingDecl::create(context, staticLoc, staticSpelling,
                                    introducerLoc, entries, declContext);
}

BridgedParamDecl BridgedParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cSpecifierLoc, BridgedIdentifier cArgName,
    BridgedSourceLoc cArgNameLoc, BridgedIdentifier cParamName,
    BridgedSourceLoc cParamNameLoc, BridgedNullableExpr cDefaultArgument,
    BridgedNullableDefaultArgumentInitializer cDefaultArgumentInitContext) {
  return ParamDecl::createParsed(
      cContext.unbridged(), cSpecifierLoc.unbridged(), cArgNameLoc.unbridged(),
      cArgName.unbridged(), cParamNameLoc.unbridged(), cParamName.unbridged(),
      cDefaultArgument.unbridged(), cDefaultArgumentInitContext.unbridged(),
      cDeclContext.unbridged());
}

void BridgedConstructorDecl_setParsedBody(BridgedConstructorDecl decl,
                                          BridgedBraceStmt body) {
  decl.unbridged()->setBody(body.unbridged(), FuncDecl::BodyKind::Parsed);
}

void BridgedFuncDecl_setParsedBody(BridgedFuncDecl decl,
                                   BridgedBraceStmt body) {
  decl.unbridged()->setBody(body.unbridged(), FuncDecl::BodyKind::Parsed);
}

void BridgedDestructorDecl_setParsedBody(BridgedDestructorDecl decl,
                                         BridgedBraceStmt body) {
  decl.unbridged()->setBody(body.unbridged(), FuncDecl::BodyKind::Parsed);
}

BridgedFuncDecl BridgedFuncDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStaticLoc, BridgedStaticSpelling cStaticSpelling,
    BridgedSourceLoc cFuncKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedParameterList parameterList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr thrownType,
    BridgedNullableTypeRepr returnType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  ASTContext &context = cContext.unbridged();

  auto *paramList = parameterList.unbridged();
  auto declName = DeclName(context, cName.unbridged(), paramList);
  auto asyncLoc = cAsyncLoc.unbridged();
  auto throwsLoc = cThrowsLoc.unbridged();
  // FIXME: rethrows

  auto *decl = FuncDecl::create(
      context, cStaticLoc.unbridged(), unbridged(cStaticSpelling),
      cFuncKeywordLoc.unbridged(), declName, cNameLoc.unbridged(),
      asyncLoc.isValid(), asyncLoc, throwsLoc.isValid(), throwsLoc,
      thrownType.unbridged(), genericParamList.unbridged(), paramList,
      returnType.unbridged(), cDeclContext.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());

  return decl;
}

BridgedConstructorDecl BridgedConstructorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cInitKeywordLoc, BridgedSourceLoc cFailabilityMarkLoc,
    bool isIUO, BridgedNullableGenericParamList genericParams,
    BridgedParameterList bridgedParameterList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr thrownType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  assert(cFailabilityMarkLoc.unbridged().isValid() || !isIUO);

  ASTContext &context = cContext.unbridged();

  auto *parameterList = bridgedParameterList.unbridged();
  auto declName =
      DeclName(context, DeclBaseName::createConstructor(), parameterList);
  auto asyncLoc = cAsyncLoc.unbridged();
  auto throwsLoc = cThrowsLoc.unbridged();
  auto failabilityMarkLoc = cFailabilityMarkLoc.unbridged();
  // FIXME: rethrows
  auto *decl = new (context) ConstructorDecl(
      declName, cInitKeywordLoc.unbridged(), failabilityMarkLoc.isValid(),
      failabilityMarkLoc, asyncLoc.isValid(), asyncLoc, throwsLoc.isValid(),
      throwsLoc, thrownType.unbridged(), parameterList,
      genericParams.unbridged(), cDeclContext.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());
  decl->setImplicitlyUnwrappedOptional(isIUO);

  return decl;
}

BridgedDestructorDecl
BridgedDestructorDecl_createParsed(BridgedASTContext cContext,
                                   BridgedDeclContext cDeclContext,
                                   BridgedSourceLoc cDeinitKeywordLoc) {
  ASTContext &context = cContext.unbridged();
  auto *decl = new (context)
      DestructorDecl(cDeinitKeywordLoc.unbridged(), cDeclContext.unbridged());

  return decl;
}

BridgedMacroDecl BridgedMacroDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cMacroLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList cGenericParams,
    BridgedParameterList cParams, BridgedSourceLoc cArrowLoc,
    BridgedNullableTypeRepr cResultType, BridgedNullableExpr cDefinition,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  ASTContext &context = cContext.unbridged();
  auto *params = cParams.unbridged();
  DeclName fullName = DeclName(context, cName.unbridged(), params);
  auto *decl = new (context)
      MacroDecl(cMacroLoc.unbridged(), fullName, cNameLoc.unbridged(),
                cGenericParams.unbridged(), params, cArrowLoc.unbridged(),
                cResultType.unbridged(), cDefinition.unbridged(),
                cDeclContext.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());
  return decl;
}

BridgedTypeAliasDecl BridgedTypeAliasDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cAliasKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedSourceLoc cEqualLoc, BridgedTypeRepr opaqueUnderlyingType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  ASTContext &context = cContext.unbridged();

  auto *decl = new (context)
      TypeAliasDecl(cAliasKeywordLoc.unbridged(), cEqualLoc.unbridged(),
                    cName.unbridged(), cNameLoc.unbridged(),
                    genericParamList.unbridged(), cDeclContext.unbridged());
  decl->setUnderlyingTypeRepr(opaqueUnderlyingType.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());

  return decl;
}

static void setParsedMembers(IterableDeclContext *IDC, BridgedArrayRef cMembers,
                             BridgedFingerprint cFingerprint) {
  auto &ctx = IDC->getDecl()->getASTContext();

  Fingerprint fp = cFingerprint.unbridged();

  ArrayRef<Decl *> members =
      ctx.AllocateTransform<Decl *>(cMembers.unbridged<BridgedDecl>(),
                                    [](auto decl) { return decl.unbridged(); });

  IDC->setMaybeHasOperatorDeclarations();
  IDC->setMaybeHasNestedClassDeclarations();
  // FIXME: Split requests. e.g. DeclMembersFingerprintRequest.
  ctx.evaluator.cacheOutput(ParseMembersRequest{IDC},
                            FingerprintAndMembers{fp, members});
}

void BridgedNominalTypeDecl_setParsedMembers(BridgedNominalTypeDecl cDecl,
                                             BridgedArrayRef cMembers,
                                             BridgedFingerprint cFingerprint) {
  setParsedMembers(cDecl.unbridged(), cMembers, cFingerprint);
}

void BridgedExtensionDecl_setParsedMembers(BridgedExtensionDecl cDecl,
                                           BridgedArrayRef cMembers,
                                           BridgedFingerprint cFingerprint) {
  setParsedMembers(cDecl.unbridged(), cMembers, cFingerprint);
}

static ArrayRef<InheritedEntry>
convertToInheritedEntries(ASTContext &ctx, BridgedArrayRef cInheritedTypes) {
  return ctx.AllocateTransform<InheritedEntry>(
      cInheritedTypes.unbridged<BridgedTypeRepr>(),
      [](auto &e) { return InheritedEntry(e.unbridged()); });
}

BridgedNominalTypeDecl BridgedEnumDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cEnumKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange) {
  ASTContext &context = cContext.unbridged();

  NominalTypeDecl *decl = new (context) EnumDecl(
      cEnumKeywordLoc.unbridged(), cName.unbridged(), cNameLoc.unbridged(),
      convertToInheritedEntries(context, cInheritedTypes),
      genericParamList.unbridged(), cDeclContext.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());
  decl->setBraces(cBraceRange.unbridged());

  return decl;
}

BridgedEnumCaseDecl
BridgedEnumCaseDecl_createParsed(BridgedDeclContext cDeclContext,
                                 BridgedSourceLoc cCaseKeywordLoc,
                                 BridgedArrayRef cElements) {
  return EnumCaseDecl::create(cCaseKeywordLoc.unbridged(),
                              cElements.unbridged<EnumElementDecl *>(),
                              cDeclContext.unbridged());
}

BridgedEnumElementDecl BridgedEnumElementDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedNullableParameterList bridgedParameterList,
    BridgedSourceLoc cEqualsLoc, BridgedNullableExpr rawValue) {
  ASTContext &context = cContext.unbridged();

  auto *parameterList = bridgedParameterList.unbridged();
  DeclName declName;
  {
    auto identifier = cName.unbridged();
    if (parameterList) {
      declName = DeclName(context, identifier, parameterList);
    } else {
      declName = identifier;
    }
  }

  return new (context) EnumElementDecl(
      cNameLoc.unbridged(), declName, parameterList, cEqualsLoc.unbridged(),
      cast_or_null<LiteralExpr>(rawValue.unbridged()),
      cDeclContext.unbridged());
}

BridgedNominalTypeDecl BridgedStructDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStructKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange) {
  ASTContext &context = cContext.unbridged();

  NominalTypeDecl *decl = new (context) StructDecl(
      cStructKeywordLoc.unbridged(), cName.unbridged(), cNameLoc.unbridged(),
      convertToInheritedEntries(context, cInheritedTypes),
      genericParamList.unbridged(), cDeclContext.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());
  decl->setBraces(cBraceRange.unbridged());

  return decl;
}

BridgedNominalTypeDecl BridgedClassDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cClassKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange, bool isActor) {
  ASTContext &context = cContext.unbridged();

  NominalTypeDecl *decl = new (context) ClassDecl(
      cClassKeywordLoc.unbridged(), cName.unbridged(), cNameLoc.unbridged(),
      convertToInheritedEntries(context, cInheritedTypes),
      genericParamList.unbridged(), cDeclContext.unbridged(), isActor);
  decl->setTrailingWhereClause(genericWhereClause.unbridged());
  decl->setBraces(cBraceRange.unbridged());

  return decl;
}

BridgedNominalTypeDecl BridgedProtocolDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cProtocolKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedArrayRef cPrimaryAssociatedTypeNames,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange) {
  ASTContext &context = cContext.unbridged();

  auto primaryAssociatedTypeNames =
      context.AllocateTransform<PrimaryAssociatedTypeName>(
          cPrimaryAssociatedTypeNames.unbridged<BridgedLocatedIdentifier>(),
          [](auto &e) -> PrimaryAssociatedTypeName {
            return {e.Name.unbridged(), e.NameLoc.unbridged()};
          });

  NominalTypeDecl *decl = new (context) ProtocolDecl(
      cDeclContext.unbridged(), cProtocolKeywordLoc.unbridged(),
      cNameLoc.unbridged(), cName.unbridged(), primaryAssociatedTypeNames,
      convertToInheritedEntries(context, cInheritedTypes),
      genericWhereClause.unbridged());
  decl->setBraces(cBraceRange.unbridged());

  return decl;
}

BridgedAssociatedTypeDecl BridgedAssociatedTypeDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cAssociatedtypeKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedArrayRef cInheritedTypes,
    BridgedNullableTypeRepr defaultType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  ASTContext &context = cContext.unbridged();

  auto *decl = AssociatedTypeDecl::createParsed(
      context, cDeclContext.unbridged(), cAssociatedtypeKeywordLoc.unbridged(),
      cName.unbridged(), cNameLoc.unbridged(), defaultType.unbridged(),
      genericWhereClause.unbridged());
  decl->setInherited(convertToInheritedEntries(context, cInheritedTypes));

  return decl;
}

BridgedExtensionDecl BridgedExtensionDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cExtensionKeywordLoc, BridgedTypeRepr extendedType,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange) {
  ASTContext &context = cContext.unbridged();

  auto *decl = ExtensionDecl::create(
      context, cExtensionKeywordLoc.unbridged(), extendedType.unbridged(),
      convertToInheritedEntries(context, cInheritedTypes),
      cDeclContext.unbridged(), genericWhereClause.unbridged());
  decl->setBraces(cBraceRange.unbridged());
  return decl;
}

BridgedMacroExpansionDecl BridgedMacroExpansionDecl_createParsed(
    BridgedDeclContext cDeclContext, BridgedSourceLoc cPoundLoc,
    BridgedDeclNameRef cMacroNameRef, BridgedDeclNameLoc cMacroNameLoc,
    BridgedSourceLoc cLeftAngleLoc, BridgedArrayRef cGenericArgs,
    BridgedSourceLoc cRightAngleLoc, BridgedNullableArgumentList cArgList) {
  auto *DC = cDeclContext.unbridged();
  auto &Context = DC->getASTContext();
  return MacroExpansionDecl::create(
      cDeclContext.unbridged(), cPoundLoc.unbridged(),
      cMacroNameRef.unbridged(), cMacroNameLoc.unbridged(),
      cLeftAngleLoc.unbridged(),
      Context.AllocateCopy(cGenericArgs.unbridged<TypeRepr *>()),
      cRightAngleLoc.unbridged(), cArgList.unbridged());
}

BridgedMissingDecl BridgedMissingDecl_create(BridgedASTContext cContext,
                                             BridgedDeclContext cDeclContext,
                                             BridgedSourceLoc cLoc) {
  return MissingDecl::create(cContext.unbridged(), cDeclContext.unbridged(),
                             cLoc.unbridged());
}

BridgedOperatorDecl BridgedOperatorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedOperatorFixity cFixity, BridgedSourceLoc cOperatorKeywordLoc,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedSourceLoc cColonLoc, BridgedIdentifier cPrecedenceGroupName,
    BridgedSourceLoc cPrecedenceGroupLoc) {
  auto colonLoc = cColonLoc.unbridged();
  assert(colonLoc.isValid() == cPrecedenceGroupName.unbridged().nonempty());
  assert(colonLoc.isValid() == cPrecedenceGroupLoc.unbridged().isValid());

  ASTContext &context = cContext.unbridged();
  auto operatorKeywordLoc = cOperatorKeywordLoc.unbridged();
  auto name = cName.unbridged();
  auto nameLoc = cNameLoc.unbridged();
  auto *declContext = cDeclContext.unbridged();

  OperatorDecl *decl = nullptr;
  switch (cFixity) {
  case BridgedOperatorFixityInfix:
    decl = new (context) InfixOperatorDecl(
        declContext, operatorKeywordLoc, name, nameLoc, cColonLoc.unbridged(),
        cPrecedenceGroupName.unbridged(), cPrecedenceGroupLoc.unbridged());
    break;
  case BridgedOperatorFixityPrefix:
    assert(colonLoc.isInvalid());
    decl = new (context)
        PrefixOperatorDecl(declContext, operatorKeywordLoc, name, nameLoc);
    break;
  case BridgedOperatorFixityPostfix:
    assert(colonLoc.isInvalid());
    decl = new (context)
        PostfixOperatorDecl(declContext, operatorKeywordLoc, name, nameLoc);
    break;
  }

  return decl;
}

BridgedPrecedenceGroupDecl BridgedPrecedenceGroupDecl_createParsed(
    BridgedDeclContext cDeclContext,
    BridgedSourceLoc cPrecedencegroupKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedSourceLoc cLeftBraceLoc,
    BridgedSourceLoc cAssociativityKeywordLoc,
    BridgedSourceLoc cAssociativityValueLoc, swift::Associativity associativity,
    BridgedSourceLoc cAssignmentKeywordLoc,
    BridgedSourceLoc cAssignmentValueLoc, bool isAssignment,
    BridgedSourceLoc cHigherThanKeywordLoc, BridgedArrayRef cHigherThanNames,
    BridgedSourceLoc cLowerThanKeywordLoc, BridgedArrayRef cLowerThanNames,
    BridgedSourceLoc cRightBraceLoc) {

  SmallVector<PrecedenceGroupDecl::Relation, 2> higherThanNames;
  for (auto &pair : cHigherThanNames.unbridged<BridgedLocatedIdentifier>()) {
    higherThanNames.push_back(
        {pair.NameLoc.unbridged(), pair.Name.unbridged(), nullptr});
  }

  SmallVector<PrecedenceGroupDecl::Relation, 2> lowerThanNames;
  for (auto &pair : cLowerThanNames.unbridged<BridgedLocatedIdentifier>()) {
    lowerThanNames.push_back(
        {pair.NameLoc.unbridged(), pair.Name.unbridged(), nullptr});
  }

  return PrecedenceGroupDecl::create(
      cDeclContext.unbridged(), cPrecedencegroupKeywordLoc.unbridged(),
      cNameLoc.unbridged(), cName.unbridged(), cLeftBraceLoc.unbridged(),
      cAssociativityKeywordLoc.unbridged(), cAssociativityValueLoc.unbridged(),
      associativity, cAssignmentKeywordLoc.unbridged(),
      cAssignmentValueLoc.unbridged(), isAssignment,
      cHigherThanKeywordLoc.unbridged(), higherThanNames,
      cLowerThanKeywordLoc.unbridged(), lowerThanNames,
      cRightBraceLoc.unbridged());
}

BridgedImportDecl BridgedImportDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cImportKeywordLoc, BridgedImportKind cImportKind,
    BridgedSourceLoc cImportKindLoc, BridgedArrayRef cImportPathElements) {
  ImportPath::Builder builder;
  for (auto &element :
       cImportPathElements.unbridged<BridgedLocatedIdentifier>()) {
    builder.push_back(element.Name.unbridged(), element.NameLoc.unbridged());
  }

  ASTContext &context = cContext.unbridged();
  return ImportDecl::create(
      context, cDeclContext.unbridged(), cImportKeywordLoc.unbridged(),
      static_cast<ImportKind>(cImportKind), cImportKindLoc.unbridged(),
      std::move(builder).get());
}

BridgedUsingDecl BridgedUsingDecl_createParsed(BridgedASTContext cContext,
                                               BridgedDeclContext cDeclContext,
                                               BridgedSourceLoc usingKeywordLoc,
                                               BridgedSourceLoc specifierLoc,
                                               BridgedUsingSpecifier specifier) {
  ASTContext &ctx = cContext.unbridged();
  return UsingDecl::create(
      ctx, usingKeywordLoc.unbridged(), specifierLoc.unbridged(),
      static_cast<UsingSpecifier>(specifier), cDeclContext.unbridged());
}

BridgedSubscriptDecl BridgedSubscriptDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStaticLoc, BridgedStaticSpelling cStaticSpelling,
    BridgedSourceLoc cSubscriptKeywordLoc,
    BridgedNullableGenericParamList cGenericParamList,
    BridgedParameterList cParamList, BridgedSourceLoc cArrowLoc,
    BridgedTypeRepr returnType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  auto *decl = SubscriptDecl::createParsed(
      cContext.unbridged(), cStaticLoc.unbridged(), unbridged(cStaticSpelling),
      cSubscriptKeywordLoc.unbridged(), cParamList.unbridged(),
      cArrowLoc.unbridged(), returnType.unbridged(), cDeclContext.unbridged(),
      cGenericParamList.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());
  return decl;
}

BridgedTopLevelCodeDecl
BridgedTopLevelCodeDecl_create(BridgedASTContext cContext,
                               BridgedDeclContext cDeclContext) {
  return new (cContext.unbridged()) TopLevelCodeDecl(cDeclContext.unbridged());
}

void BridgedTopLevelCodeDecl_setBody(BridgedTopLevelCodeDecl cDecl,
                                     BridgedBraceStmt cBody) {
  cDecl.unbridged()->setBody(cBody.unbridged());
}

BridgedVarDecl BridgedVarDec_createImplicitStringInterpolationVar(
    BridgedDeclContext cDeclContext) {
  return VarDecl::createImplicitStringInterpolationVar(
      cDeclContext.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: AbstractStorageDecl
//===----------------------------------------------------------------------===//

void BridgedAbstractStorageDecl_setAccessors(
    BridgedAbstractStorageDecl cStorage, BridgedAccessorRecord accessors) {
  cStorage.unbridged()->setAccessors(
      accessors.lBraceLoc.unbridged(),
      accessors.accessors.unbridged<AccessorDecl *>(),
      accessors.rBraceLoc.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: AccessorDecl
//===----------------------------------------------------------------------===//

void BridgedAccessorDecl_setParsedBody(BridgedAccessorDecl decl,
                                       BridgedBraceStmt body) {
  decl.unbridged()->setBody(body.unbridged(),
                            AbstractFunctionDecl::BodyKind::Parsed);
}

//===----------------------------------------------------------------------===//
// MARK: BridgedNominalTypeDecl
//===----------------------------------------------------------------------===//

bool BridgedNominalTypeDecl_isStructWithUnreferenceableStorage(
    BridgedNominalTypeDecl decl) {
  if (auto *structDecl = dyn_cast<swift::StructDecl>(decl.unbridged())) {
    return structDecl->hasUnreferenceableStorage();
  }
  return false;
}

//===----------------------------------------------------------------------===//
// MARK: BridgedParameterList
//===----------------------------------------------------------------------===//

BridgedParameterList BridgedParameterList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftParenLoc,
    BridgedArrayRef cParameters, BridgedSourceLoc cRightParenLoc) {
  ASTContext &context = cContext.unbridged();
  return ParameterList::create(context, cLeftParenLoc.unbridged(),
                               cParameters.unbridged<ParamDecl *>(),
                               cRightParenLoc.unbridged());
}

size_t BridgedParameterList_size(BridgedParameterList cParameterList) {
  return cParameterList.unbridged()->size();
}

BridgedParamDecl BridgedParameterList_get(BridgedParameterList cParameterList,
                                          size_t i) {
  return cParameterList.unbridged()->get(i);
}
