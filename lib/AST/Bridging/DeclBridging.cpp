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

BridgedDeclNameRef BridgedDeclNameRef_createParsed(BridgedASTContext cContext,
                                                   DeclBaseName baseName,
                                                   BridgedArrayRef cLabels) {
  ASTContext &context = cContext.unbridged();
  auto labels = cLabels.unbridged<swift::Identifier>();

  return DeclNameRef(DeclName(context, baseName, labels));
}

BridgedDeclNameRef BridgedDeclNameRef_createParsed(DeclBaseName baseName) {
  return DeclNameRef(baseName);
}

BridgedDeclNameLoc BridgedDeclNameLoc_createParsed(BridgedASTContext cContext,
                                                   SourceLoc baseNameLoc,
                                                   SourceLoc lParenLoc,
                                                   BridgedArrayRef cLabelLocs,
                                                   SourceLoc rParenLoc) {
  return BridgedDeclNameLoc_createParsed(cContext, SourceLoc(), baseNameLoc,
                                         lParenLoc, cLabelLocs, rParenLoc);
}

BridgedDeclNameLoc BridgedDeclNameLoc_createParsed(BridgedASTContext cContext,
                                                   SourceLoc moduleSelectorLoc,
                                                   SourceLoc baseNameLoc,
                                                   SourceLoc lParenLoc,
                                                   BridgedArrayRef cLabelLocs,
                                                   SourceLoc rParenLoc) {

  ASTContext &context = cContext.unbridged();
  auto labelLocs = cLabelLocs.unbridged<SourceLoc>();

  return DeclNameLoc(context, moduleSelectorLoc, baseNameLoc, lParenLoc,
                     labelLocs, rParenLoc);
}

BridgedDeclNameLoc BridgedDeclNameLoc_createParsed(SourceLoc baseNameLoc) {
  return DeclNameLoc(baseNameLoc);
}

BridgedDeclNameLoc BridgedDeclNameLoc_createParsed(BridgedASTContext cContext,
                                                   SourceLoc moduleSelectorLoc,
                                                   SourceLoc baseNameLoc) {
  return DeclNameLoc(cContext.unbridged(), moduleSelectorLoc, baseNameLoc);
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
    SourceLoc declLoc, SourceLoc accessorKeywordLoc,
    BridgedNullableParameterList cParamList, SourceLoc asyncLoc,
    SourceLoc throwsLoc, BridgedNullableTypeRepr cThrownType) {
  return AccessorDecl::createParsed(
      cContext.unbridged(), Kind, cStorage.unbridged(), declLoc,
      accessorKeywordLoc, cParamList.unbridged(), asyncLoc, throwsLoc,
      cThrownType.unbridged(), cDeclContext.unbridged());
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
    BridgedDeclAttributes cAttrs, SourceLoc staticLoc,
    BridgedStaticSpelling cStaticSpelling, SourceLoc introducerLoc,
    BridgedVarDeclIntroducer cIntroducer, BridgedArrayRef cBindingEntries) {

  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();

  auto introducer = unbridged(cIntroducer);
  auto staticSpelling = unbridged(cStaticSpelling);

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

    entries.emplace_back(pattern, entry.equalLoc, entry.init.unbridged(),
                         entry.initContext.unbridged());
  }

  return PatternBindingDecl::create(context, staticLoc, staticSpelling,
                                    introducerLoc, entries, declContext);
}

BridgedParamDecl BridgedParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc specifierLoc, swift::Identifier argName, SourceLoc argNameLoc,
    swift::Identifier paramName, SourceLoc paramNameLoc,
    BridgedNullableExpr cDefaultArgument,
    BridgedNullableDefaultArgumentInitializer cDefaultArgumentInitContext) {
  return ParamDecl::createParsed(
      cContext.unbridged(), specifierLoc, argNameLoc, argName, paramNameLoc,
      paramName, cDefaultArgument.unbridged(),
      cDefaultArgumentInitContext.unbridged(), cDeclContext.unbridged());
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
    SourceLoc staticLoc, BridgedStaticSpelling cStaticSpelling,
    SourceLoc funcKeywordLoc, swift::Identifier name, SourceLoc nameLoc,
    BridgedNullableGenericParamList genericParamList,
    BridgedParameterList parameterList, SourceLoc asyncLoc, SourceLoc throwsLoc,
    BridgedNullableTypeRepr thrownType, BridgedNullableTypeRepr returnType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  ASTContext &context = cContext.unbridged();

  auto *paramList = parameterList.unbridged();
  auto declName = DeclName(context, name, paramList);
  // FIXME: rethrows

  auto *decl = FuncDecl::create(
      context, staticLoc, unbridged(cStaticSpelling), funcKeywordLoc, declName,
      nameLoc, asyncLoc.isValid(), asyncLoc, throwsLoc.isValid(), throwsLoc,
      thrownType.unbridged(), genericParamList.unbridged(), paramList,
      returnType.unbridged(), cDeclContext.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());

  return decl;
}

BridgedConstructorDecl BridgedConstructorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc initKeywordLoc, SourceLoc failabilityMarkLoc, bool isIUO,
    BridgedNullableGenericParamList genericParams,
    BridgedParameterList bridgedParameterList, SourceLoc asyncLoc,
    SourceLoc throwsLoc, BridgedNullableTypeRepr thrownType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  assert(failabilityMarkLoc.isValid() || !isIUO);

  ASTContext &context = cContext.unbridged();

  auto *parameterList = bridgedParameterList.unbridged();
  auto declName =
      DeclName(context, DeclBaseName::createConstructor(), parameterList);
  // FIXME: rethrows
  auto *decl = new (context) ConstructorDecl(
      declName, initKeywordLoc, failabilityMarkLoc.isValid(),
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
                                   SourceLoc deinitKeywordLoc) {
  ASTContext &context = cContext.unbridged();
  auto *decl =
      new (context) DestructorDecl(deinitKeywordLoc, cDeclContext.unbridged());

  return decl;
}

BridgedMacroDecl BridgedMacroDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc macroLoc, swift::Identifier name, SourceLoc nameLoc,
    BridgedNullableGenericParamList cGenericParams,
    BridgedParameterList cParams, SourceLoc arrowLoc,
    BridgedNullableTypeRepr cResultType, BridgedNullableExpr cDefinition,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  ASTContext &context = cContext.unbridged();
  auto *params = cParams.unbridged();
  DeclName fullName = DeclName(context, name, params);
  auto *decl = new (context)
      MacroDecl(macroLoc, fullName, nameLoc, cGenericParams.unbridged(), params,
                arrowLoc, cResultType.unbridged(), cDefinition.unbridged(),
                cDeclContext.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());
  return decl;
}

BridgedTypeAliasDecl BridgedTypeAliasDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc aliasKeywordLoc, swift::Identifier name, SourceLoc nameLoc,
    BridgedNullableGenericParamList genericParamList, SourceLoc equalLoc,
    BridgedTypeRepr opaqueUnderlyingType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  ASTContext &context = cContext.unbridged();

  auto *decl = new (context)
      TypeAliasDecl(aliasKeywordLoc, equalLoc, name, nameLoc,
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
    SourceLoc enumKeywordLoc, swift::Identifier name, SourceLoc nameLoc,
    BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    SourceRange braceRange) {
  ASTContext &context = cContext.unbridged();

  NominalTypeDecl *decl = new (context)
      EnumDecl(enumKeywordLoc, name, nameLoc,
               convertToInheritedEntries(context, cInheritedTypes),
               genericParamList.unbridged(), cDeclContext.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());
  decl->setBraces(braceRange);

  return decl;
}

BridgedEnumCaseDecl
BridgedEnumCaseDecl_createParsed(BridgedDeclContext cDeclContext,
                                 SourceLoc caseKeywordLoc,
                                 BridgedArrayRef cElements) {
  return EnumCaseDecl::create(caseKeywordLoc,
                              cElements.unbridged<EnumElementDecl *>(),
                              cDeclContext.unbridged());
}

BridgedEnumElementDecl BridgedEnumElementDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    swift::Identifier name, SourceLoc nameLoc,
    BridgedNullableParameterList bridgedParameterList, SourceLoc equalsLoc,
    BridgedNullableExpr rawValue) {
  ASTContext &context = cContext.unbridged();

  auto *parameterList = bridgedParameterList.unbridged();
  DeclName declName;
  {
    if (parameterList) {
      declName = DeclName(context, name, parameterList);
    } else {
      declName = name;
    }
  }

  return new (context)
      EnumElementDecl(nameLoc, declName, parameterList, equalsLoc,
                      cast_or_null<LiteralExpr>(rawValue.unbridged()),
                      cDeclContext.unbridged());
}

BridgedNominalTypeDecl BridgedStructDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc structKeywordLoc, swift::Identifier name, SourceLoc nameLoc,
    BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    SourceRange braceRange) {
  ASTContext &context = cContext.unbridged();

  NominalTypeDecl *decl = new (context)
      StructDecl(structKeywordLoc, name, nameLoc,
                 convertToInheritedEntries(context, cInheritedTypes),
                 genericParamList.unbridged(), cDeclContext.unbridged());
  decl->setTrailingWhereClause(genericWhereClause.unbridged());
  decl->setBraces(braceRange);

  return decl;
}

BridgedNominalTypeDecl BridgedClassDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc classKeywordLoc, swift::Identifier name, SourceLoc nameLoc,
    BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    SourceRange braceRange, bool isActor) {
  ASTContext &context = cContext.unbridged();

  NominalTypeDecl *decl = new (context) ClassDecl(
      classKeywordLoc, name, nameLoc,
      convertToInheritedEntries(context, cInheritedTypes),
      genericParamList.unbridged(), cDeclContext.unbridged(), isActor);
  decl->setTrailingWhereClause(genericWhereClause.unbridged());
  decl->setBraces(braceRange);

  return decl;
}

BridgedNominalTypeDecl BridgedProtocolDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc protocolKeywordLoc, swift::Identifier name, SourceLoc nameLoc,
    BridgedArrayRef cPrimaryAssociatedTypeNames,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    SourceRange braceRange) {
  ASTContext &context = cContext.unbridged();

  auto primaryAssociatedTypeNames =
      context.AllocateTransform<PrimaryAssociatedTypeName>(
          cPrimaryAssociatedTypeNames.unbridged<BridgedLocatedIdentifier>(),
          [](auto &e) -> PrimaryAssociatedTypeName {
            return {e.Name, e.NameLoc};
          });

  NominalTypeDecl *decl = new (context)
      ProtocolDecl(cDeclContext.unbridged(), protocolKeywordLoc, nameLoc, name,
                   primaryAssociatedTypeNames,
                   convertToInheritedEntries(context, cInheritedTypes),
                   genericWhereClause.unbridged());
  decl->setBraces(braceRange);

  return decl;
}

BridgedAssociatedTypeDecl BridgedAssociatedTypeDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc associatedtypeKeywordLoc, swift::Identifier name,
    SourceLoc nameLoc, BridgedArrayRef cInheritedTypes,
    BridgedNullableTypeRepr defaultType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  ASTContext &context = cContext.unbridged();

  auto *decl = AssociatedTypeDecl::createParsed(
      context, cDeclContext.unbridged(), associatedtypeKeywordLoc, name,
      nameLoc, defaultType.unbridged(), genericWhereClause.unbridged());
  decl->setInherited(convertToInheritedEntries(context, cInheritedTypes));

  return decl;
}

BridgedExtensionDecl BridgedExtensionDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc extensionKeywordLoc, BridgedTypeRepr extendedType,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    SourceRange braceRange) {
  ASTContext &context = cContext.unbridged();

  auto *decl = ExtensionDecl::create(
      context, extensionKeywordLoc, extendedType.unbridged(),
      convertToInheritedEntries(context, cInheritedTypes),
      cDeclContext.unbridged(), genericWhereClause.unbridged());
  decl->setBraces(braceRange);
  return decl;
}

BridgedMacroExpansionDecl BridgedMacroExpansionDecl_createParsed(
    BridgedDeclContext cDeclContext, SourceLoc poundLoc,
    BridgedDeclNameRef cMacroNameRef, BridgedDeclNameLoc cMacroNameLoc,
    SourceLoc leftAngleLoc, BridgedArrayRef cGenericArgs,
    SourceLoc rightAngleLoc, BridgedNullableArgumentList cArgList) {
  auto *DC = cDeclContext.unbridged();
  auto &Context = DC->getASTContext();
  return MacroExpansionDecl::create(
      cDeclContext.unbridged(), poundLoc, cMacroNameRef.unbridged(),
      cMacroNameLoc.unbridged(), leftAngleLoc,
      Context.AllocateCopy(cGenericArgs.unbridged<TypeRepr *>()), rightAngleLoc,
      cArgList.unbridged());
}

BridgedMissingDecl BridgedMissingDecl_create(BridgedASTContext cContext,
                                             BridgedDeclContext cDeclContext,
                                             SourceLoc loc) {
  return MissingDecl::create(cContext.unbridged(), cDeclContext.unbridged(),
                             loc);
}

BridgedOperatorDecl BridgedOperatorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedOperatorFixity cFixity, SourceLoc operatorKeywordLoc,
    swift::Identifier name, SourceLoc nameLoc, SourceLoc colonLoc,
    swift::Identifier precedenceGroupName, SourceLoc precedenceGroupLoc) {
  assert(colonLoc.isValid() == precedenceGroupName.nonempty());
  assert(colonLoc.isValid() == precedenceGroupLoc.isValid());

  ASTContext &context = cContext.unbridged();
  auto *declContext = cDeclContext.unbridged();

  OperatorDecl *decl = nullptr;
  switch (cFixity) {
  case BridgedOperatorFixityInfix:
    decl = new (context)
        InfixOperatorDecl(declContext, operatorKeywordLoc, name, nameLoc,
                          colonLoc, precedenceGroupName, precedenceGroupLoc);
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
    BridgedDeclContext cDeclContext, SourceLoc precedencegroupKeywordLoc,
    swift::Identifier name, SourceLoc nameLoc, SourceLoc leftBraceLoc,
    SourceLoc associativityKeywordLoc, SourceLoc associativityValueLoc,
    swift::Associativity associativity, SourceLoc assignmentKeywordLoc,
    SourceLoc assignmentValueLoc, bool isAssignment,
    SourceLoc higherThanKeywordLoc, BridgedArrayRef cHigherThanNames,
    SourceLoc lowerThanKeywordLoc, BridgedArrayRef cLowerThanNames,
    SourceLoc rightBraceLoc) {

  SmallVector<PrecedenceGroupDecl::Relation, 2> higherThanNames;
  for (auto &pair : cHigherThanNames.unbridged<BridgedLocatedIdentifier>()) {
    higherThanNames.push_back({pair.NameLoc, pair.Name, nullptr});
  }

  SmallVector<PrecedenceGroupDecl::Relation, 2> lowerThanNames;
  for (auto &pair : cLowerThanNames.unbridged<BridgedLocatedIdentifier>()) {
    lowerThanNames.push_back({pair.NameLoc, pair.Name, nullptr});
  }

  return PrecedenceGroupDecl::create(
      cDeclContext.unbridged(), precedencegroupKeywordLoc, nameLoc, name,
      leftBraceLoc, associativityKeywordLoc, associativityValueLoc,
      associativity, assignmentKeywordLoc, assignmentValueLoc, isAssignment,
      higherThanKeywordLoc, higherThanNames, lowerThanKeywordLoc,
      lowerThanNames, rightBraceLoc);
}

BridgedImportDecl BridgedImportDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc importKeywordLoc, BridgedImportKind cImportKind,
    SourceLoc importKindLoc, BridgedArrayRef cImportPathElements) {
  ImportPath::Builder builder;
  for (auto &element :
       cImportPathElements.unbridged<BridgedLocatedIdentifier>()) {
    builder.push_back(element.Name, element.NameLoc);
  }

  ASTContext &context = cContext.unbridged();
  return ImportDecl::create(context, cDeclContext.unbridged(), importKeywordLoc,
                            static_cast<ImportKind>(cImportKind), importKindLoc,
                            std::move(builder).get());
}

BridgedUsingDecl
BridgedUsingDecl_createParsed(BridgedASTContext cContext,
                              BridgedDeclContext cDeclContext,
                              SourceLoc usingKeywordLoc, SourceLoc specifierLoc,
                              BridgedUsingSpecifier specifier) {
  ASTContext &ctx = cContext.unbridged();
  return UsingDecl::create(ctx, usingKeywordLoc, specifierLoc,
                           static_cast<UsingSpecifier>(specifier),
                           cDeclContext.unbridged());
}

BridgedSubscriptDecl BridgedSubscriptDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    SourceLoc staticLoc, BridgedStaticSpelling cStaticSpelling,
    SourceLoc subscriptKeywordLoc,
    BridgedNullableGenericParamList cGenericParamList,
    BridgedParameterList cParamList, SourceLoc arrowLoc,
    BridgedTypeRepr returnType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  auto *decl = SubscriptDecl::createParsed(
      cContext.unbridged(), staticLoc, unbridged(cStaticSpelling),
      subscriptKeywordLoc, cParamList.unbridged(), arrowLoc,
      returnType.unbridged(), cDeclContext.unbridged(),
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
      accessors.lBraceLoc, accessors.accessors.unbridged<AccessorDecl *>(),
      accessors.rBraceLoc);
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
    BridgedASTContext cContext, SourceLoc leftParenLoc,
    BridgedArrayRef cParameters, SourceLoc rightParenLoc) {
  ASTContext &context = cContext.unbridged();
  return ParameterList::create(context, leftParenLoc,
                               cParameters.unbridged<ParamDecl *>(),
                               rightParenLoc);
}

size_t BridgedParameterList_size(BridgedParameterList cParameterList) {
  return cParameterList.unbridged()->size();
}

BridgedParamDecl BridgedParameterList_get(BridgedParameterList cParameterList,
                                          size_t i) {
  return cParameterList.unbridged()->get(i);
}
