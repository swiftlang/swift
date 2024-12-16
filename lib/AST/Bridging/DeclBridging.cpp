//===--- Bridging/DeclBridging.cpp ----------------------------------------===//
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

  ASTContext &context = cContext.unbridged();
  SmallVector<SourceLoc, 4> labelLocs;
  for (auto &cLabelLoc : cLabelLocs.unbridged<BridgedSourceLoc>())
    labelLocs.push_back(cLabelLoc.unbridged());

  return DeclNameLoc(context, cBaseNameLoc.unbridged(), cLParenLoc.unbridged(),
                     labelLocs, cRParenLoc.unbridged());
}

BridgedDeclNameLoc
BridgedDeclNameLoc_createParsed(BridgedSourceLoc cBaseNameLoc) {
  return DeclNameLoc(cBaseNameLoc.unbridged());
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

static AccessorKind unbridged(BridgedAccessorKind kind) {
  return static_cast<AccessorKind>(kind);
}

void BridgedDecl_setAttrs(BridgedDecl decl, BridgedDeclAttributes attrs) {
  decl.unbridged()->getAttrs() = attrs.unbridged();
}

BridgedAccessorDecl BridgedAccessorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedAccessorKind cKind, BridgedAbstractStorageDecl cStorage,
    BridgedSourceLoc cDeclLoc, BridgedSourceLoc cAccessorKeywordLoc,
    BridgedNullableParameterList cParamList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr cThrownType) {
  return AccessorDecl::createParsed(
      cContext.unbridged(), unbridged(cKind), cStorage.unbridged(),
      cDeclLoc.unbridged(), cAccessorKeywordLoc.unbridged(),
      cParamList.unbridged(), cAsyncLoc.unbridged(), cThrowsLoc.unbridged(),
      cThrownType.unbridged(), cDeclContext.unbridged());
}

BridgedPatternBindingDecl BridgedPatternBindingDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cBindingKeywordLoc, BridgedArrayRef cBindingEntries, BridgedDeclAttributes cAttrs, bool isStatic, bool isLet) {
  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();

  auto introducer = isLet ? VarDecl::Introducer::Let : VarDecl::Introducer::Var;

  SmallVector<PatternBindingEntry, 4> entries;
  for (auto &entry : cBindingEntries.unbridged<BridgedPatternBindingEntry>()) {
    auto *pattern = entry.pattern.unbridged();

    // Configure all vars.
    pattern->forEachVariable([&](VarDecl *VD) {
      VD->getAttrs() = cAttrs.unbridged();
      VD->setStatic(isStatic);
      VD->setIntroducer(introducer);
    });

    entries.emplace_back(pattern, entry.equalLoc.unbridged(),
                         entry.init.unbridged(), entry.initContext.unbridged());
  }

  return PatternBindingDecl::create(
      context,
      /*StaticLoc=*/SourceLoc(),
      // FIXME: 'class' spelling kind.
      isStatic ? StaticSpellingKind::KeywordStatic : StaticSpellingKind::None,
      cBindingKeywordLoc.unbridged(), entries, declContext);
}

BridgedParamDecl BridgedParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cSpecifierLoc, BridgedIdentifier cArgName,
    BridgedSourceLoc cArgNameLoc, BridgedIdentifier cParamName,
    BridgedSourceLoc cParamNameLoc, BridgedNullableTypeRepr opaqueType,
    BridgedNullableExpr opaqueDefaultValue) {
  auto *paramDecl = ParamDecl::createParsed(
      cContext.unbridged(), cSpecifierLoc.unbridged(), cArgNameLoc.unbridged(),
      cArgName.unbridged(), cParamNameLoc.unbridged(), cParamName.unbridged(),
      opaqueDefaultValue.unbridged(), cDeclContext.unbridged());

  if (auto type = opaqueType.unbridged()) {
    paramDecl->setTypeRepr(type);

    // FIXME: Copied from 'Parser::parsePattern()'. This should be in Sema.
    // Dig through the type to find any attributes or modifiers that are
    // associated with the type but should also be reflected on the
    // declaration.
    auto unwrappedType = type;
    while (true) {
      if (auto *ATR = dyn_cast<AttributedTypeRepr>(unwrappedType)) {
        auto attrs = ATR->getAttrs();
        // At this point we actually don't know if that's valid to mark
        // this parameter declaration as `autoclosure` because type has
        // not been resolved yet - it should either be a function type
        // or typealias with underlying function type.
        bool autoclosure = llvm::any_of(attrs, [](TypeOrCustomAttr attr) {
          if (auto typeAttr = attr.dyn_cast<TypeAttribute *>())
            return isa<AutoclosureTypeAttr>(typeAttr);
          return false;
        });
        paramDecl->setAutoClosure(autoclosure);

        unwrappedType = ATR->getTypeRepr();
        continue;
      }

      if (auto *STR = dyn_cast<SpecifierTypeRepr>(unwrappedType)) {
        if (isa<IsolatedTypeRepr>(STR))
          paramDecl->setIsolated(true);
        else if (isa<CompileTimeConstTypeRepr>(STR))
          paramDecl->setCompileTimeConst(true);
        else if (isa<SendingTypeRepr>(STR))
          paramDecl->setSending(true);

        unwrappedType = STR->getBase();
        continue;
      }

      break;
    }
  }

  return paramDecl;
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
  // TODO: Handle LifetimeDependentReturnTypeRepr here.
  auto *decl = new (context) ConstructorDecl(
      declName, cInitKeywordLoc.unbridged(), failabilityMarkLoc.isValid(),
      failabilityMarkLoc, asyncLoc.isValid(), asyncLoc, throwsLoc.isValid(),
      throwsLoc, thrownType.unbridged(), parameterList,
      genericParams.unbridged(), cDeclContext.unbridged(),
      /*InitRetTy*/ nullptr);
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
    BridgedNullableTypeRepr cResultType, BridgedNullableExpr cDefinition) {
  ASTContext &context = cContext.unbridged();
  auto *params = cParams.unbridged();
  DeclName fullName = DeclName(context, cName.unbridged(), params);
  return new (context)
      MacroDecl(cMacroLoc.unbridged(), fullName, cNameLoc.unbridged(),
                cGenericParams.unbridged(), params, cArrowLoc.unbridged(),
                cResultType.unbridged(), cDefinition.unbridged(),
                cDeclContext.unbridged());
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

  SmallVector<Decl *> members;
  for (auto *decl : cMembers.unbridged<Decl *>()) {
    members.push_back(decl);

    // Add any variables bound to the list of decls.
    if (auto *PBD = dyn_cast<PatternBindingDecl>(decl)) {
      for (auto idx : range(PBD->getNumPatternEntries())) {
        PBD->getPattern(idx)->forEachVariable(
            [&](VarDecl *VD) { members.push_back(VD); });
      }
    }
    // Each enum case element is also part of the members list according to the
    // legacy parser.
    if (auto *ECD = dyn_cast<EnumCaseDecl>(decl)) {
      for (auto *EED : ECD->getElements()) {
        members.push_back(EED);
      }
    }
  }

  IDC->setMaybeHasOperatorDeclarations();
  IDC->setMaybeHasNestedClassDeclarations();
  // FIXME: Split requests. e.g. DeclMembersFingerprintRequest.
  ctx.evaluator.cacheOutput(
      ParseMembersRequest{IDC},
      FingerprintAndMembers{fp, ctx.AllocateCopy(members)});
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
    BridgedSourceLoc cAssociativityValueLoc,
    BridgedAssociativity cAssociativity, BridgedSourceLoc cAssignmentKeywordLoc,
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
      static_cast<Associativity>(cAssociativity),
      cAssignmentKeywordLoc.unbridged(), cAssignmentValueLoc.unbridged(),
      isAssignment, cHigherThanKeywordLoc.unbridged(), higherThanNames,
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

BridgedSubscriptDecl BridgedSubscriptDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStaticLoc, BridgedStaticSpelling cStaticSpelling,
    BridgedSourceLoc cSubscriptKeywordLoc,
    BridgedNullableGenericParamList cGenericParamList,
    BridgedParameterList cParamList, BridgedSourceLoc cArrowLoc,
    BridgedTypeRepr returnType) {
  return SubscriptDecl::createParsed(
      cContext.unbridged(), cStaticLoc.unbridged(), unbridged(cStaticSpelling),
      cSubscriptKeywordLoc.unbridged(), cParamList.unbridged(),
      cArrowLoc.unbridged(), returnType.unbridged(), cDeclContext.unbridged(),
      cGenericParamList.unbridged());
}

BridgedTopLevelCodeDecl BridgedTopLevelCodeDecl_createStmt(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStartLoc, BridgedStmt statement,
    BridgedSourceLoc cEndLoc) {
  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();

  auto *S = statement.unbridged();
  auto Brace = BraceStmt::create(context, cStartLoc.unbridged(), {S},
                                 cEndLoc.unbridged(),
                                 /*Implicit=*/true);
  return new (context) TopLevelCodeDecl(declContext, Brace);
}

BridgedTopLevelCodeDecl BridgedTopLevelCodeDecl_createExpr(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStartLoc, BridgedExpr expression,
    BridgedSourceLoc cEndLoc) {
  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();

  auto *E = expression.unbridged();
  auto Brace = BraceStmt::create(context, cStartLoc.unbridged(), {E},
                                 cEndLoc.unbridged(),
                                 /*Implicit=*/true);
  return new (context) TopLevelCodeDecl(declContext, Brace);
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

BridgedParameterList BridgedParameterList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftParenLoc,
    BridgedArrayRef cParameters, BridgedSourceLoc cRightParenLoc) {
  ASTContext &context = cContext.unbridged();
  return ParameterList::create(context, cLeftParenLoc.unbridged(),
                               cParameters.unbridged<ParamDecl *>(),
                               cRightParenLoc.unbridged());
}
