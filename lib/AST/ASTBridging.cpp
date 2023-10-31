//===--- ASTBridging.cpp - AST bridging functions -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
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
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ParseRequests.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PluginRegistry.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"

#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, bridging functions are not inlined and therefore
// inluded in the cpp file.
#include "swift/AST/ASTBridgingImpl.h"
#endif

using namespace swift;

static TypeAttrKind unbridged(BridgedTypeAttrKind kind) {
  switch (kind) {
#define TYPE_ATTR(X)                                                           \
  case BridgedTypeAttrKind_##X:                                                \
    return TAK_##X;
#include "swift/AST/Attr.def"
  case BridgedTypeAttrKind_Count:
    return TAK_Count;
  }
}

//===----------------------------------------------------------------------===//
// MARK: ASTContext
//===----------------------------------------------------------------------===//

BridgedIdentifier BridgedASTContext_getIdentifier(BridgedASTContext cContext,
                                                  BridgedStringRef cStr) {
  StringRef str = cStr.unbridged();
  if (str.size() == 1 && str.front() == '_')
    return BridgedIdentifier();

  // If this was a back-ticked identifier, drop the back-ticks.
  if (str.size() >= 2 && str.front() == '`' && str.back() == '`') {
    str = str.drop_front().drop_back();
  }

  return cContext.unbridged().getIdentifier(str);
}

bool BridgedASTContext_langOptsHasFeature(BridgedASTContext cContext,
                                          BridgedFeature feature) {
  return cContext.unbridged().LangOpts.hasFeature((Feature)feature);
}

//===----------------------------------------------------------------------===//
// MARK: AST nodes
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

// Define `.asStmt` on each BridgedXXXStmt type.
#define STMT(Id, Parent)                                                       \
  BridgedStmt Bridged##Id##Stmt_asStmt(Bridged##Id##Stmt stmt) {               \
    return static_cast<Stmt *>(stmt.unbridged());                              \
  }
#define ABSTRACT_STMT(Id, Parent) STMT(Id, Parent)
#include "swift/AST/StmtNodes.def"

// Define `.asExpr` on each BridgedXXXExpr type.
#define EXPR(Id, Parent)                                                       \
  BridgedExpr Bridged##Id##Expr_asExpr(Bridged##Id##Expr expr) {               \
    return static_cast<Expr *>(expr.unbridged());                              \
  }
#define ABSTRACT_EXPR(Id, Parent) EXPR(Id, Parent)
#include "swift/AST/ExprNodes.def"

// Define `.asTypeRepr` on each BridgedXXXTypeRepr type.
#define TYPEREPR(Id, Parent)                                                   \
  BridgedTypeRepr Bridged##Id##TypeRepr_asTypeRepr(                            \
      Bridged##Id##TypeRepr typeRepr) {                                        \
    return static_cast<TypeRepr *>(typeRepr.unbridged());                      \
  }
#define ABSTRACT_TYPEREPR(Id, Parent) TYPEREPR(Id, Parent)
#include "swift/AST/TypeReprNodes.def"

//===----------------------------------------------------------------------===//
// MARK: Diagnostics
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedDiagnosticArgument) >= sizeof(DiagnosticArgument),
              "BridgedDiagnosticArgument has wrong size");

BridgedDiagnosticArgument::BridgedDiagnosticArgument(SwiftInt i)
    : BridgedDiagnosticArgument(DiagnosticArgument((int)i)) {}

BridgedDiagnosticArgument::BridgedDiagnosticArgument(BridgedStringRef s)
    : BridgedDiagnosticArgument(DiagnosticArgument(s.unbridged())) {}

static_assert(sizeof(BridgedDiagnosticFixIt) >= sizeof(DiagnosticInfo::FixIt),
              "BridgedDiagnosticFixIt has wrong size");

BridgedDiagnosticFixIt::BridgedDiagnosticFixIt(BridgedSourceLoc start,
                                               uint32_t length,
                                               BridgedStringRef text)
    : BridgedDiagnosticFixIt(DiagnosticInfo::FixIt(
          CharSourceRange(start.unbridged(), length), text.unbridged(),
          llvm::ArrayRef<DiagnosticArgument>())) {}

void BridgedDiagnosticEngine_diagnose(
    BridgedDiagnosticEngine bridgedEngine, BridgedSourceLoc loc,
    BridgedDiagID bridgedDiagID,
    BridgedArrayRef /*BridgedDiagnosticArgument*/ bridgedArguments,
    BridgedSourceLoc highlightStart, uint32_t hightlightLength,
    BridgedArrayRef /*BridgedDiagnosticFixIt*/ bridgedFixIts) {
  auto *D = bridgedEngine.unbridged();

  auto diagID = static_cast<DiagID>(bridgedDiagID);
  SmallVector<DiagnosticArgument, 2> arguments;
  for (auto arg : bridgedArguments.unbridged<BridgedDiagnosticArgument>()) {
    arguments.push_back(arg.unbridged());
  }
  auto inflight = D->diagnose(loc.unbridged(), diagID, arguments);

  // Add highlight.
  if (highlightStart.unbridged().isValid()) {
    CharSourceRange highlight(highlightStart.unbridged(),
                              (unsigned)hightlightLength);
    inflight.highlightChars(highlight.getStart(), highlight.getEnd());
  }

  // Add fix-its.
  for (const BridgedDiagnosticFixIt &fixIt :
       bridgedFixIts.unbridged<BridgedDiagnosticFixIt>()) {
    auto range = fixIt.unbridged().getRange();
    auto text = fixIt.unbridged().getText();
    inflight.fixItReplaceChars(range.getStart(), range.getEnd(), text);
  }
}

bool BridgedDiagnosticEngine_hadAnyError(
    BridgedDiagnosticEngine bridgedEngine) {
  return bridgedEngine.unbridged()->hadAnyError();
}

struct BridgedDiagnostic::Impl {
  typedef llvm::MallocAllocator Allocator;

  InFlightDiagnostic inFlight;
  std::vector<StringRef> textBlobs;

  Impl(InFlightDiagnostic inFlight, std::vector<StringRef> textBlobs)
      : inFlight(std::move(inFlight)), textBlobs(std::move(textBlobs)) {}

  Impl(const Impl &) = delete;
  Impl(Impl &&) = delete;
  Impl &operator=(const Impl &) = delete;
  Impl &operator=(Impl &&) = delete;

  ~Impl() {
    inFlight.flush();

    Allocator allocator;
    for (auto text : textBlobs) {
      allocator.Deallocate(text.data(), text.size());
    }
  }
};

BridgedDiagnostic BridgedDiagnostic_create(BridgedSourceLoc cLoc,
                                           BridgedStringRef cText,
                                           BridgedDiagnosticSeverity severity,
                                           BridgedDiagnosticEngine cDiags) {
  StringRef origText = cText.unbridged();
  BridgedDiagnostic::Impl::Allocator alloc;
  StringRef text = origText.copy(alloc);

  SourceLoc loc = cLoc.unbridged();

  Diag<StringRef> diagID;
  switch (severity) {
  case BridgedDiagnosticSeverity::BridgedError:
    diagID = diag::bridged_error;
    break;
  case BridgedDiagnosticSeverity::BridgedFatalError:
    diagID = diag::bridged_fatal_error;
    break;
  case BridgedDiagnosticSeverity::BridgedNote:
    diagID = diag::bridged_note;
    break;
  case BridgedDiagnosticSeverity::BridgedRemark:
    diagID = diag::bridged_remark;
    break;
  case BridgedDiagnosticSeverity::BridgedWarning:
    diagID = diag::bridged_warning;
    break;
  }

  DiagnosticEngine &diags = *cDiags.unbridged();
  return new BridgedDiagnostic::Impl{diags.diagnose(loc, diagID, text), {text}};
}

/// Highlight a source range as part of the diagnostic.
void BridgedDiagnostic_highlight(BridgedDiagnostic cDiag,
                                 BridgedSourceLoc cStartLoc,
                                 BridgedSourceLoc cEndLoc) {
  SourceLoc startLoc = cStartLoc.unbridged();
  SourceLoc endLoc = cEndLoc.unbridged();

  BridgedDiagnostic::Impl *diag = cDiag.unbridged();
  diag->inFlight.highlightChars(startLoc, endLoc);
}

/// Add a Fix-It to replace a source range as part of the diagnostic.
void BridgedDiagnostic_fixItReplace(BridgedDiagnostic cDiag,
                                    BridgedSourceLoc cStartLoc,
                                    BridgedSourceLoc cEndLoc,
                                    BridgedStringRef cReplaceText) {

  SourceLoc startLoc = cStartLoc.unbridged();
  SourceLoc endLoc = cEndLoc.unbridged();

  StringRef origReplaceText = cReplaceText.unbridged();
  BridgedDiagnostic::Impl::Allocator alloc;
  StringRef replaceText = origReplaceText.copy(alloc);

  BridgedDiagnostic::Impl *diag = cDiag.unbridged();
  diag->textBlobs.push_back(replaceText);
  diag->inFlight.fixItReplaceChars(startLoc, endLoc, replaceText);
}

/// Finish the given diagnostic and emit it.
void BridgedDiagnostic_finish(BridgedDiagnostic cDiag) {
  BridgedDiagnostic::Impl *diag = cDiag.unbridged();
  delete diag;
}

//===----------------------------------------------------------------------===//
// MARK: Decls
//===----------------------------------------------------------------------===//

BridgedPatternBindingDecl BridgedPatternBindingDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cBindingKeywordLoc, BridgedExpr nameExpr,
    BridgedExpr initExpr, bool isStatic, bool isLet) {
  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();

  auto *name = cast<UnresolvedDeclRefExpr>(nameExpr.unbridged());
  auto *varDecl = new (context) VarDecl(
      isStatic, isLet ? VarDecl::Introducer::Let : VarDecl::Introducer::Var,
      name->getLoc(), name->getName().getBaseIdentifier(), declContext);
  auto *pattern = new (context) NamedPattern(varDecl);
  return PatternBindingDecl::create(context,
                                    /*StaticLoc=*/SourceLoc(), // FIXME
                                    isStatic ? StaticSpellingKind::KeywordStatic
                                             : StaticSpellingKind::None,
                                    cBindingKeywordLoc.unbridged(), pattern,
                                    /*EqualLoc=*/SourceLoc(), // FIXME
                                    initExpr.unbridged(), declContext);
}

BridgedParamDecl BridgedParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cSpecifierLoc, BridgedIdentifier cFirstName,
    BridgedSourceLoc cFirstNameLoc, BridgedIdentifier cSecondName,
    BridgedSourceLoc cSecondNameLoc, BridgedNullableTypeRepr opaqueType,
    BridgedNullableExpr opaqueDefaultValue) {
  auto firstName = cFirstName.unbridged();
  auto firstNameLoc = cFirstNameLoc.unbridged();
  auto secondName = cSecondName.unbridged();
  auto secondNameLoc = cSecondNameLoc.unbridged();

  assert(secondNameLoc.isValid() == secondName.nonempty());
  if (secondName.empty()) {
    secondName = firstName;
    secondNameLoc = firstNameLoc;
  }

  auto *declContext = cDeclContext.unbridged();

  auto *defaultValue = opaqueDefaultValue.unbridged();
  DefaultArgumentKind defaultArgumentKind;

  if (declContext->getParentSourceFile()->Kind == SourceFileKind::Interface &&
      isa<SuperRefExpr>(defaultValue)) {
    defaultValue = nullptr;
    defaultArgumentKind = DefaultArgumentKind::Inherited;
  } else {
    defaultArgumentKind = getDefaultArgKind(defaultValue);
  }

  auto *paramDecl = new (cContext.unbridged())
      ParamDecl(cSpecifierLoc.unbridged(), firstNameLoc, firstName,
                secondNameLoc, secondName, declContext);
  paramDecl->setTypeRepr(opaqueType.unbridged());
  paramDecl->setDefaultExpr(defaultValue, /*isTypeChecked*/ false);
  paramDecl->setDefaultArgumentKind(defaultArgumentKind);

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
    BridgedSourceLoc cStaticLoc, BridgedSourceLoc cFuncKeywordLoc,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedNullableGenericParamList genericParamList,
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
      context, cStaticLoc.unbridged(), StaticSpellingKind::None,
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

static void setParsedMembers(IterableDeclContext *IDC,
                             BridgedArrayRef bridgedMembers) {
  auto &ctx = IDC->getDecl()->getASTContext();

  SmallVector<Decl *> members;
  for (auto *decl : bridgedMembers.unbridged<Decl *>()) {
    members.push_back(decl);
    // Each enum case element is also part of the members list according to the
    // legacy parser.
    if (auto *ECD = dyn_cast<EnumCaseDecl>(decl)) {
      for (auto *EED : ECD->getElements()) {
        members.push_back(EED);
      }
    }
  }

  ctx.evaluator.cacheOutput(
      ParseMembersRequest{IDC},
      FingerprintAndMembers{llvm::None, ctx.AllocateCopy(members)});
}

void BridgedNominalTypeDecl_setParsedMembers(BridgedNominalTypeDecl bridgedDecl,
                                             BridgedArrayRef bridgedMembers) {
  setParsedMembers(bridgedDecl.unbridged(), bridgedMembers);
}

void BridgedExtensionDecl_setParsedMembers(BridgedExtensionDecl bridgedDecl,
                                           BridgedArrayRef bridgedMembers) {
  setParsedMembers(bridgedDecl.unbridged(), bridgedMembers);
}

static SmallVector<InheritedEntry>
convertToInheritedEntries(BridgedArrayRef cInheritedTypes) {
  SmallVector<InheritedEntry> inheritedEntries;
  for (auto &repr : cInheritedTypes.unbridged<BridgedTypeRepr>()) {
    inheritedEntries.emplace_back(repr.unbridged());
  }

  return inheritedEntries;
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
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)),
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
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)),
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
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)),
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
  SmallVector<PrimaryAssociatedTypeName, 2> primaryAssociatedTypeNames;
  for (auto &pair :
       cPrimaryAssociatedTypeNames.unbridged<BridgedIdentifierAndSourceLoc>()) {
    primaryAssociatedTypeNames.emplace_back(pair.Name.unbridged(),
                                            pair.NameLoc.unbridged());
  }

  ASTContext &context = cContext.unbridged();
  NominalTypeDecl *decl = new (context) ProtocolDecl(
      cDeclContext.unbridged(), cProtocolKeywordLoc.unbridged(),
      cNameLoc.unbridged(), cName.unbridged(),
      context.AllocateCopy(primaryAssociatedTypeNames),
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)),
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
  decl->setInherited(
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)));

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
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)),
      cDeclContext.unbridged(), genericWhereClause.unbridged());
  decl->setBraces(cBraceRange.unbridged());
  return decl;
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
  for (auto &pair :
       cHigherThanNames.unbridged<BridgedIdentifierAndSourceLoc>()) {
    higherThanNames.push_back(
        {pair.NameLoc.unbridged(), pair.Name.unbridged(), nullptr});
  }

  SmallVector<PrecedenceGroupDecl::Relation, 2> lowerThanNames;
  for (auto &pair :
       cLowerThanNames.unbridged<BridgedIdentifierAndSourceLoc>()) {
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
       cImportPathElements.unbridged<BridgedIdentifierAndSourceLoc>()) {
    builder.push_back(element.Name.unbridged(), element.NameLoc.unbridged());
  }

  ASTContext &context = cContext.unbridged();
  return ImportDecl::create(
      context, cDeclContext.unbridged(), cImportKeywordLoc.unbridged(),
      static_cast<ImportKind>(cImportKind), cImportKindLoc.unbridged(),
      std::move(builder).get());
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
// MARK: Exprs
//===----------------------------------------------------------------------===//

BridgedClosureExpr
BridgedClosureExpr_createParsed(BridgedASTContext cContext,
                                BridgedDeclContext cDeclContext,
                                BridgedBraceStmt body) {
  DeclAttributes attributes;
  SourceRange bracketRange;
  SourceLoc asyncLoc;
  SourceLoc throwsLoc;
  SourceLoc arrowLoc;
  SourceLoc inLoc;

  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();

  auto params = ParameterList::create(context, inLoc, {}, inLoc);

  auto *out = new (context) ClosureExpr(
      attributes, bracketRange, nullptr, nullptr, asyncLoc, throwsLoc,
      /*FIXME:thrownType=*/nullptr, arrowLoc, inLoc, nullptr, declContext);
  out->setBody(body.unbridged(), true);
  out->setParameterList(params);
  return out;
}

BridgedSequenceExpr BridgedSequenceExpr_createParsed(BridgedASTContext cContext,
                                                     BridgedArrayRef exprs) {
  return SequenceExpr::create(cContext.unbridged(), exprs.unbridged<Expr *>());
}

BridgedTupleExpr BridgedTupleExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLParen,
                                               BridgedArrayRef subs,
                                               BridgedArrayRef names,
                                               BridgedArrayRef cNameLocs,
                                               BridgedSourceLoc cRParen) {
  ASTContext &context = cContext.unbridged();
  return TupleExpr::create(
      context, cLParen.unbridged(), subs.unbridged<Expr *>(),
      names.unbridged<Identifier>(), cNameLocs.unbridged<SourceLoc>(),
      cRParen.unbridged(), /*Implicit*/ false);
}

BridgedCallExpr BridgedCallExpr_createParsed(BridgedASTContext cContext,
                                             BridgedExpr fn,
                                             BridgedTupleExpr args) {
  ASTContext &context = cContext.unbridged();
  TupleExpr *TE = args.unbridged();
  SmallVector<Argument, 8> arguments;
  for (unsigned i = 0; i < TE->getNumElements(); ++i) {
    arguments.emplace_back(TE->getElementNameLoc(i), TE->getElementName(i),
                           TE->getElement(i));
  }
  auto *argList = ArgumentList::create(context, TE->getLParenLoc(), arguments,
                                       TE->getRParenLoc(), llvm::None,
                                       /*isImplicit*/ false);
  return CallExpr::create(context, fn.unbridged(), argList,
                          /*implicit*/ false);
}

BridgedUnresolvedDeclRefExpr BridgedUnresolvedDeclRefExpr_createParsed(
    BridgedASTContext cContext, BridgedIdentifier base, BridgedSourceLoc cLoc) {
  ASTContext &context = cContext.unbridged();
  auto name = DeclNameRef{base.unbridged()};
  return new (context) UnresolvedDeclRefExpr(name, DeclRefKind::Ordinary,
                                             DeclNameLoc{cLoc.unbridged()});
}

BridgedStringLiteralExpr
BridgedStringLiteralExpr_createParsed(BridgedASTContext cContext,
                                      BridgedStringRef cStr,
                                      BridgedSourceLoc cTokenLoc) {
  ASTContext &context = cContext.unbridged();
  auto str = context.AllocateCopy(cStr.unbridged());
  return new (context) StringLiteralExpr(str, cTokenLoc.unbridged());
}

BridgedIntegerLiteralExpr
BridgedIntegerLiteralExpr_createParsed(BridgedASTContext cContext,
                                       BridgedStringRef cStr,
                                       BridgedSourceLoc cTokenLoc) {
  ASTContext &context = cContext.unbridged();
  auto str = context.AllocateCopy(cStr.unbridged());
  return new (context) IntegerLiteralExpr(str, cTokenLoc.unbridged());
}

BridgedArrayExpr BridgedArrayExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLLoc,
                                               BridgedArrayRef elements,
                                               BridgedArrayRef commas,
                                               BridgedSourceLoc cRLoc) {
  ASTContext &context = cContext.unbridged();
  return ArrayExpr::create(context, cLLoc.unbridged(),
                           elements.unbridged<Expr *>(),
                           commas.unbridged<SourceLoc>(), cRLoc.unbridged());
}

BridgedBooleanLiteralExpr
BridgedBooleanLiteralExpr_createParsed(BridgedASTContext cContext, bool value,
                                       BridgedSourceLoc cTokenLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) BooleanLiteralExpr(value, cTokenLoc.unbridged());
}

BridgedNilLiteralExpr
BridgedNilLiteralExpr_createParsed(BridgedASTContext cContext,
                                   BridgedSourceLoc cNilKeywordLoc) {
  return new (cContext.unbridged()) NilLiteralExpr(cNilKeywordLoc.unbridged());
}

BridgedSingleValueStmtExpr BridgedSingleValueStmtExpr_createWithWrappedBranches(
    BridgedASTContext cContext, BridgedStmt S, BridgedDeclContext cDeclContext,
    bool mustBeExpr) {
  ASTContext &context = cContext.unbridged();
  DeclContext *declContext = cDeclContext.unbridged();
  return SingleValueStmtExpr::createWithWrappedBranches(
      context, S.unbridged(), declContext, mustBeExpr);
}

BridgedUnresolvedDotExpr BridgedUnresolvedDotExpr_createParsed(
    BridgedASTContext cContext, BridgedExpr base, BridgedSourceLoc cDotLoc,
    BridgedIdentifier name, BridgedSourceLoc cNameLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) UnresolvedDotExpr(
      base.unbridged(), cDotLoc.unbridged(), DeclNameRef(name.unbridged()),
      DeclNameLoc(cNameLoc.unbridged()), false);
}

//===----------------------------------------------------------------------===//
// MARK: Stmts
//===----------------------------------------------------------------------===//

BridgedBraceStmt BridgedBraceStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLBLoc,
                                               BridgedArrayRef elements,
                                               BridgedSourceLoc cRBLoc) {
  llvm::SmallVector<ASTNode, 6> nodes;
  for (auto node : elements.unbridged<BridgedASTNode>()) {
    if (node.Kind == ASTNodeKindExpr) {
      auto expr = (Expr *)node.Raw;
      nodes.push_back(expr);
    } else if (node.Kind == ASTNodeKindStmt) {
      auto stmt = (Stmt *)node.Raw;
      nodes.push_back(stmt);
    } else {
      assert(node.Kind == ASTNodeKindDecl);
      auto decl = (Decl *)node.Raw;
      nodes.push_back(decl);

      // Variable declarations are part of the list on par with pattern binding
      // declarations per the legacy parser.
      if (auto *bindingDecl = dyn_cast<PatternBindingDecl>(decl)) {
        for (auto i : range(bindingDecl->getNumPatternEntries())) {
          bindingDecl->getPattern(i)->forEachVariable(
              [&nodes](VarDecl *variable) { nodes.push_back(variable); });
        }
      }
    }
  }

  ASTContext &context = cContext.unbridged();
  return BraceStmt::create(context, cLBLoc.unbridged(),
                           context.AllocateCopy(nodes), cRBLoc.unbridged());
}

BridgedIfStmt BridgedIfStmt_createParsed(BridgedASTContext cContext,
                                         BridgedSourceLoc cIfLoc,
                                         BridgedExpr cond, BridgedStmt then,
                                         BridgedSourceLoc cElseLoc,
                                         BridgedNullableStmt elseStmt) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      IfStmt(cIfLoc.unbridged(), cond.unbridged(), then.unbridged(),
             cElseLoc.unbridged(), elseStmt.unbridged(), llvm::None, context);
}

BridgedReturnStmt BridgedReturnStmt_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cLoc,
                                                 BridgedNullableExpr expr) {
  ASTContext &context = cContext.unbridged();
  return new (context) ReturnStmt(cLoc.unbridged(), expr.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: TypeAttributes
//===----------------------------------------------------------------------===//

BridgedTypeAttrKind BridgedTypeAttrKind_fromString(BridgedStringRef cStr) {
  TypeAttrKind kind = TypeAttributes::getAttrKindFromString(cStr.unbridged());
  switch (kind) {
#define TYPE_ATTR(X)                                                           \
  case TAK_##X:                                                                \
    return BridgedTypeAttrKind_##X;
#include "swift/AST/Attr.def"
  case TAK_Count:
    return BridgedTypeAttrKind_Count;
  }
}

BridgedTypeAttributes BridgedTypeAttributes_create() {
  return {new TypeAttributes()};
}

void BridgedTypeAttributes_addSimpleAttr(BridgedTypeAttributes cAttributes,
                                         BridgedTypeAttrKind cKind,
                                         BridgedSourceLoc cAtLoc,
                                         BridgedSourceLoc cAttrLoc) {
  TypeAttributes *typeAttributes = cAttributes.unbridged();
  typeAttributes->setAttr(unbridged(cKind), cAttrLoc.unbridged());
  if (typeAttributes->AtLoc.isInvalid())
    typeAttributes->AtLoc = cAtLoc.unbridged();
}

//===----------------------------------------------------------------------===//
// MARK: TypeReprs
//===----------------------------------------------------------------------===//

BridgedTypeRepr BridgedSimpleIdentTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLoc, BridgedIdentifier id) {
  ASTContext &context = cContext.unbridged();
  return new (context) SimpleIdentTypeRepr(DeclNameLoc(cLoc.unbridged()),
                                           DeclNameRef(id.unbridged()));
}

BridgedTypeRepr BridgedGenericIdentTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedIdentifier name,
    BridgedSourceLoc cNameLoc, BridgedArrayRef genericArgs,
    BridgedSourceLoc cLAngleLoc, BridgedSourceLoc cRAngleLoc) {
  ASTContext &context = cContext.unbridged();
  auto Loc = DeclNameLoc(cNameLoc.unbridged());
  auto Name = DeclNameRef(name.unbridged());
  SourceLoc lAngleLoc = cLAngleLoc.unbridged();
  SourceLoc rAngleLoc = cRAngleLoc.unbridged();
  return GenericIdentTypeRepr::create(context, Loc, Name,
                                      genericArgs.unbridged<TypeRepr *>(),
                                      SourceRange{lAngleLoc, rAngleLoc});
}

BridgedTypeRepr
BridgedOptionalTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr base,
                                     BridgedSourceLoc cQuestionLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      OptionalTypeRepr(base.unbridged(), cQuestionLoc.unbridged());
}

BridgedTypeRepr BridgedImplicitlyUnwrappedOptionalTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedSourceLoc cExclamationLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context) ImplicitlyUnwrappedOptionalTypeRepr(
      base.unbridged(), cExclamationLoc.unbridged());
}

BridgedTypeRepr BridgedArrayTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedSourceLoc cLSquareLoc, BridgedSourceLoc cRSquareLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc lSquareLoc = cLSquareLoc.unbridged();
  SourceLoc rSquareLoc = cRSquareLoc.unbridged();
  return new (context)
      ArrayTypeRepr(base.unbridged(), SourceRange{lSquareLoc, rSquareLoc});
}

BridgedTypeRepr BridgedDictionaryTypeRepr_createParsed(
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

BridgedTypeRepr
BridgedMetatypeTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr baseType,
                                     BridgedSourceLoc cTypeLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc tyLoc = cTypeLoc.unbridged();
  return new (context) MetatypeTypeRepr(baseType.unbridged(), tyLoc);
}

BridgedTypeRepr
BridgedProtocolTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr baseType,
                                     BridgedSourceLoc cProtoLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc protoLoc = cProtoLoc.unbridged();
  return new (context) ProtocolTypeRepr(baseType.unbridged(), protoLoc);
}

BridgedTypeRepr
BridgedPackExpansionTypeRepr_createParsed(BridgedASTContext cContext,
                                          BridgedTypeRepr base,
                                          BridgedSourceLoc cRepeatLoc) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      PackExpansionTypeRepr(cRepeatLoc.unbridged(), base.unbridged());
}

BridgedTypeRepr
BridgedAttributedTypeRepr_createParsed(BridgedASTContext cContext,
                                       BridgedTypeRepr base,
                                       BridgedTypeAttributes cAttributes) {
  TypeAttributes *typeAttributes = cAttributes.unbridged();
  if (typeAttributes->empty())
    return base;

  ASTContext &context = cContext.unbridged();
  auto attributedType =
      new (context) AttributedTypeRepr(*typeAttributes, base.unbridged());
  delete typeAttributes;
  return attributedType;
}

BridgedTypeRepr BridgedSpecifierTypeRepr_createParsed(
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
  case BridgedAttributedTypeSpecifierConst: {
    return new (context) CompileTimeConstTypeRepr(baseType, loc);
  }
  case BridgedAttributedTypeSpecifierIsolated: {
    return new (context) IsolatedTypeRepr(baseType, loc);
  }
  }
}

BridgedTypeRepr
BridgedVarargTypeRepr_createParsed(BridgedASTContext cContext,
                                   BridgedTypeRepr base,
                                   BridgedSourceLoc cEllipsisLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc ellipsisLoc = cEllipsisLoc.unbridged();
  TypeRepr *baseType = base.unbridged();
  return new (context) VarargTypeRepr(baseType, ellipsisLoc);
}

BridgedTypeRepr BridgedTupleTypeRepr_createParsed(BridgedASTContext cContext,
                                                  BridgedArrayRef elements,
                                                  BridgedSourceLoc cLParenLoc,
                                                  BridgedSourceLoc cRParenLoc) {
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

BridgedTypeRepr
BridgedMemberTypeRepr_createParsed(BridgedASTContext cContext,
                                   BridgedTypeRepr baseComponent,
                                   BridgedArrayRef bridgedMemberComponents) {
  ASTContext &context = cContext.unbridged();
  auto memberComponents = bridgedMemberComponents.unbridged<IdentTypeRepr *>();

  return MemberTypeRepr::create(context, baseComponent.unbridged(),
                                memberComponents);
}

BridgedTypeRepr
BridgedCompositionTypeRepr_createEmpty(BridgedASTContext cContext,
                                       BridgedSourceLoc cAnyLoc) {
  ASTContext &context = cContext.unbridged();
  SourceLoc anyLoc = cAnyLoc.unbridged();
  return CompositionTypeRepr::createEmptyComposition(context, anyLoc);
}

BridgedTypeRepr
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

BridgedTypeRepr BridgedFunctionTypeRepr_createParsed(
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

BridgedTypeRepr
BridgedNamedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                              BridgedTypeRepr baseTy) {
  ASTContext &context = cContext.unbridged();
  return new (context) NamedOpaqueReturnTypeRepr(baseTy.unbridged(), nullptr);
}

BridgedTypeRepr
BridgedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                         BridgedSourceLoc cOpaqueLoc,
                                         BridgedTypeRepr baseTy) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      OpaqueReturnTypeRepr(cOpaqueLoc.unbridged(), baseTy.unbridged());
}
BridgedTypeRepr
BridgedExistentialTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cAnyLoc,
                                        BridgedTypeRepr baseTy) {
  ASTContext &context = cContext.unbridged();
  return new (context)
      ExistentialTypeRepr(cAnyLoc.unbridged(), baseTy.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: Misc
//===----------------------------------------------------------------------===//

BridgedGenericParamList BridgedGenericParamList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftAngleLoc,
    BridgedArrayRef cParameters,
    BridgedNullableTrailingWhereClause bridgedGenericWhereClause,
    BridgedSourceLoc cRightAngleLoc) {
  SourceLoc whereLoc;
  ArrayRef<RequirementRepr> requirements;
  if (auto *genericWhereClause = bridgedGenericWhereClause.unbridged()) {
    whereLoc = genericWhereClause->getWhereLoc();
    requirements = genericWhereClause->getRequirements();
  }

  return GenericParamList::create(
      cContext.unbridged(), cLeftAngleLoc.unbridged(),
      cParameters.unbridged<GenericTypeParamDecl *>(), whereLoc, requirements,
      cRightAngleLoc.unbridged());
}

BridgedGenericTypeParamDecl BridgedGenericTypeParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cEachLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableTypeRepr bridgedInheritedType,
    size_t index) {
  auto eachLoc = cEachLoc.unbridged();
  auto *decl = GenericTypeParamDecl::createParsed(
      cDeclContext.unbridged(), cName.unbridged(), cNameLoc.unbridged(),
      eachLoc, index,
      /*isParameterPack*/ eachLoc.isValid());

  if (auto *inheritedType = bridgedInheritedType.unbridged()) {
    auto entry = InheritedEntry(inheritedType);
    ASTContext &context = cContext.unbridged();
    decl->setInherited(context.AllocateCopy(llvm::makeArrayRef(entry)));
  }

  return decl;
}

BridgedTrailingWhereClause
BridgedTrailingWhereClause_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cWhereKeywordLoc,
                                        BridgedArrayRef cRequirements) {
  SmallVector<RequirementRepr> requirements;
  for (auto &cReq : cRequirements.unbridged<BridgedRequirementRepr>()) {
    switch (cReq.Kind) {
    case BridgedRequirementReprKindTypeConstraint:
      requirements.push_back(RequirementRepr::getTypeConstraint(
          cReq.FirstType.unbridged(), cReq.SeparatorLoc.unbridged(),
          cReq.SecondType.unbridged(),
          /*isExpansionPattern*/ false));
      break;
    case BridgedRequirementReprKindSameType:
      requirements.push_back(RequirementRepr::getSameType(
          cReq.FirstType.unbridged(), cReq.SeparatorLoc.unbridged(),
          cReq.SecondType.unbridged(),
          /*isExpansionPattern*/ false));
      break;
    case BridgedRequirementReprKindLayoutConstraint:
      llvm_unreachable("cannot handle layout constraints!");
    }
  }

  SourceLoc whereKeywordLoc = cWhereKeywordLoc.unbridged();
  SourceLoc endLoc;
  if (requirements.empty()) {
    endLoc = whereKeywordLoc;
  } else {
    endLoc = requirements.back().getSourceRange().End;
  }

  return TrailingWhereClause::create(cContext.unbridged(), whereKeywordLoc,
                                     endLoc, requirements);
}

BridgedParameterList BridgedParameterList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftParenLoc,
    BridgedArrayRef cParameters, BridgedSourceLoc cRightParenLoc) {
  ASTContext &context = cContext.unbridged();
  return ParameterList::create(context, cLeftParenLoc.unbridged(),
                               cParameters.unbridged<ParamDecl *>(),
                               cRightParenLoc.unbridged());
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"

void BridgedTopLevelCodeDecl_dump(void *decl) {
  static_cast<TopLevelCodeDecl *>(decl)->dump(llvm::errs());
}
void BridgedExpr_dump(void *expr) {
  static_cast<Expr *>(expr)->dump(llvm::errs());
}
void BridgedDecl_dump(void *decl) {
  static_cast<Decl *>(decl)->dump(llvm::errs());
}
void BridgedStmt_dump(void *statement) {
  static_cast<Stmt *>(statement)->dump(llvm::errs());
}
void BridgedTypeRepr_dump(void *type) { static_cast<TypeRepr *>(type)->dump(); }

#pragma clang diagnostic pop

//===----------------------------------------------------------------------===//
// MARK: Plugins
//===----------------------------------------------------------------------===//

PluginCapabilityPtr Plugin_getCapability(PluginHandle handle) {
  auto *plugin = static_cast<LoadedExecutablePlugin *>(handle);
  return plugin->getCapability();
}

void Plugin_setCapability(PluginHandle handle, PluginCapabilityPtr data) {
  auto *plugin = static_cast<LoadedExecutablePlugin *>(handle);
  plugin->setCapability(data);
}

const char *Plugin_getExecutableFilePath(PluginHandle handle) {
  auto *plugin = static_cast<LoadedExecutablePlugin *>(handle);
  return plugin->getExecutablePath().data();
}

void Plugin_lock(PluginHandle handle) {
  auto *plugin = static_cast<LoadedExecutablePlugin *>(handle);
  plugin->lock();
}

void Plugin_unlock(PluginHandle handle) {
  auto *plugin = static_cast<LoadedExecutablePlugin *>(handle);
  plugin->unlock();
}

bool Plugin_spawnIfNeeded(PluginHandle handle) {
  auto *plugin = static_cast<LoadedExecutablePlugin *>(handle);
  auto error = plugin->spawnIfNeeded();
  bool hadError(error);
  llvm::consumeError(std::move(error));
  return hadError;
}

bool Plugin_sendMessage(PluginHandle handle, const BridgedData data) {
  auto *plugin = static_cast<LoadedExecutablePlugin *>(handle);
  StringRef message(data.BaseAddress, data.Length);
  auto error = plugin->sendMessage(message);
  if (error) {
    // FIXME: Pass the error message back to the caller.
    llvm::consumeError(std::move(error));
    //    llvm::handleAllErrors(std::move(error), [](const llvm::ErrorInfoBase
    //    &err) {
    //      llvm::errs() << err.message() << "\n";
    //    });
    return true;
  }
  return false;
}

bool Plugin_waitForNextMessage(PluginHandle handle, BridgedData *out) {
  auto *plugin = static_cast<LoadedExecutablePlugin *>(handle);
  auto result = plugin->waitForNextMessage();
  if (!result) {
    // FIXME: Pass the error message back to the caller.
    llvm::consumeError(result.takeError());
    //    llvm::handleAllErrors(result.takeError(), [](const llvm::ErrorInfoBase
    //    &err) {
    //      llvm::errs() << err.message() << "\n";
    //    });
    return true;
  }
  auto &message = result.get();
  auto size = message.size();
  auto outPtr = malloc(size);
  memcpy(outPtr, message.data(), size);
  *out = BridgedData{(const char *)outPtr, size};
  return false;
}
