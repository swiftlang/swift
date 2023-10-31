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

template <typename T>
static inline llvm::ArrayRef<T>
unbridgedArrayRef(const BridgedArrayRef bridged) {
  return bridged.get<T>();
}

static inline StringRef unbridged(BridgedStringRef cStr) {
  return cStr.get();
}

static inline ASTContext &unbridged(BridgedASTContext cContext) {
  return *static_cast<ASTContext *>(cContext.raw);
}

static inline SourceLoc unbridged(BridgedSourceLoc cLoc) {
  return cLoc.get();
}

static inline SourceRange unbridged(BridgedSourceRange cRange) {
  return SourceRange(unbridged(cRange.startLoc), unbridged(cRange.endLoc));
}

static inline Identifier unbridged(BridgedIdentifier cIdentifier) {
  return Identifier::getFromOpaquePointer(const_cast<void *>(cIdentifier.raw));
}

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
  StringRef str = unbridged(cStr);
  if (str.size() == 1 && str.front() == '_')
    return BridgedIdentifier();

  // If this was a back-ticked identifier, drop the back-ticks.
  if (str.size() >= 2 && str.front() == '`' && str.back() == '`') {
    str = str.drop_front().drop_back();
  }

  return {unbridged(cContext).getIdentifier(str).getAsOpaquePointer()};
}

bool BridgedASTContext_langOptsHasFeature(BridgedASTContext cContext,
                                          BridgedFeature feature) {
  return unbridged(cContext).LangOpts.hasFeature((Feature)feature);
}

//===----------------------------------------------------------------------===//
// MARK: AST nodes
//===----------------------------------------------------------------------===//

// Define `bridged` overloads for each AST node.
#define AST_BRIDGING_WRAPPER(Name)                                             \
  [[maybe_unused]]                                                             \
  static Bridged##Name bridged(Name *raw) {                                    \
    return {raw};                                                              \
  }                                                                            \
// Don't define `bridged` overloads for the TypeRepr subclasses, since we always
// return a BridgedTypeRepr currently, only define `bridged(TypeRepr *)`.
#define TYPEREPR(Id, Parent)
#include "swift/AST/ASTBridgingWrappers.def"

// Define `unbridged` overloads for each AST node.
#define AST_BRIDGING_WRAPPER(Name)                                             \
  [[maybe_unused]] static Name *unbridged(Bridged##Name bridged) {             \
    return bridged.get();                                                      \
  }
#include "swift/AST/ASTBridgingWrappers.def"

#define AST_BRIDGING_WRAPPER_NULLABLE(Name)                                    \
  [[maybe_unused]] static Name *unbridged(BridgedNullable##Name bridged) {     \
    return bridged.get();                                                      \
  }
#define AST_BRIDGING_WRAPPER_NONNULL(Name)
#include "swift/AST/ASTBridgingWrappers.def"

// Define `.asDecl` on each BridgedXXXDecl type.
#define DECL(Id, Parent)                                                       \
  BridgedDecl Bridged##Id##Decl_asDecl(Bridged##Id##Decl decl) {               \
    return bridged(static_cast<Decl *>(unbridged(decl)));                      \
  }
#define ABSTRACT_DECL(Id, Parent) DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

// Define `.asDeclContext` on each BridgedXXXDecl type that's also a
// DeclContext.
#define DECL(Id, Parent)
#define CONTEXT_DECL(Id, Parent)                                               \
  BridgedDeclContext Bridged##Id##Decl_asDeclContext(Bridged##Id##Decl decl) { \
    return bridged(static_cast<DeclContext *>(unbridged(decl)));               \
  }
#define ABSTRACT_CONTEXT_DECL(Id, Parent) CONTEXT_DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

// Define `.asStmt` on each BridgedXXXStmt type.
#define STMT(Id, Parent)                                                       \
  BridgedStmt Bridged##Id##Stmt_asStmt(Bridged##Id##Stmt stmt) {               \
    return bridged(static_cast<Stmt *>(unbridged(stmt)));                      \
  }
#define ABSTRACT_STMT(Id, Parent) STMT(Id, Parent)
#include "swift/AST/StmtNodes.def"

// Define `.asExpr` on each BridgedXXXExpr type.
#define EXPR(Id, Parent)                                                       \
  BridgedExpr Bridged##Id##Expr_asExpr(Bridged##Id##Expr expr) {               \
    return bridged(static_cast<Expr *>(unbridged(expr)));                      \
  }
#define ABSTRACT_EXPR(Id, Parent) EXPR(Id, Parent)
#include "swift/AST/ExprNodes.def"

// Define `.asTypeRepr` on each BridgedXXXTypeRepr type.
#define TYPEREPR(Id, Parent)                                                   \
  BridgedTypeRepr Bridged##Id##TypeRepr_asTypeRepr(                            \
      Bridged##Id##TypeRepr typeRepr) {                                        \
    return bridged(static_cast<TypeRepr *>(unbridged(typeRepr)));              \
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
    : BridgedDiagnosticArgument(DiagnosticArgument(s.get())) {}

static_assert(sizeof(BridgedDiagnosticFixIt) >= sizeof(DiagnosticInfo::FixIt),
              "BridgedDiagnosticFixIt has wrong size");

BridgedDiagnosticFixIt::BridgedDiagnosticFixIt(BridgedSourceLoc start,
                                               uint32_t length,
                                               BridgedStringRef text)
    : BridgedDiagnosticFixIt(DiagnosticInfo::FixIt(
          CharSourceRange(start.get(), length), text.get(),
          llvm::ArrayRef<DiagnosticArgument>())) {}

void BridgedDiagnosticEngine_diagnose(
    BridgedDiagnosticEngine bridgedEngine, BridgedSourceLoc loc,
    BridgedDiagID bridgedDiagID,
    BridgedArrayRef /*BridgedDiagnosticArgument*/ bridgedArguments,
    BridgedSourceLoc highlightStart, uint32_t hightlightLength,
    BridgedArrayRef /*BridgedDiagnosticFixIt*/ bridgedFixIts) {
  auto *D = bridgedEngine.get();

  auto diagID = static_cast<DiagID>(bridgedDiagID);
  SmallVector<DiagnosticArgument, 2> arguments;
  for (auto arg : bridgedArguments.get<BridgedDiagnosticArgument>()) {
    arguments.push_back(arg.get());
  }
  auto inflight = D->diagnose(loc.get(), diagID, arguments);

  // Add highlight.
  if (highlightStart.get().isValid()) {
    CharSourceRange highlight(highlightStart.get(), (unsigned)hightlightLength);
    inflight.highlightChars(highlight.getStart(), highlight.getEnd());
  }

  // Add fix-its.
  for (const BridgedDiagnosticFixIt &fixIt :
       bridgedFixIts.get<BridgedDiagnosticFixIt>()) {
    auto range = fixIt.get().getRange();
    auto text = fixIt.get().getText();
    inflight.fixItReplaceChars(range.getStart(), range.getEnd(), text);
  }
}

bool BridgedDiagnosticEngine_hadAnyError(
    BridgedDiagnosticEngine bridgedEngine) {
  return bridgedEngine.get()->hadAnyError();
}

namespace {
struct BridgedDiagnosticImpl {
  typedef llvm::MallocAllocator Allocator;

  InFlightDiagnostic inFlight;
  std::vector<StringRef> textBlobs;

  BridgedDiagnosticImpl(InFlightDiagnostic inFlight,
                        std::vector<StringRef> textBlobs)
      : inFlight(std::move(inFlight)), textBlobs(std::move(textBlobs)) {}

  BridgedDiagnosticImpl(const BridgedDiagnosticImpl &) = delete;
  BridgedDiagnosticImpl(BridgedDiagnosticImpl &&) = delete;
  BridgedDiagnosticImpl &operator=(const BridgedDiagnosticImpl &) = delete;
  BridgedDiagnosticImpl &operator=(BridgedDiagnosticImpl &&) = delete;

  ~BridgedDiagnosticImpl() {
    inFlight.flush();

    Allocator allocator;
    for (auto text : textBlobs) {
      allocator.Deallocate(text.data(), text.size());
    }
  }
};
} // namespace

static inline BridgedDiagnosticImpl *unbridged(BridgedDiagnostic cDiag) {
  return static_cast<BridgedDiagnosticImpl *>(cDiag.raw);
}

BridgedDiagnostic BridgedDiagnostic_create(BridgedSourceLoc cLoc,
                                           BridgedStringRef cText,
                                           BridgedDiagnosticSeverity severity,
                                           BridgedDiagnosticEngine cDiags) {
  StringRef origText = unbridged(cText);
  BridgedDiagnosticImpl::Allocator alloc;
  StringRef text = origText.copy(alloc);

  SourceLoc loc = unbridged(cLoc);

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

  DiagnosticEngine &diags = *unbridged(cDiags);
  return {new BridgedDiagnosticImpl{diags.diagnose(loc, diagID, text), {text}}};
}

/// Highlight a source range as part of the diagnostic.
void BridgedDiagnostic_highlight(BridgedDiagnostic cDiag,
                                 BridgedSourceLoc cStartLoc,
                                 BridgedSourceLoc cEndLoc) {
  SourceLoc startLoc = unbridged(cStartLoc);
  SourceLoc endLoc = unbridged(cEndLoc);

  BridgedDiagnosticImpl *diag = unbridged(cDiag);
  diag->inFlight.highlightChars(startLoc, endLoc);
}

/// Add a Fix-It to replace a source range as part of the diagnostic.
void BridgedDiagnostic_fixItReplace(BridgedDiagnostic cDiag,
                                    BridgedSourceLoc cStartLoc,
                                    BridgedSourceLoc cEndLoc,
                                    BridgedStringRef cReplaceText) {

  SourceLoc startLoc = unbridged(cStartLoc);
  SourceLoc endLoc = unbridged(cEndLoc);

  StringRef origReplaceText = unbridged(cReplaceText);
  BridgedDiagnosticImpl::Allocator alloc;
  StringRef replaceText = origReplaceText.copy(alloc);

  BridgedDiagnosticImpl *diag = unbridged(cDiag);
  diag->textBlobs.push_back(replaceText);
  diag->inFlight.fixItReplaceChars(startLoc, endLoc, replaceText);
}

/// Finish the given diagnostic and emit it.
void BridgedDiagnostic_finish(BridgedDiagnostic cDiag) {
  BridgedDiagnosticImpl *diag = unbridged(cDiag);
  delete diag;
}

//===----------------------------------------------------------------------===//
// MARK: Decls
//===----------------------------------------------------------------------===//

BridgedPatternBindingDecl BridgedPatternBindingDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cBindingKeywordLoc, BridgedExpr nameExpr,
    BridgedExpr initExpr, bool isStatic, bool isLet) {
  ASTContext &context = unbridged(cContext);
  DeclContext *declContext = unbridged(cDeclContext);

  auto *name = cast<UnresolvedDeclRefExpr>(unbridged(nameExpr));
  auto *varDecl = new (context) VarDecl(
      isStatic, isLet ? VarDecl::Introducer::Let : VarDecl::Introducer::Var,
      name->getLoc(), name->getName().getBaseIdentifier(), declContext);
  auto *pattern = new (context) NamedPattern(varDecl);
  auto *PBD = PatternBindingDecl::create(
      context,
      /*StaticLoc=*/SourceLoc(), // FIXME
      isStatic ? StaticSpellingKind::KeywordStatic : StaticSpellingKind::None,
      unbridged(cBindingKeywordLoc), pattern,
      /*EqualLoc=*/SourceLoc(), // FIXME
      unbridged(initExpr), declContext);
  return bridged(PBD);
}

BridgedParamDecl BridgedParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cSpecifierLoc, BridgedIdentifier cFirstName,
    BridgedSourceLoc cFirstNameLoc, BridgedIdentifier cSecondName,
    BridgedSourceLoc cSecondNameLoc, BridgedNullableTypeRepr opaqueType,
    BridgedNullableExpr opaqueDefaultValue) {
  assert(cSecondNameLoc.get().isValid() == (bool)cSecondName.raw);
  if (!cSecondName.raw) {
    cSecondName = cFirstName;
    cSecondNameLoc = cFirstNameLoc;
  }

  auto *declContext = unbridged(cDeclContext);

  auto *defaultValue = unbridged(opaqueDefaultValue);
  DefaultArgumentKind defaultArgumentKind;

  if (declContext->getParentSourceFile()->Kind == SourceFileKind::Interface &&
      isa<SuperRefExpr>(defaultValue)) {
    defaultValue = nullptr;
    defaultArgumentKind = DefaultArgumentKind::Inherited;
  } else {
    defaultArgumentKind = getDefaultArgKind(defaultValue);
  }

  auto *paramDecl = new (unbridged(cContext)) ParamDecl(
      unbridged(cSpecifierLoc), unbridged(cFirstNameLoc), unbridged(cFirstName),
      unbridged(cSecondNameLoc), unbridged(cSecondName), declContext);
  paramDecl->setTypeRepr(unbridged(opaqueType));
  paramDecl->setDefaultExpr(defaultValue, /*isTypeChecked*/ false);
  paramDecl->setDefaultArgumentKind(defaultArgumentKind);

  return bridged(paramDecl);
}

void BridgedConstructorDecl_setParsedBody(BridgedConstructorDecl decl,
                                          BridgedBraceStmt body) {
  unbridged(decl)->setBody(unbridged(body), FuncDecl::BodyKind::Parsed);
}

void BridgedFuncDecl_setParsedBody(BridgedFuncDecl decl,
                                   BridgedBraceStmt body) {
  unbridged(decl)->setBody(unbridged(body), FuncDecl::BodyKind::Parsed);
}

void BridgedDestructorDecl_setParsedBody(BridgedDestructorDecl decl,
                                         BridgedBraceStmt body) {
  unbridged(decl)->setBody(unbridged(body), FuncDecl::BodyKind::Parsed);
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
  ASTContext &context = unbridged(cContext);

  auto *paramList = unbridged(parameterList);
  auto declName = DeclName(context, unbridged(cName), paramList);
  auto asyncLoc = unbridged(cAsyncLoc);
  auto throwsLoc = unbridged(cThrowsLoc);
  // FIXME: rethrows

  auto *decl = FuncDecl::create(
      context, unbridged(cStaticLoc), StaticSpellingKind::None,
      unbridged(cFuncKeywordLoc), declName, unbridged(cNameLoc),
      asyncLoc.isValid(), asyncLoc, throwsLoc.isValid(), throwsLoc,
      unbridged(thrownType), unbridged(genericParamList), paramList,
      unbridged(returnType), unbridged(cDeclContext));
  decl->setTrailingWhereClause(unbridged(genericWhereClause));

  return bridged(decl);
}

BridgedConstructorDecl BridgedConstructorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cInitKeywordLoc, BridgedSourceLoc cFailabilityMarkLoc,
    bool isIUO, BridgedNullableGenericParamList genericParams,
    BridgedParameterList bridgedParameterList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr thrownType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  assert(cFailabilityMarkLoc.get().isValid() || !isIUO);

  ASTContext &context = unbridged(cContext);

  auto *parameterList = unbridged(bridgedParameterList);
  auto declName =
      DeclName(context, DeclBaseName::createConstructor(), parameterList);
  auto asyncLoc = unbridged(cAsyncLoc);
  auto throwsLoc = unbridged(cThrowsLoc);
  auto failabilityMarkLoc = unbridged(cFailabilityMarkLoc);
  // FIXME: rethrows

  auto *decl = new (context) ConstructorDecl(
      declName, unbridged(cInitKeywordLoc), failabilityMarkLoc.isValid(),
      failabilityMarkLoc, asyncLoc.isValid(), asyncLoc, throwsLoc.isValid(),
      throwsLoc, unbridged(thrownType), parameterList, unbridged(genericParams),
      unbridged(cDeclContext));
  decl->setTrailingWhereClause(unbridged(genericWhereClause));
  decl->setImplicitlyUnwrappedOptional(isIUO);

  return bridged(decl);
}

BridgedDestructorDecl
BridgedDestructorDecl_createParsed(BridgedASTContext cContext,
                                   BridgedDeclContext cDeclContext,
                                   BridgedSourceLoc cDeinitKeywordLoc) {
  ASTContext &context = unbridged(cContext);
  auto *decl = new (context)
      DestructorDecl(unbridged(cDeinitKeywordLoc), unbridged(cDeclContext));

  return bridged(decl);
}

BridgedTypeAliasDecl BridgedTypeAliasDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cAliasKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedSourceLoc cEqualLoc, BridgedTypeRepr opaqueUnderlyingType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  ASTContext &context = unbridged(cContext);

  auto *decl = new (context)
      TypeAliasDecl(unbridged(cAliasKeywordLoc), unbridged(cEqualLoc),
                    unbridged(cName), unbridged(cNameLoc),
                    unbridged(genericParamList), unbridged(cDeclContext));
  decl->setUnderlyingTypeRepr(unbridged(opaqueUnderlyingType));
  decl->setTrailingWhereClause(unbridged(genericWhereClause));

  return bridged(decl);
}

static void setParsedMembers(IterableDeclContext *IDC,
                             BridgedArrayRef bridgedMembers) {
  auto &ctx = IDC->getDecl()->getASTContext();

  SmallVector<Decl *> members;
  for (auto *decl : unbridgedArrayRef<Decl *>(bridgedMembers)) {
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
  setParsedMembers(unbridged(bridgedDecl), bridgedMembers);
}

void BridgedExtensionDecl_setParsedMembers(BridgedExtensionDecl bridgedDecl,
                                           BridgedArrayRef bridgedMembers) {
  setParsedMembers(unbridged(bridgedDecl), bridgedMembers);
}

static SmallVector<InheritedEntry>
convertToInheritedEntries(BridgedArrayRef cInheritedTypes) {
  SmallVector<InheritedEntry> inheritedEntries;
  for (auto &repr : unbridgedArrayRef<BridgedTypeRepr>(cInheritedTypes)) {
    inheritedEntries.emplace_back(unbridged(repr));
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
  ASTContext &context = unbridged(cContext);

  NominalTypeDecl *decl = new (context) EnumDecl(
      unbridged(cEnumKeywordLoc), unbridged(cName), unbridged(cNameLoc),
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)),
      unbridged(genericParamList), unbridged(cDeclContext));
  decl->setTrailingWhereClause(unbridged(genericWhereClause));
  decl->setBraces(unbridged(cBraceRange));

  return bridged(decl);
}

BridgedEnumCaseDecl
BridgedEnumCaseDecl_createParsed(BridgedDeclContext cDeclContext,
                                 BridgedSourceLoc cCaseKeywordLoc,
                                 BridgedArrayRef cElements) {
  auto *decl = EnumCaseDecl::create(
      unbridged(cCaseKeywordLoc),
      unbridgedArrayRef<EnumElementDecl *>(cElements), unbridged(cDeclContext));

  return bridged(decl);
}

BridgedEnumElementDecl BridgedEnumElementDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedNullableParameterList bridgedParameterList,
    BridgedSourceLoc cEqualsLoc, BridgedNullableExpr rawValue) {
  ASTContext &context = unbridged(cContext);

  auto *parameterList = unbridged(bridgedParameterList);
  DeclName declName;
  {
    auto identifier = unbridged(cName);
    if (parameterList) {
      declName = DeclName(context, identifier, parameterList);
    } else {
      declName = identifier;
    }
  }

  auto *EED = new (context) EnumElementDecl(
      unbridged(cNameLoc), declName, parameterList, unbridged(cEqualsLoc),
      cast_or_null<LiteralExpr>(unbridged(rawValue)), unbridged(cDeclContext));
  return bridged(EED);
}

BridgedNominalTypeDecl BridgedStructDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStructKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange) {
  ASTContext &context = unbridged(cContext);

  NominalTypeDecl *decl = new (context) StructDecl(
      unbridged(cStructKeywordLoc), unbridged(cName), unbridged(cNameLoc),
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)),
      unbridged(genericParamList), unbridged(cDeclContext));
  decl->setTrailingWhereClause(unbridged(genericWhereClause));
  decl->setBraces(unbridged(cBraceRange));

  return bridged(decl);
}

BridgedNominalTypeDecl BridgedClassDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cClassKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange, bool isActor) {
  ASTContext &context = unbridged(cContext);

  NominalTypeDecl *decl = new (context) ClassDecl(
      unbridged(cClassKeywordLoc), unbridged(cName), unbridged(cNameLoc),
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)),
      unbridged(genericParamList), unbridged(cDeclContext), isActor);
  decl->setTrailingWhereClause(unbridged(genericWhereClause));
  decl->setBraces(unbridged(cBraceRange));

  return bridged(decl);
}

BridgedNominalTypeDecl BridgedProtocolDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cProtocolKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedArrayRef cPrimaryAssociatedTypeNames,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange) {
  SmallVector<PrimaryAssociatedTypeName, 2> primaryAssociatedTypeNames;
  for (auto &pair : unbridgedArrayRef<BridgedIdentifierAndSourceLoc>(
           cPrimaryAssociatedTypeNames)) {
    primaryAssociatedTypeNames.emplace_back(unbridged(pair.name),
                                            unbridged(pair.nameLoc));
  }

  ASTContext &context = unbridged(cContext);
  NominalTypeDecl *decl = new (context) ProtocolDecl(
      unbridged(cDeclContext), unbridged(cProtocolKeywordLoc),
      unbridged(cNameLoc), unbridged(cName),
      context.AllocateCopy(primaryAssociatedTypeNames),
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)),
      unbridged(genericWhereClause));
  decl->setBraces(unbridged(cBraceRange));

  return bridged(decl);
}

BridgedAssociatedTypeDecl BridgedAssociatedTypeDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cAssociatedtypeKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedArrayRef cInheritedTypes,
    BridgedNullableTypeRepr defaultType,
    BridgedNullableTrailingWhereClause genericWhereClause) {
  ASTContext &context = unbridged(cContext);

  auto *decl = AssociatedTypeDecl::createParsed(
      context, unbridged(cDeclContext), unbridged(cAssociatedtypeKeywordLoc),
      unbridged(cName), unbridged(cNameLoc), unbridged(defaultType),
      unbridged(genericWhereClause));
  decl->setInherited(
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)));

  return bridged(decl);
}

BridgedExtensionDecl BridgedExtensionDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cExtensionKeywordLoc, BridgedTypeRepr extendedType,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange) {
  ASTContext &context = unbridged(cContext);

  auto *decl = ExtensionDecl::create(
      context, unbridged(cExtensionKeywordLoc), unbridged(extendedType),
      context.AllocateCopy(convertToInheritedEntries(cInheritedTypes)),
      unbridged(cDeclContext), unbridged(genericWhereClause));
  decl->setBraces(unbridged(cBraceRange));
  return bridged(decl);
}

BridgedOperatorDecl BridgedOperatorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedOperatorFixity cFixity, BridgedSourceLoc cOperatorKeywordLoc,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedSourceLoc cColonLoc, BridgedIdentifier cPrecedenceGroupName,
    BridgedSourceLoc cPrecedenceGroupLoc) {
  auto colonLoc = cColonLoc.get();
  assert(colonLoc.isValid() == (bool)cPrecedenceGroupName.raw);
  assert(colonLoc.isValid() == cPrecedenceGroupLoc.get().isValid());

  ASTContext &context = unbridged(cContext);
  auto operatorKeywordLoc = unbridged(cOperatorKeywordLoc);
  auto name = unbridged(cName);
  auto nameLoc = unbridged(cNameLoc);
  auto *declContext = unbridged(cDeclContext);

  OperatorDecl *decl = nullptr;
  switch (cFixity) {
  case BridgedOperatorFixityInfix:
    decl = new (context) InfixOperatorDecl(
        declContext, operatorKeywordLoc, name, nameLoc, unbridged(cColonLoc),
        unbridged(cPrecedenceGroupName), unbridged(cPrecedenceGroupLoc));
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

  return bridged(decl);
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
       unbridgedArrayRef<BridgedIdentifierAndSourceLoc>(cHigherThanNames)) {
    higherThanNames.push_back(
        {unbridged(pair.nameLoc), unbridged(pair.name), nullptr});
  }

  SmallVector<PrecedenceGroupDecl::Relation, 2> lowerThanNames;
  for (auto &pair :
       unbridgedArrayRef<BridgedIdentifierAndSourceLoc>(cLowerThanNames)) {
    lowerThanNames.push_back(
        {unbridged(pair.nameLoc), unbridged(pair.name), nullptr});
  }

  auto *decl = PrecedenceGroupDecl::create(
      unbridged(cDeclContext), unbridged(cPrecedencegroupKeywordLoc),
      unbridged(cNameLoc), unbridged(cName), unbridged(cLeftBraceLoc),
      unbridged(cAssociativityKeywordLoc), unbridged(cAssociativityValueLoc),
      static_cast<Associativity>(cAssociativity),
      unbridged(cAssignmentKeywordLoc), unbridged(cAssignmentValueLoc),
      isAssignment, unbridged(cHigherThanKeywordLoc), higherThanNames,
      unbridged(cLowerThanKeywordLoc), lowerThanNames,
      unbridged(cRightBraceLoc));

  return bridged(decl);
}

BridgedImportDecl BridgedImportDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cImportKeywordLoc, BridgedImportKind cImportKind,
    BridgedSourceLoc cImportKindLoc, BridgedArrayRef cImportPathElements) {
  ImportPath::Builder builder;
  for (auto &element :
       unbridgedArrayRef<BridgedIdentifierAndSourceLoc>(cImportPathElements)) {
    builder.push_back(unbridged(element.name), unbridged(element.nameLoc));
  }

  ASTContext &context = unbridged(cContext);
  auto *decl = ImportDecl::create(
      context, unbridged(cDeclContext), unbridged(cImportKeywordLoc),
      static_cast<ImportKind>(cImportKind), unbridged(cImportKindLoc),
      std::move(builder).get());

  return bridged(decl);
}

BridgedTopLevelCodeDecl BridgedTopLevelCodeDecl_createStmt(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStartLoc, BridgedStmt statement,
    BridgedSourceLoc cEndLoc) {
  ASTContext &context = unbridged(cContext);
  DeclContext *declContext = unbridged(cDeclContext);

  auto *S = unbridged(statement);
  auto Brace =
      BraceStmt::create(context, unbridged(cStartLoc), {S}, unbridged(cEndLoc),
                        /*Implicit=*/true);
  return bridged(new (context) TopLevelCodeDecl(declContext, Brace));
}

BridgedTopLevelCodeDecl BridgedTopLevelCodeDecl_createExpr(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStartLoc, BridgedExpr expression,
    BridgedSourceLoc cEndLoc) {
  ASTContext &context = unbridged(cContext);
  DeclContext *declContext = unbridged(cDeclContext);

  auto *E = unbridged(expression);
  auto Brace =
      BraceStmt::create(context, unbridged(cStartLoc), {E}, unbridged(cEndLoc),
                        /*Implicit=*/true);
  return bridged(new (context) TopLevelCodeDecl(declContext, Brace));
}

//===----------------------------------------------------------------------===//
// MARK: BridgedNominalTypeDecl
//===----------------------------------------------------------------------===//

bool BridgedNominalTypeDecl_isStructWithUnreferenceableStorage(
    BridgedNominalTypeDecl decl) {
  if (auto *structDecl = dyn_cast<swift::StructDecl>(decl.get())) {
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

  ASTContext &context = unbridged(cContext);
  DeclContext *declContext = unbridged(cDeclContext);

  auto params = ParameterList::create(context, inLoc, {}, inLoc);

  auto *out = new (context) ClosureExpr(
      attributes, bracketRange, nullptr, nullptr, asyncLoc, throwsLoc,
      /*FIXME:thrownType=*/nullptr, arrowLoc, inLoc, nullptr, declContext);
  out->setBody(unbridged(body), true);
  out->setParameterList(params);
  return bridged(out);
}

BridgedSequenceExpr BridgedSequenceExpr_createParsed(BridgedASTContext cContext,
                                                     BridgedArrayRef exprs) {
  auto *SE = SequenceExpr::create(unbridged(cContext),
                                  unbridgedArrayRef<Expr *>(exprs));
  return bridged(SE);
}

BridgedTupleExpr BridgedTupleExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLParen,
                                               BridgedArrayRef subs,
                                               BridgedArrayRef names,
                                               BridgedArrayRef cNameLocs,
                                               BridgedSourceLoc cRParen) {
  ASTContext &context = unbridged(cContext);
  auto *TE = TupleExpr::create(context, unbridged(cLParen),
                               unbridgedArrayRef<Expr *>(subs),
                               unbridgedArrayRef<Identifier>(names),
                               unbridgedArrayRef<SourceLoc>(cNameLocs),
                               unbridged(cRParen), /*Implicit*/ false);
  return bridged(TE);
}

BridgedCallExpr BridgedCallExpr_createParsed(BridgedASTContext cContext,
                                             BridgedExpr fn,
                                             BridgedTupleExpr args) {
  ASTContext &context = unbridged(cContext);
  TupleExpr *TE = unbridged(args);
  SmallVector<Argument, 8> arguments;
  for (unsigned i = 0; i < TE->getNumElements(); ++i) {
    arguments.emplace_back(TE->getElementNameLoc(i), TE->getElementName(i),
                           TE->getElement(i));
  }
  auto *argList = ArgumentList::create(context, TE->getLParenLoc(), arguments,
                                       TE->getRParenLoc(), llvm::None,
                                       /*isImplicit*/ false);
  auto *CE = CallExpr::create(context, unbridged(fn), argList,
                              /*implicit*/ false);
  return bridged(CE);
}

BridgedUnresolvedDeclRefExpr BridgedUnresolvedDeclRefExpr_createParsed(
    BridgedASTContext cContext, BridgedIdentifier base, BridgedSourceLoc cLoc) {
  ASTContext &context = unbridged(cContext);
  auto name = DeclNameRef{unbridged(base)};
  auto *E = new (context) UnresolvedDeclRefExpr(name, DeclRefKind::Ordinary,
                                                DeclNameLoc{unbridged(cLoc)});
  return bridged(E);
}

BridgedStringLiteralExpr
BridgedStringLiteralExpr_createParsed(BridgedASTContext cContext,
                                      BridgedStringRef cStr,
                                      BridgedSourceLoc cTokenLoc) {
  ASTContext &context = unbridged(cContext);
  auto str = context.AllocateCopy(unbridged(cStr));
  return bridged(new (context) StringLiteralExpr(str, unbridged(cTokenLoc)));
}

BridgedIntegerLiteralExpr
BridgedIntegerLiteralExpr_createParsed(BridgedASTContext cContext,
                                       BridgedStringRef cStr,
                                       BridgedSourceLoc cTokenLoc) {
  ASTContext &context = unbridged(cContext);
  auto str = context.AllocateCopy(unbridged(cStr));
  return bridged(new (context) IntegerLiteralExpr(str, unbridged(cTokenLoc)));
}

BridgedArrayExpr BridgedArrayExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLLoc,
                                               BridgedArrayRef elements,
                                               BridgedArrayRef commas,
                                               BridgedSourceLoc cRLoc) {
  ASTContext &context = unbridged(cContext);
  auto *AE = ArrayExpr::create(
      context, unbridged(cLLoc), unbridgedArrayRef<Expr *>(elements),
      unbridgedArrayRef<SourceLoc>(commas), unbridged(cRLoc));
  return bridged(AE);
}

BridgedBooleanLiteralExpr
BridgedBooleanLiteralExpr_createParsed(BridgedASTContext cContext, bool value,
                                       BridgedSourceLoc cTokenLoc) {
  ASTContext &context = unbridged(cContext);
  return bridged(new (context) BooleanLiteralExpr(value, unbridged(cTokenLoc)));
}

BridgedNilLiteralExpr
BridgedNilLiteralExpr_createParsed(BridgedASTContext cContext,
                                   BridgedSourceLoc cNilKeywordLoc) {
  auto *e = new (unbridged(cContext)) NilLiteralExpr(unbridged(cNilKeywordLoc));
  return bridged(e);
}

BridgedSingleValueStmtExpr BridgedSingleValueStmtExpr_createWithWrappedBranches(
    BridgedASTContext cContext, BridgedStmt S, BridgedDeclContext cDeclContext,
    bool mustBeExpr) {
  ASTContext &context = unbridged(cContext);
  DeclContext *declContext = unbridged(cDeclContext);
  auto *SVE = SingleValueStmtExpr::createWithWrappedBranches(
      context, unbridged(S), declContext, mustBeExpr);
  return bridged(SVE);
}

BridgedUnresolvedDotExpr BridgedUnresolvedDotExpr_createParsed(
    BridgedASTContext cContext, BridgedExpr base, BridgedSourceLoc cDotLoc,
    BridgedIdentifier name, BridgedSourceLoc cNameLoc) {
  ASTContext &context = unbridged(cContext);
  auto *UDE = new (context) UnresolvedDotExpr(
      unbridged(base), unbridged(cDotLoc), DeclNameRef(unbridged(name)),
      DeclNameLoc(unbridged(cNameLoc)), false);
  return bridged(UDE);
}

//===----------------------------------------------------------------------===//
// MARK: Stmts
//===----------------------------------------------------------------------===//

BridgedBraceStmt BridgedBraceStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLBLoc,
                                               BridgedArrayRef elements,
                                               BridgedSourceLoc cRBLoc) {
  llvm::SmallVector<ASTNode, 6> nodes;
  for (auto node : unbridgedArrayRef<BridgedASTNode>(elements)) {
    if (node.kind == ASTNodeKindExpr) {
      auto expr = (Expr *)node.ptr;
      nodes.push_back(expr);
    } else if (node.kind == ASTNodeKindStmt) {
      auto stmt = (Stmt *)node.ptr;
      nodes.push_back(stmt);
    } else {
      assert(node.kind == ASTNodeKindDecl);
      auto decl = (Decl *)node.ptr;
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

  ASTContext &context = unbridged(cContext);
  auto *BS = BraceStmt::create(context, unbridged(cLBLoc),
                               context.AllocateCopy(nodes), unbridged(cRBLoc));
  return bridged(BS);
}

BridgedIfStmt BridgedIfStmt_createParsed(BridgedASTContext cContext,
                                         BridgedSourceLoc cIfLoc,
                                         BridgedExpr cond, BridgedStmt then,
                                         BridgedSourceLoc cElseLoc,
                                         BridgedNullableStmt elseStmt) {
  ASTContext &context = unbridged(cContext);
  auto *IS = new (context)
      IfStmt(unbridged(cIfLoc), unbridged(cond), unbridged(then),
             unbridged(cElseLoc), unbridged(elseStmt), llvm::None, context);
  return bridged(IS);
}

BridgedReturnStmt BridgedReturnStmt_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cLoc,
                                                 BridgedNullableExpr expr) {
  ASTContext &context = unbridged(cContext);
  return bridged(new (context) ReturnStmt(unbridged(cLoc), unbridged(expr)));
}

//===----------------------------------------------------------------------===//
// MARK: TypeAttributes
//===----------------------------------------------------------------------===//

BridgedTypeAttrKind BridgedTypeAttrKind_fromString(BridgedStringRef cStr) {
  TypeAttrKind kind = TypeAttributes::getAttrKindFromString(unbridged(cStr));
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
  TypeAttributes *typeAttributes = unbridged(cAttributes);
  typeAttributes->setAttr(unbridged(cKind), unbridged(cAttrLoc));
  if (typeAttributes->AtLoc.isInvalid())
    typeAttributes->AtLoc = unbridged(cAtLoc);
}

//===----------------------------------------------------------------------===//
// MARK: TypeReprs
//===----------------------------------------------------------------------===//

BridgedTypeRepr BridgedSimpleIdentTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLoc, BridgedIdentifier id) {
  ASTContext &context = unbridged(cContext);
  auto *SI = new (context) SimpleIdentTypeRepr(DeclNameLoc(unbridged(cLoc)),
                                               DeclNameRef(unbridged(id)));
  return bridged(SI);
}

BridgedTypeRepr BridgedGenericIdentTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedIdentifier name,
    BridgedSourceLoc cNameLoc, BridgedArrayRef genericArgs,
    BridgedSourceLoc cLAngleLoc, BridgedSourceLoc cRAngleLoc) {
  ASTContext &context = unbridged(cContext);
  auto Loc = DeclNameLoc(unbridged(cNameLoc));
  auto Name = DeclNameRef(unbridged(name));
  SourceLoc lAngleLoc = unbridged(cLAngleLoc);
  SourceLoc rAngleLoc = unbridged(cRAngleLoc);
  auto *GI = GenericIdentTypeRepr::create(
      context, Loc, Name, unbridgedArrayRef<TypeRepr *>(genericArgs),
      SourceRange{lAngleLoc, rAngleLoc});
  return bridged(GI);
}

BridgedTypeRepr
BridgedOptionalTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr base,
                                     BridgedSourceLoc cQuestionLoc) {
  ASTContext &context = unbridged(cContext);
  return bridged(
      new (context) OptionalTypeRepr(unbridged(base), unbridged(cQuestionLoc)));
}

BridgedTypeRepr BridgedImplicitlyUnwrappedOptionalTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedSourceLoc cExclamationLoc) {
  ASTContext &context = unbridged(cContext);
  auto *IUO = new (context) ImplicitlyUnwrappedOptionalTypeRepr(
      unbridged(base), unbridged(cExclamationLoc));
  return bridged(IUO);
}

BridgedTypeRepr BridgedArrayTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedSourceLoc cLSquareLoc, BridgedSourceLoc cRSquareLoc) {
  ASTContext &context = unbridged(cContext);
  SourceLoc lSquareLoc = unbridged(cLSquareLoc);
  SourceLoc rSquareLoc = unbridged(cRSquareLoc);
  auto *ATR = new (context)
      ArrayTypeRepr(unbridged(base), SourceRange{lSquareLoc, rSquareLoc});
  return bridged(ATR);
}

BridgedTypeRepr BridgedDictionaryTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLSquareLoc,
    BridgedTypeRepr keyType, BridgedSourceLoc cColonloc,
    BridgedTypeRepr valueType, BridgedSourceLoc cRSquareLoc) {
  ASTContext &context = unbridged(cContext);
  SourceLoc lSquareLoc = unbridged(cLSquareLoc);
  SourceLoc colonLoc = unbridged(cColonloc);
  SourceLoc rSquareLoc = unbridged(cRSquareLoc);
  auto *DTR = new (context)
      DictionaryTypeRepr(unbridged(keyType), unbridged(valueType), colonLoc,
                         SourceRange{lSquareLoc, rSquareLoc});
  return bridged(DTR);
}

BridgedTypeRepr
BridgedMetatypeTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr baseType,
                                     BridgedSourceLoc cTypeLoc) {
  ASTContext &context = unbridged(cContext);
  SourceLoc tyLoc = unbridged(cTypeLoc);
  return bridged(new (context) MetatypeTypeRepr(unbridged(baseType), tyLoc));
}

BridgedTypeRepr
BridgedProtocolTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr baseType,
                                     BridgedSourceLoc cProtoLoc) {
  ASTContext &context = unbridged(cContext);
  SourceLoc protoLoc = unbridged(cProtoLoc);
  return bridged(new (context) ProtocolTypeRepr(unbridged(baseType), protoLoc));
}

BridgedTypeRepr
BridgedPackExpansionTypeRepr_createParsed(BridgedASTContext cContext,
                                          BridgedTypeRepr base,
                                          BridgedSourceLoc cRepeatLoc) {
  ASTContext &context = unbridged(cContext);
  auto *PE = new (context)
      PackExpansionTypeRepr(unbridged(cRepeatLoc), unbridged(base));
  return bridged(PE);
}

BridgedTypeRepr
BridgedAttributedTypeRepr_createParsed(BridgedASTContext cContext,
                                       BridgedTypeRepr base,
                                       BridgedTypeAttributes cAttributes) {
  TypeAttributes *typeAttributes = unbridged(cAttributes);
  if (typeAttributes->empty())
    return base;

  ASTContext &context = unbridged(cContext);
  auto attributedType =
      new (context) AttributedTypeRepr(*typeAttributes, unbridged(base));
  delete typeAttributes;
  return bridged(attributedType);
}

BridgedTypeRepr BridgedSpecifierTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedAttributedTypeSpecifier specifier, BridgedSourceLoc cSpecifierLoc) {
  ASTContext &context = unbridged(cContext);
  SourceLoc loc = unbridged(cSpecifierLoc);
  TypeRepr *baseType = unbridged(base);
  switch (specifier) {
  case BridgedAttributedTypeSpecifierInOut: {
    auto *OT =
        new (context) OwnershipTypeRepr(baseType, ParamSpecifier::InOut, loc);
    return bridged(OT);
  }
  case BridgedAttributedTypeSpecifierBorrowing: {
    auto *OT = new (context)
        OwnershipTypeRepr(baseType, ParamSpecifier::Borrowing, loc);
    return bridged(OT);
  }
  case BridgedAttributedTypeSpecifierConsuming: {
    auto *OT = new (context)
        OwnershipTypeRepr(baseType, ParamSpecifier::Consuming, loc);
    return bridged(OT);
  }
  case BridgedAttributedTypeSpecifierLegacyShared: {
    auto *OT = new (context)
        OwnershipTypeRepr(baseType, ParamSpecifier::LegacyShared, loc);
    return bridged(OT);
  }
  case BridgedAttributedTypeSpecifierLegacyOwned: {
    auto *OT = new (context)
        OwnershipTypeRepr(baseType, ParamSpecifier::LegacyOwned, loc);
    return bridged(OT);
  }
  case BridgedAttributedTypeSpecifierConst: {
    return bridged(new (context) CompileTimeConstTypeRepr(baseType, loc));
  }
  case BridgedAttributedTypeSpecifierIsolated: {
    return bridged(new (context) IsolatedTypeRepr(baseType, loc));
  }
  }
}

BridgedTypeRepr
BridgedVarargTypeRepr_createParsed(BridgedASTContext cContext,
                                   BridgedTypeRepr base,
                                   BridgedSourceLoc cEllipsisLoc) {
  ASTContext &context = unbridged(cContext);
  SourceLoc ellipsisLoc = unbridged(cEllipsisLoc);
  TypeRepr *baseType = unbridged(base);
  return bridged(new (context) VarargTypeRepr(baseType, ellipsisLoc));
}

BridgedTypeRepr BridgedTupleTypeRepr_createParsed(BridgedASTContext cContext,
                                                  BridgedArrayRef elements,
                                                  BridgedSourceLoc cLParenLoc,
                                                  BridgedSourceLoc cRParenLoc) {
  ASTContext &context = unbridged(cContext);
  SourceLoc lParen = unbridged(cLParenLoc);
  SourceLoc rParen = unbridged(cRParenLoc);

  SmallVector<TupleTypeReprElement, 8> tupleElements;
  for (auto element : unbridgedArrayRef<BridgedTupleTypeElement>(elements)) {
    TupleTypeReprElement elementRepr;
    elementRepr.Name = unbridged(element.Name);
    elementRepr.NameLoc = unbridged(element.NameLoc);
    elementRepr.SecondName = unbridged(element.SecondName);
    elementRepr.SecondNameLoc = unbridged(element.SecondNameLoc);
    elementRepr.UnderscoreLoc = unbridged(element.UnderscoreLoc);
    elementRepr.ColonLoc = unbridged(element.ColonLoc);
    elementRepr.Type = unbridged(element.Type);
    elementRepr.TrailingCommaLoc = unbridged(element.TrailingCommaLoc);
    tupleElements.emplace_back(elementRepr);
  }

  auto *TT = TupleTypeRepr::create(context, tupleElements,
                                   SourceRange{lParen, rParen});
  return bridged(TT);
}

BridgedTypeRepr
BridgedMemberTypeRepr_createParsed(BridgedASTContext cContext,
                                   BridgedTypeRepr baseComponent,
                                   BridgedArrayRef bridgedMemberComponents) {
  ASTContext &context = unbridged(cContext);
  auto memberComponents =
      unbridgedArrayRef<IdentTypeRepr *>(bridgedMemberComponents);

  auto *MT = MemberTypeRepr::create(context, unbridged(baseComponent),
                                    memberComponents);
  return bridged(MT);
}

BridgedTypeRepr
BridgedCompositionTypeRepr_createEmpty(BridgedASTContext cContext,
                                       BridgedSourceLoc cAnyLoc) {
  ASTContext &context = unbridged(cContext);
  SourceLoc anyLoc = unbridged(cAnyLoc);
  return bridged(CompositionTypeRepr::createEmptyComposition(context, anyLoc));
}

BridgedTypeRepr
BridgedCompositionTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedArrayRef cTypes,
                                        BridgedSourceLoc cFirstAmpLoc) {
  ASTContext &context = unbridged(cContext);
  SourceLoc firstAmpLoc = unbridged(cFirstAmpLoc);
  auto types = unbridgedArrayRef<TypeRepr *>(cTypes);
  auto *CT = CompositionTypeRepr::create(
      context, types, types.front()->getStartLoc(),
      SourceRange{firstAmpLoc, types.back()->getEndLoc()});
  return bridged(CT);
}

BridgedTypeRepr BridgedFunctionTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr argsTy,
    BridgedSourceLoc cAsyncLoc, BridgedSourceLoc cThrowsLoc,
    BridgedNullableTypeRepr thrownType, BridgedSourceLoc cArrowLoc,
    BridgedTypeRepr resultType) {
  ASTContext &context = unbridged(cContext);
  auto *FT = new (context) FunctionTypeRepr(
      nullptr, cast<TupleTypeRepr>(unbridged(argsTy)), unbridged(cAsyncLoc),
      unbridged(cThrowsLoc), unbridged(thrownType), unbridged(cArrowLoc),
      unbridged(resultType));
  return bridged(FT);
}

BridgedTypeRepr
BridgedNamedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                              BridgedTypeRepr baseTy) {
  ASTContext &context = unbridged(cContext);
  auto *NR =
      new (context) NamedOpaqueReturnTypeRepr(unbridged(baseTy), nullptr);
  return bridged(NR);
}

BridgedTypeRepr
BridgedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                         BridgedSourceLoc cOpaqueLoc,
                                         BridgedTypeRepr baseTy) {
  ASTContext &context = unbridged(cContext);
  auto *OT = new (context)
      OpaqueReturnTypeRepr(unbridged(cOpaqueLoc), unbridged(baseTy));
  return bridged(OT);
}
BridgedTypeRepr
BridgedExistentialTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cAnyLoc,
                                        BridgedTypeRepr baseTy) {
  ASTContext &context = unbridged(cContext);
  auto *ET =
      new (context) ExistentialTypeRepr(unbridged(cAnyLoc), unbridged(baseTy));
  return bridged(ET);
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
  if (auto *genericWhereClause = unbridged(bridgedGenericWhereClause)) {
    whereLoc = genericWhereClause->getWhereLoc();
    requirements = genericWhereClause->getRequirements();
  }

  auto *GP = GenericParamList::create(
      unbridged(cContext), unbridged(cLeftAngleLoc),
      unbridgedArrayRef<GenericTypeParamDecl *>(cParameters), whereLoc,
      requirements, unbridged(cRightAngleLoc));
  return bridged(GP);
}

BridgedGenericTypeParamDecl BridgedGenericTypeParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cEachLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableTypeRepr bridgedInheritedType,
    size_t index) {
  auto eachLoc = unbridged(cEachLoc);
  auto *decl = GenericTypeParamDecl::createParsed(
      unbridged(cDeclContext), unbridged(cName), unbridged(cNameLoc), eachLoc,
      index,
      /*isParameterPack*/ eachLoc.isValid());

  if (auto *inheritedType = unbridged(bridgedInheritedType)) {
    auto entry = InheritedEntry(inheritedType);
    ASTContext &context = unbridged(cContext);
    decl->setInherited(context.AllocateCopy(llvm::makeArrayRef(entry)));
  }

  return bridged(decl);
}

BridgedTrailingWhereClause
BridgedTrailingWhereClause_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cWhereKeywordLoc,
                                        BridgedArrayRef cRequirements) {
  SmallVector<RequirementRepr> requirements;
  for (auto &cReq : unbridgedArrayRef<BridgedRequirementRepr>(cRequirements)) {
    switch (cReq.Kind) {
    case BridgedRequirementReprKindTypeConstraint:
      requirements.push_back(RequirementRepr::getTypeConstraint(
          unbridged(cReq.FirstType), unbridged(cReq.SeparatorLoc),
          unbridged(cReq.SecondType),
          /*isExpansionPattern*/ false));
      break;
    case BridgedRequirementReprKindSameType:
      requirements.push_back(RequirementRepr::getSameType(
          unbridged(cReq.FirstType), unbridged(cReq.SeparatorLoc),
          unbridged(cReq.SecondType),
          /*isExpansionPattern*/ false));
      break;
    case BridgedRequirementReprKindLayoutConstraint:
      llvm_unreachable("cannot handle layout constraints!");
    }
  }

  SourceLoc whereKeywordLoc = unbridged(cWhereKeywordLoc);
  SourceLoc endLoc;
  if (requirements.empty()) {
    endLoc = whereKeywordLoc;
  } else {
    endLoc = requirements.back().getSourceRange().End;
  }

  auto *TW = TrailingWhereClause::create(unbridged(cContext), whereKeywordLoc,
                                         endLoc, requirements);
  return bridged(TW);
}

BridgedParameterList BridgedParameterList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftParenLoc,
    BridgedArrayRef cParameters, BridgedSourceLoc cRightParenLoc) {
  ASTContext &context = unbridged(cContext);
  auto *PL = ParameterList::create(context, unbridged(cLeftParenLoc),
                                   unbridgedArrayRef<ParamDecl *>(cParameters),
                                   unbridged(cRightParenLoc));
  return bridged(PL);
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
