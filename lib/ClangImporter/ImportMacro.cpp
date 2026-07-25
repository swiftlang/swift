//===--- ImportMacro.cpp - Import Clang preprocessor macros ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements support for translating some kinds of C preprocessor
// macros into Swift declarations.
//
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "SwiftDeclSynthesizer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/Basic/Unicode.h"
#include "swift/ClangImporter/ClangModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/Lex/MacroInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/Parser.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"

using namespace swift;
using namespace importer;

static ValueDecl *
createMacroConstant(ClangImporter::Implementation &Impl,
                    const clang::MacroInfo *macro,
                    Identifier name,
                    DeclContext *dc,
                    Type type,
                    const clang::APValue &value,
                    ConstantConvertKind convertKind,
                    bool isStatic,
                    ClangNode ClangN) {
  Impl.ImportedMacroConstants[macro] = {value, type};
  return SwiftDeclSynthesizer(Impl).createConstant(name, dc, type, value,
                                                   convertKind, isStatic,
                                                   ClangN, AccessLevel::Public);
}

static bool isStringToken(const clang::Token &tok) {
  return tok.is(clang::tok::string_literal) ||
         tok.is(clang::tok::utf8_string_literal);
}

static ValueDecl *importStringConstant(ClangImporter::Implementation &Impl,
                                       DeclContext *DC, Identifier name,
                                       const clang::StringLiteral *literal,
                                       ClangNode ClangN) {
  if (!literal->isOrdinary() && !literal->isUTF8())
    return nullptr;

  StringRef text = literal->getString();
  if (!unicode::isWellFormedUTF8(text))
    return nullptr;

  Type importTy = Impl.getNamedSwiftType(Impl.getStdlibModule(), "String");
  if (!importTy)
    return nullptr;

  return SwiftDeclSynthesizer(Impl).createConstant(
      name, DC, importTy, text, ConstantConvertKind::None,
      /*isStatic=*/false, ClangN, AccessLevel::Public);
}

static ValueDecl *importStringLiteral(ClangImporter::Implementation &Impl,
                                      DeclContext *DC, Identifier name,
                                      const clang::Token &tok,
                                      ClangNode ClangN) {
  assert(isStringToken(tok));

  clang::ActionResult<clang::Expr *> result =
      Impl.getClangSema().ActOnStringLiteral(tok);
  if (!result.isUsable())
    return nullptr;

  const auto *parsed = dyn_cast<clang::StringLiteral>(result.get());
  if (!parsed)
    return nullptr;

  return importStringConstant(Impl, DC, name, parsed, ClangN);
}

static ValueDecl *importNil(ClangImporter::Implementation &Impl,
                            DeclContext *DC, Identifier name,
                            ClangNode clangN) {
  // We use a dummy type since we don't have a convenient type for 'nil'.  Any
  // use of this will be an error anyway.
  auto type = TupleType::getEmpty(Impl.SwiftContext);
  return Impl.createUnavailableDecl(
      name, DC, type, "use 'nil' instead of this imported macro",
      /*isStatic=*/false, clangN, AccessLevel::Public);
}

namespace {
ValueDecl *importDeclAlias(ClangImporter::Implementation &clang,
                           swift::DeclContext *DC, const clang::ValueDecl *D,
                           Identifier alias) {
  if (!DC->getASTContext().LangOpts.hasFeature(Feature::ImportMacroAliases))
    return nullptr;

  // Variadic functions cannot be imported into Swift.
  // FIXME(compnerd) emit a diagnostic for the missing diagnostic.
  if (const auto *FD = dyn_cast<clang::FunctionDecl>(D))
    if (FD->isVariadic())
      return nullptr;

  // Ignore self-referential macros.
  if (D->getName() == alias.str())
    return nullptr;

  swift::ValueDecl *VD =
      dyn_cast_or_null<ValueDecl>(clang.importDecl(D, clang.CurrentVersion));
  if (VD == nullptr)
    return nullptr;

  // If the imported decl is named identically, avoid the aliasing.
  if (VD->getBaseIdentifier().str() == alias.str())
    return nullptr;

  swift::ASTContext &Ctx = DC->getASTContext();
  ImportedType Ty =
      clang.importType(D->getType(), ImportTypeKind::Abstract,
                       [&clang, &D](Diagnostic &&Diag) {
                         clang.addImportDiagnostic(D, std::move(Diag),
                                                   D->getLocation());
                       }, /*AllowsNSUIntegerAsInt*/true,
                       Bridgeability::None, { });
  swift::Type GetterTy = FunctionType::get({}, Ty.getType(), ASTExtInfo{});
  swift::Type SetterTy =
      FunctionType::get({AnyFunctionType::Param(Ty.getType())},
                        Ctx.TheEmptyTupleType, ASTExtInfo{});

  /* Storage */
  auto *V = new (Ctx) VarDecl(/*isStatic=*/false, VarDecl::Introducer::Var,
                              /*nameLoc=*/SourceLoc(), alias, DC);

  V->setAccess(swift::AccessLevel::Public);
  V->setInterfaceType(Ty.getType());

  V->addAttribute(new (Ctx) TransparentAttr(/*Implicit*/ true));
  V->addAttribute(new (Ctx) InlineAttr(InlineKind::AlwaysUnderscored));
  V->addAttribute(new (Ctx) PreconcurrencyAttr(/*IsImplicit=*/true));
  V->setSynthesized();

  /* Accessor */
  swift::AccessorDecl *G = nullptr;
  {
    G = AccessorDecl::createImplicit(Ctx, AccessorKind::Get, V, false, false,
                                     TypeLoc(), GetterTy, DC);
    G->setAccess(swift::AccessLevel::Public);
    G->setInterfaceType(GetterTy);
    G->setIsTransparent(true);
    G->setParameters(ParameterList::createEmpty(Ctx));

    DeclRefExpr *DRE =
        new (Ctx) DeclRefExpr(ConcreteDeclRef(VD), {}, /*Implicit*/true,
                              AccessSemantics::Ordinary, Ty.getType());
    ReturnStmt *RS = ReturnStmt::createImplicit(Ctx, DRE);

    G->setBody(BraceStmt::createImplicit(Ctx, {RS}),
               AbstractFunctionDecl::BodyKind::TypeChecked);
  }

  swift::AccessorDecl *S = nullptr;
  if (const auto *Var = dyn_cast<clang::VarDecl>(D);
      Var && !Var->getType().isConstQualified()) {
    S = AccessorDecl::createImplicit(Ctx, AccessorKind::Set, V, false, false,
                                     TypeLoc(), Ctx.TheEmptyTupleType, DC);
    S->setAccess(swift::AccessLevel::Public);
    S->setInterfaceType(SetterTy);
    S->setIsTransparent(true);
    S->setParameters(ParameterList::create(Ctx, {
      ParamDecl::createImplicit(Ctx, Identifier(), Ctx.getIdentifier("newValue"),
                                Ty.getType(), DC)
    }));

    DeclRefExpr *LHS =
        new (Ctx) DeclRefExpr(ConcreteDeclRef(VD), {}, /*Implicit*/true,
                              AccessSemantics::Ordinary, Ty.getType());
    DeclRefExpr *RHS =
        new (Ctx) DeclRefExpr(S->getParameters()->get(0), {}, /*Implicit*/true,
                              AccessSemantics::Ordinary, Ty.getType());
    AssignExpr *AE = new (Ctx) AssignExpr(LHS, SourceLoc(), RHS, true);
    AE->setType(Ctx.TheEmptyTupleType);
    S->setBody(BraceStmt::createImplicit(Ctx, {AE}),
               AbstractFunctionDecl::BodyKind::TypeChecked);
  }

  ClangImporter::Implementation::makeComputed(V, G, S);

  return V;
}
} // namespace

static llvm::ArrayRef<clang::Token>
stripTokenParens(llvm::ArrayRef<clang::Token> tokens) {
  while (tokens.size() > 2 && tokens.front().is(clang::tok::l_paren) &&
         tokens.back().is(clang::tok::r_paren)) {
    tokens.consume_front();
    tokens.consume_back();
  }
  return tokens;
}

/// Check whether \p II has a macro definition, first ensuring that any
/// lazily-deserialized module macros are loaded. Prefer this over calling
/// IdentifierInfo::hasMacroDefinition() directly, which can return stale
/// results when Clang modules are in use.
static bool hasMacroDefinition(const clang::IdentifierInfo *II,
                               const clang::Preprocessor &PP) {
  if (II->isOutOfDate())
    (void)PP.getLeafModuleMacros(II);
  return II->hasMacroDefinition();
}

/// Match token patterns that can't go through the constexpr evaluator:
/// nil macros (NULL, (void*)0, (id)0), ObjC @"string", and CFSTR("string").
static ValueDecl *
tryImportMacroByTokenPattern(ClangImporter::Implementation &impl,
                             DeclContext *DC, Identifier name,
                             const clang::MacroInfo *macro, ClangNode ClangN) {
  const auto tokens = stripTokenParens(macro->tokens());
  if (tokens.empty())
    return nullptr;

  // Single identifier that names a nil macro (NULL, nil, Nil).
  if (tokens.size() == 1 && tokens.front().is(clang::tok::identifier)) {
    auto *clangID = tokens.front().getIdentifierInfo();
    if (hasMacroDefinition(clangID, impl.getClangPreprocessor())) {
      auto isNilMacro = llvm::StringSwitch<bool>(clangID->getName())
#define NIL_MACRO(NAME) .Case(#NAME, true)
#include "MacroTable.def"
#undef NIL_MACRO
        .Default(false);
      if (isNilMacro)
        return importNil(impl, DC, name, ClangN);
    }
  }

  auto isZeroToken = [&impl](const clang::Token &tok) -> bool {
    if (!tok.is(clang::tok::numeric_constant))
      return false;
    const auto result = impl.getClangSema().ActOnNumericConstant(tok);
    if (!result.isUsable())
      return false;
    const auto *intLit = dyn_cast<clang::IntegerLiteral>(result.get());
    return intLit && intLit->getValue() == 0;
  };

  // Import `(id)0` as nil.
  if (tokens.size() == 4 &&
      tokens[0].is(clang::tok::l_paren) &&
      tokens[1].is(clang::tok::identifier) &&
      tokens[1].getIdentifierInfo()->isStr("id") &&
      tokens[2].is(clang::tok::r_paren) &&
      isZeroToken(tokens[3])) {
    return importNil(impl, DC, name, ClangN);
  }

  // Import `(void*)0` as nil.
  if (tokens.size() == 5 &&
      tokens[0].is(clang::tok::l_paren) &&
      tokens[1].is(clang::tok::kw_void) &&
      tokens[2].is(clang::tok::star) &&
      tokens[3].is(clang::tok::r_paren) &&
      isZeroToken(tokens[4])) {
    return importNil(impl, DC, name, ClangN);
  }

  // Import ObjC `@"string"` as string.
  if (tokens.size() == 2 &&
      tokens[0].is(clang::tok::at) &&
      isStringToken(tokens[1])) {
    return importStringLiteral(impl, DC, name, tokens[1], ClangN);
  }

  // Import CoreFoundation's `CFSTR("string")` as string.
  if (tokens.size() == 4 &&
      tokens[0].is(clang::tok::identifier) &&
      tokens[0].getIdentifierInfo()->isStr("CFSTR") &&
      tokens[1].is(clang::tok::l_paren) &&
      isStringToken(tokens[2]) &&
      tokens[3].is(clang::tok::r_paren)) {
    return importStringLiteral(impl, DC, name, tokens[2], ClangN);
  }

  return nullptr;
}

/// Recursively checks whether a macro's expansion chain references
/// _Pragma/__pragma.
static bool macroExpansionContainsPragma(const clang::MacroInfo *macro,
                                         const clang::Preprocessor &pp) {
  llvm::SmallVector<const clang::MacroInfo *, 4> worklist;
  llvm::SmallPtrSet<const clang::MacroInfo *, 8> visited;
  worklist.push_back(macro);
  visited.insert(macro);
  while (!worklist.empty()) {
    const auto *mi = worklist.pop_back_val();
    for (const auto &tok : mi->tokens()) {
      if (!tok.is(clang::tok::identifier))
        continue;
      const auto *II = tok.getIdentifierInfo();
      if (!II)
        continue;
      if (II->isStr("_Pragma") || II->isStr("__pragma"))
        return true;
      if (hasMacroDefinition(II, pp))
        if (const auto *refMI = pp.getMacroInfo(II))
          if (visited.insert(refMI).second)
            worklist.push_back(refMI);
    }
  }
  return false;
}

/// Try to import a C/C++ object-like macro as a Swift constant by using
/// Clang's full expression parser and constant evaluator. Returns nullptr if
/// the macro cannot be parsed, evaluated, or mapped.
static ValueDecl *
tryImportMacroAsConstantExpr(ClangImporter::Implementation &impl,
                             DeclContext *DC, Identifier name,
                             const clang::MacroInfo *macro, ClangNode ClangN) {
  if (!impl.getClangParser())
    return nullptr;

  auto &pp = impl.getClangPreprocessor();
  auto &sema = impl.getClangSema();
  auto &parser = *impl.getClangParser();

  if (macro->tokens_empty())
    return nullptr;

  // Walk the macro expansion chain to detect _Pragma/__pragma at any depth.
  // Macros whose expansion triggers _Pragma() hit an assertion in the
  // diagnostic state tracker (DiagStateMap expects monotonically increasing
  // source locations, but our injected token stream has locations from the
  // original header).
  if (macroExpansionContainsPragma(macro, pp))
    return nullptr;

  // Save the parser's current token so we can restore state afterward.
  // The token sequence we inject is: [macro tokens..., eof, savedToken].
  clang::Token savedToken = parser.getCurToken();

  clang::Token eof;
  eof.startToken();
  eof.setKind(clang::tok::eof);
  eof.setLocation(savedToken.getEndLoc());

  llvm::SmallVector<clang::Token, 8> tokens;
  const size_t numTokensAsSizeT = macro->getNumTokens();
  tokens.reserve(numTokensAsSizeT + 2);
  tokens.append(macro->tokens_begin(), macro->tokens_end());
  tokens.push_back(eof);
  tokens.push_back(savedToken);

  pp.EnterTokenStream(tokens, /*DisableMacroExpansion=*/false,
                      /*IsReinject=*/false);
  parser.ConsumeAnyToken(/*ConsumeCodeCompletionTok=*/true);

  // Suppress diagnostics so that parse/eval failures for non-constant
  // macros don't produce spurious Clang warnings or errors.
  clang::DiagnosticsEngine &clangDiags = sema.getDiagnostics();
  bool savedSuppress = clangDiags.getSuppressAllDiagnostics();
  clangDiags.setSuppressAllDiagnostics(true);

  clang::ExprResult parseResult = parser.ParseConstantExpression();

  bool success =
      !parseResult.isInvalid() && parser.getCurToken().is(clang::tok::eof);

  if (!success)
    parser.SkipUntil(clang::tok::eof);

  clangDiags.setSuppressAllDiagnostics(savedSuppress);

  if (!success)
    return nullptr;

  clang::Expr *resultExpr = parseResult.get()->IgnoreParenImpCasts();

  // String literals can't go through EvaluateAsConstantExpr (they aren't
  // scalar constants), so handle them directly.
  if (const auto *stringLiteral = dyn_cast<clang::StringLiteral>(resultExpr))
    return importStringConstant(impl, DC, name, stringLiteral, ClangN);

  // Template-dependent expressions can't be evaluated at compile time.
  clang::Expr::EvalResult evalResult;
  if (resultExpr->isValueDependent() ||
      !resultExpr->EvaluateAsConstantExpr(evalResult, sema.getASTContext()))
    return nullptr;

  if (!evalResult.Val.isInt() && !evalResult.Val.isFloat())
    return nullptr;

  // createMacroConstant() has an assertion for some reason:
  // assert(value.getFloat().isFinite() && "can't handle infinities or NaNs");
  if (evalResult.Val.isFloat() && !evalResult.Val.getFloat().isFinite())
    return nullptr;

  auto clangType = resultExpr->getType();

  // Reject macros whose result type carries unavailability or deprecation
  // attributes.
  if (const auto *typedefType = clangType->getAs<clang::TypedefType>()) {
    auto avail = typedefType->getDecl()->getAvailability();
    if (avail == clang::AR_Unavailable || avail == clang::AR_Deprecated)
      return nullptr;
  }

  // Map comparison (==, <, etc.) and logical (&&, ||, !) operators to bool.
  bool shouldImportAsBool = clangType->isBooleanType();
  if (!shouldImportAsBool) {
    const clang::Expr *inner = resultExpr->IgnoreParenImpCasts();
    if (const auto *binOp = dyn_cast<clang::BinaryOperator>(inner))
      shouldImportAsBool = binOp->isComparisonOp() || binOp->isLogicalOp();
    else if (const auto *unOp = dyn_cast<clang::UnaryOperator>(inner))
      shouldImportAsBool = unOp->getOpcode() == clang::UO_LNot;
  }

  Type swiftType;
  if (shouldImportAsBool && evalResult.Val.isInt()) {
    swiftType = impl.SwiftContext.getBoolType();
  } else {
    swiftType = impl.importTypeIgnoreIUO(
        clangType, ImportTypeKind::Value,
        ImportDiagnosticAdder(impl, macro, macro->getDefinitionLoc()),
        isInSystemModule(DC), Bridgeability::None, ImportTypeAttrs());
  }
  if (!swiftType)
    return nullptr;

  // createMacroConstant() preconditions.
  auto &ctx = DC->getASTContext();
  auto *constantTyNominal = swiftType->getAnyNominal();
  if (!constantTyNominal)
    return nullptr;
  if (evalResult.Val.isInt()) {
    if (!isBoolOrBoolEnumType(swiftType) &&
        !ctx.getIntBuiltinInitDecl(constantTyNominal))
      return nullptr;
  } else {
    if (!ctx.getFloatBuiltinInitDecl(constantTyNominal))
      return nullptr;
  }

  return createMacroConstant(impl, macro, name, DC, swiftType, evalResult.Val,
                             ConstantConvertKind::None,
                             /*isStatic=*/false, ClangN);
}

/// Try to import a single-identifier macro as a Swift alias for a C/C++ decl.
/// If the referenced decl's imported Swift name already matches the macro
/// name, sets \p alreadyImported to true and returns nullptr, the declaration
/// is already available under that name so neither an alias nor a constant
/// should be created.
static ValueDecl *tryImportMacroAsAlias(ClangImporter::Implementation &impl,
                                        DeclContext *DC, Identifier name,
                                        const clang::MacroInfo *macro,
                                        ClangNode ClangN,
                                        bool &alreadyImported) {
  alreadyImported = false;
  auto tokens = stripTokenParens(macro->tokens());
  if (tokens.size() != 1 || !tokens[0].is(clang::tok::identifier))
    return nullptr;

  clang::Sema &S = impl.getClangSema();
  clang::LookupResult R(S, {{tokens[0].getIdentifierInfo()}, {}},
                        clang::Sema::LookupAnyName);
  if (!S.LookupName(R, S.TUScope) ||
      R.getResultKind() != clang::LookupResultKind::Found)
    return nullptr;

  const auto *foundDecl = R.getFoundDecl();

  if (auto *imported = dyn_cast_or_null<ValueDecl>(
          impl.importDecl(foundDecl, impl.CurrentVersion))) {
    if (imported->getBaseIdentifier() == name) {
      alreadyImported = true;
      return nullptr;
    }
  }

  if (const auto *VD = dyn_cast<clang::ValueDecl>(foundDecl))
    return importDeclAlias(impl, DC, VD, name);

  return nullptr;
}

/// Import a C/C++ object-like macro as a Swift declaration.
static ValueDecl *importMacro(ClangImporter::Implementation &impl,
                              DeclContext *DC, Identifier name,
                              const clang::MacroInfo *macro, ClangNode ClangN) {
  if (name.empty())
    return nullptr;

  if (macro->isFunctionLike()) {
    impl.addImportDiagnostic(
        macro, Diagnostic(diag::macro_not_imported_function_like, name.str()),
        macro->getDefinitionLoc());
    return nullptr;
  }

  // Token patterns first: handles special forms (nil, ObjC/CF strings) that
  // rely on specific token shapes and can't be expressed as constexpr.
  if (auto *result =
          tryImportMacroByTokenPattern(impl, DC, name, macro, ClangN))
    return result;

  // Alias before constexpr: a macro like `#define FOO some_enum_value` should
  // produce an alias to the original decl, not a duplicate integer constant
  // (which is what constexpr evaluation would yield).
  if (DC->getASTContext().LangOpts.hasFeature(Feature::ImportMacroAliases)) {
    bool alreadyImported;
    if (auto *result = tryImportMacroAsAlias(impl, DC, name, macro, ClangN,
                                             alreadyImported))
      return result;
    if (alreadyImported)
      return nullptr;
  }

  // General-purpose fallback: parse and evaluate as a constant expression.
  if (auto *result =
          tryImportMacroAsConstantExpr(impl, DC, name, macro, ClangN))
    return result;

  impl.addImportDiagnostic(macro,
                           Diagnostic(diag::macro_not_imported, name.str()),
                           macro->getDefinitionLoc());
  return nullptr;
}

ValueDecl *ClangImporter::Implementation::importMacro(Identifier name,
                                                      ClangNode macroNode) {
  const clang::MacroInfo *macro = macroNode.getAsMacro();
  if (!macro)
    return nullptr;

  PrettyStackTraceStringAction stackRAII{"importing macro", name.str()};

  // Look for macros imported with the same name.
  auto [known, inserted] = ImportedMacros.try_emplace(
      name, SmallVector<std::pair<const clang::MacroInfo *, ValueDecl *>, 2>{});
  if (inserted) {
    // Push in a placeholder to break circularity.
    known->getSecond().push_back({macro, nullptr});
  } else {
    // Check whether this macro has already been imported.
    for (const auto &entry : known->second) {
      if (entry.first == macro)
        return entry.second;
    }

    // Otherwise, check whether this macro is identical to a macro that has
    // already been imported.
    auto &clangPP = getClangPreprocessor();
    for (const auto &entry : known->second) {
      // If the macro is equal to an existing macro, map down to the same
      // declaration.
      if (macro->isIdenticalTo(*entry.first, clangPP, true)) {
        ValueDecl *result = entry.second;
        known->second.push_back({macro, result});
        return result;
      }
    }

    // If not, push in a placeholder to break circularity.
    known->second.push_back({macro, nullptr});
  }

  startedImportingEntity();

  // We haven't tried to import this macro yet. Do so now, and cache the
  // result.

  DeclContext *DC;
  if (const clang::Module *module =
          importer::getClangOwningModule(macroNode, getClangASTContext())) {
    // Get the parent module because currently we don't model Clang submodules
    // in Swift.
    DC = getWrapperForModule(module->getTopLevelModule());
  } else {
    DC = ImportedHeaderUnit;
  }

  auto valueDecl = ::importMacro(*this, DC, name, macro, macroNode);

  // Update the entry for the value we just imported.
  // It's /probably/ the last entry in ImportedMacros[name], but there's an
  // outside chance more macros with the same name have been imported
  // re-entrantly since this method started.
  if (valueDecl) {
    auto entryIter = llvm::find_if(llvm::reverse(ImportedMacros[name]),
        [macro](std::pair<const clang::MacroInfo *, ValueDecl *> entry) {
      return entry.first == macro;
    });
    assert(entryIter != llvm::reverse(ImportedMacros[name]).end() &&
           "placeholder not found");
    entryIter->second = valueDecl;
  }

  return valueDecl;
}
