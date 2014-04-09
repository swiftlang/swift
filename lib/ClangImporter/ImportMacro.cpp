//===--- ImportMacro.cpp - Import Clang preprocessor macros ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements support for translating some kinds of C preprocessor
// macros into Swift declarations.
//
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "llvm/ADT/SmallString.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/Lex/MacroInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangModule.h"

using namespace swift;

clang::Module *ClangImporter::Implementation::getClangSubmoduleForMacro(
    const clang::MacroInfo *MI) {
  auto *ExternalSource = getClangASTContext().getExternalSource();
  return ExternalSource->getModule(MI->getOwningModuleID());
}

ClangModuleUnit *ClangImporter::Implementation::getClangModuleForMacro(
    const clang::MacroInfo *MI) {
  clang::Module *M = getClangSubmoduleForMacro(MI);
  if (!M)
    return nullptr;

  // Get the parent module because currently we don't represent submodules with
  // ClangModule.
  // FIXME: this is just a workaround until we can import submodules.
  M = M->getTopLevelModule();

  auto &importer =
    static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());
  return getWrapperForModule(importer, M);
}

static ValueDecl *importNumericLiteral(ClangImporter::Implementation &Impl,
                                       DeclContext *DC,
                                       const clang::MacroInfo *MI,
                                       Identifier name,
                                       const clang::Token *signTok,
                                       const clang::Token &tok) {
  assert(tok.getKind() == clang::tok::numeric_constant &&
         "not a numeric token");
  {
    // Temporary hack to reject literals with ud-suffix.
    // FIXME: remove this when the following radar is implemented:
    // <rdar://problem/16445608> Swift should set up a DiagnosticConsumer for
    // Clang
    llvm::SmallString<32> SpellingBuffer;
    bool Invalid = false;
    StringRef TokSpelling =
        Impl.getClangPreprocessor().getSpelling(tok, SpellingBuffer, &Invalid);
    if (Invalid)
      return nullptr;
    if (TokSpelling.find('_') != StringRef::npos)
      return nullptr;
  }

  clang::ActionResult<clang::Expr*> result =
    Impl.getClangSema().ActOnNumericConstant(tok);
  if (result.isUsable()) {
    clang::Expr *parsed = result.get();
    if (auto *integer = dyn_cast<clang::IntegerLiteral>(parsed)) {
      auto type = Impl.importType(integer->getType(), ImportTypeKind::Normal);
      if (!type)
        return nullptr;

      // Determine the value.
      llvm::APSInt value(integer->getValue(),
                         integer->getType()->isUnsignedIntegerType());

      // If there was a - sign, negate the value.
      if (signTok && signTok->is(clang::tok::minus) &&
          !value.isMinSignedValue()) {
        value = -value;
      }

      return Impl.createConstant(name, DC, type, clang::APValue(value),
                                 ConstantConvertKind::Coerce,
                                 /*static*/ false);
    }

    if (auto *floating = dyn_cast<clang::FloatingLiteral>(parsed)) {
      auto type = Impl.importType(floating->getType(), ImportTypeKind::Normal);
      if (!type)
        return nullptr;

      llvm::APFloat value = floating->getValue();

      // If there was a - sign, negate the value.
      if (signTok && signTok->is(clang::tok::minus)) {
        value.changeSign();
      }

      return Impl.createConstant(name, DC, type, clang::APValue(value),
                                 ConstantConvertKind::Coerce,
                                 /*static*/ false);
    }
    // TODO: Other numeric literals (complex, imaginary, etc.)
  }
  return nullptr;
}

static bool isStringToken(const clang::Token &tok) {
  return tok.is(clang::tok::string_literal) ||
         tok.is(clang::tok::utf8_string_literal);
}

static ValueDecl *importStringLiteral(ClangImporter::Implementation &Impl,
                                      DeclContext *DC,
                                      const clang::MacroInfo *MI,
                                      Identifier name,
                                      const clang::Token &tok,
                                      bool isObjC) {
  DeclContext *dc = Impl.getClangModuleForMacro(MI);
  if (!dc)
    return nullptr;

  assert(isStringToken(tok));

  clang::ActionResult<clang::Expr*> result =
    Impl.getClangSema().ActOnStringLiteral(&tok, /*numToks*/1);
  if (!result.isUsable())
    return nullptr;

  auto parsed = dyn_cast<clang::StringLiteral>(result.get());
  if (!parsed)
    return nullptr;

  Type importTy;
  if (isObjC)
    importTy = Impl.getNamedSwiftType(Impl.getStdlibModule(), "String");
  else
    importTy = Impl.getNamedSwiftType(Impl.getStdlibModule(), "CString");
  if (!importTy)
    return nullptr;

  return Impl.createConstant(name, dc, importTy, parsed->getString(),
                             ConstantConvertKind::Coerce, /*static*/ false);
}

static ValueDecl *importLiteral(ClangImporter::Implementation &Impl,
                                DeclContext *DC,
                                const clang::MacroInfo *MI,
                                Identifier name,
                                const clang::Token &tok) {
  switch (tok.getKind()) {
  case clang::tok::numeric_constant:
    return importNumericLiteral(Impl, DC, MI, name, /*signTok*/nullptr, tok);

  case clang::tok::string_literal:
  case clang::tok::utf8_string_literal:
    return importStringLiteral(Impl, DC, MI, name, tok, /*isObjC*/false);

  // TODO: char literals.
  default:
    return nullptr;
  }
}

static bool isSignToken(const clang::Token &tok) {
  return tok.is(clang::tok::plus) || tok.is(clang::tok::minus);
}

static ValueDecl *importMacro(ClangImporter::Implementation &impl,
                              DeclContext *DC,
                              Identifier name, clang::MacroInfo *macro) {
  // Ignore include guards.
  if (macro->isUsedForHeaderGuard())
    return nullptr;

  // Currently we only convert non-function-like macros.
  if (macro->isFunctionLike())
    return nullptr;

  // Consult the blacklist of macros to suppress.
  auto suppressMacro =
    llvm::StringSwitch<bool>(name.str())
#define SUPPRESS_MACRO(NAME) .Case(#NAME, true)
#include "SuppressedMacros.def"
    .Default(false);

  if (suppressMacro)
    return nullptr;

  // FIXME: Ask Clang to try to parse and evaluate the expansion as a constant
  // expression instead of doing these special-case pattern matches.
  if (macro->getNumTokens() == 1) {
    // Check for a single-token expansion of the form <literal>.
    // TODO: or <identifier>.
    clang::Token const &tok = *macro->tokens_begin();
    
    // If it's a literal token, we might be able to translate the literal.
    if (tok.isLiteral()) {
      return importLiteral(impl, DC, macro, name, tok);
    }

    if (tok.is(clang::tok::identifier)) {
      auto clangID = tok.getIdentifierInfo();
      // If it's an identifier that is itself a macro, look into that macro.
      if (clangID->hasMacroDefinition()) {
        auto macroID = impl.getClangPreprocessor().getMacroInfo(clangID);
        return importMacro(impl, DC, name, macroID);
      }

      // FIXME: If the identifier refers to a declaration, alias it?
    }
    return nullptr;
  }

  if (macro->getNumTokens() == 2) {
    // Check for a two-token expansion of the form +<number> or -<number>.
    // These are technically subtly wrong because they allow things like:
    //   #define EOF -1
    //   int pred(int x) { return x EOF; }
    // but are pervasive in C headers anyway.
    clang::Token const &first = macro->tokens_begin()[0];
    clang::Token const &second = macro->tokens_begin()[1];
    
    if (isSignToken(first) && second.is(clang::tok::numeric_constant))
      return importNumericLiteral(impl, DC, macro, name, &first, second);

    // We also allow @"string".
    if (first.is(clang::tok::at) && isStringToken(second))
      return importStringLiteral(impl, DC, macro, name, second, /*objc*/true);

    return nullptr;
  }

  if (macro->getNumTokens() == 4) {
    // Check for a four-token expansion of the form (+<number>) or (-<number>).
    clang::Token const &lparenTok = macro->tokens_begin()[0];
    clang::Token const &signTok = macro->tokens_begin()[1];
    clang::Token const &litTok = macro->tokens_begin()[2];
    clang::Token const &rparenTok = macro->tokens_begin()[3];
    
    if (lparenTok.is(clang::tok::l_paren) &&
        rparenTok.is(clang::tok::r_paren) &&
        isSignToken(signTok) &&
        litTok.is(clang::tok::numeric_constant)) {
      return importNumericLiteral(impl, DC, macro, name, &signTok, litTok);
    }
  }

  return nullptr;
}

ValueDecl *ClangImporter::Implementation::importMacro(Identifier name,
                                                      clang::MacroInfo *macro) {
  // Look for the value for an already-imported macro.
  auto known = ImportedMacros.find(macro);
  if (known != ImportedMacros.end()) {
    return known->second;
  }

  ImportingEntityRAII ImportingEntity(*this);
  // We haven't tried to import this macro yet. Do so now, and cache the
  // result.

  DeclContext *DC = getClangModuleForMacro(macro);
  if (!DC)
    return nullptr;

  auto valueDecl = ::importMacro(*this, DC, name, macro);
  ImportedMacros[macro] = valueDecl;
  if (valueDecl) {
    valueDecl->setClangNode(macro);
  }
  return valueDecl;
}
