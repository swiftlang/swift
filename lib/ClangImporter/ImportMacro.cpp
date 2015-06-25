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

Optional<clang::Module *>
ClangImporter::Implementation::getClangSubmoduleForMacro(
    const clang::MacroInfo *MI) {
  auto *ExternalSource = getClangASTContext().getExternalSource();
  return ExternalSource->getModule(MI->getOwningModuleID());
}

ClangModuleUnit *ClangImporter::Implementation::getClangModuleForMacro(
    const clang::MacroInfo *MI) {
  auto maybeModule = getClangSubmoduleForMacro(MI);
  if (!maybeModule)
    return nullptr;
  if (!maybeModule.getValue())
    return ImportedHeaderUnit;

  // Get the parent module because currently we don't represent submodules with
  // ClangModule.
  auto *M = maybeModule.getValue()->getTopLevelModule();

  auto &importer =
    static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());
  return getWrapperForModule(importer, M);
}

template <typename T = clang::Expr>
static const T *
parseNumericLiteral(ClangImporter::Implementation &impl,
                    const clang::Token &tok) {
  auto result = impl.getClangSema().ActOnNumericConstant(tok);
  if (result.isUsable())
    return dyn_cast<T>(result.get());
  return nullptr;
}

static bool isInSystemModule(DeclContext *D) {
  if (cast<ClangModuleUnit>(D->getModuleScopeContext())->isSystemModule())
    return true;
  return false;
}

static ValueDecl *importNumericLiteral(ClangImporter::Implementation &Impl,
                                       DeclContext *DC,
                                       const clang::MacroInfo *MI,
                                       Identifier name,
                                       const clang::Token *signTok,
                                       const clang::Token &tok,
                                       const clang::MacroInfo *ClangN) {
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

  if (const clang::Expr *parsed = parseNumericLiteral<>(Impl, tok)) {
    auto clangTy = parsed->getType();
    auto type = Impl.importType(clangTy, ImportTypeKind::Value,
                                isInSystemModule(DC));
    if (!type)
      return nullptr;

    if (auto *integer = dyn_cast<clang::IntegerLiteral>(parsed)) {
      // Determine the value.
      llvm::APSInt value{integer->getValue(), clangTy->isUnsignedIntegerType()};

      // If there was a - sign, negate the value.
      // If there was a ~, flip all bits.
      if (signTok) {
        if (signTok->is(clang::tok::minus)) {
          if (!value.isMinSignedValue())
            value = -value;
        } else if (signTok->is(clang::tok::tilde)) {
          value.flipAllBits();
        }
      }

      return Impl.createConstant(name, DC, type, clang::APValue(value),
                                 ConstantConvertKind::Coerce,
                                 /*static*/ false, ClangN);
    }

    if (auto *floating = dyn_cast<clang::FloatingLiteral>(parsed)) {
      // ~ doesn't make sense with floating-point literals.
      if (signTok && signTok->is(clang::tok::tilde))
        return nullptr;

      llvm::APFloat value = floating->getValue();

      // If there was a - sign, negate the value.
      if (signTok && signTok->is(clang::tok::minus)) {
        value.changeSign();
      }

      return Impl.createConstant(name, DC, type, clang::APValue(value),
                                 ConstantConvertKind::Coerce,
                                 /*static*/ false, ClangN);
    }
    // TODO: Other numeric literals (complex, imaginary, etc.)
  }
  return nullptr;
}

static bool isStringToken(const clang::Token &tok) {
  return tok.is(clang::tok::string_literal) ||
         tok.is(clang::tok::utf8_string_literal);
}

// Describes the kind of string literal we're importing.
enum class MappedStringLiteralKind {
  CString,  // "string"
  NSString, // @"string"
  CFString  // CFSTR("string")
};

static ValueDecl *importStringLiteral(ClangImporter::Implementation &Impl,
                                      DeclContext *DC,
                                      const clang::MacroInfo *MI,
                                      Identifier name,
                                      const clang::Token &tok,
                                      MappedStringLiteralKind kind,
                                      const clang::MacroInfo *ClangN) {
  DeclContext *dc = Impl.getClangModuleForMacro(MI);
  if (!dc)
    return nullptr;

  assert(isStringToken(tok));

  clang::ActionResult<clang::Expr*> result =
    Impl.getClangSema().ActOnStringLiteral(tok);
  if (!result.isUsable())
    return nullptr;

  auto parsed = dyn_cast<clang::StringLiteral>(result.get());
  if (!parsed)
    return nullptr;

  Type importTy = Impl.getNamedSwiftType(Impl.getStdlibModule(), "String");
  if (!importTy)
    return nullptr;

  return Impl.createConstant(name, dc, importTy, parsed->getString(),
                             ConstantConvertKind::Coerce, /*static*/ false,
                             ClangN);
}

static ValueDecl *importLiteral(ClangImporter::Implementation &Impl,
                                DeclContext *DC,
                                const clang::MacroInfo *MI,
                                Identifier name,
                                const clang::Token &tok,
                                const clang::MacroInfo *ClangN) {
  switch (tok.getKind()) {
  case clang::tok::numeric_constant:
    return importNumericLiteral(Impl, DC, MI, name, /*signTok*/nullptr, tok,
                                ClangN);

  case clang::tok::string_literal:
  case clang::tok::utf8_string_literal:
    return importStringLiteral(Impl, DC, MI, name, tok,
                               MappedStringLiteralKind::CString, ClangN);

  // TODO: char literals.
  default:
    return nullptr;
  }
}

static ValueDecl *importNil(ClangImporter::Implementation &Impl,
                            DeclContext *DC, Identifier name,
                            const clang::MacroInfo *clangN) {
  // We use a dummy type since we don't have a convenient type for 'nil'.  Any
  // use of this will be an error anyway.
  auto type = TupleType::getEmpty(Impl.SwiftContext);
  return Impl.createUnavailableDecl(name, DC, type,
                                    "use 'nil' instead of this imported macro",
                                    /*static=*/false, clangN);
}

static bool isSignToken(const clang::Token &tok) {
  return tok.is(clang::tok::plus) || tok.is(clang::tok::minus) ||
         tok.is(clang::tok::tilde);
}

/// Returns true if it is expected that the macro is ignored.
bool ClangImporter::Implementation::shouldIgnoreMacro(StringRef name,
                                                const clang::MacroInfo *macro) {
  // Ignore include guards.
  if (macro->isUsedForHeaderGuard())
    return true;

  if (macro->tokens_empty())
    return true;

  // Consult the blacklist of macros to suppress.
  auto suppressMacro =
    llvm::StringSwitch<bool>(name)
#define SUPPRESS_MACRO(NAME) .Case(#NAME, true)
#include "MacroTable.def"
    .Default(false);

  if (suppressMacro)
    return true;

  return false;
}

static ValueDecl *importMacro(ClangImporter::Implementation &impl,
                              DeclContext *DC,
                              Identifier name,
                              const clang::MacroInfo *macro,
                              const clang::MacroInfo *ClangN) {
  if (impl.shouldIgnoreMacro(name.str(), macro))
    return nullptr;

  // Currently we only convert non-function-like macros.
  if (macro->isFunctionLike())
    return nullptr;

  auto numTokens = macro->getNumTokens();
  auto tokenI = macro->tokens_begin(), tokenE = macro->tokens_end();

  // Drop one layer of parentheses.
  if (numTokens > 2 &&
      tokenI[0].is(clang::tok::l_paren) &&
      tokenE[-1].is(clang::tok::r_paren)) {
    ++tokenI;
    --tokenE;
    numTokens -= 2;
  }

  // FIXME: Ask Clang to try to parse and evaluate the expansion as a constant
  // expression instead of doing these special-case pattern matches.
  switch (numTokens) {
  case 1: {
    // Check for a single-token expansion of the form <literal>.
    // TODO: or <identifier>.
    const clang::Token &tok = *tokenI;
    
    // If it's a literal token, we might be able to translate the literal.
    if (tok.isLiteral()) {
      return importLiteral(impl, DC, macro, name, tok, ClangN);
    }

    if (tok.is(clang::tok::identifier)) {
      auto clangID = tok.getIdentifierInfo();

      // If it's an identifier that is itself a macro, look into that macro.
      if (clangID->hasMacroDefinition()) {
        auto isNilMacro =
          llvm::StringSwitch<bool>(clangID->getName())
#define NIL_MACRO(NAME) .Case(#NAME, true)
#include "MacroTable.def"
          .Default(false);
        if (isNilMacro)
          return importNil(impl, DC, name, ClangN);

        auto macroID = impl.getClangPreprocessor().getMacroInfo(clangID);
        if (macroID && macroID != macro)
          return importMacro(impl, DC, name, macroID, ClangN);
      }

      // FIXME: If the identifier refers to a declaration, alias it?
    }
    return nullptr;
  }
  case 2: {
    // Check for a two-token expansion of the form +<number> or -<number>.
    // These are technically subtly wrong without parentheses because they
    // allow things like:
    //   #define EOF -1
    //   int pred(int x) { return x EOF; }
    // but are pervasive in C headers anyway.
    clang::Token const &first = tokenI[0];
    clang::Token const &second = tokenI[1];
    
    if (isSignToken(first) && second.is(clang::tok::numeric_constant))
      return importNumericLiteral(impl, DC, macro, name, &first, second, ClangN);

    // We also allow @"string".
    if (first.is(clang::tok::at) && isStringToken(second))
      return importStringLiteral(impl, DC, macro, name, second,
                                 MappedStringLiteralKind::NSString, ClangN);

    break;
  }
  case 3: {
    // Check for a three-token expression of the form <number> << <number>.
    // No signs or inner parentheses are allowed here.
    // FIXME: What about people who define BIT_MASK(pos) helper macros?
    if (tokenI[0].is(clang::tok::numeric_constant) &&
        tokenI[1].is(clang::tok::lessless) &&
        tokenI[2].is(clang::tok::numeric_constant)) {
      auto *base = parseNumericLiteral<clang::IntegerLiteral>(impl, tokenI[0]);
      auto *shift = parseNumericLiteral<clang::IntegerLiteral>(impl, tokenI[2]);
      if (!base || !shift)
        return nullptr;

      auto clangTy = base->getType();
      auto type = impl.importType(clangTy, ImportTypeKind::Value,
                                  isInSystemModule(DC));
      if (!type)
        return nullptr;

      llvm::APSInt value{ base->getValue() << shift->getValue(),
                          clangTy->isUnsignedIntegerType() };
      return impl.createConstant(name, DC, type, clang::APValue(value),
                                 ConstantConvertKind::Coerce, /*static=*/false,
                                 ClangN);
    }
    break;
  }
  case 4: {
    // Check for a CFString literal of the form CFSTR("string").
    if (tokenI[0].is(clang::tok::identifier) &&
        tokenI[0].getIdentifierInfo()->isStr("CFSTR") &&
        tokenI[1].is(clang::tok::l_paren) &&
        isStringToken(tokenI[2]) &&
        tokenI[3].is(clang::tok::r_paren)) {
      return importStringLiteral(impl, DC, macro, name, tokenI[2],
                                 MappedStringLiteralKind::CFString, ClangN);
    }
    break;
  }
  case 5:
    // Check for the literal series of tokens (void*)0. (We've already stripped
    // one layer of parentheses.)
    if (tokenI[0].is(clang::tok::l_paren) &&
        tokenI[1].is(clang::tok::kw_void) &&
        tokenI[2].is(clang::tok::star) &&
        tokenI[3].is(clang::tok::r_paren) &&
        tokenI[4].is(clang::tok::numeric_constant)) {
      auto *integerLiteral =
        parseNumericLiteral<clang::IntegerLiteral>(impl, tokenI[4]);
      if (!integerLiteral || integerLiteral->getValue() != 0)
        break;
      return importNil(impl, DC, name, ClangN);
    }
    break;
  default:
    break;
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

  auto valueDecl = ::importMacro(*this, DC, name, macro, macro);
  ImportedMacros[macro] = valueDecl;
  return valueDecl;
}
