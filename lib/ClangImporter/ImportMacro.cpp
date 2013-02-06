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
#include "clang/Sema/Sema.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"

using namespace swift;

static ValueDecl *importNumericLiteral(ClangImporter::Implementation &Impl,
                                       Identifier name,
                                       clang::Token const *signTok,
                                       clang::Token const &tok) {
  auto &clangContext = Impl.getClangASTContext();
  DeclContext *dc = Impl.firstClangModule;
  
  assert(tok.getKind() == clang::tok::numeric_constant &&
         "not a numeric token");
  clang::ActionResult<clang::Expr*> result =
    Impl.Instance->getSema().ActOnNumericConstant(tok);
  if (result.isUsable()) {
    clang::Expr *parsed = result.get();
    if (auto *integer = dyn_cast<clang::IntegerLiteral>(parsed)) {
      // Note: all integer literals are imported with type 'int'.
      // FIXME: What if int is too small?
      auto type = Impl.importType(clangContext.IntTy);
      if (!type)
        return nullptr;

      llvm::APSInt value(integer->getValue(),
                         integer->getType()->isUnsignedIntegerType());
      return Impl.createConstant(name, dc, type, clang::APValue(value),
                                 /*requiresCast=*/true);
    }

    if (auto *floating = dyn_cast<clang::FloatingLiteral>(parsed)) {
      // Note: all integer literals are imported with type 'double'.
      // FIXME: What if double is too small?
      auto type = Impl.importType(clangContext.DoubleTy);
      if (!type)
        return nullptr;

      return Impl.createConstant(name, dc, type,
                                 clang::APValue(floating->getValue()),
                                 /*requiresCast=*/true);
    }
    // TODO: Other numeric literals (complex, imaginary, etc.)
  }
  return nullptr;
}

static ValueDecl *importLiteral(ClangImporter::Implementation &Impl,
                                Identifier name,
                                clang::Token const &tok) {  
  switch (tok.getKind()) {
  case clang::tok::numeric_constant:
    return importNumericLiteral(Impl, name, /*signTok*/nullptr, tok);
    
  // TODO: char and string literals.
  default:
    return nullptr;
  }
}

static bool isSignToken(clang::Token const &tok) {
  return tok.getKind() == clang::tok::plus ||
         tok.getKind() == clang::tok::minus;
}

ValueDecl *ClangImporter::Implementation::importMacro(Identifier name,
                                                      clang::MacroInfo *macro) {
  // Don't import macros private to the module.
  if (!macro->isPublic())
    return nullptr;
  
  // Currently we only convert non-function-like macros.
  if (macro->isFunctionLike())
    return nullptr;

  // FIXME: Ask Clang to try to parse and evaluate the expansion as a constant
  // expression instead of doing these special-case pattern matches.
  if (macro->getNumTokens() == 1) {
    // Check for a single-token expansion of the form <literal>.
    // TODO: or <identifier>.
    clang::Token const &tok = *macro->tokens_begin();
    
    // If it's a literal token, we might be able to translate the literal.
    if (tok.isLiteral()) {
      return importLiteral(*this, name, tok);
    }
    
    // TODO: If it's an identifier token, alias the named identifier.
  } else if (macro->getNumTokens() == 2) {
    // Check for a two-token expansion of the form +<number> or -<number>.
    // These are technically subtly wrong because they allow things like:
    //   #define EOF -1
    //   int pred(int x) { return x EOF; }
    // but are pervasive in C headers anyway.
    clang::Token const &signTok = macro->tokens_begin()[0];
    clang::Token const &litTok = macro->tokens_begin()[1];
    
    if (isSignToken(signTok) &&
        litTok.getKind() == clang::tok::numeric_constant) {
      return importNumericLiteral(*this, name, &signTok, litTok);
    }
  } else if (macro->getNumTokens() == 4) {
    // Check for a four-token expansion of the form (+<number>) or (-<number>).
    clang::Token const &lparenTok = macro->tokens_begin()[0];
    clang::Token const &signTok = macro->tokens_begin()[1];
    clang::Token const &litTok = macro->tokens_begin()[2];
    clang::Token const &rparenTok = macro->tokens_begin()[3];
    
    if (lparenTok.getKind() == clang::tok::l_paren &&
        rparenTok.getKind() == clang::tok::r_paren &&
        isSignToken(signTok) &&
        litTok.getKind() == clang::tok::numeric_constant) {
      return importNumericLiteral(*this, name, &signTok, litTok);
    }
  }

  return nullptr;
}