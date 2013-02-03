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
#include "clang/AST/Expr.h"
#include "clang/Lex/MacroInfo.h"
#include "clang/Sema/Sema.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"

using namespace swift;

// FIXME: Use actual constants when we have those.
static ValueDecl *makeConstantDeclForLiteralExpr(ASTContext &C,
                                                 DeclContext *dc,
                                                 Identifier name,
                                                 Type literalTy,
                                                 Expr *literalExpr) {
  auto getterTy = FunctionType::get(TupleType::getEmpty(C),
                                    literalTy, C);
  auto returnStmt = new (C) ReturnStmt(SourceLoc(), literalExpr);
  auto bodyStmt = BraceStmt::create(C,
                                    SourceLoc(),
                                    BraceStmt::ExprStmtOrDecl(returnStmt),
                                    SourceLoc());
  auto getterFunc = FuncExpr::create(C, SourceLoc(),
                                     /*ArgParams*/ {}, /*BodyParams*/ {},
                                     TypeLoc::withoutLoc(literalTy),
                                     bodyStmt, dc);
  auto getterFuncDecl = new (C) FuncDecl(SourceLoc(), SourceLoc(),
                                         Identifier(),
                                         SourceLoc(),
                                         nullptr,
                                         getterTy,
                                         getterFunc,
                                         dc);
  auto varDecl = new (C) VarDecl(SourceLoc(),
                                 name, literalTy, dc);
  varDecl->setProperty(C, SourceLoc(),
                       getterFuncDecl,
                       /*setter=*/ nullptr,
                       SourceLoc());
  return varDecl;
}

static Expr *applySignExpr(ASTContext &C, Expr *literalExpr,
                           clang::Token const *signTok) {
  if (signTok && signTok->getKind() == clang::tok::minus) {
    // FIXME: apply unary minus
    return literalExpr;
  }
  return literalExpr;
}

static ValueDecl *importNumericLiteral(ClangImporter::Implementation &Impl,
                                       Identifier name,
                                       clang::Token const *signTok,
                                       clang::Token const &tok) {
  ASTContext &C = Impl.SwiftContext;
  DeclContext *dc = Impl.firstClangModule;
  
  assert(tok.getKind() == clang::tok::numeric_constant &&
         "not a numeric token");
  clang::ActionResult<clang::Expr*> result =
    Impl.Instance->getSema().ActOnNumericConstant(tok);
  if (result.isUsable()) {
    clang::Expr *parsed = result.get();
    if (auto *integer = dyn_cast<clang::IntegerLiteral>(parsed)) {
      // FIXME: preserve the original radix.
      llvm::SmallString<8> value;
      integer->getValue().toString(value, 10, /*Signed*/ true);
      auto valueTextArray = C.AllocateCopy(value);
      StringRef valueText(valueTextArray.data(), valueTextArray.size());
      Expr *valueExpr = new (C) IntegerLiteralExpr(valueText, SourceLoc());
      valueExpr = applySignExpr(C, valueExpr, signTok);

      // Synthesize a read-only Int property.
      // FIXME: 'Int' might not be the most convenient type for C APIs.
      auto intTy = Impl.getNamedSwiftType(Impl.getSwiftModule(), "Int");
      return makeConstantDeclForLiteralExpr(C, dc, name, intTy, valueExpr);
    }
    if (auto *floating = dyn_cast<clang::FloatingLiteral>(parsed)) {
      // FIXME: preserve the original radix.
      llvm::SmallString<8> value;
      floating->getValue().toString(value, 10, /*Signed*/ true);
      auto valueTextArray = C.AllocateCopy(value);
      StringRef valueText(valueTextArray.data(), valueTextArray.size());
      Expr *valueExpr = new (C) FloatLiteralExpr(valueText, SourceLoc());
      valueExpr = applySignExpr(C, valueExpr, signTok);

      // Synthesize a read-only Double property.
      // FIXME: 'Double' might not be the best type.
      auto doubleTy = Impl.getNamedSwiftType(Impl.getSwiftModule(), "Double");
      return makeConstantDeclForLiteralExpr(C, dc, name, doubleTy, valueExpr);
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
  auto known = ImportedMacros.find(macro);
  if (known != ImportedMacros.end())
    return known->second;
  
  // Don't import macros private to the module.
  if (!macro->isPublic())
    return nullptr;
  
  // Currently we only convert non-function-like macros.
  if (macro->isFunctionLike())
    return nullptr;

  // FIXME: setClangDecl on the returned decl to something?
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