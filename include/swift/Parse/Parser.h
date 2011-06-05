//===--- Parser.h - Swift Language Parser -----------------------*- C++ -*-===//
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
//  This file defines the Parser interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSER_H
#define SWIFT_PARSER_H

#include "swift/Parse/Token.h"

namespace llvm {
  class SourceMgr;
  template <typename PT1, typename PT2>
  class PointerUnion;
  template<class T>
  class NullablePtr;
  class Twine;
  template <typename T> class SmallVectorImpl;
}

namespace swift {
  class Lexer;
  class Sema;
  class Expr;
  class Type;
  class Decl;
  class DeclAttributes;
  class TranslationUnitDecl;
  class TypeAliasDecl;
  class FuncDecl;
  class VarDecl;
  class ASTContext;
  class DeclVarName;
  class TupleTypeElt;
  class Identifier;
  
  /// performNameBinding - Once parsing is complete, this walks the AST to
  /// resolve names and do other top-level validation.
  void performNameBinding(TranslationUnitDecl *TUD, ASTContext &Ctx);
  
  /// performTypeChecking - Once parsing and namebinding are complete, this
  /// walks the AST to resolve types and diagnose problems therein.
  ///
  /// FIXME: This should be moved out to somewhere else.
  void performTypeChecking(TranslationUnitDecl *TUD, ASTContext &Ctx);

class Parser {
  llvm::SourceMgr &SourceMgr;
  Lexer &L;
  Sema &S;
  
  /// Tok - This is the current token being considered by the parser.
  Token Tok;
  
  Parser(const Parser&);         // DO NOT IMPLEMENT
  void operator=(const Parser&); // DO NOT IMPLEMENT
public:
  Parser(unsigned BufferID, ASTContext &Ctx);
  ~Parser();
  
  TranslationUnitDecl *parseTranslationUnit();
  
  typedef llvm::PointerUnion<Expr*, Decl*> ExprOrDecl;

private:
  // Utilities.
  llvm::SMLoc consumeToken();
  llvm::SMLoc consumeToken(tok::TokenKind K) {
    assert(Tok.is(K) && "Consuming wrong token kind");
    return consumeToken();
  }
  
  /// consumeIf - If the current token is the specified kind, consume it and
  /// return true.  Otherwise, return false without consuming it.
  bool consumeIf(tok::TokenKind K) {
    if (Tok.isNot(K)) return false;
    consumeToken(K);
    return true;
  }
  
  /// skipUntil - Read tokens until we get to the specified token, then return
  /// without consuming it.  Because we cannot guarantee that the token will
  /// ever occur, this skips to some likely good stopping point.
  ///
  void skipUntil(tok::TokenKind T);
  
  void note(llvm::SMLoc Loc, const llvm::Twine &Message);
  void warning(llvm::SMLoc Loc, const llvm::Twine &Message);
  void error(llvm::SMLoc Loc, const llvm::Twine &Message);
  
  // Primitive Parsing
  bool parseIdentifier(Identifier &Result, const llvm::Twine &Message);

  /// parseToken - The parser expects that 'K' is next in the input.  If so, it
  /// is consumed and false is returned.
  ///
  /// If the input is malformed, this emits the specified error diagnostic.
  /// Next, if SkipToTok is specified, it calls skipUntil(SkipToTok).  Finally,
  /// true is returned.
  bool parseToken(tok::TokenKind K, const char *Message,
                  tok::TokenKind SkipToTok = tok::unknown);
  
  bool parseValueSpecifier(Type &Ty, llvm::NullablePtr<Expr> &Init);
  
  // Decl Parsing
  bool parseDeclExprList(llvm::SmallVectorImpl<ExprOrDecl> &Decls,
                         bool &MissingSemiAtEnd, bool IsTopLevel);
  TypeAliasDecl *parseDeclTypeAlias();
  void parseAttributeList(DeclAttributes &Attributes);
  bool parseAttribute(DeclAttributes &Attributes);
  bool parseVarName(DeclVarName &Name);
  
  Decl *parseDeclImport();
  Decl *parseDeclOneOf();
  bool parseDeclStruct(llvm::SmallVectorImpl<ExprOrDecl> &Decls);
  bool parseDeclVar(llvm::SmallVectorImpl<ExprOrDecl> &Decls);
  FuncDecl *parseDeclFunc();
  
  // Type Parsing
  bool parseType(Type &Result);
  bool parseType(Type &Result, const llvm::Twine &Message);
  bool parseTypeTupleBody(llvm::SMLoc LPLoc, Type &Result);
  bool parseTypeOneOfBody(llvm::SMLoc OneOfLoc, const DeclAttributes &Attrs,
                          Type &Result, TypeAliasDecl *TypeName = 0);

  // Expression Parsing
  bool parseExpr(llvm::NullablePtr<Expr> &Result, bool NonBraceOnly,
                 const char *Message = 0);
  bool parseExprSingle(llvm::NullablePtr<Expr> &Result, const char *Message =0);
  bool parseExprPrimary(llvm::SmallVectorImpl<Expr*> &Result);
  bool parseExprIdentifier(llvm::NullablePtr<Expr> &Result);
  bool parseExprDollarIdentifier(llvm::NullablePtr<Expr> &Result);
  bool parseExprParen(llvm::NullablePtr<Expr> &Result);
  bool parseExprBrace(llvm::NullablePtr<Expr> &Result);
  bool parseExprIf(llvm::NullablePtr<Expr> &Result);
};
  
} // end namespace swift

#endif
