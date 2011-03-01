//===--- Parser.cpp - Swift Language Parser -------------------------------===//
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
//  This file implements the Swift parser.
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/Sema.h"
#include "swift/Sema/Scope.h"
#include "swift/AST/ASTConsumer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/NullablePtr.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"
using namespace swift;
using llvm::SMLoc;
using llvm::NullablePtr;

//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//

Parser::Parser(unsigned BufferID, ASTConsumer &consumer)
  : Consumer(consumer),
    SourceMgr(Consumer.getContext().SourceMgr),
    L(*new Lexer(BufferID, Consumer.getContext())),
    S(*new Sema(Consumer.getContext())) {
}

Parser::~Parser() {
  delete &L;
  delete &S;
}

void Parser::Note(SMLoc Loc, const llvm::Twine &Message) {
  SourceMgr.PrintMessage(Loc, Message, "note");
}

void Parser::Warning(SMLoc Loc, const llvm::Twine &Message) {
  SourceMgr.PrintMessage(Loc, Message, "warning");
}

void Parser::Error(SMLoc Loc, const llvm::Twine &Message) {
  Consumer.getContext().setHadError();
  SourceMgr.PrintMessage(Loc, Message, "error");
}

void Parser::ConsumeToken() {
  assert(Tok.isNot(tok::eof) && "Lexing past eof!");
  L.Lex(Tok);
}

/// SkipUntil - Read tokens until we get to the specified token, then return.
/// Because we cannot guarantee that the token will ever occur, this skips to
/// some likely good stopping point.
///
void Parser::SkipUntil(tok::TokenKind T) {
  // tok::unknown is a sentinel that means "don't skip".
  if (T == tok::unknown) return;
  
  while (Tok.isNot(tok::eof) && Tok.isNot(T)) {
    switch (Tok.getKind()) {
    default: ConsumeToken(); break;
    // TODO: Handle paren/brace/bracket recovery.
    }
  }
}


//===----------------------------------------------------------------------===//
// Primitive Parsing
//===----------------------------------------------------------------------===//

/// ParseIdentifier - Consume an identifier if present and return its name in
/// Result.  Otherwise, emit an error and return true.
bool Parser::ParseIdentifier(llvm::StringRef &Result,const llvm::Twine &Message,
                             tok::TokenKind SkipToTok) {
  if (Tok.is(tok::identifier)) {
    Result = Tok.getText();
    ConsumeToken(tok::identifier);
    return false;
  }
  
  Error(Tok.getLoc(), Message);
  return true;
}

/// ParseToken - The parser expects that 'K' is next in the input.  If so, it is
/// consumed and false is returned.
///
/// If the input is malformed, this emits the specified error diagnostic.
/// Next, if SkipToTok is specified, it calls SkipUntil(SkipToTok).  Finally,
/// true is returned.
bool Parser::ParseToken(tok::TokenKind K, const char *Message,
                        tok::TokenKind SkipToTok) {
  if (Tok.is(K)) {
    ConsumeToken(K);
    return false;
  }
  
  Error(Tok.getLoc(), Message);
  SkipUntil(SkipToTok);
  
  // If we skipped ahead to the missing token and found it, consume it as if
  // there were no error.
  if (K == SkipToTok && Tok.is(SkipToTok))
    ConsumeToken();
  return true;
}

/// value-specifier:
///   ':' type
///   ':' type '=' expr
///   '=' expr
bool Parser::ParseValueSpecifier(Type *&Ty, NullablePtr<Expr> &Init) {
  // Diagnose when we don't have a type or an expression.
  if (Tok.isNot(tok::colon) && Tok.isNot(tok::equal)) {
    Error(Tok.getLoc(), "expected a type or an initializer");
    // TODO: Recover better by still creating var, but making it have
    // 'invalid' type so that uses of the identifier are not errors.
    return true;
  }
  
  if (ConsumeIf(tok::colon) &&
      ParseType(Ty, "expected type in var declaration"))
    return true;
  
  if (ConsumeIf(tok::equal)) {
    if (ParseExpr(Init, "expected expression in var declaration"))
      return true;
    
    // If there was an expression, but it had a parse error, give the var decl
    // an artificial int type to avoid chained errors.
    // FIXME: We really need to distinguish erroneous expr from missing expr in
    // ActOnVarDecl.
    if (Init.isNull() && Ty == 0)
      Ty = S.Context.TheInt32Type;
  }
  
  return false;
}


//===----------------------------------------------------------------------===//
// Decl Parsing
//===----------------------------------------------------------------------===//

/// ParseTranslationUnit
///   translation-unit:
///     decl-top-level*
void Parser::ParseTranslationUnit() {
  // Prime the lexer.
  ConsumeToken();
  
  {
    // The entire translation unit is in a big scope.
    Scope OuterScope(S.decl);
  
    while (Tok.isNot(tok::eof)) {
      if (Decl *D = ParseDeclTopLevel())
        Consumer.HandleTopLevelDecl(D);
    }
  }
  
  // Notify sema about the end of the translation unit.
  S.decl.handleEndOfTranslationUnit();
  
  // Notify consumer about the end of the translation unit.
  Consumer.HandleEndOfTranslationUnit();
}

/// ParseDeclTopLevel
///   decl-top-level:
///     ';'
///     decl-oneof
///     decl-struct
///     decl-func
///     decl-typealias
///     decl-var
Decl *Parser::ParseDeclTopLevel() {
  switch (Tok.getKind()) {
  default:
    Error(Tok.getLoc(), "expected a top level declaration");
    break;
  case tok::semi:
    ConsumeToken(tok::semi);
    return 0; // Could do a top-level semi decl.
      
  case tok::kw_typealias:
    if (TypeAliasDecl *D = ParseDeclTypeAlias())
      return D;
    return 0;

  case tok::kw_oneof:
    if (Decl *O = ParseDeclOneOf())
      return O;
    break;
  case tok::kw_struct:
    if (Decl *D = ParseDeclStruct())
      return D;
    break;
  case tok::kw_func:
    if (FuncDecl *D = ParseDeclFunc()) {
      S.decl.ActOnTopLevelDecl(D);
      return D;
    }
    break;
  case tok::kw_var:
    if (VarDecl *D = ParseDeclVar()) {
      S.decl.ActOnTopLevelDecl(D);
      return D;
    }
    break;
  }
  
  S.decl.ActOnTopLevelDeclError();
  
  // On error, skip to the next top level declaration.
  while (Tok.isNot(tok::eof) && Tok.isNot(tok::kw_var) &&
         Tok.isNot(tok::kw_func))
    ConsumeToken();
  return 0;
}


/// ParseAttribute
///   attribute:
///     'infix' '=' numeric_constant
bool Parser::ParseAttribute(DeclAttributes &Attributes) {
  if (Tok.is(tok::identifier) && Tok.getText() == "infix") {
    if (Attributes.InfixPrecedence != -1)
      Error(Tok.getLoc(), "infix precedence repeatedly specified");
    ConsumeToken(tok::identifier);

    // The default infix precedence is 100.
    Attributes.InfixPrecedence = 100;
    
    if (ConsumeIf(tok::equal)) {
      SMLoc PrecLoc = Tok.getLoc();
      llvm::StringRef Text = Tok.getText();
      if (!ParseToken(tok::numeric_constant,
                      "expected precedence number in 'infix' attribute")) {
        long long Value;
        if (Text.getAsInteger(10, Value) || Value > 255 || Value < 0)
          Error(PrecLoc, "invalid precedence: value must be between 0 and 255");
        else
          Attributes.InfixPrecedence = Value;
      }
    }
    
    return false;
  }
  
  Error(Tok.getLoc(), "unknown declaration attribute");
  SkipUntil(tok::r_square);
  return true;
}

/// ParseAttributeList
///   attribute-list:
///     '[' ']'
///     '[' attribute (',' attribute)* ']'
void Parser::ParseAttributeList(DeclAttributes &Attributes) {
  Attributes.LSquareLoc = Tok.getLoc();
  ConsumeToken(tok::l_square);
  
  // If this is an empty attribute list, consume it and return.
  if (Tok.is(tok::r_square)) {
    Attributes.RSquareLoc = Tok.getLoc();
    ConsumeToken(tok::r_square);
    return;
  }
  
  bool HadError = ParseAttribute(Attributes);
  while (Tok.is(tok::comma)) {
    ConsumeToken(tok::comma);
    HadError |= ParseAttribute(Attributes);
  }

  Attributes.RSquareLoc = Tok.getLoc();
  if (ConsumeIf(tok::r_square))
    return;
  
  // Otherwise, there was an error parsing the attribute list.  If we already
  // reported an error, skip to a ], otherwise report the error.
  if (!HadError)
    ParseToken(tok::r_square, "expected ']' or ',' in attribute list",
               tok::r_square);
  else {
    SkipUntil(tok::r_square);
    ConsumeIf(tok::r_square);
  }
}

/// NameRecord - This represents either a single identifier or a tree with
/// children.
namespace swift {
class NameRecord {
public:
  Identifier Name;  // In the identifier case, this is the identifier.
  llvm::SMLoc Loc;  // This is the first character of this name record.
  unsigned NumChildren;
  NameRecord *Children;
  
  NameRecord() : NumChildren(0), Children(0) {}
};
}

/// ParseVarName
///   var-name:
///     identifier
///     '(' ')'
///     '(' name (',' name)* ')'
bool Parser::ParseVarName(NameRecord &Record) {
  Record.Loc = Tok.getLoc();

  // Single name case.
  if (Tok.is(tok::identifier)) {
    Record.Name = S.Context.getIdentifier(Tok.getText());
    ConsumeToken(tok::identifier);
    return false;
  }
  
  if (ParseToken(tok::l_paren, "expected identifier or '(' in var name"))
    return true;
  
  llvm::SmallVector<NameRecord, 8> ChildNames;
  
  if (Tok.isNot(tok::r_paren)) {
    do {
      ChildNames.push_back(NameRecord());
      if (ParseVarName(ChildNames.back())) return true;
    } while (ConsumeIf(tok::comma));
  }

  Record.Children = 
    (NameRecord *)S.Context.Allocate(sizeof(NameRecord)*ChildNames.size(), 8);
  memcpy(Record.Children, ChildNames.data(),
         sizeof(NameRecord)*ChildNames.size());
  Record.NumChildren = ChildNames.size();

  if (ParseToken(tok::r_paren, "expected ')' at end of var name"))
    Note(Record.Loc, "to match this '('");
  
  return false;
}


/// ParseDeclTypeAlias
///   decl-typealias:
///     'typealias' identifier ':' type
TypeAliasDecl *Parser::ParseDeclTypeAlias() {
  SMLoc TypeAliasLoc = Tok.getLoc();
  ConsumeToken(tok::kw_typealias);
  
  llvm::StringRef Id;
  Type *Ty = 0;
  if (ParseIdentifier(Id, "expected identifier in var declaration") ||
      ParseToken(tok::colon, "expected ':' in typealias declaration") ||
      ParseType(Ty, "expected type in var declaration"))
    return 0;

  return S.decl.ActOnTypeAlias(TypeAliasLoc, S.Context.getIdentifier(Id), Ty);
}


/// AddElementNamesForVarDecl - This recursive function walks a name specifier
/// adding ElementRefDecls for the named subcomponents and checking that types
/// match up correctly.
static void AddElementNamesForVarDecl(const NameRecord &Name,
                                    llvm::SmallVectorImpl<unsigned> &AccessPath,
                                      VarDecl *VD, SemaDecl &SD) {
  // If this is a leaf name, ask sema to create a ElementRefDecl for us with the
  // specified access path.
  if (Name.Name.get()) {
    ValueDecl *END = 
      SD.ActOnElementName(Name.Name, Name.Loc, VD,
                          AccessPath.data(), AccessPath.size());
    SD.AddToScope(END);
    return;
  }
  
  // Otherwise, we have the paren case.  Verify that the currently named type
  // has the right number of elements.  If so, we recursively process each.
  if (SD.CheckAccessPathArity(Name.NumChildren, Name.Loc, VD,
                              AccessPath.data(), AccessPath.size()))
    return;
 
  AccessPath.push_back(0);
  for (unsigned i = 0, e = Name.NumChildren; i != e; ++i) {
    AccessPath.back() = i;
    AddElementNamesForVarDecl(Name.Children[i], AccessPath, VD, SD);
  }
  AccessPath.pop_back();
}

/// ParseDeclVar - Parse a 'var' declaration, returning null (and doing no
/// token skipping) on error.
///
///   decl-var:
///      'var' attribute-list? var-name value-specifier
VarDecl *Parser::ParseDeclVar() {
  SMLoc VarLoc = Tok.getLoc();
  ConsumeToken(tok::kw_var);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    ParseAttributeList(Attributes);

  NameRecord Name;
  if (ParseVarName(Name)) return 0;
  
  Type *Ty = 0;
  NullablePtr<Expr> Init;
  if (ParseValueSpecifier(Ty, Init))
    return 0;

  VarDecl *VD = S.decl.ActOnVarDecl(VarLoc, Name.Name, Ty, Init.getPtrOrNull(),
                                    Attributes);
  if (VD == 0) return 0;
  
  // Enter the declaration into the current scope.  Since var's are not allowed
  // to be recursive, so they are entered after its initializer is parsed.  This
  // does mean that stuff like this is different than C:
  //    var x = 1; { var x = x+1; assert(x == 2); }
  if (Name.Name.get())
    S.decl.AddToScope(VD);
  else {
    // If there is a more interesting name presented here, then we need to walk
    // through it and synthesize the decls that reference the var elements as
    // appropriate.
    llvm::SmallVector<unsigned, 8> AccessPath;
    AddElementNamesForVarDecl(Name, AccessPath, VD, S.decl);
    
  }
  return VD;
}


/// ParseDeclFunc - Parse a 'func' declaration, returning null on error.  The
/// caller handles this case and does recovery as appropriate.
///
///   decl-func:
///     'func' attribute-list? identifier arg-list-type '=' expr
///     'func' attribute-list? identifier arg-list-type expr-brace
///     'func' attribute-list? identifier arg-list-type 
FuncDecl *Parser::ParseDeclFunc() {
  SMLoc FuncLoc = Tok.getLoc();
  ConsumeToken(tok::kw_func);

  DeclAttributes Attributes;
  // FIXME: Implicitly add immutable attribute.
  if (Tok.is(tok::l_square))
    ParseAttributeList(Attributes);

  llvm::StringRef Identifier;
  if (ParseIdentifier(Identifier, "expected identifier in func declaration"))
    return 0;
  
  // We force first type of a func declaration to be a tuple for consistency.
  if (Tok.isNot(tok::l_paren)) {
    Error(Tok.getLoc(), "expected '(' in argument list of func declaration");
    return 0;
  }
    
  Type *FuncTy = 0;
  if (ParseType(FuncTy))
    return 0;
  
  // If the parsed function type is not a function, then it is implicitly a
  // function that returns void.
  if (!llvm::isa<FunctionType>(FuncTy))
    FuncTy = S.type.ActOnFunctionType(FuncTy, SMLoc(),
                                      S.Context.TheEmptyTupleType);

  // Build the decl for the function.
  FuncDecl *FD = S.decl.ActOnFuncDecl(FuncLoc,
                                      S.Context.getIdentifier(Identifier),
                                      FuncTy, Attributes);
  
  // Enter the func into the current scope, which allows it to be visible and
  // used within its body.
  if (FD)
    S.decl.AddToScope(FD);
  
  // Enter the arguments for the function into a new function-body scope.  We
  // need this even if there is no function body to detect argument name
  // duplication.
  Scope FnBodyScope(S.decl);
  
  if (FD)
    S.decl.CreateArgumentDeclsForFunc(FD);

  // Then parse the expression.
  llvm::NullablePtr<Expr> Body;

  // Check to see if we have a "= expr" or "{" which is a brace expr.
  if (ConsumeIf(tok::equal)) {
    if (ParseExpr(Body, "expected expression parsing func body") ||
        Body.isNull())
      return 0;  // FIXME: Need to call a new ActOnFuncBodyError?
  } else if (Tok.is(tok::l_brace)) {
    if (ParseExprBrace(Body) || Body.isNull())
      return 0;  // FIXME: Need to call a new ActOnFuncBodyError?
  }

  // If this is a declaration, we're done.
  if (Body.isNull())
    return FD;

  return S.decl.ActOnFuncBody(FD, Body.get());
}

/// ParseDeclOneOf - Parse a 'oneof' declaration, returning null (and doing no
/// token skipping) on error.
///
///   decl-oneof:
///      'oneof' attribute-list? identifier oneof-body
///   oneof-body:
///      '{' oneof-element-list '}'
///   oneof-element-list:
///      oneof-element ','?
///      oneof-element ',' oneof-element-list
///   oneof-element:
///      identifier
///      identifier ':' type
///      
Decl *Parser::ParseDeclOneOf() {
  SMLoc OneOfLoc = Tok.getLoc();
  ConsumeToken(tok::kw_oneof);

  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    ParseAttributeList(Attributes);
  
  SMLoc NameLoc = Tok.getLoc();
  llvm::StringRef OneOfName;
  if (ParseIdentifier(OneOfName, "expected identifier in oneof declaration"))
    return 0;

  Type *OneOfType = 0;
  if (ParseTypeOneOfBody(OneOfLoc, Attributes, OneOfType))
    return 0;

  return S.decl.ActOnTypeAlias(NameLoc, S.Context.getIdentifier(OneOfName),
                               OneOfType);
}


/// ParseDeclStruct - Parse a 'struct' declaration, returning null (and doing no
/// token skipping) on error.  A 'struct' is just syntactic sugar for a oneof
/// with a single element.
///
///   decl-struct:
///      'struct' attribute-list? identifier type
///
Decl *Parser::ParseDeclStruct() {
  SMLoc StructLoc = Tok.getLoc();
  ConsumeToken(tok::kw_struct);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    ParseAttributeList(Attributes);
  
  llvm::StringRef StructName;
  if (ParseIdentifier(StructName, "expected identifier in struct declaration"))
    return 0;

  if (Tok.isNot(tok::l_paren)) {
    Error(Tok.getLoc(), "expected '(' in struct declaration");
    return 0;
  }
  
  Type *Ty = 0;
  if (ParseType(Ty)) return 0;
  
  return S.decl.ActOnStructDecl(StructLoc, Attributes, StructName, Ty);
}


//===----------------------------------------------------------------------===//
// Type Parsing
//===----------------------------------------------------------------------===//

/// ParseType
///   type:
///     type-simple
///     type-function
///     type-array
///
///   type-function:
///     type-simple '->' type 
///
///   type-array:
///     type '[' ']'
///     type '[' expr ']'
///
///   type-simple:
///     '__builtin_int32_type'
///     identifier
///     type-tuple
///     type-oneof
///
///   type-oneof:
///     'oneof' attribute-list? oneof-body
///
bool Parser::ParseType(Type *&Result, const llvm::Twine &Message) {
  // Parse type-simple first.
  switch (Tok.getKind()) {
  case tok::identifier:
    Result = S.type.ActOnTypeName(Tok.getLoc(),
                                  S.Context.getIdentifier(Tok.getText()));
    ConsumeToken(tok::identifier);
    break;
  case tok::kw___builtin_int32_type:
    Result = S.type.ActOnInt32Type(Tok.getLoc());
    ConsumeToken(tok::kw___builtin_int32_type);
    break;
  case tok::l_paren:
    if (ParseTypeTuple(Result))
      return true;
    break;
  case tok::kw_oneof: {
    SMLoc OneOfLoc = Tok.getLoc();
    ConsumeToken(tok::kw_oneof);
      
    DeclAttributes Attributes;
    if (Tok.is(tok::l_square))
      ParseAttributeList(Attributes);

    if (ParseTypeOneOfBody(OneOfLoc, Attributes, Result))
      return true;
    break;
  }
  default:
    Error(Tok.getLoc(), Message);
    return true;
  }
  
   while (1) {
    // If there is an arrow, parse the rest of the type.
    SMLoc TokLoc = Tok.getLoc();
    if (ConsumeIf(tok::arrow)) {
      Type *SecondHalf = 0;
      if (ParseType(SecondHalf, "expected type in result of function type"))
        return true;
      Result = S.type.ActOnFunctionType(Result, TokLoc, SecondHalf);
      continue;
    }
    
    // If there is a square bracket, we have an array.
    if (ConsumeIf(tok::l_square)) {
      llvm::NullablePtr<Expr> Size;
      if (!Tok.is(tok::r_square) &&
          ParseExpr(Size, "expected expression for array type size"))
        return true;
      
      SMLoc RArrayTok = Tok.getLoc();
      if (ParseToken(tok::r_square, "expected ']' in array type")) {
        Note(TokLoc, "to match this '['");
        return true;
      }
      
      Result = S.type.ActOnArrayType(Result, TokLoc, Size.getPtrOrNull(),
                                     RArrayTok);
      continue;
    }
    
    break;
  }
        
  
  return false;
}

bool Parser::ParseType(Type *&Result) {
  return ParseType(Result, "expected type");
}

/// ParseTypeTuple
///   type-tuple:
///     '(' ')'
///     '(' identifier? value-specifier (',' identifier? value-specifier)* ')'
///
bool Parser::ParseTypeTuple(Type *&Result) {
  assert(Tok.is(tok::l_paren) && "Not start of type tuple");
  SMLoc LPLoc = Tok.getLoc();
  ConsumeToken(tok::l_paren);

  llvm::SmallVector<TupleTypeElt, 8> Elements;

  if (Tok.isNot(tok::r_paren)) {
    bool HadError = false;
    do {
      Elements.push_back(TupleTypeElt());
      TupleTypeElt &Result = Elements.back();
      
      if (Tok.is(tok::identifier)) {
        llvm::StringRef Name;
        HadError = ParseIdentifier(Name,"expected identifier in tuple element");
        if (HadError) break;
        Result.Name = S.Context.getIdentifier(Name);
      }
      
      NullablePtr<Expr> Init;
      if ((HadError = ParseValueSpecifier(Result.Ty, Init)))
        break;
      Result.Init = Init.getPtrOrNull();
    } while (ConsumeIf(tok::comma));
    
    if (HadError) {
      SkipUntil(tok::r_paren);
      if (Tok.is(tok::r_paren))
        ConsumeToken(tok::r_paren);
      return true;
    }
  }
  
  SMLoc RPLoc = Tok.getLoc();
  if (ParseToken(tok::r_paren, "expected ')' at end of tuple list",
                 tok::r_paren)) {
    Note(LPLoc, "to match this opening '('");
    return true;
  }
  
  Result = S.type.ActOnTupleType(LPLoc, Elements.data(), Elements.size(),RPLoc);
  return false;
}

///   oneof-body:
///      '{' oneof-element-list '}'
///   oneof-element-list:
///      oneof-element ','?
///      oneof-element ',' oneof-element-list
///   oneof-element:
///      identifier
///      identifier ':' type
///      
bool Parser::ParseTypeOneOfBody(SMLoc OneOfLoc, const DeclAttributes &Attrs,
                                Type *&Result) {
  if (ParseToken(tok::l_brace, "expected '{' in oneof type"))
    return true;
  
  llvm::SmallVector<SemaType::OneOfElementInfo, 8> ElementInfos;
  
  // Parse the comma separated list of oneof elements.
  while (Tok.is(tok::identifier)) {
    SemaType::OneOfElementInfo ElementInfo;
    ElementInfo.Name = Tok.getText();
    ElementInfo.NameLoc = Tok.getLoc();
    ElementInfo.EltType = 0;
    
    ConsumeToken(tok::identifier);
    
    // See if we have a type specifier for this oneof element.  If so, parse it.
    if (ConsumeIf(tok::colon))
      if (ParseType(ElementInfo.EltType,
                    "expected type while parsing oneof element '" +
                    ElementInfo.Name + "'")) {
        SkipUntil(tok::r_brace);
        return true;
      }
    
    ElementInfos.push_back(ElementInfo);
    
    // Require comma separation.
    if (!ConsumeIf(tok::comma))
      break;
  }
  
  ParseToken(tok::r_brace, "expected '}' at end of oneof");
  
  Result = S.type.ActOnOneOfType(OneOfLoc, Attrs, ElementInfos);
  return false;
}


//===----------------------------------------------------------------------===//
// Expression Parsing
//===----------------------------------------------------------------------===//

bool Parser::isStartOfExpr(Token &Tok) const {
  if (Tok.is(tok::identifier)) {
    // If this is a binary operator, then it isn't the start of an expr.
    ValueDecl *VD =
      S.decl.LookupValueName(S.Context.getIdentifier(Tok.getText()));
    
    // Use of undeclared identifier.
    if (VD == 0) return true;
    
    return VD->Attrs.InfixPrecedence == -1;
  }
  
  return Tok.is(tok::numeric_constant) || Tok.is(tok::colon) ||
         Tok.is(tok::l_paren) || Tok.is(tok::l_brace) ||
         Tok.is(tok::dollarident);
}

/// ParseExpr
///   expr:
///     expr-single+
bool Parser::ParseExpr(NullablePtr<Expr> &Result, const char *Message) {
  llvm::SmallVector<Expr*, 8> SequencedExprs;
  
  Expr *LastExpr = 0;
  do {
    // Parse the expr-single.
    Result = 0;
    if (ParseExprSingle(Result) || Result.isNull()) return true;

    // Check to see if this juxtaposition is application of a function with its
    // arguments.  If so, bind the function application, otherwise, we have a
    // sequence.
    if (LastExpr == 0)
      LastExpr = Result.get();
    else {
      llvm::PointerIntPair<Expr*, 1, bool>
        ApplyRes = S.expr.ActOnJuxtaposition(LastExpr, Result.get());

      if (!ApplyRes.getInt()) {
        // Function application.
        LastExpr = ApplyRes.getPointer();
        if (LastExpr == 0) return true;
      } else {
        // Sequencing.
        assert(ApplyRes.getPointer() == 0 && "Sequencing with a result?");
        SequencedExprs.push_back(LastExpr);
        LastExpr = Result.get();
      }
    }
  } while (isStartOfExpr(Tok));

  assert(LastExpr && "Should have parsed at least one valid expression");
  
  // If there is exactly one element in the sequence, it is a degenerate
  // sequence that just returns the last value anyway, shortcut ActOnSequence.
  if (SequencedExprs.empty()) {
    Result = LastExpr;
    return false;
  }
  
  SequencedExprs.push_back(LastExpr);
  Result = S.expr.ActOnSequence(SequencedExprs.data(), SequencedExprs.size());
  return false;
}

/// ParseExprSingle
///   expr-single:
///     expr-primary (binary-operator expr-primary)*
bool Parser::ParseExprSingle(llvm::NullablePtr<Expr> &Result,
                             const char *Message) {
  return ParseExprPrimary(Result, Message) || ParseExprBinaryRHS(Result);
}

/// ParseExprPrimary
///   expr-primary:
///     expr-literal
///     expr-identifier
///     ':' identifier
///     expr-paren
///     expr-brace
///     expr-field
///     expr-subscript
///     expr-primary-fn expr-primary
///
///   expr-literal:
///     numeric_constant
///
///   expr-primary-fn:
///     expr-primary      Type sensitive: iff expr has fn type
///
///   expr-field:
///     expr-primary '.' identifier
///     expr-primary '.' dollarident
///
///   expr-subscript:
///     expr-primary '[' expr-single ']'
bool Parser::ParseExprPrimary(NullablePtr<Expr> &Result, const char *Message) {
  switch (Tok.getKind()) {
  case tok::numeric_constant:
    Result = S.expr.ActOnNumericConstant(Tok.getText(), Tok.getLoc());
    ConsumeToken(tok::numeric_constant);
    break;

  case tok::dollarident: // $1
  case tok::identifier:  // foo   and  foo::bar
    if (ParseExprIdentifier(Result)) return true;
    break;

  case tok::colon: {     // :foo
    SMLoc ColonLoc = Tok.getLoc();
    ConsumeToken(tok::colon);
    llvm::StringRef Name;
    SMLoc NameLoc = Tok.getLoc();
    if (ParseIdentifier(Name, "expected identifier after ':' expression"))
      return true;
    Result = S.expr.ActOnUnresolvedMemberExpr(ColonLoc, NameLoc, Name);
    break;
  }
      
  case tok::l_paren:
    if (ParseExprParen(Result)) return true;
    break;
      
  case tok::l_brace:
    if (ParseExprBrace(Result)) return true;
    break;
    
  default:
    Error(Tok.getLoc(), Message ? Message : "expected expression");
    return true;
  }
  
  // Handle suffix expressions.
  while (1) {
    // Check for a .foo suffix.
    SMLoc TokLoc = Tok.getLoc();
    if (ConsumeIf(tok::period)) {
      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::dollarident)) {
        Error(Tok.getLoc(), "expected field name");
        return true;
      }
        
      if (!Result.isNull())
        Result = S.expr.ActOnDotIdentifier(Result.get(), TokLoc, Tok.getText(),
                                           Tok.getLoc());
      if (Tok.is(tok::identifier))
        ConsumeToken(tok::identifier);
      else
        ConsumeToken(tok::dollarident);
      continue;
    }
    
    // Check for a [expr] suffix.
    if (ConsumeIf(tok::l_square)) {
      NullablePtr<Expr> Idx;
      if (ParseExprSingle(Idx, "expected expression parsing array index"))
        return true;
      
      SMLoc RLoc = Tok.getLoc();
      if (ParseToken(tok::r_square, "expected ']'")) {
        Note(TokLoc, "to match this '['");
        return true;        
      }
      
      if (!Result.isNull() && !Idx.isNull())
        Result = S.expr.ActOnArraySubscript(Result.get(), TokLoc, Idx.get(),
                                            RLoc);
    }
        
    break;
  }
  
  // Okay, we parsed the expression primary and any suffix expressions.  If the
  // result has function type and if this is followed by another expression,
  // then we have a juxtaposition case which is parsed.  Note that this
  // production is ambiguous with the higher level "expr: expr-single+"
  // production, as witnessed by examples like:
  //     A + B C * D
  // Which can be parsed either as:
  //     A + (B C) * D         <-- Juxtaposition here
  //     (A + B) (C * D)       <-- Juxtaposition in the expr production
  // This is disambiguated based on whether B has function type or not.
  while (!Result.isNull() && isStartOfExpr(Tok)) {
    NullablePtr<Expr> RHS;
    switch (S.expr.getJuxtapositionGreediness(Result.get())) {
    default: assert(0 && "Unknown juxtaposition greediness");
    case SemaExpr::JG_NonGreedy: return false;
    case SemaExpr::JG_LocallyGreedy:
      if (ParseExprPrimary(RHS,
                           "expected expression after juxtaposed operator") ||
          RHS.isNull())
        return true;
      break;
    case SemaExpr::JG_Greedy:
      if (ParseExprSingle(RHS,
                          "expected expression after juxtaposed operator") ||
          RHS.isNull())
        return true;
      break;
    }
    
    llvm::PointerIntPair<Expr*, 1, bool>
    Op = S.expr.ActOnJuxtaposition(Result.get(), RHS.get());
    assert(!Op.getInt() && "ShouldGreedilyJuxtapose guaranteed an apply");
    
    Result = Op.getPointer();
  }
  
  return false;
}

/// ParseExprIdentifier - Parse an identifier expression:
///
///   expr-identifier:
///     identifier
///     dollarident
///     identifier '::' identifier
bool Parser::ParseExprIdentifier(llvm::NullablePtr<Expr> &Result) {
  llvm::StringRef Name = Tok.getText();
  SMLoc Loc = Tok.getLoc();
  
  if (Tok.is(tok::dollarident)) {
    ConsumeToken(tok::dollarident);
    assert(Name[0] == '$' && "Not a dollarident");
    bool AllNumeric = true;
    for (unsigned i = 1, e = Name.size(); i != e; ++i)
      AllNumeric &= isdigit(Name[i]);
    
    if (Name.size() == 1 || !AllNumeric) {
      Error(Loc, "invalid identifier, expected expression");
      return true;
    }
    Result = S.expr.ActOnDollarIdentExpr(Name, Loc);
    return false;
  }

  ConsumeToken(tok::identifier);

  if (Tok.isNot(tok::coloncolon)) {
    Result = S.expr.ActOnIdentifierExpr(Name, Loc);
    return false;
  }
  
  SMLoc ColonColonLoc = Tok.getLoc();
  ConsumeToken(tok::coloncolon);

  SMLoc Loc2 = Tok.getLoc();
  llvm::StringRef Name2;
  if (ParseIdentifier(Name2, "expected identifier after '" + Name +
                      "::' expression"))
    return true;

  Result = S.expr.ActOnScopedIdentifierExpr(Name, Loc, ColonColonLoc,
                                            Name2, Loc2);
  return false;
}


/// ParseExprParen - Parse a tuple expression.
///
///   expr-paren: 
///     '(' ')'
///     '(' expr-paren-element (',' expr-paren-element)* ')'
///
///   expr-paren-element:
///     ('.' identifier '=')? expr
///
bool Parser::ParseExprParen(llvm::NullablePtr<Expr> &Result) {
  SMLoc LPLoc = Tok.getLoc();  
  ConsumeToken(tok::l_paren);
  
  llvm::SmallVector<Expr*, 8> SubExprs;
  llvm::SmallVector<Identifier, 8> SubExprNames; 
  bool AnyErroneousSubExprs = false;
  
  if (Tok.isNot(tok::r_paren)) {
    do {
      Identifier FieldName;
      // Check to see if there is a field specifier.
      if (ConsumeIf(tok::period)) {
        llvm::StringRef FieldNameStr;
        if (ParseIdentifier(FieldNameStr,
                          "expected field specifier name in tuple expression")||
            ParseToken(tok::equal, "expected '=' in tuple expression"))
          return true;
        FieldName = S.Context.getIdentifier(FieldNameStr);
      }
      
      if (!SubExprNames.empty())
        SubExprNames.push_back(FieldName);
      else if (FieldName.get()) {
        SubExprNames.resize(SubExprs.size());
        SubExprNames.push_back(FieldName);
      }
      
      NullablePtr<Expr> SubExpr;
      if (ParseExpr(SubExpr, "expected expression in parentheses")) return true;
      
      if (SubExpr.isNull())
        AnyErroneousSubExprs = true;
      else
        SubExprs.push_back(SubExpr.get());
    
    } while (ConsumeIf(tok::comma));
  }
  
  SMLoc RPLoc = Tok.getLoc();  
  if (ParseToken(tok::r_paren, "expected ')' in parenthesis expression")) {
    Note(LPLoc, "to match this opening '('");
    return true;
  }
  
  if (!AnyErroneousSubExprs)
    Result = S.expr.ActOnTupleExpr(LPLoc, SubExprs.data(),
                                   SubExprNames.empty()?0 : SubExprNames.data(),
                                   SubExprs.size(), RPLoc);
  return false;
}


/// ParseExprBrace - A brace enclosed expression list which may optionally end
/// with a ; inside of it.  For example { 1; 4+5; } or { 1; 2 }.
///
///   expr-brace:
///     '{' expr-brace-item* '}'
///   expr-brace-item:
///     expr
///     decl-var
///     ';'
bool Parser::ParseExprBrace(NullablePtr<Expr> &Result) {
  SMLoc LBLoc = Tok.getLoc();
  ConsumeToken(tok::l_brace);
  
  // This brace expression forms a lexical scope.
  Scope BraceScope(S.decl);

  llvm::SmallVector<llvm::PointerUnion<Expr*, Decl*>, 16> Entries;
  
  // MissingSemiAtEnd - Keep track of whether the last expression in the block
  // had no semicolon.
  bool MissingSemiAtEnd = false;
  
  while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
    MissingSemiAtEnd = false;

    // If this is a semi, eat it and ignore it.
    if (ConsumeIf(tok::semi))
      continue;
    
    // Otherwise, we must have a var decl or expression.  Parse it up
    Entries.push_back(llvm::PointerUnion<Expr*, Decl*>());
    
    // Parse the decl or expression.    
    switch (Tok.getKind()) {
    case tok::kw_var:
      Entries.back() = ParseDeclVar();
      break;
    case tok::kw_typealias:
      Entries.back() = ParseDeclTypeAlias();
      break;
    case tok::kw_oneof:
      Entries.back() = ParseDeclOneOf();
      break;
    case tok::kw_struct:
      Entries.back() = ParseDeclStruct();
      break;
    default:
      NullablePtr<Expr> ResultExpr;
      if (!ParseExpr(ResultExpr) && !ResultExpr.isNull()) {
        Entries.back() = ResultExpr.get();
        MissingSemiAtEnd = true;
      }
      break;
    }
    
    if (Entries.back().isNull()) {
      if (Tok.is(tok::semi)) {
        Entries.pop_back();
        continue;  // Consume the ';' and keep going.
      }
      
      // FIXME: QOI: Improve error recovery.
      if (Tok.is(tok::semi) && Tok.isNot(tok::r_brace))
        SkipUntil(tok::r_brace);
      ConsumeIf(tok::r_brace);
      return true;
    }
  }
  
  SMLoc RBLoc = Tok.getLoc();
  if (ParseToken(tok::r_brace, "expected '}' at end of brace expression",
                 tok::r_brace)) {
    Note(LBLoc, "to match this opening '{'");
    return true;
  }
  
  Result = S.expr.ActOnBraceExpr(LBLoc, Entries, MissingSemiAtEnd, RBLoc);
  return false;
}


/// getBinOp - Return the ValueDecl for the token if it is an infix binary
/// operator, otherwise return null.
static ValueDecl *getBinOp(const Token &Tok, Sema &S) {
  if (Tok.isNot(tok::identifier))
    return 0;
  ValueDecl *VD =S.decl.LookupValueName(S.Context.getIdentifier(Tok.getText()));
  if (VD == 0 || VD->Attrs.InfixPrecedence == -1)
    return 0;
  return VD;
}


/// ParseExprBinaryRHS - Parse the right hand side of a binary expression and
/// assemble it according to precedence rules.
///
///   expr-binary-rhs:
///     (binary-operator expr-primary)*
bool Parser::ParseExprBinaryRHS(NullablePtr<Expr> &Result, unsigned MinPrec) {
  ValueDecl *NextTokOp = getBinOp(Tok, S);
  int NextTokPrec = NextTokOp ? NextTokOp->Attrs.InfixPrecedence : -1;
  // Assignment is a hack until we get generics.  Assignment gets the lowest
  // precedence since it "returns void".
  if (Tok.is(tok::equal)) NextTokPrec = 1;
  while (1) {
    // If this token has a lower precedence than we are allowed to parse (e.g.
    // because we are called recursively, or because the token is not a binop),
    // then we are done!
    if (NextTokPrec < (int)MinPrec)
      return false;
    
    // Consume the operator, saving the operator location.
    SMLoc OpLoc = Tok.getLoc();
    ConsumeToken();
    
    // TODO: Support ternary operators some day.
    
    // Parse another leaf here for the RHS of the operator.
    NullablePtr<Expr> Leaf;
    if (ParseExprPrimary(Leaf, "expected expression after binary operator"))
      return true;

    // Remember the precedence of this operator and get the precedence of the
    // operator immediately to the right of the RHS.
    int ThisPrec = NextTokPrec;
    ValueDecl *ThisTokOp = NextTokOp;
    
    NextTokOp = getBinOp(Tok, S);
    NextTokPrec = NextTokOp ? NextTokOp->Attrs.InfixPrecedence : -1;
    if (Tok.is(tok::equal)) NextTokPrec = 1;

    // TODO: All operators are left associative at the moment.
    
    // If the next operator binds more tightly with RHS than we do, evaluate the
    // RHS as a complete subexpression first
    if (ThisPrec < NextTokPrec) {
      // Only parse things on the RHS that bind more tightly than the current
      // operator.
      if (ParseExprBinaryRHS(Leaf, ThisPrec + 1))
        return true;
      
      NextTokOp = getBinOp(Tok, S);
      NextTokPrec = NextTokOp ? NextTokOp->Attrs.InfixPrecedence : -1;
    }
    assert(NextTokPrec <= ThisPrec && "Recursion didn't work!");
    
    // Okay, we've finished the parse, form the AST node for the binop now.
    if (Result.isNonNull() && Leaf.isNonNull())
      Result = S.expr.ActOnBinaryExpr(Result.get(), ThisTokOp, OpLoc,
                                      Leaf.get());
  }
  
  return false;
}
