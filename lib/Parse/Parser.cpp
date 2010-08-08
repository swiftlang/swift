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
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/PointerUnion.h"
using namespace swift;
using llvm::SMLoc;
using llvm::NullablePtr;

//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//

Parser::Parser(unsigned BufferID, ASTConsumer &consumer)
  : Consumer(consumer),
    SourceMgr(Consumer.getContext().SourceMgr),
    L(*new Lexer(BufferID, SourceMgr)),
    S(*new Sema(Consumer.getContext())) {
}

Parser::~Parser() {
  delete &L;
  delete &S;
}

void Parser::Note(SMLoc Loc, const char *Message) {
  SourceMgr.PrintMessage(Loc, Message, "note");
}

void Parser::Warning(SMLoc Loc, const char *Message) {
  SourceMgr.PrintMessage(Loc, Message, "warning");
}

void Parser::Error(SMLoc Loc, const char *Message) {
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
bool Parser::ParseIdentifier(llvm::StringRef &Result, const char *Message,
                             tok::TokenKind SkipToTok) {
  if (Tok.is(tok::identifier)) {
    Result = Tok.getText();
    ConsumeToken();
    return false;
  }
  
  Error(Tok.getLoc(), Message ? Message : "expected identifier");
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
  
  // Notify consumer about the end of the translation unit.
  Consumer.HandleEndOfTranslationUnit();
}

/// ParseDeclTopLevel
///   decl-top-level:
///     ';'
///     decl-var ';'
///     decl-func
Decl *Parser::ParseDeclTopLevel() {
  switch (Tok.getKind()) {
  default:
    Error(Tok.getLoc(), "expected a top level declaration");
    break;
  case tok::semi:
    ConsumeToken(tok::semi);
    return 0; // Could do a top-level semi decl.
      
  case tok::kw_typealias:
    if (ParseTypeAlias()) break;
    return 0;
      
  case tok::kw_var:
    if (VarDecl *D = ParseDeclVar()) {
      S.decl.ActOnTopLevelDecl(D);
            
      // On successful parse, eat the ;
      ParseToken(tok::semi, "expected ';' at end of var declaration",
                 tok::semi);
      return D;
    }
    break;
  case tok::kw_func:
    if (FuncDecl *D = ParseDeclFunc()) {
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


/// ParseDeclAttribute
///   decl-attribute:
///     'infix' '=' numeric_constant
bool Parser::ParseDeclAttribute(DeclAttributes &Attributes) {
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

/// ParseDeclAttributeList
///   decl-attribute-list:
///     '[' ']'
///     '[' decl-attribute (',' decl-attribute)* ']'
void Parser::ParseDeclAttributeList(DeclAttributes &Attributes) {
  Attributes.LSquareLoc = Tok.getLoc();
  ConsumeToken(tok::l_square);
  
  // If this is an empty attribute list, consume it and return.
  if (Tok.is(tok::r_square)) {
    Attributes.RSquareLoc = Tok.getLoc();
    ConsumeToken(tok::r_square);
    return;
  }
  
  bool HadError = ParseDeclAttribute(Attributes);
  while (Tok.is(tok::comma)) {
    ConsumeToken(tok::comma);
    HadError |= ParseDeclAttribute(Attributes);
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

/// ParseName
///   name:
///     identifier
///     '(' ')'
///     '(' name (',' name)* ')'
bool Parser::ParseName(NameRecord &Record) {
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
      if (ParseName(ChildNames.back())) return true;
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


/// ParseTypeAlias
///   alias-type:
///     'typealias' identifier ':' type
bool Parser::ParseTypeAlias() {
  SMLoc TypeAliasLoc = Tok.getLoc();
  ConsumeToken(tok::kw_typealias);
  
  llvm::StringRef Identifier;
  Type *Ty = 0;
  if (ParseIdentifier(Identifier, "expected identifier in var declaration") ||
      ParseToken(tok::colon, "expected ':' in typealias declaration") ||
      ParseType(Ty, "expected type in var declaration"))
    return true;

  S.type.ActOnTypeAlias(TypeAliasLoc, Identifier, Ty);
  return false;
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
    NamedDecl *END = 
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
///      'var' decl-attribute-list? name ':' type
///      'var' decl-attribute-list? name ':' type '=' expression 
///      'var' decl-attribute-list? name '=' expression
VarDecl *Parser::ParseDeclVar() {
  SMLoc VarLoc = Tok.getLoc();
  ConsumeToken(tok::kw_var);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    ParseDeclAttributeList(Attributes);
  
  NameRecord Name;
  if (ParseName(Name)) return 0;

  Type *Ty = 0;
  if (ConsumeIf(tok::colon) &&
      ParseType(Ty, "expected type in var declaration"))
    return 0;
  
  NullablePtr<Expr> Init;
  if (ConsumeIf(tok::equal)) {
    if (ParseExpr(Init, "expected expression in var declaration"))
      return 0;
  
    // If there was an expression, but it had a parse error, give the var decl
    // an artificial int type to avoid chained errors.
    // FIXME: We really need to distinguish erroneous expr from missing expr in
    // ActOnVarDecl.
    if (Init.isNull() && Ty == 0)
      Ty = S.Context.TheInt32Type;
  }

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
///     'func' decl-attribute-list? identifier arg-list-type '='? expr
///     'func' decl-attribute-list? identifier arg-list-type ';'
FuncDecl *Parser::ParseDeclFunc() {
  SMLoc FuncLoc = Tok.getLoc();
  ConsumeToken(tok::kw_func);

  DeclAttributes Attributes;
  // FIXME: Add immutable attribute.
  if (Tok.is(tok::l_square))
    ParseDeclAttributeList(Attributes);

  llvm::StringRef Identifier;
  if (ParseIdentifier(Identifier, "expected identifier in func declaration"))
    return 0;
  
  // We force first type of a func declaration to be a tuple for consistency.
  if (Tok.isNot(tok::l_paren)) {
    Error(Tok.getLoc(), "expected '(' in argument list of func declaration");
    return 0;
  }
    
  Type *FuncTy = 0;
  if (ParseArgListFnType(FuncTy))
    return 0;
  
  // If the parsed function type is not a function, then it is implicitly a
  // function that returns void.
  if (!llvm::isa<FunctionType>(FuncTy))
    FuncTy = S.type.ActOnFunctionType(FuncTy, SMLoc(),
                                      S.Context.TheEmptyTupleType);

  // Build the decl for the function.
  FuncDecl *FD = S.decl.ActOnFuncDecl(FuncLoc, Identifier, FuncTy, Attributes);
  
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

  // If this is a declaration, we're done.
  if (ConsumeIf(tok::semi))
    return FD;

  // Otherwise, we must have an expression.  Eat the optional '=' if present.
  ConsumeIf(tok::equal);

  // Then parse the expression.
  llvm::NullablePtr<Expr> Body;
  if (ParseExpr(Body, "expected expression parsing func body") ||
      Body.isNull())
    return 0;  // FIXME: Need to call a new ActOnFuncBodyError?

  return S.decl.ActOnFuncBody(FD, Body.get());
}

//===----------------------------------------------------------------------===//
// Type Parsing
//===----------------------------------------------------------------------===//

/// ParseType
///   type:
///     type-simple
///     type-simple '->' type
///
///   type-simple:
///     'int'
///     'void'   // FIXME: Should be a 'type alias' for () in standard library.
///     type-tuple
///
bool Parser::ParseType(Type *&Result, const char *Message) {
  // Parse type-simple first.
  switch (Tok.getKind()) {
  case tok::identifier:
    Result = S.type.ActOnTypeName(Tok.getLoc(), Tok.getText());
    if (Result == 0) {
      Error(Tok.getLoc(), Message ? Message : "expected type");
      return true;
    }
    ConsumeToken(tok::identifier);
    break;
  case tok::kw___builtin_int32_type:
    Result = S.type.ActOnInt32Type(Tok.getLoc());
    ConsumeToken(tok::kw___builtin_int32_type);
    break;
  case tok::kw___builtin_else_hack_type:
    Result = S.type.ActOnElseHackType(Tok.getLoc());
    ConsumeToken(tok::kw___builtin_else_hack_type);
    break;
  case tok::l_paren:
    if (ParseTypeTuple(Result))
      return true;
    break;
  default:
    Error(Tok.getLoc(), Message ? Message : "expected type");
    return true;
  }
  
  // If there is an arrow, parse the rest of the type.
  SMLoc ArrowLoc = Tok.getLoc();
  if (ConsumeIf(tok::arrow)) {
    Type *SecondHalf = 0;
    if (ParseType(SecondHalf, "expected type in result of function type"))
      return true;
    Result = S.type.ActOnFunctionType(Result, ArrowLoc, SecondHalf);
  }
  
  return false;
}

/// ParseTypeTupleElement
///   type-tuple-element:
///     type
///     '.' identifier ':' type
bool Parser::ParseTypeTupleElement(TupleTypeElt &Result) {
  if (!ConsumeIf(tok::period))
    return ParseType(Result.Ty, "expected type in tuple element");

  llvm::StringRef Name;
  if (ParseIdentifier(Name, "expected identifier in tuple element") ||
      ParseToken(tok::colon, "expected ':' after tuple element name") ||
      ParseType(Result.Ty, "expected type in tuple element"))
    return true;
  
  Result.Name = S.Context.getIdentifier(Name);
  return false;
}


/// ParseTypeTuple
///   type-tuple:
///     '(' ')'
///     '(' type-tuple-element (',' type-tuple-element)* ')'
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
      if ((HadError = ParseTypeTupleElement(Elements.back())))
        break;
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

/// ParseArgListFnType
///   arg-list-fn-type:
///     arg-list-type ('->' arg-list-fn-type)?
///
bool Parser::ParseArgListFnType(Type *&Result) {
  if (ParseArgListType(Result)) return true;

  SMLoc ArrowLoc = Tok.getLoc();
  if (ConsumeIf(tok::arrow)) {
    Type *FnResult = 0;
    if (ParseArgListFnType(FnResult)) return true;
    
    // Build the function type.
    Result = S.type.ActOnFunctionType(Result, ArrowLoc, FnResult);
  }
  
  return false;
}  

/// ParseArgListType
///   arg-list-type:
///     arg-list-tuple
///     type
///
///   arg-list-tuple:
///     '(' ')'
///     '(' arg-list-tuple-elt (',' arg-list-tuple-elt)* ')'
///   arg-list-tuple-elt:
///     identifier? ':' type
///
/// TODO: eventually allow default arguments and attributes on arguments.
///
bool Parser::ParseArgListType(Type *&Result) {
  if (Tok.isNot(tok::l_paren))
    return ParseType(Result);
  
  SMLoc LPLoc = Tok.getLoc();
  if (ParseToken(tok::l_paren,
                 "expected '(' in argument list of func declaration"))
    return true;
  
  if (ConsumeIf(tok::r_paren)) {
    Result = S.Context.TheEmptyTupleType;
    return false;
  }
  
  llvm::SmallVector<TupleTypeElt, 8> Elements;
  
  // Read the comma-separated argument list.
  do {
    llvm::StringRef ParamIdentifier = Tok.getText();
    SMLoc StartLoc = Tok.getLoc();
    if (!ConsumeIf(tok::identifier))
      ParamIdentifier = llvm::StringRef();
    
    Type *ParamType = 0;
    if (ParseToken(tok::colon,
                   "expected ':' in argument list of func declaration") ||
        ParseType(ParamType, "expected type in func argument list"))
      return true;
    
    Elements.push_back(TupleTypeElt(ParamType,
                                    S.Context.getIdentifier(ParamIdentifier)));
  } while (ConsumeIf(tok::comma));
  
  SMLoc RPLoc = Tok.getLoc();
  if (ParseToken(tok::r_paren,
                 "expected ')' at end of func declaration argument list")) {
    Note(LPLoc, "to match this opening '('");
    return true;
  }

  Result = 
    S.type.ActOnTupleType(LPLoc, Elements.data(), Elements.size(),Tok.getLoc());
  
  return false;
}


//===----------------------------------------------------------------------===//
// Expression Parsing
//===----------------------------------------------------------------------===//

static bool isStartOfExpr(Token &Tok, Sema &S) {
  if (Tok.is(tok::identifier)) {
    // If this is a binary operator, then it isn't the start of an expr.
    NamedDecl *ND = S.decl.LookupName(S.Context.getIdentifier(Tok.getText()));
    
    // Use of undeclared identifier.
    if (ND == 0) return true;
    
    return ND->Attrs.InfixPrecedence == -1;
  }
  
  return Tok.is(tok::numeric_constant) ||
         Tok.is(tok::l_paren) || Tok.is(tok::l_brace);
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
  } while (isStartOfExpr(Tok, S));

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
///     numeric_constant
///     identifier
///     expr-paren
///     expr-brace
///     expr-primary '.' identifier
///     expr-primary expr-primary     Type sensitive: iff first one has fn type
bool Parser::ParseExprPrimary(NullablePtr<Expr> &Result, const char *Message) {
  switch (Tok.getKind()) {
  case tok::numeric_constant:
    Result = S.expr.ActOnNumericConstant(Tok.getText(), Tok.getLoc());
    ConsumeToken(tok::numeric_constant);
    break;

  case tok::identifier:
    Result = S.expr.ActOnIdentifierExpr(Tok.getText(), Tok.getLoc());
    ConsumeToken(tok::identifier);
    break;
      
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
  
  // Check for a .foo suffix.
  SMLoc DotLoc = Tok.getLoc();
  while (ConsumeIf(tok::period)) {
    if (Tok.isNot(tok::identifier)) {
      Error(Tok.getLoc(), "expected field name");
      return true;
    }
      
    if (!Result.isNull())
      Result = S.expr.ActOnDotIdentifier(Result.get(), DotLoc, Tok.getText(),
                                         Tok.getLoc());
    ConsumeToken(tok::identifier);
    DotLoc = Tok.getLoc();
    continue;
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
  while (!Result.isNull() && S.expr.ShouldGreedilyJuxtapose(Result.get()) &&
         isStartOfExpr(Tok, S)) {
    NullablePtr<Expr> RHS;
    if (ParseExprSingle(RHS, "expected expression after juxtaposed operator") ||
        RHS.isNull())
      return true;
    
    llvm::PointerIntPair<Expr*, 1, bool>
    Op = S.expr.ActOnJuxtaposition(Result.get(), RHS.get());
    assert(!Op.getInt() && "ShouldGreedilyJuxtapose guaranteed an apply");
    
    Result = Op.getPointer();
  }
  
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
///     '{' (expr-or-decl-var ';')* expr? }
///   expr-or-decl-var:
///     type
///     decl-var
bool Parser::ParseExprBrace(NullablePtr<Expr> &Result) {
  SMLoc LBLoc = Tok.getLoc();
  ConsumeToken(tok::l_brace);
  
  // This brace expression forms a lexical scope.
  Scope BraceScope(S.decl);

  llvm::SmallVector<llvm::PointerUnion<Expr*, NamedDecl*>, 16> Entries;
  
  // MissingSemiAtEnd - Keep track of whether the last expression in the block
  // had no semicolon.
  bool MissingSemiAtEnd = true;
  
  // Parse the semicolon separated expression/var decls.
  do {
    // If we found a r_brace, then we either have an empty list {} or an
    // expression list terminated with a semicolon.  Remember this and break.
    if (Tok.is(tok::r_brace)) {
      MissingSemiAtEnd = false;
      break;
    }

    // Otherwise, we must have a var decl or expression.  Parse it up
    Entries.push_back(llvm::PointerUnion<Expr*, NamedDecl*>());
    
    // Parse the var or expression.  If we have an error, try to do nice
    // recovery.
    bool HadError = false;
    if (Tok.is(tok::kw_var)) {
      Entries.back() = ParseDeclVar();
      if (Entries.back().isNull())
        HadError = true;
    } else {
      NullablePtr<Expr> ResultExpr;
      if (ParseExpr(ResultExpr) || ResultExpr.isNull())
        HadError = true;
      else
        Entries.back() = ResultExpr.get();
    }
    
    if (HadError) {
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
    
  } while (ConsumeIf(tok::semi));
  
  SMLoc RBLoc = Tok.getLoc();
  if (ParseToken(tok::r_brace, "expected '}' at end of brace expression",
                 tok::r_brace)) {
    Note(LBLoc, "to match this opening '{'");
    return true;
  }
  
  // Diagnose cases where there was a ; missing after a 'var'.
  if (MissingSemiAtEnd && Entries.back().is<NamedDecl*>()) {
    Error(RBLoc, "expected ';' after var declaration");
    MissingSemiAtEnd = false;
  }
  
  Result = S.expr.ActOnBraceExpr(LBLoc, Entries.data(), Entries.size(),
                                 MissingSemiAtEnd, RBLoc);
  return false;
}


/// getBinOp - Return the NamedDecl for the token if it is an infix binary
/// operator, otherwise return null.
static NamedDecl *getBinOp(const Token &Tok, Sema &S) {
  if (Tok.isNot(tok::identifier))
    return 0;
  NamedDecl *ND = S.decl.LookupName(S.Context.getIdentifier(Tok.getText()));
  if (ND == 0 || ND->Attrs.InfixPrecedence == -1)
    return 0;
  return ND;
}


/// ParseExprBinaryRHS - Parse the right hand side of a binary expression and
/// assemble it according to precedence rules.
///
///   expr-binary-rhs:
///     (binary-operator expr-primary)*
bool Parser::ParseExprBinaryRHS(NullablePtr<Expr> &Result, unsigned MinPrec) {
  NamedDecl *NextTokOp = getBinOp(Tok, S);
  int NextTokPrec = NextTokOp ? NextTokOp->Attrs.InfixPrecedence : -1;
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
    NamedDecl *ThisTokOp = NextTokOp;
    
    NextTokOp = getBinOp(Tok, S);
    NextTokPrec = NextTokOp ? NextTokOp->Attrs.InfixPrecedence : -1;

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
