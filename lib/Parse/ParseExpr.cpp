//===--- ParseExpr.cpp - Swift Language Parser for Expressions ------------===//
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
// Expression Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "Parser.h"
#include "swift/AST/Diagnostics.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/APFloat.h"
using namespace swift;

bool Parser::isStartOfExpr(const Token &Tok, const Token &Next) {
  if (Tok.is(tok::numeric_constant) || Tok.is(tok::colon) ||
      Tok.is(tok::l_paren_space) || Tok.is(tok::dollarident) ||
      Tok.is(tok::identifier) || Tok.is(tok::oper))
    return true;
  
  // "func(" and "func{" are func expressions.  "func x" is a func declaration.
  if (Tok.is(tok::kw_func) &&
      (Next.is(tok::l_paren) || Next.is(tok::l_paren_space) ||
       Next.is(tok::l_brace)))
    return true;
  return false;
}

/// parseSingleExpr
///
/// Parse an expression in a context that requires a single expression.
ParseResult<Expr> Parser::parseSingleExpr(Diag<> Message) {
  ParseResult<Expr> Result = parseExpr(Message);
  if (Result) return true;

  // Kill all the following expressions.  This is not necessarily
  // good for certain kinds of recovery.
  if (isStartOfExpr(Tok, peekToken())) {
    diagnose(Tok, diag::expected_single_expr);
    do {
      ParseResult<Expr> Extra = parseExpr(Message);
      if (Extra) break;
    } while (isStartOfExpr(Tok, peekToken()));
  }

  return Result;
}

/// parseExpr
///   expr:
///     expr-unary
///     expr-unary operator expr
///
/// The sequencing here is not structural, i.e. binary operators are
/// not inherently right-associative.
ParseResult<Expr> Parser::parseExpr(Diag<> Message) {
  SmallVector<Expr*, 8> SequencedExprs;

  bool HasSemaError = false;

  while (true) {
    // Parse a primary expression.
    ParseResult<Expr> Primary = parseExprUnary(Message);
    if (Primary.isParseError())
      return true;

    if (Primary.isSemaError()) {
      HasSemaError = true;
    } else {
      SequencedExprs.push_back(Primary.get());
    }

    // If the next token is not an operator, we're done.
    if (Tok.isNot(tok::oper))
      break;

    // Parse the operator.
    Expr *Operator = parseExprOperator();
    SequencedExprs.push_back(Operator);

    // The message is only valid for the first subexpr.
    Message = diag::expected_expr_after_operator;
  }

  // If we had semantic errors, just fail here.
  if (HasSemaError)
    return ParseResult<Expr>::getSemaError();
  assert(!SequencedExprs.empty());

  // If we saw no operators, don't build a sequence.
  if (SequencedExprs.size() == 1)
    return SequencedExprs[0];

  return SequenceExpr::create(Context, SequencedExprs);
}

/// parseExprUnary
///
///   expr-unary:
///     expr-postfix
///     operator expr-unary
ParseResult<Expr> Parser::parseExprUnary(Diag<> Message) {
  // If the next token is not an operator, just parse this as expr-postfix
  if (Tok.isNot(tok::oper))
    return parseExprPostfix(Message);
  
  // Parse the operator.
  Expr *Operator = parseExprOperator();

  ParseResult<Expr> SubExpr = parseExprUnary(Message);
  if (!SubExpr.isSuccess()) return SubExpr;
  return new (Context) UnaryExpr(Operator, SubExpr.get());
}

/// parseExprPostfix
///
///   expr-literal:
///     numeric_constant
///
///   expr-primary:
///     expr-literal
///     expr-identifier
///     ':' identifier
///     expr-paren
///     expr-func
///
///   expr-dot:
///     expr-postfix '.' identifier
///     expr-postfix '.' dollarident
///
///   expr-subscript:
///     expr-postfix '[' expr ']'
///
///   expr-call:
///     expr-postfix expr-paren
///
///   expr-postfix:
///     expr-primary
///     expr-dot
///     expr-subscript
///     expr-call
///
ParseResult<Expr> Parser::parseExprPostfix(Diag<> ID) {
  ParseResult<Expr> Result;
  switch (Tok.getKind()) {
  case tok::numeric_constant:
    Result = parseExprNumericConstant();
    break;

  case tok::dollarident: // $1
    Result = parseExprDollarIdentifier();
    break;
  case tok::identifier:  // foo   and  foo::bar
    Result = parseExprIdentifier();
    break;

  case tok::colon: {     // :foo
    SourceLoc ColonLoc = consumeToken(tok::colon);
    Identifier Name;
    SourceLoc NameLoc = Tok.getLoc();
    if (parseIdentifier(Name, diag::expected_identifier_after_colon_expr))
      return true;
    
    // Handle :foo by just making an AST node.
    Result = new (Context) UnresolvedMemberExpr(ColonLoc, NameLoc, Name);
    break;
  }

  // A spaced left parenthesis can generally start a tuple expression.
  // What it can't do is start a call.
  case tok::l_paren:
  case tok::l_paren_space:
    Result = parseExprParen();
    break;

  case tok::kw_func:
    Result = parseExprFunc();
    break;
      
  default:
    diagnose(Tok.getLoc(), ID);
    return true;
  }
  
  // If we had a parse error, don't attempt to parse suffixes.  Do keep going if
  // we had semantic errors though.
  if (Result.isParseError())
    return true;
    
  // Handle suffix expressions.
  while (1) {
    // Check for a .foo suffix.
    SourceLoc TokLoc = Tok.getLoc();
    
    if (consumeIf(tok::period)) {
      if (Tok.isNot(tok::identifier) && Tok.isNot(tok::dollarident)) {
        diagnose(Tok, diag::expected_field_name);
        return true;
      }
        
      if (!Result.isSemaError()) {
        Identifier Name = Context.getIdentifier(Tok.getText());
        Result = new (Context) UnresolvedDotExpr(Result.get(), TokLoc, Name,
                                                 Tok.getLoc());
      }
      if (Tok.is(tok::identifier))
        consumeToken(tok::identifier);
      else
        consumeToken(tok::dollarident);
      continue;
    }
    
    // Check for a () suffix, which indicates a call.
    // Note that this cannot be a l_paren_space.
    if (Tok.is(tok::l_paren)) {
      ParseResult<Expr> Arg = parseExprParen();
      if (Arg.isParseError())
        return true;
      if (Arg.isSemaError())
        Result = ParseResult<Expr>::getSemaError();
      else if (!Result.isSemaError())
        Result = new (Context) CallExpr(Result.get(), Arg.get(),
                                        /*DotSyntax=*/false,
                                        TypeJudgement());
      continue;
    }
    
    // Check for a [expr] suffix.
    if (consumeIf(tok::l_square)) {
      ParseResult<Expr> Idx;
      SourceLoc RLoc;
      if ((Idx = parseSingleExpr(diag::expected_expr_subscript_value)) ||
          parseMatchingToken(tok::r_square, RLoc,
                             diag::expected_bracket_array_subscript,
                             TokLoc, diag::opening_bracket))
        return true;
      
      if (!Result.isSemaError() && !Idx.isSemaError()) {
        // FIXME: Implement.  This should modify Result like the cases
        // above.
        Result = Result;
      }
    }
        
    break;
  }
  
  return Result;
}

ParseResult<Expr> Parser::parseExprNumericConstant() {
  StringRef Text = Tok.getText();
  SourceLoc Loc = consumeToken(tok::numeric_constant);

  // Check to see if we have an integer constant.
  size_t DotPos = Text.find('.');
  if (DotPos == StringRef::npos) {
    // The integer literal must fit in 64-bits.
    unsigned long long Val;
    if (Text.getAsInteger(0, Val)) {
      diagnose(Loc, diag::int_literal_too_large);
      return ParseResult<Expr>::getSemaError();
    }
    
    return new (Context) IntegerLiteralExpr(Text, Loc);
  }
  
  // Okay, we have a floating point constant.  Verify we have a single dot.
  DotPos = Text.find('.', DotPos+1);
  if (DotPos != StringRef::npos) {
    diagnose(Loc.getAdvancedLoc(DotPos), diag::float_literal_multi_decimal);
    return ParseResult<Expr>::getSemaError();
  }
  
  llvm::APFloat Val(llvm::APFloat::IEEEdouble);
  switch (Val.convertFromString(Text, llvm::APFloat::rmNearestTiesToEven)) {
  default: break;
  case llvm::APFloat::opOverflow: {
    llvm::SmallString<20> Buffer;
    llvm::APFloat::getLargest(Val.getSemantics()).toString(Buffer);
    diagnose(Loc, diag::float_literal_overflow, Buffer);
    break;
  }
  case llvm::APFloat::opUnderflow: {
    // Denormals are ok, but reported as underflow by APFloat.
    if (!Val.isZero()) break;
    llvm::SmallString<20> Buffer;
    llvm::APFloat::getSmallest(Val.getSemantics()).toString(Buffer);
    diagnose(Loc, diag::float_literal_underflow, Buffer);
    break;
  }
  }
  
  // The type of a float literal is always "float_literal_type", which
  // should be defined by the library.
  Identifier TyName = Context.getIdentifier("float_literal_type");
  Type Ty = ScopeInfo.lookupOrInsertTypeName(TyName, Loc);
  return new (Context) FloatLiteralExpr(Val.convertToDouble(), Loc, Ty);
}

///   expr-identifier:
///     dollarident
ParseResult<Expr> Parser::parseExprDollarIdentifier() {
  StringRef Name = Tok.getText();
  SourceLoc Loc = consumeToken(tok::dollarident);
  assert(Name[0] == '$' && "Not a dollarident");
  bool AllNumeric = true;
  for (unsigned i = 1, e = Name.size(); i != e; ++i)
    AllNumeric &= isdigit(Name[i]);
  
  if (Name.size() == 1 || !AllNumeric) {
    diagnose(Loc.getAdvancedLoc(1), diag::expected_dollar_numeric);
    return ParseResult<Expr>::getSemaError();
  }
  
  unsigned ArgNo = 0;
  if (Name.substr(1).getAsInteger(10, ArgNo)) {
    diagnose(Loc.getAdvancedLoc(1), diag::dollar_numeric_too_large);
    return ParseResult<Expr>::getSemaError();
  }
  
  return new (Context) AnonClosureArgExpr(ArgNo, Loc);
}


/// parseExprOperator - Parse an operator reference expression.  These
/// are not "proper" expressions; they can only appear interlaced in
/// SequenceExprs.
Expr *Parser::parseExprOperator() {
  assert(Tok.is(tok::oper));
  SourceLoc Loc = Tok.getLoc();
  Identifier Name = Context.getIdentifier(Tok.getText());
  consumeToken(tok::oper);

  return actOnIdentifierExpr(Name, Loc);
}

/// parseExprIdentifier - Parse an identifier expression:
///
///   expr-identifier:
///     identifier
///     scope-qualifier identifier
ParseResult<Expr> Parser::parseExprIdentifier() {
  assert(Tok.is(tok::identifier));
  SourceLoc Loc = Tok.getLoc();
  Identifier Name = Context.getIdentifier(Tok.getText());
  consumeToken(tok::identifier);

  if (Tok.isNot(tok::coloncolon))
    return actOnIdentifierExpr(Name, Loc);
  
  SourceLoc ColonColonLoc = consumeToken(tok::coloncolon);

  SourceLoc Loc2 = Tok.getLoc();
  Identifier Name2;
  if (parseIdentifier(Name2, diag::expected_identifier_after_coloncolon_expr,
                      Name))
    return true;
  
  TypeAliasDecl *TypeDeclFromScope = ScopeInfo.lookupScopeName(Name, Loc);
  return new (Context) UnresolvedScopedIdentifierExpr(TypeDeclFromScope,
                                                      Name, Loc,
                                                      ColonColonLoc,
                                                      Name2, Loc2);
}

Expr *Parser::actOnIdentifierExpr(Identifier Text, SourceLoc Loc) {
  ValueDecl *D = ScopeInfo.lookupValueName(Text);
  
  if (D == 0)
    return new (Context) UnresolvedDeclRefExpr(Text, Loc);
  
  return new (Context) DeclRefExpr(D, Loc);
}


/// parseExprParen - Parse a tuple expression.
///
///   expr-paren: 
///     '(' ')'
///     '(' expr-paren-element (',' expr-paren-element)* ')'
///
///   expr-paren-element:
///     ('.'? identifier '=')? expr
///
ParseResult<Expr> Parser::parseExprParen() {
  SourceLoc LPLoc = consumeToken();
  
  SmallVector<Expr*, 8> SubExprs;
  SmallVector<Identifier, 8> SubExprNames; 
  bool AnySubExprSemaErrors = false;
  
  if (Tok.isNot(tok::r_paren)) {
    do {
      Identifier FieldName;
      // Check to see if there is a field specifier.  This is either ".x =" or
      // "x =".
      if (consumeIf(tok::period) ||
          (Tok.is(tok::identifier) && peekToken().is(tok::equal))) {
        if (parseIdentifier(FieldName,
                            diag::expected_field_spec_name_tuple_expr) ||
            parseToken(tok::equal, diag::expected_equal_in_tuple_expr))
          return true;
      }
      
      if (!SubExprNames.empty())
        SubExprNames.push_back(FieldName);
      else if (FieldName.get()) {
        SubExprNames.resize(SubExprs.size());
        SubExprNames.push_back(FieldName);
      }
      
      ParseResult<Expr> SubExpr;
      if ((SubExpr = parseSingleExpr(diag::expected_expr_parentheses)))
        return true;
      
      if (SubExpr.isSemaError())
        AnySubExprSemaErrors = true;
      else
        SubExprs.push_back(SubExpr.get());
    
    } while (consumeIf(tok::comma));
  }
  
  SourceLoc RPLoc;
  if (parseMatchingToken(tok::r_paren, RPLoc,
                         diag::expected_rparen_parenthesis_expr,
                         LPLoc, diag::opening_paren))
    return true;

  if (AnySubExprSemaErrors)
    return ParseResult<Expr>::getSemaError();

  Expr **NewSubExprs =
    Context.AllocateCopy<Expr*>(SubExprs.data(),
                                SubExprs.data()+SubExprs.size());
  
  Identifier *NewSubExprsNames = 0;
  if (!SubExprNames.empty())
    NewSubExprsNames =
      Context.AllocateCopy<Identifier>(SubExprNames.data(),
                                       SubExprNames.data()+SubExprs.size());
  
  bool IsGrouping = false;
  if (SubExprs.size() == 1 &&
      (SubExprNames.empty() || SubExprNames[0].empty()))
    IsGrouping = true;
  
  return new (Context) TupleExpr(LPLoc, NewSubExprs, NewSubExprsNames,
                                 SubExprs.size(), RPLoc, IsGrouping);
}


/// parseExprFunc - Parse a func expression.
///
///   expr-func: 
///     'func' type? stmt-brace
///
/// The type must start with '(' if present.
///
ParseResult<Expr> Parser::parseExprFunc() {
  SourceLoc FuncLoc = consumeToken(tok::kw_func);

  Type Ty;
  if (Tok.is(tok::l_brace)) {
    Ty = TupleType::getEmpty(Context);
  } else if (!Tok.is(tok::l_paren) && !Tok.is(tok::l_paren_space)) {
    diagnose(Tok, diag::func_decl_without_paren);
    return true;
  } else if (parseType(Ty)) {
    return true;
  }
  
  // If the parsed type is not spelled as a function type (i.e., has no '->' in
  // it), then it is implicitly a function that returns ().
  if (!isa<FunctionType>(Ty.getPointer()))
    Ty = FunctionType::get(Ty, TupleType::getEmpty(Context), Context);

  // The arguments to the func are defined in their own scope.
  Scope FuncBodyScope(this);
  FuncExpr *FE = actOnFuncExprStart(FuncLoc, Ty);

  // Establish the new context.
  ContextChange CC(*this, FE);
  
  // Then parse the expression.
  ParseResult<BraceStmt> Body;
  if ((Body = parseStmtBrace(diag::expected_lbrace_func_expr)))
    return true;
  if (Body.isSemaError())
    return ParseResult<Expr>::getSemaError();
  
  FE->setBody(Body.get());
  return FE;
}


/// FuncTypePiece - This little enum is used by AddFuncArgumentsToScope to keep
/// track of where in a function type it is currently looking.  This affects how
/// the decls are processed and created.
enum class FuncTypePiece {
  Function,  // Looking at the initial functiontype itself.
  Input,     // Looking at the input to the function type
  Output     // Looking at the output to the function type.
};

/// AddFuncArgumentsToScope - Walk the type specified for a Func object (which
/// is known to be a FunctionType on the outer level) creating and adding named
/// arguments to the current scope.  This causes redefinition errors to be
/// emitted.
static void AddFuncArgumentsToScope(Type Ty,
                                    FuncTypePiece Mode,
                                    SourceLoc FuncLoc, 
                                    SmallVectorImpl<ArgDecl*> &ArgDecls,
                                    Parser &P) {
  // Handle the function case first.
  if (Mode == FuncTypePiece::Function) {
    FunctionType *FT = cast<FunctionType>(Ty.getPointer());
    AddFuncArgumentsToScope(FT->Input, FuncTypePiece::Input,
                            FuncLoc, ArgDecls, P);
    
    // If this is a->b->c then we treat b as an input, not (b->c) as an output.
    if (isa<FunctionType>(FT->Result.getPointer()))
      AddFuncArgumentsToScope(FT->Result, FuncTypePiece::Function, FuncLoc,
                              ArgDecls, P);
    else    
      AddFuncArgumentsToScope(FT->Result, FuncTypePiece::Output, FuncLoc,
                              ArgDecls, P);
    return;
  }
  
  // Otherwise, we're looking at an input or output to the func.  The only type
  // we currently dive into is the humble tuple, which can be recursive.  This
  // should dive in syntactically.
  ///
  /// Note that we really *do* want dyn_cast here, not getAs, because we do not
  /// want to look through type aliases or other sugar, we want to see what the
  /// user wrote in the func declaration.
  TupleType *TT = dyn_cast<TupleType>(Ty.getPointer());
  if (TT == 0) return;
  
  
  // For tuples, recursively processes their elements (to handle cases like:
  //    (x : (a : int, b : int), y : int) -> ...
  // and create decls for any named elements.
  for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
    AddFuncArgumentsToScope(TT->Fields[i].Ty, Mode, FuncLoc, ArgDecls, P);
    
    // If this field is named, create the argument decl for it.
    Identifier Name = TT->Fields[i].Name;
    // Ignore unnamed fields.
    if (Name.get() == 0) continue;
    
    
    // Create the argument decl for this named argument.
    ArgDecl *AD = new (P.Context) ArgDecl(FuncLoc, Name, TT->Fields[i].Ty,
                                          P.CurDeclContext);
    ArgDecls.push_back(AD);
    
    // Eventually we should mark the input/outputs as readonly vs writeonly.
    //bool isInput = Mode == FuncTypePiece::Input;
    
    P.ScopeInfo.addToScope(AD);
  }
}


FuncExpr *Parser::actOnFuncExprStart(SourceLoc FuncLoc, Type FuncTy) {
  SmallVector<ArgDecl*, 8> ArgDecls;
  AddFuncArgumentsToScope(FuncTy, FuncTypePiece::Function,
                          FuncLoc, ArgDecls, *this);
  
  ArrayRef<ArgDecl*> Args = ArgDecls;
  
  FuncExpr *FE = new (Context) FuncExpr(FuncLoc, FuncTy,
                                        Context.AllocateCopy(Args), 0,
                                        CurDeclContext);

  // Reparent all the arguments.
  for (ArgDecl *Arg : Args)
    Arg->Context = FE;

  return FE;
}



