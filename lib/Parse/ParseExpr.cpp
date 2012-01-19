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
#include "llvm/ADT/Twine.h"
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
  case tok::identifier:  // foo
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
                                        TypeJudgement());
      continue;
    }
    
    // Check for a [expr] suffix.
    if (consumeIf(tok::l_square)) {
      ParseResult<Expr> Idx;
      SourceLoc RLoc;
      if ((Idx = parseExpr(diag::expected_expr_subscript_value)) ||
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
  if (DotPos == StringRef::npos)
    return new (Context) IntegerLiteralExpr(Text, Loc);
  
  // Okay, we have a floating point constant.  Verify we have a single dot.
  DotPos = Text.find('.', DotPos+1);
  if (DotPos != StringRef::npos) {
    diagnose(Loc.getAdvancedLoc(DotPos), diag::float_literal_multi_decimal);
    return ParseResult<Expr>::getSemaError();
  }
  
  return new (Context) FloatLiteralExpr(Text, Loc);
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
ParseResult<Expr> Parser::parseExprIdentifier() {
  assert(Tok.is(tok::identifier));
  SourceLoc Loc = Tok.getLoc();
  Identifier Name = Context.getIdentifier(Tok.getText());
  consumeToken(tok::identifier);
  return actOnIdentifierExpr(Name, Loc);
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
      if ((SubExpr = parseExpr(diag::expected_expr_parentheses)))
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

  MutableArrayRef<Expr *> NewSubExprs = Context.AllocateCopy(SubExprs);
  
  Identifier *NewSubExprsNames = 0;
  if (!SubExprNames.empty())
    NewSubExprsNames =
      Context.AllocateCopy<Identifier>(SubExprNames.data(),
                                       SubExprNames.data()+SubExprs.size());
  
  // A tuple with a single, unlabelled element is just parentheses.
  if (SubExprs.size() == 1 &&
      (SubExprNames.empty() || SubExprNames[0].empty())) {
    return new (Context) ParenExpr(LPLoc, SubExprs[0], RPLoc);
  }
  
  return new (Context) TupleExpr(LPLoc, NewSubExprs, NewSubExprsNames, RPLoc);
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
                                    FuncExpr *FE,
                                    Parser &P) {
  // Handle the function case first.
  if (Mode == FuncTypePiece::Function) {
    FunctionType *FT = cast<FunctionType>(Ty);
    AddFuncArgumentsToScope(FT->Input, FuncTypePiece::Input, FE, P);
    
    // If this is a->b->c then we treat b as an input, not (b->c) as an output.
    if (isa<FunctionType>(FT->Result.getPointer()))
      AddFuncArgumentsToScope(FT->Result, FuncTypePiece::Function, FE, P);
    else    
      AddFuncArgumentsToScope(FT->Result, FuncTypePiece::Output, FE, P);
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
  for (const TupleTypeElt &Field : TT->Fields) {
    AddFuncArgumentsToScope(Field.getType(), Mode, FE, P);
    
    // If this field is named, create the argument decl for it.
    // Otherwise, ignore unnamed fields.
    if (!Field.hasName()) continue;

    // Create the argument decl for this named argument.
    ArgDecl *AD = new (P.Context) ArgDecl(FE->getFuncLoc(), Field.getName(),
                                          Field.getType(), FE);

    // Modify the TupleType in-place.  This is okay, as we're
    // essentially still processing it.
    const_cast<TupleTypeElt&>(Field).setArgDecl(AD);
    
    // Eventually we should mark the input/outputs as readonly vs writeonly.
    //bool isInput = Mode == FuncTypePiece::Input;
    
    P.ScopeInfo.addToScope(AD);
  }
}


FuncExpr *Parser::actOnFuncExprStart(SourceLoc FuncLoc, Type FuncTy) {
  FuncExpr *FE = new (Context) FuncExpr(FuncLoc, FuncTy, 0, CurDeclContext);

  AddFuncArgumentsToScope(FuncTy, FuncTypePiece::Function, FE, *this);
  
  return FE;
}



