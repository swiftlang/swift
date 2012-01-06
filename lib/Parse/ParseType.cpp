//===--- ParseType.cpp - Swift Language Parser for Types ------------------===//
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
// Type Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "Parser.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

bool Parser::parseType(Type &Result) {
  return parseType(Result, diag::expected_type);
}

/// parseType
///   type:
///     type-simple
///     type-function
///     type-array
///
///   type-function:
///     type-simple '->' type 
///
///   type-simple:
///     type-identifier
///     type-tuple
///
bool Parser::parseType(Type &Result, Diag<> MessageID) {
  // Parse type-simple first.
  switch (Tok.getKind()) {
  case tok::identifier:
    if (parseTypeIdentifier(Result))
      return true;
    break;
  case tok::l_paren:
  case tok::l_paren_space: {
    SourceLoc LPLoc = consumeToken(), RPLoc;
    if (parseTypeTupleBody(LPLoc, Result))
      return true;
    // FIXME: matching.
    if (parseMatchingToken(tok::r_paren, RPLoc,
                           diag::expected_rparen_tuple_type_list,
                           LPLoc, diag::opening_paren))
      return true;
    break;
  }
  default:
    diagnose(Tok.getLoc(), MessageID);
    return true;
  }
  
  while (1) {
    // If there is an arrow, parse the rest of the type.
    SourceLoc TokLoc = Tok.getLoc();
    if (consumeIf(tok::arrow)) {
      Type SecondHalf;
      if (parseType(SecondHalf, diag::expected_type_function_result))
        return true;
      Result = FunctionType::get(Result, SecondHalf, Context);
      continue;
    }
    
    // If there is a square bracket, we have an array.
    if (consumeIf(tok::l_square)) {
      if (parseTypeArray(TokLoc, Result)) return true;
      continue;
    }
    
    break;
  }
        
  
  return false;
}

/// parseTypeIdentifier
///   
///   type-identifier:
///     identifier
///     scope-qualifier identifier
///
bool Parser::parseTypeIdentifier(Type &Result) {
  if (Tok.isNot(tok::identifier)) {
    diagnose(Tok.getLoc(), diag::expected_identifier_for_type);
    return true;
  }
  
  Identifier Name = Context.getIdentifier(Tok.getText());
  SourceLoc NameLoc = Tok.getLoc();
  consumeToken(tok::identifier);
  
  if (Tok.isNot(tok::coloncolon)) {
    Result = ScopeInfo.lookupOrInsertTypeName(Name, NameLoc);
    return false;
  }
  
  consumeToken(tok::coloncolon);
  SourceLoc Loc2 = Tok.getLoc();
  Identifier Name2;
  if (parseIdentifier(Name2,
                      diag::expected_identifier_after_coloncolon_type, Name))
    return true;
    
  Result = ScopeInfo.getQualifiedTypeName(Name, NameLoc, Name2, Loc2);
  return false;
}



/// parseTypeTupleBody
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)*
///   type-tuple-element:
///     identifier value-specifier
///     type
bool Parser::parseTypeTupleBody(SourceLoc LPLoc, Type &Result) {
  SmallVector<TupleTypeElt, 8> Elements;

  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::r_brace) &&
      !isStartOfDecl(Tok, peekToken())) {
    bool HadError = false;
    do {
      Elements.push_back(TupleTypeElt());
      TupleTypeElt &Result = Elements.back();

      // If the tuple element starts with "ident :" or "ident =", then
      // the identifier is an element tag, and it is followed by a
      // value-specifier.
      if (Tok.is(tok::identifier) &&
          (peekToken().is(tok::colon) || peekToken().is(tok::equal))) {
        Result.Name = Context.getIdentifier(Tok.getText());
        consumeToken(tok::identifier);

        NullablePtr<Expr> Init;
        if ((HadError = parseValueSpecifier(Result.Ty, Init, /*single*/ true)))
          break;
        Result.Init = Init.getPtrOrNull();
        continue;
      }

      // Otherwise, this has to be a type.
      if ((HadError = parseType(Result.Ty)))
        break;

      // Parse the optional default value expression.
      if (Tok.is(tok::colon)) {
        ParseResult<Expr> Init =
          parseSingleExpr(diag::expected_initializer_expr);

        // Die if there was a parse error.
        if (Init) {
          HadError = true;
          break;
        }

        if (!Init.isSemaError()) {
          Result.Init = Init.get();
        }
      }
    } while (consumeIf(tok::comma));
    
    if (HadError) {
      skipUntil(tok::r_paren);
      if (Tok.is(tok::r_paren))
        consumeToken(tok::r_paren);
      return true;
    }
  }

  // A "tuple" with one anonymous element is actually not a tuple.
  if (Elements.size() == 1 && Elements.back().Name.empty()) {
    Result = ParenType::get(Context, Elements.back().Ty);
    return false;
  }
  
  Result = TupleType::get(Elements, Context);
  return false;
}


/// parseTypeArray - The l_square has already been consumed.
///   type-array:
///     type '[' ']'
///     type '[' expr ']'
///
bool Parser::parseTypeArray(SourceLoc LSquareLoc, Type &Result) {
  // Handle the [] production, and unsized array.
  if (consumeIf(tok::r_square)) {
    if (isa<ErrorType>(Result.getPointer()))
      return Result;
    return ArrayType::get(Result, 0, Context);
  }
  
  ParseResult<Expr> SizeEx;
  SourceLoc RArrayTok;
  if ((SizeEx = parseSingleExpr(diag::expected_expr_array_type)) ||
      parseMatchingToken(tok::r_square, RArrayTok,
                         diag::expected_rbracket_array_type,
                         LSquareLoc, diag::opening_bracket))
    return true;
  
  // If we had a semantic error on the size or if the base type is invalid,
  // propagate up an error type.
  if (SizeEx.isSemaError() || isa<ErrorType>(Result.getPointer())) {
    Result = ErrorType::get(Context);
    return false;
  }
  
  // Semantic analysis.
  
  Expr *Size = SizeEx.get();
  
  // FIXME: Add real support for evaluating constant expressions for array
  // sizes.
  uint64_t SizeVal;
  if (IntegerLiteralExpr *IL = dyn_cast<IntegerLiteralExpr>(Size)) {
    SizeVal = IL->getValue().getZExtValue() /*hack*/;
  } else {
    diagnose(Size->getLoc(), diag::non_constant_array);
    return ErrorType::get(Context);
  }
  
  if (SizeVal == 0) {
    diagnose(Size->getLoc(), diag::zero_length_array);
    return ErrorType::get(Context);
  }
  
  Result = ArrayType::get(Result, SizeVal, Context);
  return false;
}

