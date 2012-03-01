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
#include "swift/AST/Attr.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

bool Parser::parseTypeAnnotation(Type &result) {
  return parseTypeAnnotation(result, diag::expected_type);
}

/// parseTypeAnnotation
///   type-annotation:
///     attribute-list type
bool Parser::parseTypeAnnotation(Type &result, Diag<> message) {
  // Parse attributes.
  DeclAttributes attrs;
  parseAttributeList(attrs);

  // Parse the type.
  if (parseType(result, message))
    return true;

  // Apply those attributes that do apply.
  if (attrs.empty()) return false;

  if (attrs.isByref()) {
    LValueType::Qual quals;
    if (attrs.isByrefImplicit()) quals |= LValueType::Qual::Implicit;
    result = LValueType::get(result, quals, Context);
    attrs.Byref = false; // so that the empty() check below works
  }

  // FIXME: this is lame.
  if (!attrs.empty())
    diagnose(attrs.LSquareLoc, diag::attribute_does_not_apply_to_type);

  return false;
}

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
    if (parseTypeTupleBody(LPLoc, Result) ||
        parseMatchingToken(tok::r_paren, RPLoc,
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
///     identifier ('.' identifier)*
///
bool Parser::parseTypeIdentifier(Type &Result) {
  if (Tok.isNot(tok::identifier)) {
    diagnose(Tok.getLoc(), diag::expected_identifier_for_type);
    return true;
  }
  
  SmallVector<IdentifierType::Component, 4> Components;
  Components.push_back(IdentifierType::Component(Tok.getLoc(),
                                     Context.getIdentifier(Tok.getText())));
  consumeToken(tok::identifier);
  
  while (consumeIf(tok::period)) {
    SourceLoc Loc = Tok.getLoc();
    Identifier Name;
    if (parseIdentifier(Name, diag::expected_identifier_in_dotted_type))
      return true;
    Components.push_back(IdentifierType::Component(Loc, Name));
  }

  // Lookup element #0 through our current scope chains in case it is some thing
  // local (this returns null if nothing is found).
  Components[0].Value = ScopeInfo.lookupValueName(Components[0].Id);

  auto Ty = IdentifierType::getNew(Context, Components);
  UnresolvedIdentifierTypes.push_back(Ty);
  Result = Ty;
  return false;
}



/// parseTypeTupleBody
///   type-tuple:
///     lparen-any type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)*
///   type-tuple-element:
///     identifier value-specifier
///     type-annotation
bool Parser::parseTypeTupleBody(SourceLoc LPLoc, Type &Result) {
  SmallVector<TupleTypeElt, 8> Elements;

  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::r_brace) &&
      !isStartOfDecl(Tok, peekToken())) {
    bool HadError = false;
    do {
      Type type;
      Identifier name;

      // If the tuple element starts with "ident :" or "ident =", then
      // the identifier is an element tag, and it is followed by a
      // value-specifier.
      if (Tok.is(tok::identifier) &&
          (peekToken().is(tok::colon) || peekToken().is(tok::equal))) {
        name = Context.getIdentifier(Tok.getText());
        consumeToken(tok::identifier);

        NullablePtr<Expr> init;
        if ((HadError = parseValueSpecifier(type, init)))
          break;

        Elements.push_back(TupleTypeElt(type, name, init.getPtrOrNull()));
        continue;
      }

      // Otherwise, this has to be a type.
      if ((HadError = parseTypeAnnotation(type)))
        break;

      Expr *init = nullptr;

      // Parse the optional default value expression.
      if (consumeIf(tok::equal)) {
        ParseResult<Expr> initResult =
          parseExpr(diag::expected_initializer_expr);

        // Die if there was a parse error.
        if (initResult) {
          HadError = true;
          break;
        }

        if (!initResult.isSemaError()) {
          init = initResult.get();
        }
      }

      Elements.push_back(TupleTypeElt(type, name, init));
    } while (consumeIf(tok::comma));
    
    if (HadError) {
      skipUntil(tok::r_paren);
      if (Tok.is(tok::r_paren))
        consumeToken(tok::r_paren);
      return true;
    }
  }

  // A "tuple" with one anonymous element is actually not a tuple.
  if (Elements.size() == 1 && !Elements.back().hasName()) {
    Result = ParenType::get(Context, Elements.back().getType());
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
  if ((SizeEx = parseExpr(diag::expected_expr_array_type)) ||
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

