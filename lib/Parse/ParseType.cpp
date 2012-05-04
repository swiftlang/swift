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
#include "llvm/ADT/SmallString.h"
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
    if (!attrs.isByrefHeap()) quals |= LValueType::Qual::NonHeap;
    result = LValueType::get(result, quals, Context);
    attrs.Byref = false; // so that the empty() check below works
  }

  // Handle the auto_closure attribute.
  if (attrs.isAutoClosure()) {
    FunctionType *FT = dyn_cast<FunctionType>(result.getPointer());
    TupleType *InputTy = 0;
    if (FT) InputTy = dyn_cast<TupleType>(FT->getInput().getPointer());
    if (FT == 0) {
      // Autoclosure's require a syntactic function type.
      diagnose(attrs.LSquareLoc, diag::autoclosure_requires_function_type);
    } else if (InputTy == 0 || !InputTy->getFields().empty()) {
      // Function must take () syntactically.
      diagnose(attrs.LSquareLoc, diag::autoclosure_function_input_nonunit,
               FT->getInput());
    } else {
      // Otherwise, we're ok, rebuild type, adding the AutoClosure bit.
      result = FunctionType::get(FT->getInput(), FT->getResult(), true,Context);
    }
    attrs.AutoClosure = false;
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
///     type-tuple '->' type 
///
///   type-simple:
///     type-identifier
///     type-tuple
///
bool Parser::parseType(Type &Result, Diag<> MessageID) {
  // Parse type-simple first.
  SourceLoc TypeLoc = Tok.getLoc();
  bool isTupleType = false;
  switch (Tok.getKind()) {
  case tok::identifier:
    if (parseTypeIdentifier(Result))
      return true;
    break;
  case tok::l_paren:
  case tok::l_paren_space: {
    isTupleType = true;
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

  // Handle type-function if we have an arrow.
  if (consumeIf(tok::arrow)) {
    // If the argument was not syntactically a tuple type, report an error.
    if (!isTupleType) {
      diagnose(TypeLoc, diag::expected_function_argument_must_be_paren);
    }
    
    Type SecondHalf;
    if (parseType(SecondHalf, diag::expected_type_function_result))
      return true;
    Result = FunctionType::get(Result, SecondHalf, Context);
    return false;
  }

  // If there is a square bracket without a space, we have an array.
  if (Tok.is(tok::l_square))
    return parseTypeArray(Result);
  
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
  bool HadExpr = false;

  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::r_brace) &&
      !isStartOfDecl(Tok, peekToken())) {
    bool HadError = false;
    do {
      // If the tuple element starts with "ident :" or "ident =", then
      // the identifier is an element tag, and it is followed by a
      // value-specifier.
      if (Tok.is(tok::identifier) &&
          (peekToken().is(tok::colon) || peekToken().is(tok::equal))) {
        Identifier name = Context.getIdentifier(Tok.getText());
        consumeToken(tok::identifier);

        NullablePtr<Expr> init;
        Type type;
        if ((HadError = parseValueSpecifier(type, init)))
          break;

        HadExpr |= init.isNonNull();
        Elements.push_back(TupleTypeElt(type, name, init.getPtrOrNull()));
        continue;
      }

      // Otherwise, this has to be a type.
      Type type;
      if ((HadError = parseTypeAnnotation(type)))
        break;

      Elements.push_back(TupleTypeElt(type, Identifier(), nullptr));
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
    assert(!HadExpr && "Only TupleTypes have default values");
    Result = ParenType::get(Context, Elements.back().getType());
    return false;
  }

  TupleType *TT = TupleType::get(Elements, Context);
  if (HadExpr)
    TypesWithDefaultValues.emplace_back(TT, CurDeclContext);
  Result = TT;
  return false;
}


/// parseTypeArray - Parse the type-array production, given that we
/// are looking at the initial l_square.  Note that this index
/// clause is actually the outermost (first-indexed) clause.
///
///   type-array:
///     type-simple
///     type-array '[' ']'
///     type-array '[' expr ']'
///
bool Parser::parseTypeArray(Type &result) {
  SourceLoc lsquareLoc = Tok.getLoc();
  consumeToken(tok::l_square);

  // Handle the [] production, meaning an array slice.
  if (consumeIf(tok::r_square)) {
    // If we're starting another square-bracket clause, recurse.
    if (Tok.is(tok::l_square) && parseTypeArray(result)) {
      return true;

    // Propagate an error type out.
    } else if (isa<ErrorType>(result)) {
      return true;
    }

    // Just build a normal array slice type.
    result = ArraySliceType::get(result, lsquareLoc, Context);
    return false;
  }
  
  NullablePtr<Expr> sizeEx = parseExpr(diag::expected_expr_array_type);
  if (sizeEx.isNull()) return true;

  SourceLoc rsquareLoc;
  if (parseMatchingToken(tok::r_square, rsquareLoc,
                         diag::expected_rbracket_array_type,
                         lsquareLoc, diag::opening_bracket))
    return true;

  // If we're starting another square-bracket clause, recurse.
  if (Tok.is(tok::l_square) && parseTypeArray(result)) {
    return true;
  
  // If we had a semantic error on the size or if the base type is invalid,
  // propagate up an error type.
  } else if (isa<ErrorType>(result)) {
    return true;
  }
  
  // Semantic analysis.
  
  Expr *size = sizeEx.get();
  
  // FIXME: Add real support for evaluating constant expressions for array
  // sizes.
  uint64_t sizeVal;
  if (IntegerLiteralExpr *lit = dyn_cast<IntegerLiteralExpr>(size)) {
    sizeVal = lit->getValue().getZExtValue() /*hack*/;
  } else {
    diagnose(size->getLoc(), diag::non_constant_array)
      << size->getSourceRange();
    result = ErrorType::get(Context);
    return true;
  }
  
  if (sizeVal == 0) {
    diagnose(size->getLoc(), diag::zero_length_array)
      << size->getSourceRange();
    result = ErrorType::get(Context);
    return true;
  }
  
  result = ArrayType::get(result, sizeVal, Context);
  return false;
}

