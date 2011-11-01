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
///     type-protocol
///
///   type-identifier:
///     identifier
///     scope-qualifier identifier
///
bool Parser::parseType(Type &Result, Diag<> MessageID) {
  // Parse type-simple first.
  switch (Tok.getKind()) {
  case tok::identifier: {
    Identifier Name = Context.getIdentifier(Tok.getText());
    SourceLoc NameLoc = Tok.getLoc();
    consumeToken(tok::identifier);

    if (Tok.isNot(tok::coloncolon)) {
      Result = ScopeInfo.lookupOrInsertTypeName(Name, NameLoc);
    } else {
      consumeToken(tok::coloncolon);
      SourceLoc Loc2 = Tok.getLoc();
      Identifier Name2;
      if (parseIdentifier(Name2,
                      diag::expected_identifier_after_coloncolon_type, Name))
        return true;

      Result = ScopeInfo.getQualifiedTypeName(Name, NameLoc, Name2, Loc2);
    }

    break;
  }
  case tok::l_paren:
  case tok::l_paren_space: {
    SourceLoc LPLoc = consumeToken();
    if (parseTypeTupleBody(LPLoc, Result))
      return true;

    if (parseToken(tok::r_paren, diag::expected_rparen_tuple_type_list,
                   tok::r_paren)) {
      diagnose(LPLoc, diag::opening_paren);
      return true;
    }
    break;
  }
  case tok::kw_protocol:
    if (parseTypeProtocol(Result))
      return true;
    break;
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


/// parseTypeTupleBody
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)*
///   type-tuple-element:
///     identifier value-specifier
///     type ('=' expr)?
bool Parser::parseTypeTupleBody(SourceLoc LPLoc, Type &Result) {
  SmallVector<TupleTypeElt, 8> Elements;

  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::r_brace)) {
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
  if ((SizeEx = parseSingleExpr(diag::expected_expr_array_type)))
    return true;
  
  SourceLoc RArrayTok = Tok.getLoc();
  if (parseToken(tok::r_square, diag::expected_rbracket_array_type)) {
    diagnose(LSquareLoc, diag::opening_bracket);
    return true;
  }
  
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
    SizeVal = IL->getValue();
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

///   type-protocol:
///      'protocol' attribute-list protocol-body
///
bool Parser::parseTypeProtocol(Type &Result) {
  SourceLoc ProtocolLoc = consumeToken(tok::kw_protocol);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  return parseTypeProtocolBody(ProtocolLoc, Attributes, Result);
}


///   protocol-body:
///      '{' protocol-element* '}'
///   protocol-element:
///      decl-func
///      decl-var-simple
///      // 'typealias' identifier
///
bool Parser::parseTypeProtocolBody(SourceLoc ProtocolLoc, 
                                   const DeclAttributes &Attributes,
                                   Type &Result, TypeAliasDecl *TypeName) {
  // Parse the body.
  if (parseToken(tok::l_brace, diag::expected_lbrace_protocol_type))
    return true;
  
  SmallVector<ValueDecl*, 8> Elements;
  
  // Parse the list of protocol elements.
  do {
    switch (Tok.getKind()) {
    default:
      diagnose(Tok, diag::expected_protocol_member);
      return true;
    case tok::r_brace:  // End of protocol body.
      break;
        
    case tok::kw_func:
      Elements.push_back(parseDeclFunc(false));
      if (Elements.back() == 0) return true;
      break;
    case tok::kw_var:
      Elements.push_back(parseDeclVarSimple());
      if (Elements.back() == 0) return true;
      break;
    }
  } while (Tok.isNot(tok::r_brace));
  
  consumeToken(tok::r_brace);

  
  // Act on what we've parsed.
  if (!Attributes.empty())
    diagnose(Attributes.LSquareLoc, diag::protocol_attributes);
  
  ProtocolType *NewProto = ProtocolType::getNew(ProtocolLoc, Elements,
                                                CurDeclContext);
  
  TupleTypeElt ThisElt(NewProto, Context.getIdentifier("this"));
  if (TypeName) ThisElt.Ty = TypeName->getAliasType();
  TupleType *ThisTy = TupleType::get(ThisElt, Context);
  
  // Install all of the members of protocol into the protocol's DeclContext, and
  // give each ValueDecl member an implicit "this" argument.
  // FIXME: This isn't quite right, 'this' should be found by name lookup when
  // parsing the body of each of these.
  for (Decl *D : Elements) {
    // Install the right DeclContext.
    D->Context = NewProto;
    
    // Change the type of the value decls to be functions that return whatever
    // they were declared as.
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
      VD->Ty = FunctionType::get(ThisTy, VD->Ty, Context);
  }
    
  if (TypeName) {
    // If we have a pretty name for this, complete it to its actual type.
    assert(TypeName->UnderlyingTy.isNull() &&
           "Not an incomplete decl to complete!");
    TypeName->UnderlyingTy = NewProto;
  }

  Result = NewProto;
  return false;
}
