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

#include "swift/Parse/Parser.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;

bool Parser::parseTypeAnnotation(TypeLoc &result) {
  return parseTypeAnnotation(result, diag::expected_type);
}

/// parseTypeAnnotation
///   type-annotation:
///     attribute-list type
bool Parser::parseTypeAnnotation(TypeLoc &result, Diag<> message) {
  // Parse attributes.
  DeclAttributes attrs;
  parseAttributeList(attrs);

  // Parse the type.
  if (parseType(result, message))
    return true;
  return applyAttributeToType(result, attrs);
}

bool Parser::applyAttributeToType(TypeLoc &result, DeclAttributes &attrs) {
  // Apply those attributes that do apply.
  if (attrs.empty()) return false;

  result = new (Context) AttributedTypeRepr(attrs, result.getTypeRepr());
  return false;
}

bool Parser::parseType(TypeLoc &Result) {
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
///     type-composition
///
bool Parser::parseType(TypeLoc &Result, Diag<> MessageID) {
  // Parse type-simple first.
  switch (Tok.getKind()) {
  case tok::kw_This:
  case tok::identifier:
    if (parseTypeIdentifier(Result))
      return true;
    break;
  case tok::kw_protocol:
    if (parseTypeComposition(Result))
      return true;
    break;
  case tok::l_paren: {
    if (parseTypeTupleBody(Result))
      return true;
    break;
  }
  default:
    diagnose(Tok.getLoc(), MessageID);
    return true;
  }

  // '.metatype' still leaves us with type-simple.
  while ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
         peekToken().is(tok::kw_metatype)) {
    consumeToken();
    SourceLoc metatypeLoc = consumeToken(tok::kw_metatype);
    TypeRepr *metaTyR = new (Context) MetaTypeTypeRepr(Result.getTypeRepr(),
                                                       metatypeLoc);
    Result = metaTyR;
  }

  // Handle type-function if we have an arrow.
  if (consumeIf(tok::arrow)) {
    TypeLoc SecondHalf;
    if (parseType(SecondHalf,
                  diag::expected_type_function_result))
      return true;
    TypeRepr *FnTyR = new (Context) FunctionTypeRepr(Result.getTypeRepr(),
                                                     SecondHalf.getTypeRepr());
    Result = FnTyR;
    return false;
  }

  // If there is a square bracket without a newline, we have an array.
  if (Tok.isFollowingLSquare())
    return parseTypeArray(Result);
  
  return false;
}

bool Parser::parseGenericArguments(MutableArrayRef<TypeLoc> &Args,
                                   SourceLoc &LAngleLoc,
                                   SourceLoc &RAngleLoc) {
  // Parse the opening '<'.
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  LAngleLoc = consumeStartingLess();

  SmallVector<TypeLoc, 4> GenericArgs;
  
  do {
    TypeLoc Result;
    if (parseType(Result, diag::expected_type)) {
      // Skip until we hit the '>'.
      skipUntilAnyOperator();
      if (startsWithGreater(Tok))
        consumeStartingGreater();
      return true;
    }

    GenericArgs.push_back(Result);
    // Parse the comma, if the list continues.
  } while (consumeIf(tok::comma));

  if (!startsWithGreater(Tok)) {
    diagnose(Tok.getLoc(), diag::expected_rangle_generic_arg_list);
    diagnose(LAngleLoc, diag::opening_angle);

    // Skip until we hit the '>'.
    skipUntilAnyOperator();
    if (startsWithGreater(Tok))
      RAngleLoc = consumeStartingGreater();
    return true;
  } else {
    RAngleLoc = consumeStartingGreater();
  }

  Args = Context.AllocateCopy(GenericArgs);
  return false;
}

/// parseTypeIdentifier
///   
///   type-identifier:
///     identifier generic-args? ('.' identifier generic-args?)*
///
bool Parser::parseTypeIdentifier(TypeLoc &Result) {
  if (Tok.isNot(tok::identifier) && Tok.isNot(tok::kw_This)) {
    diagnose(Tok.getLoc(), diag::expected_identifier_for_type);
    return true;
  }

  SmallVector<IdentTypeRepr::Component, 4> ComponentsR;
  SourceLoc EndLoc;
  while (true) {
    SourceLoc Loc;
    Identifier Name;
    if (parseIdentifier(Name, Loc, diag::expected_identifier_in_dotted_type))
      return true;
    SourceLoc LAngle, RAngle;
    // FIXME: Wasted allocation.
    MutableArrayRef<TypeLoc> GenericArgs;
    if (startsWithLess(Tok)) {
      if (parseGenericArguments(GenericArgs, LAngle, RAngle))
        return true;
    }
    EndLoc = Loc;
      
    SmallVector<TypeRepr *, 4> GenericArgsTyR;
    for (auto &TyLoc : GenericArgs)
      GenericArgsTyR.push_back(TyLoc.getTypeRepr());
    ComponentsR.push_back(IdentTypeRepr::Component(Loc, Name,
                                           Context.AllocateCopy(GenericArgsTyR),
                                                   CurDeclContext));

    // Treat 'Foo.<anything>' as an attempt to write a dotted type
    // unless <anything> is 'metatype'.
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        peekToken().isNot(tok::kw_metatype)) {
      consumeToken();
    } else {
      break;
    }
  }

  // Lookup element #0 through our current scope chains in case it is some thing
  // local (this returns null if nothing is found).
  if (auto Entry = ScopeInfo.lookupValueName(ComponentsR[0].getIdentifier()))
    ComponentsR[0].setValue(Entry);

  auto TyR = IdentTypeRepr::create(Context, ComponentsR);
  Result = TyR;
  return false;
}

/// parseTypeComposition
///   
///   type-composition:
///     'protocol' '<' type-composition-list? '>'
///
///   type-composition-list:
///     type-identifier (',' type-identifier)*
///
bool Parser::parseTypeComposition(TypeLoc &Result) {
  SourceLoc ProtocolLoc = consumeToken(tok::kw_protocol);
 
  // Check for the starting '<'.
  if (!startsWithLess(Tok)) {
    diagnose(Tok.getLoc(), diag::expected_langle_protocol);
    return true;
  }
  SourceLoc LAngleLoc = consumeStartingLess();
  
  // Check for empty protocol composition.
  if (startsWithGreater(Tok)) {
    SourceLoc RAngleLoc = consumeStartingGreater();
    auto ResultTyR = new (Context) ProtocolCompositionTypeRepr(
                                             MutableArrayRef<IdentTypeRepr *>(),
                                             ProtocolLoc,
                                             SourceRange(LAngleLoc, RAngleLoc));
    Result = ResultTyR;
    return false;
  }
  
  // Parse the type-composition-list.
  bool Invalid = false;
  SmallVector<TypeLoc, 4> Protocols;
  do {
    // Parse the type-identifier.
    TypeLoc Protocol;
    if (parseTypeIdentifier(Protocol)) {
      Invalid = true;
      break;
    }
    
    Protocols.push_back(Protocol);
  } while (consumeIf(tok::comma));
  
  // Check for the terminating '>'.
  SourceLoc EndLoc = Tok.getLoc();
  if (!startsWithGreater(Tok)) {
    if (!Invalid) {
      diagnose(Tok.getLoc(), diag::expected_rangle_protocol);
      diagnose(LAngleLoc, diag::opening_angle);
    }

    // Skip until we hit the '>'.
    skipUntilAnyOperator();
    if (startsWithGreater(Tok))
      EndLoc = consumeStartingGreater();    
  } else {
    EndLoc = consumeStartingGreater();
  }

  SmallVector<IdentTypeRepr *, 4> ProtocolTypesR;
  for (TypeLoc T : Protocols) {
    ProtocolTypesR.push_back(cast<IdentTypeRepr>(T.getTypeRepr()));
  }
  Result =  ProtocolCompositionTypeRepr::create(Context, ProtocolTypesR,
                                                ProtocolLoc,
                                                SourceRange(LAngleLoc, EndLoc));
  return false;
}

/// parseTypeTupleBody
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)* '...'?
///   type-tuple-element:
///     identifier ':' type-annotation
///     type-annotation
bool Parser::parseTypeTupleBody(TypeLoc &Result) {
  SourceLoc RPLoc, LPLoc = consumeToken(tok::l_paren);
  SourceLoc EllipsisLoc;
  SmallVector<TypeRepr *, 8> ElementsR;
  bool HadEllipsis = false;

  bool Invalid = parseList(tok::r_paren, LPLoc, RPLoc,
                           tok::comma, /*OptionalSep=*/false,
                           diag::expected_rparen_tuple_type_list,
                           [&] () -> bool {
    // If the tuple element starts with "ident :", then
    // the identifier is an element tag, and it is followed by a type
    // annotation.
    if (isStartOfBindingName(Tok) && peekToken().is(tok::colon)) {
      // Consume the name
      // FIXME: Should the identifier '_' ever be formed?
      Identifier name = Context.getIdentifier(Tok.getText());
      SourceLoc nameLoc = consumeToken();

      // Consume the ':'.
      consumeToken(tok::colon);

      // Parse the type annotation.
      TypeLoc type;
      if (parseTypeAnnotation(type, diag::expected_type))
        return true;

      ElementsR.push_back(new (Context) NamedTypeRepr(name, type.getTypeRepr(),
                                                      nameLoc));
    } else {
      // Otherwise, this has to be a type.
      TypeLoc type;
      if (parseTypeAnnotation(type))
        return true;
      ElementsR.push_back(type.getTypeRepr());
    }

    // Parse '= expr' here so we can complain about it directly, rather
    // than dying when we see it.
    if (Tok.is(tok::equal)) {
      SourceLoc equalLoc = consumeToken(tok::equal);
      auto init = parseExpr(diag::expected_initializer_expr);
      auto inFlight = diagnose(equalLoc, diag::tuple_type_init);
      if (init.isNonNull())
        inFlight.fixItRemove(SourceRange(equalLoc, init.get()->getEndLoc()));
    }

    if (Tok.is(tok::ellipsis)) {
      EllipsisLoc = consumeToken(tok::ellipsis);
      if (Tok.is(tok::r_paren)) {
        HadEllipsis = true;
      } else {
        diagnose(EllipsisLoc, diag::unexpected_ellipsis_in_tuple);
        Invalid = true;
      }
    }
    return false;
  });

  if (HadEllipsis && ElementsR.empty()) {
    diagnose(EllipsisLoc, diag::empty_tuple_ellipsis);
    return true;
  }

  Result = TupleTypeRepr::create(Context, ElementsR,
                                 SourceRange(LPLoc, Tok.getLoc()),
                                 HadEllipsis ? EllipsisLoc : SourceLoc());
  return Invalid;
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
bool Parser::parseTypeArray(TypeLoc &result) {
  assert(Tok.isFollowingLSquare());
  SourceLoc lsquareLoc = consumeToken();

  // Handle the [] production, meaning an array slice.
  if (Tok.is(tok::r_square)) {
    SourceLoc rsquareLoc = consumeToken(tok::r_square);

    // If we're starting another square-bracket clause, recur.
    if (Tok.isFollowingLSquare() && parseTypeArray(result))
      return true;

    // Just build a normal array slice type.
    result = new (Context) ArrayTypeRepr(result.getTypeRepr(),
                                         nullptr,
                                         SourceRange(lsquareLoc, rsquareLoc));
    return false;
  }

  NullablePtr<Expr> sizeEx = parseExpr(diag::expected_expr_array_type);
  if (sizeEx.isNull()) return true;

  SourceLoc rsquareLoc;
  if (parseMatchingToken(tok::r_square, rsquareLoc,
                         diag::expected_rbracket_array_type, lsquareLoc))
    return true;

  // If we're starting another square-bracket clause, recur.
  if (Tok.isFollowingLSquare() && parseTypeArray(result))
    return true;
  
  // FIXME: We don't supported fixed-length arrays yet.
  diagnose(lsquareLoc, diag::unsupported_fixed_length_array)
    .highlight(sizeEx.get()->getSourceRange());
  
  return true;
}

//===--------------------------------------------------------------------===//
// Speculative type list parsing
//===--------------------------------------------------------------------===//

static bool isGenericTypeDisambiguatingToken(Token &tok) {
  switch (tok.getKind()) {
  default:
    return false;
  case tok::r_paren:
  case tok::r_square:
  case tok::l_brace:
  case tok::r_brace:
  case tok::period:
  case tok::comma:
  case tok::semi:
  case tok::eof:
    return true;
  
  case tok::period_prefix:
    // These will be turned into following tokens if they appear unspaced after
    // a generic angle bracket.
    return tok.getText().data()[-1] == '>';
      
  case tok::l_paren:
  case tok::l_square:
    // These only apply to the generic type if they don't start a new line.
    return !tok.isAtStartOfLine();
  }
}

bool Parser::canParseAsGenericArgumentList() {
  if (!Tok.isAnyOperator() || !Tok.getText().equals("<"))
    return false;

  BacktrackingScope backtrack(*this);

  if (canParseGenericArguments())
    return isGenericTypeDisambiguatingToken(Tok);

  return false;
}

bool Parser::canParseGenericArguments() {
  // Parse the opening '<'.
  if (!startsWithLess(Tok))
    return false;
  consumeStartingLess();
  
  do {
    if (!canParseType())
      return false;
    // Parse the comma, if the list continues.
  } while (consumeIf(tok::comma));
  
  if (!startsWithGreater(Tok)) {
    return false;
  } else {
    consumeStartingGreater();
    return true;
  }
}

bool Parser::canParseType() {
  switch (Tok.getKind()) {
  case tok::kw_This:
  case tok::identifier:
    if (!canParseTypeIdentifier())
      return false;
    break;
  case tok::kw_protocol:
    if (!canParseTypeComposition())
      return false;
    break;
  case tok::l_paren: {
    consumeToken();
    if (!canParseTypeTupleBody())
      return false;
    break;
  }
  default:
    return false;
  }
  
  // '.metatype' still leaves us with type-simple.
  while ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
         peekToken().is(tok::kw_metatype)) {
    consumeToken();
    consumeToken(tok::kw_metatype);
  }
  
  // Handle type-function if we have an arrow.
  if (consumeIf(tok::arrow)) {
    if (!canParseType())
      return false;
    return true;
  }
  
  // If there is a square bracket without a newline, we have an array.
  if (Tok.isFollowingLSquare())
    return canParseTypeArray();
  
  return true;
}

bool Parser::canParseTypeIdentifier() {
  if (Tok.isNot(tok::identifier) && Tok.isNot(tok::kw_This)) {
    return false;
  }
  
  while (true) {
    switch (Tok.getKind()) {
#define IDENTIFIER_KEYWORD(kw) case tok::kw_##kw:
#include "swift/Parse/Tokens.def"
    case tok::identifier:
      consumeToken();
      break;
    default:
      return false;
    }
    
    if (startsWithLess(Tok)) {
      if (!canParseGenericArguments())
        return false;
    }

    // Treat 'Foo.<anything>' as an attempt to write a dotted type
    // unless <anything> is 'metatype'.
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        peekToken().isNot(tok::kw_metatype)) {
      consumeToken();
    } else {
      return true;
    }
  }
}

bool Parser::canParseTypeComposition() {
  consumeToken(tok::kw_protocol);
  
  // Check for the starting '<'.
  if (!startsWithLess(Tok)) {
    return false;
  }
  consumeStartingLess();
  
  // Check for empty protocol composition.
  if (startsWithGreater(Tok)) {
    consumeStartingGreater();
    return true;
  }
  
  // Parse the type-composition-list.
  do {
    if (!canParseTypeIdentifier()) {
      return false;
    }
  } while (consumeIf(tok::comma));
  
  // Check for the terminating '>'.
  if (!startsWithGreater(Tok)) {
    return false;
  }
  consumeStartingGreater();
  
  return true;
}

bool Parser::canParseTypeTupleBody() {
  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::r_brace) &&
      Tok.isNot(tok::ellipsis) && !isStartOfDecl(Tok, peekToken())) {
    do {
      // If the tuple element starts with "ident :", then it is followed
      // by a type annotation.
      if (Tok.is(tok::identifier) && peekToken().is(tok::colon)) {
        consumeToken(tok::identifier);
        consumeToken(tok::colon);

        // Parse attributes.
        if (consumeIf(tok::l_square)) {
          while (Tok.isNot(tok::eof) && Tok.isNot(tok::r_brace) &&
                 Tok.isNot(tok::r_square) && Tok.isNot(tok::r_paren) &&
                 !isStartOfDecl(Tok, peekToken()))
            skipSingle();

          if (!consumeIf(tok::r_square))
            return false;
        }

        // Parse the type.
        if (!canParseType())
          return false;

        // Parse default values. This aren't actually allowed, but we recover
        // better if we skip over them.
        if (consumeIf(tok::equal)) {
          while (Tok.isNot(tok::eof) && Tok.isNot(tok::r_paren) &&
                 Tok.isNot(tok::r_brace) && Tok.isNot(tok::ellipsis) &&
                 Tok.isNot(tok::comma) &&
                 !isStartOfDecl(Tok, peekToken())) {
            skipSingle();
          }
        }

        continue;
      }
      
      // Otherwise, this has to be a type.

      // Parse attributes.
      if (consumeIf(tok::l_square)) {
        while (Tok.isNot(tok::eof) && Tok.isNot(tok::r_brace) &&
               Tok.isNot(tok::r_square) && Tok.isNot(tok::r_paren) &&
               !isStartOfDecl(Tok, peekToken()))
          skipSingle();

        if (!consumeIf(tok::r_square))
          return false;
      }

      if (!canParseType())
        return false;
    } while (consumeIf(tok::comma));
  }
  
  if (Tok.is(tok::ellipsis)) {
    consumeToken();
  }
  
  if (Tok.is(tok::r_paren)) {
    consumeToken();
    return true;
  } else {
    return false;
  }
}


bool Parser::canParseTypeArray() {
  assert(Tok.isFollowingLSquare());
  consumeToken();
  
  // Handle the [] production, meaning an array slice.
  if (Tok.is(tok::r_square)) {
    consumeToken(tok::r_square);
    
    // If we're starting another square-bracket clause, recur.
    if (Tok.isFollowingLSquare())
      return canParseTypeArray();
    
    return true;
  }
  
  // FIXME: Size expressions!
  return false;
}

