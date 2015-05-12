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
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;

TypeRepr *Parser::applyAttributeToType(TypeRepr *ty,
                                       const TypeAttributes &attrs) {
  // Apply those attributes that do apply.
  if (attrs.empty())
    return ty;

  return new (Context) AttributedTypeRepr(attrs, ty);
}

ParserResult<TypeRepr> Parser::parseTypeSimple() {
  return parseTypeSimple(diag::expected_type);
}

/// parseTypeSimple
///   type-simple:
///     type-identifier
///     type-tuple
///     type-composition
///     type-simple '.Type'
///     type-simple '.Protocol'
///     type-simple '?'
///     type-simple '!'
///     type-collection
ParserResult<TypeRepr> Parser::parseTypeSimple(Diag<> MessageID) {
  ParserResult<TypeRepr> ty;
  // If this is an "inout" marker for an identifier type, consume the inout.
  SourceLoc InOutLoc;
  consumeIf(tok::kw_inout, InOutLoc);

  switch (Tok.getKind()) {
  case tok::kw_Self:
  case tok::identifier: {
    ty = parseTypeIdentifier();
    if (InOutLoc.isValid())
      ty = makeParserResult(new (Context) InOutTypeRepr(ty.get(),
                                                        InOutLoc));
    break;
  }
  case tok::kw_protocol:
    ty = parseTypeComposition();
    break;
  case tok::l_paren: {
    ty = parseTypeTupleBody();
    break;
  }
  case tok::code_complete: {
    if (CodeCompletion)
      CodeCompletion->completeTypeSimpleBeginning();
    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionResult<TypeRepr>();
  }
  case tok::kw_super:
  case tok::kw_dynamicType:
  case tok::kw_self:
    // These keywords don't start a decl or a statement, and thus should be
    // safe to skip over.
    diagnose(Tok, MessageID);
    ty = makeParserErrorResult(new (Context) ErrorTypeRepr(Tok.getLoc()));
    consumeToken();
    // FIXME: we could try to continue to parse.
    return ty;
  case tok::l_square:
    ty = parseTypeCollection();
    break;
  default:
    checkForInputIncomplete();
    diagnose(Tok, MessageID);
    return nullptr;
  }

  // '.Type', '.Protocol', '?', and '!' still leave us with type-simple.
  while (ty.isNonNull()) {
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix))) {
      if (peekToken().isContextualKeyword("Type")) {
        consumeToken();
        SourceLoc metatypeLoc = consumeToken(tok::identifier);
        ty = makeParserResult(ty,
          new (Context) MetatypeTypeRepr(ty.get(), metatypeLoc));
        continue;
      }
      if (peekToken().isContextualKeyword("Protocol")) {
        consumeToken();
        SourceLoc protocolLoc = consumeToken(tok::identifier);
        ty = makeParserResult(ty,
          new (Context) ProtocolTypeRepr(ty.get(), protocolLoc));
        continue;
      }
    }

    if (!Tok.isAtStartOfLine()) {
      if (isOptionalToken(Tok)) {
        ty = parseTypeOptional(ty.get());
        continue;
      }
      if (isImplicitlyUnwrappedOptionalToken(Tok)) {
        ty = parseTypeImplicitlyUnwrappedOptional(ty.get());
        continue;
      }
    }
    break;
  }

  return ty;
}

ParserResult<TypeRepr> Parser::parseType() {
  return parseType(diag::expected_type);
}

/// parseType
///   type:
///     attribute-list type-function
///     attribute-list type-array
///
///   type-function:
///     type-simple '->' type
///
ParserResult<TypeRepr> Parser::parseType(Diag<> MessageID) {
  // Parse attributes.
  TypeAttributes attrs;
  parseTypeAttributeList(attrs);

  // Parse Generic Parameters. Generic Parameters are visible in the function
  // body.
  GenericParamList *generics = nullptr;
  if (isInSILMode()) {
    generics = maybeParseGenericParams();
  }

  ParserResult<TypeRepr> ty = parseTypeSimple(MessageID);
  if (ty.hasCodeCompletion())
    return makeParserCodeCompletionResult<TypeRepr>();

  if (ty.isNull())
    return nullptr;
  
  ParserPosition beforeThrowsPos;
  SourceLoc throwsLoc;
  bool rethrows = false;
  if (Tok.isAny(tok::kw_throws, tok::kw_rethrows)) {
    beforeThrowsPos = getParserPosition();
    rethrows = Tok.is(tok::kw_rethrows);
    throwsLoc = consumeToken();
  }

  // Handle type-function if we have an arrow.
  SourceLoc arrowLoc;
  if (consumeIf(tok::arrow, arrowLoc)) {
    ParserResult<TypeRepr> SecondHalf =
      parseType(diag::expected_type_function_result);
    if (SecondHalf.hasCodeCompletion())
      return makeParserCodeCompletionResult<TypeRepr>();
    if (SecondHalf.isNull())
      return nullptr;
    if (rethrows) {
      // 'rethrows' is only allowed on function declarations for now.
      diagnose(throwsLoc, diag::rethrowing_function_type);
    }
    auto fnTy = new (Context) FunctionTypeRepr(generics, ty.get(),
                                               throwsLoc,
                                               arrowLoc,
                                               SecondHalf.get());
    return makeParserResult(applyAttributeToType(fnTy, attrs));
  } else if (throwsLoc.isValid()) {
    // Don't consume 'throws', so we can emit a more useful diagnostic when
    // parsing a function decl.
    restoreParserPosition(beforeThrowsPos);
    return ty;
  }
  
  // Only function types may be generic.
  if (generics) {
    auto brackets = generics->getSourceRange();
    diagnose(brackets.Start, diag::generic_non_function);
  }

  // Parse legacy array types for migration.
  while (ty.isNonNull() && !Tok.isAtStartOfLine()) {
    if (Tok.is(tok::l_square)) {
      ty = parseTypeArray(ty.get());
    } else {
      break;
    }
  }

  if (ty.isNonNull() && !ty.hasCodeCompletion()) {
    ty = makeParserResult(applyAttributeToType(ty.get(), attrs));
  }
  return ty;
}

ParserResult<TypeRepr> Parser::parseTypeIdentifierWithRecovery(
    Diag<> MessageID, Diag<TypeLoc> NonIdentifierTypeMessageID) {
  ParserResult<TypeRepr> Ty = parseType(MessageID);

  if (!Ty.isParseError() && !isa<IdentTypeRepr>(Ty.get()) &&
      !isa<ErrorTypeRepr>(Ty.get())) {
    diagnose(Ty.get()->getStartLoc(), NonIdentifierTypeMessageID, Ty.get())
        .highlight(Ty.get()->getSourceRange());
    Ty.setIsParseError();
    Ty = makeParserResult(
        Ty, new (Context) ErrorTypeRepr(Ty.get()->getSourceRange()));
  }

  assert(Ty.isNull() ||
         isa<IdentTypeRepr>(Ty.get()) ||
         isa<ErrorTypeRepr>(Ty.get()));
  return Ty;
}

bool Parser::parseGenericArguments(SmallVectorImpl<TypeRepr*> &Args,
                                   SourceLoc &LAngleLoc,
                                   SourceLoc &RAngleLoc) {
  // Parse the opening '<'.
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  LAngleLoc = consumeStartingLess();

  do {
    ParserResult<TypeRepr> Ty = parseType(diag::expected_type);
    if (Ty.isNull() || Ty.hasCodeCompletion()) {
      // Skip until we hit the '>'.
      skipUntilGreaterInTypeList();
      if (startsWithGreater(Tok))
        consumeStartingGreater();
      return true;
    }

    Args.push_back(Ty.get());
    // Parse the comma, if the list continues.
  } while (consumeIf(tok::comma));

  if (!startsWithGreater(Tok)) {
    checkForInputIncomplete();
    diagnose(Tok, diag::expected_rangle_generic_arg_list);
    diagnose(LAngleLoc, diag::opening_angle);

    // Skip until we hit the '>'.
    skipUntilGreaterInTypeList();
    if (startsWithGreater(Tok))
      RAngleLoc = consumeStartingGreater();
    return true;
  } else {
    RAngleLoc = consumeStartingGreater();
  }

  return false;
}

/// parseTypeIdentifier
///   
///   type-identifier:
///     identifier generic-args? ('.' identifier generic-args?)*
///
ParserResult<IdentTypeRepr> Parser::parseTypeIdentifier() {
  if (Tok.isNot(tok::identifier) && Tok.isNot(tok::kw_Self)) {
    if (Tok.is(tok::code_complete)) {
      if (CodeCompletion)
        CodeCompletion->completeTypeIdentifierWithDot(nullptr);
      // Eat the code completion token because we handled it.
      consumeToken(tok::code_complete);
      return makeParserCodeCompletionResult<IdentTypeRepr>();
    }

    diagnose(Tok, diag::expected_identifier_for_type);
    return nullptr;
  }

  ParserStatus Status;
  SmallVector<ComponentIdentTypeRepr *, 4> ComponentsR;
  SourceLoc EndLoc;
  while (true) {
    SourceLoc Loc;
    Identifier Name;
    if (Tok.is(tok::kw_Self)) {
      Loc = consumeIdentifier(&Name);
    } else {
      // FIXME: specialize diagnostic for 'Type': type can not start with
      // 'metatype'
      // FIXME: offer a fixit: 'self' -> 'Self'
      if (parseIdentifier(Name, Loc, diag::expected_identifier_in_dotted_type))
        Status.setIsParseError();
    }

    if (Loc.isValid()) {
      SourceLoc LAngle, RAngle;
      SmallVector<TypeRepr*, 8> GenericArgs;
      if (startsWithLess(Tok)) {
        if (parseGenericArguments(GenericArgs, LAngle, RAngle))
          return nullptr;
      }
      EndLoc = Loc;

      ComponentIdentTypeRepr *CompT;
      if (!GenericArgs.empty())
        CompT = new (Context) GenericIdentTypeRepr(Loc, Name,
                                             Context.AllocateCopy(GenericArgs),
                                             SourceRange(LAngle, RAngle));
      else
        CompT = new (Context) SimpleIdentTypeRepr(Loc, Name);
      ComponentsR.push_back(CompT);
    }

    // Treat 'Foo.<anything>' as an attempt to write a dotted type
    // unless <anything> is 'Type'.
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix))) {
      if (peekToken().is(tok::code_complete)) {
        Status.setHasCodeCompletion();
        break;
      }
      if (!peekToken().isContextualKeyword("Type")
          && !peekToken().isContextualKeyword("Protocol")) {
        consumeToken();
        continue;
      }
    } else if (Tok.is(tok::code_complete)) {
      if (!Tok.isAtStartOfLine())
        Status.setHasCodeCompletion();
      break;
    }
    break;
  }

  IdentTypeRepr *ITR = nullptr;
  if (!ComponentsR.empty()) {
    // Lookup element #0 through our current scope chains in case it is some
    // thing local (this returns null if nothing is found).
    if (auto Entry = lookupInScope(ComponentsR[0]->getIdentifier()))
      ComponentsR[0]->setValue(Entry);

    ITR = IdentTypeRepr::create(Context, ComponentsR);
  }

  if (Status.hasCodeCompletion() && CodeCompletion) {
    if (Tok.isNot(tok::code_complete)) {
      // We have a dot.
      consumeToken();
      CodeCompletion->completeTypeIdentifierWithDot(ITR);
    } else {
      CodeCompletion->completeTypeIdentifierWithoutDot(ITR);
    }
    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
  }

  return makeParserResult(Status, ITR);
}

/// parseTypeComposition
///   
///   type-composition:
///     'protocol' '<' type-composition-list? '>'
///
///   type-composition-list:
///     type-identifier (',' type-identifier)*
///
ParserResult<ProtocolCompositionTypeRepr> Parser::parseTypeComposition() {
  SourceLoc ProtocolLoc = consumeToken(tok::kw_protocol);
 
  // Check for the starting '<'.
  if (!startsWithLess(Tok)) {
    diagnose(Tok, diag::expected_langle_protocol);
    return nullptr;
  }
  SourceLoc LAngleLoc = consumeStartingLess();
  
  // Check for empty protocol composition.
  if (startsWithGreater(Tok)) {
    SourceLoc RAngleLoc = consumeStartingGreater();
    return makeParserResult(new (Context) ProtocolCompositionTypeRepr(
                                             ArrayRef<IdentTypeRepr *>(),
                                             ProtocolLoc,
                                             SourceRange(LAngleLoc,
                                                         RAngleLoc)));
  }
  
  // Parse the type-composition-list.
  ParserStatus Status;
  SmallVector<IdentTypeRepr *, 4> Protocols;
  do {
    // Parse the type-identifier.
    ParserResult<IdentTypeRepr> Protocol = parseTypeIdentifier();
    Status |= Protocol;
    if (Protocol.isNonNull())
      Protocols.push_back(Protocol.get());
  } while (consumeIf(tok::comma));
  
  // Check for the terminating '>'.
  SourceLoc EndLoc = Tok.getLoc();
  if (!startsWithGreater(Tok)) {
    if (Status.isSuccess()) {
      diagnose(Tok, diag::expected_rangle_protocol);
      diagnose(LAngleLoc, diag::opening_angle);
      Status.setIsParseError();
    }

    // Skip until we hit the '>'.
    skipUntilGreaterInTypeList();
    if (startsWithGreater(Tok))
      EndLoc = consumeStartingGreater();    
  } else {
    EndLoc = consumeStartingGreater();
  }

  return makeParserResult(Status, ProtocolCompositionTypeRepr::create(
      Context, Protocols, ProtocolLoc, SourceRange(LAngleLoc, EndLoc)));
}

/// parseTypeTupleBody
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)* '...'?
///   type-tuple-element:
///     identifier ':' type
///     type
ParserResult<TupleTypeRepr> Parser::parseTypeTupleBody() {
  Parser::StructureMarkerRAII ParsingTypeTuple(*this, Tok);
  SourceLoc RPLoc, LPLoc = consumeToken(tok::l_paren);
  SourceLoc EllipsisLoc;
  SmallVector<TypeRepr *, 8> ElementsR;
  bool HadEllipsis = false;

  ParserStatus Status = parseList(tok::r_paren, LPLoc, RPLoc,
                                  tok::comma, /*OptionalSep=*/false,
                                  /*AllowSepAfterLast=*/false,
                                  diag::expected_rparen_tuple_type_list,
                                  [&] () -> ParserStatus {
    // If this is an inout marker in an argument list, consume the inout.
    SourceLoc InOutLoc;
    consumeIf(tok::kw_inout, InOutLoc);

    // If the tuple element starts with "ident :", then
    // the identifier is an element tag, and it is followed by a type
    // annotation.
    if (Tok.isIdentifierOrUnderscore() && peekToken().is(tok::colon)) {
      // Consume the name
      Identifier name;
      if (!Tok.is(tok::kw__))
        name = Context.getIdentifier(Tok.getText());
      SourceLoc nameLoc = consumeToken();

      // Consume the ':'.
      consumeToken(tok::colon);

      // Parse the type annotation.
      ParserResult<TypeRepr> type = parseType(diag::expected_type);
      if (type.hasCodeCompletion())
        return makeParserCodeCompletionStatus();
      if (type.isNull())
        return makeParserError();

      // If an 'inout' marker was specified, build the type.  Note that we bury
      // the inout locator within the named locator.  This is weird but required
      // by sema apparently.
      if (InOutLoc.isValid())
        type = makeParserResult(new (Context) InOutTypeRepr(type.get(),
                                                            InOutLoc));

      
      ElementsR.push_back(
          new (Context) NamedTypeRepr(name, type.get(), nameLoc));
    } else {
      // Otherwise, this has to be a type.
      ParserResult<TypeRepr> type = parseType();
      if (type.hasCodeCompletion())
        return makeParserCodeCompletionStatus();
      if (type.isNull())
        return makeParserError();
      if (InOutLoc.isValid())
        type = makeParserResult(new (Context) InOutTypeRepr(type.get(),
                                                            InOutLoc));

      ElementsR.push_back(type.get());
    }

    // Parse '= expr' here so we can complain about it directly, rather
    // than dying when we see it.
    if (Tok.is(tok::equal)) {
      SourceLoc equalLoc = consumeToken(tok::equal);
      auto init = parseExpr(diag::expected_init_value);
      auto inFlight = diagnose(equalLoc, diag::tuple_type_init);
      if (init.isNonNull())
        inFlight.fixItRemove(SourceRange(equalLoc, init.get()->getEndLoc()));
    }

    if (Tok.isEllipsis()) {
      EllipsisLoc = consumeToken();
      if (Tok.is(tok::r_paren)) {
        HadEllipsis = true;
      } else {
        diagnose(EllipsisLoc, diag::unexpected_ellipsis_in_tuple);
        return makeParserError();
      }
    }
    return makeParserSuccess();
  });

  if (HadEllipsis && ElementsR.empty()) {
    diagnose(EllipsisLoc, diag::empty_tuple_ellipsis);
    return nullptr;
  }

  return makeParserResult(Status, TupleTypeRepr::create(
      Context, ElementsR, SourceRange(LPLoc, RPLoc),
      HadEllipsis ? EllipsisLoc : SourceLoc()));
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
ParserResult<ArrayTypeRepr> Parser::parseTypeArray(TypeRepr *Base) {
  assert(Tok.isFollowingLSquare());
  Parser::StructureMarkerRAII ParsingArrayBound(*this, Tok);
  SourceLoc lsquareLoc = consumeToken();
  ParserResult<TypeRepr> NestedType = makeParserResult(Base);
  ArrayTypeRepr *ATR = nullptr;
  
  // Handle the [] production, meaning an array slice.
  if (Tok.is(tok::r_square)) {
    SourceLoc rsquareLoc = consumeToken(tok::r_square);
    
      
    // If we're starting another square-bracket clause, recur.
    if (Tok.isFollowingLSquare()) {
      NestedType = parseTypeArray(Base);
      if (NestedType.hasCodeCompletion())
        return makeParserCodeCompletionResult<ArrayTypeRepr>();
      if (NestedType.isNull()) {
        // We could not parse the rest of the type, but we still have the base
        // type.
        NestedType = makeParserErrorResult(Base);
      }
    }
    
    // Just build a normal array slice type.
    ATR = new (Context) ArrayTypeRepr(NestedType.get(), nullptr,
                                           SourceRange(lsquareLoc, rsquareLoc),
                                      /*OldSyntax=*/true);

    if (NestedType.isParseError())
      return makeParserErrorResult(ATR);
    else {
      diagnose(lsquareLoc, diag::new_array_syntax)
        .fixItInsert(Base->getStartLoc(), "[")
        .fixItRemove(lsquareLoc);

      return makeParserResult(ATR);
    }
  }

  SourceLoc rsquareLoc;

  // We currently only accept an integer literal as the inner expression.
  // FIXME: Should we decide to support integer constant expressions in the
  // future, we will need to remove this check to accept any compositional
  // expressions
  ParserResult<Expr> sizeEx = parseExprBasic(diag::expected_expr_array_type);

  parseMatchingToken(tok::r_square, rsquareLoc,
                     diag::expected_rbracket_array_type, lsquareLoc);

  if (!sizeEx.isNull() && isa<IntegerLiteralExpr>(sizeEx.get())) {
    if (sizeEx.hasCodeCompletion())
      return makeParserCodeCompletionStatus();

    NestedType = makeParserErrorResult(Base);
    
    // FIXME: We don't supported fixed-length arrays yet.
    diagnose(lsquareLoc, diag::unsupported_fixed_length_array)
    .highlight(sizeEx.get()->getSourceRange());
    
    ATR = new (Context) ArrayTypeRepr(NestedType.get(),
                                      nullptr,
                                      SourceRange(lsquareLoc,
                                                  getEndOfPreviousLoc()),
                                      /*OldSyntax=*/true);
    return makeParserErrorResult(ATR);
  }

  // If the size expression is null, we would have raised the
  // expected_expr_array_type error above when the token stream failed to
  // parse as an expression
  if (!sizeEx.isNull()) {
    diagnose(lsquareLoc, diag::expected_expr_array_type)
    .highlight(sizeEx.get()->getSourceRange());
  } else {
    // Skip until the next decl, statement or block
    skipUntilDeclStmtRBrace(tok::l_brace);
  }

  // Create an array slice type for the malformed array type specification
  NestedType = makeParserErrorResult(Base);
  ATR = new (Context) ArrayTypeRepr(NestedType.get(), nullptr,
                                    SourceRange(lsquareLoc,
                                                PreviousLoc),
                                    /*OldSyntax=*/true);
  return makeParserErrorResult(ATR);
}

ParserResult<TypeRepr> Parser::parseTypeCollection() {
  // Parse the leading '['.
  assert(Tok.is(tok::l_square));
  Parser::StructureMarkerRAII parsingCollection(*this, Tok);
  SourceLoc lsquareLoc = consumeToken();

  // Parse the element type.
  ParserResult<TypeRepr> firstTy = parseType(diag::expected_element_type);

  // If there is a ':', this is a dictionary type.
  SourceLoc colonLoc;
  ParserResult<TypeRepr> secondTy;
  if (Tok.is(tok::colon)) {
    colonLoc = consumeToken();

    // Parse the second type.
    secondTy = parseType(diag::expected_dictionary_value_type);
  }

  // Parse the closing ']'.
  SourceLoc rsquareLoc;
  parseMatchingToken(tok::r_square, rsquareLoc,
                     colonLoc.isValid()
                       ? diag::expected_rbracket_dictionary_type
                       : diag::expected_rbracket_array_type, 
                     lsquareLoc);

  // If we couldn't parse anything for one of the types, propagate the error.
  if (firstTy.isNull() || (colonLoc.isValid() && secondTy.isNull()))
    return makeParserError();

  // Form the dictionary type.
  SourceRange brackets(lsquareLoc, rsquareLoc);
  if (colonLoc.isValid())
    return makeParserResult(ParserStatus(firstTy) | ParserStatus(secondTy),
                            new (Context) DictionaryTypeRepr(firstTy.get(),
                                                             secondTy.get(),
                                                             colonLoc,
                                                             brackets));
    
  // Form the array type.
  return makeParserResult(firstTy,
                          new (Context) ArrayTypeRepr(firstTy.get(),
                                                      nullptr,
                                                      brackets,
                                                      /*OldSyntax=*/false));
}

bool Parser::isOptionalToken(const Token &T) const {
  // A postfix '?' by itself is obviously optional.
  if (T.is(tok::question_postfix))
    return true;
  
  // A postfix or bound infix operator token that begins with '?' can be
  // optional too. We'll munch off the '?', so long as it is left-bound with
  // the type (i.e., parsed as a postfix or unspaced binary operator).
  if ((T.is(tok::oper_postfix) || T.is(tok::oper_binary_unspaced)) &&
      T.getText().startswith("?"))
    return true;
  return false;
}

bool Parser::isImplicitlyUnwrappedOptionalToken(const Token &T) const {
  // A postfix '!' by itself, or a '!' in SIL mode, is obviously implicitly
  // unwrapped optional.
  if (T.is(tok::exclaim_postfix) || T.is(tok::sil_exclamation))
    return true;
  // A postfix or bound infix operator token that begins with '!' can be
  // implicitly unwrapped optional too. We'll munch off the '!', so long as it
  // is left-bound with the type (i.e., parsed as a postfix or unspaced binary
  // operator).
  if ((T.is(tok::oper_postfix) || T.is(tok::oper_binary_unspaced)) &&
      T.getText().startswith("!"))
    return true;
  return false;
}

SourceLoc Parser::consumeOptionalToken() {
  assert(isOptionalToken(Tok) && "not a '?' token?!");
  return consumeStartingCharacterOfCurrentToken();
}

SourceLoc Parser::consumeImplicitlyUnwrappedOptionalToken() {
  assert(isImplicitlyUnwrappedOptionalToken(Tok) && "not a '!' token?!");
  // If the text of the token is just '!', grab the next token.
  return consumeStartingCharacterOfCurrentToken();
}

/// Parse a single optional suffix, given that we are looking at the
/// question mark.
ParserResult<OptionalTypeRepr> Parser::parseTypeOptional(TypeRepr *base) {
  SourceLoc questionLoc = consumeOptionalToken();
  return makeParserResult(new (Context) OptionalTypeRepr(base, questionLoc));
}

/// Parse a single implicitly unwrapped optional suffix, given that we
/// are looking at the exclamation mark.
ParserResult<ImplicitlyUnwrappedOptionalTypeRepr>
Parser::parseTypeImplicitlyUnwrappedOptional(TypeRepr *base) {
  SourceLoc exclamationLoc = consumeImplicitlyUnwrappedOptionalToken();
  return makeParserResult(
           new (Context) ImplicitlyUnwrappedOptionalTypeRepr(
                           base, exclamationLoc));
}

//===--------------------------------------------------------------------===//
// Speculative type list parsing
//===--------------------------------------------------------------------===//

static bool isGenericTypeDisambiguatingToken(Parser &P,
                                             Token &tok) {
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
  case tok::code_complete:
  case tok::exclaim_postfix:
  case tok::question_postfix:
    return true;
  
  case tok::oper_binary_unspaced:
  case tok::oper_binary_spaced:
  case tok::oper_postfix:
    // These might be '?' or '!' type modifiers.
    return P.isOptionalToken(tok) || P.isImplicitlyUnwrappedOptionalToken(tok);
      
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
    return isGenericTypeDisambiguatingToken(*this, Tok);

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
  case tok::kw_Self:
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
  case tok::at_sign: {
    consumeToken();
    if (!canParseTypeAttribute())
      return false;
    return canParseType();
  }
  case tok::l_square:
    consumeToken();
    if (!canParseType())
      return false;
    if (consumeIf(tok::colon)) {
      if (!canParseType())
        return false;
    }
    if (!consumeIf(tok::r_square))
      return false;
    break;


  default:
    return false;
  }
  
  // '.Type', '.Protocol', '?', and '!' still leave us with type-simple.
  while (true) {
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        (peekToken().isContextualKeyword("Type")
         || peekToken().isContextualKeyword("Protocol"))) {
      consumeToken();
      consumeToken(tok::identifier);
      continue;
    }
    if (isOptionalToken(Tok)) {
      consumeOptionalToken();
      continue;
    }
    if (isImplicitlyUnwrappedOptionalToken(Tok)) {
      consumeImplicitlyUnwrappedOptionalToken();
      continue;
    }
    break;
  }
  
  // Handle type-function if we have an arrow.
  if (consumeIf(tok::arrow)) {
    if (!canParseType())
      return false;
    return true;
  }

  return true;
}

bool Parser::canParseTypeIdentifier() {
  if (Tok.isNot(tok::identifier) && Tok.isNot(tok::kw_Self))
    return false;
  
  while (true) {
    switch (Tok.getKind()) {
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
    // unless <anything> is 'Type'.
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        !peekToken().isContextualKeyword("Type") &&
        !peekToken().isContextualKeyword("Protocol")) {
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

bool Parser::canParseAttributes() {
  while (consumeIf(tok::at_sign)) {
    if (!consumeIf(tok::identifier)) return false;
    
    if (consumeIf(tok::equal)) {
      if (Tok.isNot(tok::identifier) &&
          Tok.isNot(tok::integer_literal) &&
          Tok.isNot(tok::floating_literal))
        return false;
      consumeToken();
    } else if (Tok.is(tok::l_paren)) {
      // Attributes like cc(x,y,z)
      skipSingle();
    }
    
    consumeIf(tok::comma);
  }
  return true;
}

bool Parser::canParseTypeTupleBody() {
  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::r_brace) &&
      Tok.isNotEllipsis() && !isStartOfDecl()) {
    do {
      // The contextual inout marker is part of argument lists.
      consumeIf(tok::kw_inout);

      // If the tuple element starts with "ident :", then it is followed
      // by a type annotation.
      if (Tok.is(tok::identifier) && peekToken().is(tok::colon)) {
        consumeToken(tok::identifier);
        consumeToken(tok::colon);

        // Parse attributes then a type.
        if (!canParseAttributes() ||
            !canParseType())
          return false;

        // Parse default values. This aren't actually allowed, but we recover
        // better if we skip over them.
        if (consumeIf(tok::equal)) {
          while (Tok.isNot(tok::eof) && Tok.isNot(tok::r_paren) &&
                 Tok.isNot(tok::r_brace) && Tok.isNotEllipsis() &&
                 Tok.isNot(tok::comma) &&
                 !isStartOfDecl()) {
            skipSingle();
          }
        }

        continue;
      }
      
      // Otherwise, this has to be a type.

      // Parse attributes.
      if (!canParseAttributes())
        return false;
 
      if (!canParseType())
        return false;
    } while (consumeIf(tok::comma));
  }
  
  if (Tok.isEllipsis())
    consumeToken();

  return consumeIf(tok::r_paren);
}



