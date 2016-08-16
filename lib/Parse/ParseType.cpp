//===--- ParseType.cpp - Swift Language Parser for Types ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
///     'Any'
///     type-simple '.Type'
///     type-simple '.Protocol'
///     type-simple '?'
///     type-simple '!'
///     type-collection
ParserResult<TypeRepr> Parser::parseTypeSimple(Diag<> MessageID,
                                               bool HandleCodeCompletion) {
  ParserResult<TypeRepr> ty;
  // If this is an "inout" marker for an identifier type, consume the inout.
  SourceLoc InOutLoc;
  consumeIf(tok::kw_inout, InOutLoc);

  switch (Tok.getKind()) {
  case tok::kw_Self:
    ty = parseTypeIdentifier();
    break;
  case tok::identifier:
  case tok::kw_protocol:
  case tok::kw_Any:
    ty = parseTypeIdentifierOrTypeComposition();
    break;
  case tok::l_paren:
    ty = parseTypeTupleBody();
    break;
  case tok::code_complete:
    if (!HandleCodeCompletion)
      break;
    if (CodeCompletion)
      CodeCompletion->completeTypeSimpleBeginning();
    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionResult<TypeRepr>();
  case tok::kw_super:
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

  // If we parsed an inout modifier, prepend it.
  if (InOutLoc.isValid())
    ty = makeParserResult(new (Context) InOutTypeRepr(ty.get(),
                                                      InOutLoc));

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
ParserResult<TypeRepr> Parser::parseType(Diag<> MessageID,
                                         bool HandleCodeCompletion) {
  // Parse attributes.
  TypeAttributes attrs;
  parseTypeAttributeList(attrs);

  // Parse Generic Parameters. Generic Parameters are visible in the function
  // body.
  GenericParamList *generics = nullptr;
  if (isInSILMode()) {
    generics = maybeParseGenericParams().getPtrOrNull();
  }

  ParserResult<TypeRepr> ty = parseTypeSimple(MessageID, HandleCodeCompletion);
  if (ty.hasCodeCompletion())
    return makeParserCodeCompletionResult<TypeRepr>();

  if (ty.isNull())
    return nullptr;

  // Parse a throws specifier.  'throw' is probably a typo for 'throws',
  // but in local contexts we could just be at the end of a statement,
  // so we need to check for the arrow.
  ParserPosition beforeThrowsPos;
  SourceLoc throwsLoc;
  bool rethrows = false;
  if (Tok.isAny(tok::kw_throws, tok::kw_rethrows) ||
      (Tok.is(tok::kw_throw) && peekToken().is(tok::arrow))) {
    if (Tok.is(tok::kw_throw)) {
      diagnose(Tok.getLoc(), diag::throw_in_function_type)
        .fixItReplace(Tok.getLoc(), "throws");
    }

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
      RAngleLoc = skipUntilGreaterInTypeList();
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
    RAngleLoc = skipUntilGreaterInTypeList();
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
ParserResult<TypeRepr> Parser::parseTypeIdentifier() {
  if (Tok.isNot(tok::identifier) && Tok.isNot(tok::kw_Self)) {
    // is this the 'Any' type
    if (Tok.is(tok::kw_Any)) {
      return parseAnyType();
    } else if (Tok.is(tok::code_complete)) {
      if (CodeCompletion)
        CodeCompletion->completeTypeSimpleBeginning();
      // Eat the code completion token because we handled it.
      consumeToken(tok::code_complete);
      return makeParserCodeCompletionResult<IdentTypeRepr>();
    }

    diagnose(Tok, diag::expected_identifier_for_type);

    // If there is a keyword at the start of a new line, we won't want to
    // skip it as a recovery but rather keep it.
    if (Tok.isKeyword() && !Tok.isAtStartOfLine())
      consumeToken();

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
      // FIXME: specialize diagnostic for 'Type': type cannot start with
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
      if (isa<TypeDecl>(Entry))
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

/// parseTypeIdentifierOrTypeComposition
/// - Identifiers and compositions both start with the same identifier
///   token, parse it and continue constructing a composition if the
///   next token is '&'
///
///   type-composition:
///     type-identifier ('&' type-identifier)*
///     'protocol' '<' type-composition-list-deprecated? '>'
///
///   type-composition-list-deprecated:
///     type-identifier (',' type-identifier)*
ParserResult<TypeRepr> Parser::parseTypeIdentifierOrTypeComposition() {
  
  // Handle deprecated case
  if (Tok.getKind() == tok::kw_protocol && startsWithLess(peekToken())) {
    SourceLoc ProtocolLoc = consumeToken(tok::kw_protocol);
    SourceLoc LAngleLoc = consumeStartingLess();
    
    // Check for empty protocol composition.
    if (startsWithGreater(Tok)) {
      SourceLoc RAngleLoc = consumeStartingGreater();
      auto AnyRange = SourceRange(ProtocolLoc, RAngleLoc);
      
      // Warn that 'protocol<>' is deprecated and offer to
      // replace with the 'Any' keyword
      diagnose(LAngleLoc, diag::deprecated_any_composition)
        .fixItReplace(AnyRange, "Any");
      
      return makeParserResult(
        ProtocolCompositionTypeRepr::createEmptyComposition(Context, ProtocolLoc));
    }
    
    // Parse the type-composition-list.
    ParserStatus Status;
    SmallVector<IdentTypeRepr *, 4> Protocols;
    do {
      // Parse the type-identifier.
      ParserResult<TypeRepr> Protocol = parseTypeIdentifier();
      Status |= Protocol;
      if (auto *ident = dyn_cast_or_null<IdentTypeRepr>(Protocol.getPtrOrNull()))
        Protocols.push_back(ident);
    } while (consumeIf(tok::comma));

    // Check for the terminating '>'.
    SourceLoc RAngleLoc = PreviousLoc;
    if (startsWithGreater(Tok)) {
      RAngleLoc = consumeStartingGreater();
    } else {
      if (Status.isSuccess()) {
        diagnose(Tok, diag::expected_rangle_protocol);
        diagnose(LAngleLoc, diag::opening_angle);
        Status.setIsParseError();
      }
      
      // Skip until we hit the '>'.
      RAngleLoc = skipUntilGreaterInTypeList(/*protocolComposition=*/true);
    }
    
    auto composition = ProtocolCompositionTypeRepr::create(
      Context, Protocols, ProtocolLoc, {LAngleLoc, RAngleLoc});

    if (Status.isSuccess()) {
      // Only if we have complete protocol<...> construct, diagnose deprecated.

      auto extractText = [&](IdentTypeRepr *Ty) -> StringRef {
        auto SourceRange = Ty->getSourceRange();
        return SourceMgr.extractText(
          Lexer::getCharSourceRangeFromSourceRange(SourceMgr, SourceRange));
      };
      SmallString<32> replacement;
      auto Begin = Protocols.begin();
      replacement += extractText(*Begin);
      while(++Begin != Protocols.end()) {
        replacement += " & ";
        replacement += extractText(*Begin);
      }

      // Replace 'protocol<T1, T2>' with 'T1 & T2'
      diagnose(ProtocolLoc,
        Protocols.size() > 1 ? diag::deprecated_protocol_composition
                             : diag::deprecated_protocol_composition_single)
        .highlight(composition->getSourceRange())
        .fixItReplace(composition->getSourceRange(), replacement);
    }

    return makeParserResult(Status, composition);
  }
  
  SourceLoc FirstTypeLoc = Tok.getLoc();
  
  // Parse the first type
  ParserResult<TypeRepr> FirstType = parseTypeIdentifier();
  if (!Tok.isContextualPunctuator("&"))
    return FirstType;
  
  SmallVector<IdentTypeRepr *, 4> Protocols;
  ParserStatus Status;

  // If it is not 'Any', add it to the protocol list
  if (auto *ident = dyn_cast_or_null<IdentTypeRepr>(FirstType.getPtrOrNull()))
    Protocols.push_back(ident);
  Status |= FirstType;
  auto FirstAmpersandLoc = Tok.getLoc();
  
  while (Tok.isContextualPunctuator("&")) {
    consumeToken(); // consume '&'
    ParserResult<TypeRepr> Protocol = parseTypeIdentifier();
    Status |= Protocol;
    if (auto *ident = dyn_cast_or_null<IdentTypeRepr>(Protocol.getPtrOrNull()))
      Protocols.push_back(ident);
  };
  
  return makeParserResult(Status, ProtocolCompositionTypeRepr::create(
    Context, Protocols, FirstTypeLoc, {FirstAmpersandLoc, PreviousLoc}));
}

ParserResult<ProtocolCompositionTypeRepr> Parser::parseAnyType() {
  return makeParserResult(ProtocolCompositionTypeRepr
    ::createEmptyComposition(Context, consumeToken(tok::kw_Any)));
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
  unsigned EllipsisIdx;
  SmallVector<TypeRepr *, 8> ElementsR;

  // We keep track of the labels separately, and apply them at the end.
  SmallVector<std::tuple<Identifier, SourceLoc, Identifier, SourceLoc>, 4>
    Labels;

  ParserStatus Status = parseList(tok::r_paren, LPLoc, RPLoc,
                                  tok::comma, /*OptionalSep=*/false,
                                  /*AllowSepAfterLast=*/false,
                                  diag::expected_rparen_tuple_type_list,
                                  [&] () -> ParserStatus {
    // If this is a deprecated use of the inout marker in an argument list,
    // consume the inout.
    SourceLoc InOutLoc;
    bool hasAnyInOut = false;
    bool hasValidInOut = false;
    if (consumeIf(tok::kw_inout, InOutLoc)) {
      hasAnyInOut = true;
      hasValidInOut = false;
    }
    // If the tuple element starts with a potential argument label followed by a
    // ':' or another potential argument label, then the identifier is an
    // element tag, and it is followed by a type annotation.
    if (Tok.canBeArgumentLabel() &&
        (peekToken().is(tok::colon) || peekToken().canBeArgumentLabel())) {
      // Consume the name
      Identifier name;
      if (!Tok.is(tok::kw__))
        name = Context.getIdentifier(Tok.getText());
      SourceLoc nameLoc = consumeToken();

      // If there is a second name, consume it as well.
      Identifier secondName;
      SourceLoc secondNameLoc;
      if (Tok.canBeArgumentLabel()) {
        if (!Tok.is(tok::kw__))
          secondName = Context.getIdentifier(Tok.getText());
        secondNameLoc = consumeToken();
      }

      // Consume the ':'.
      if (!consumeIf(tok::colon))
        diagnose(Tok, diag::expected_parameter_colon);

      SourceLoc postColonLoc = Tok.getLoc();

      // Consume 'inout' if present.
      if (!hasAnyInOut && consumeIf(tok::kw_inout, InOutLoc)) {
        hasValidInOut = true;
      }

      SourceLoc extraneousInOutLoc;
      while (consumeIf(tok::kw_inout, extraneousInOutLoc)) {
        diagnose(Tok, diag::parameter_inout_var_let_repeated)
          .fixItRemove(extraneousInOutLoc);
      }

      // Parse the type annotation.
      ParserResult<TypeRepr> type = parseType(diag::expected_type);
      if (type.hasCodeCompletion())
        return makeParserCodeCompletionStatus();
      if (type.isNull())
        return makeParserError();

      if (!hasValidInOut && hasAnyInOut) {
        diagnose(Tok.getLoc(), diag::inout_as_attr_disallowed)
          .fixItRemove(InOutLoc)
          .fixItInsert(postColonLoc, "inout ");
      }

      // If an 'inout' marker was specified, build the type.  Note that we bury
      // the inout locator within the named locator.  This is weird but required
      // by sema apparently.
      if (InOutLoc.isValid())
        type = makeParserResult(new (Context) InOutTypeRepr(type.get(),
                                                            InOutLoc));

      // Record the label. We will look at these at the end.
      if (Labels.empty()) {
        Labels.assign(ElementsR.size(),
                      std::make_tuple(Identifier(), SourceLoc(),
                                      Identifier(), SourceLoc()));
      }
      Labels.push_back(std::make_tuple(name, nameLoc,
                                       secondName, secondNameLoc));

      ElementsR.push_back(type.get());
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

      if (!Labels.empty()) {
        Labels.push_back(std::make_tuple(Identifier(), SourceLoc(),
                                         Identifier(), SourceLoc()));
      }

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
      if (EllipsisLoc.isValid()) {
        diagnose(Tok, diag::multiple_ellipsis_in_tuple)
          .highlight(EllipsisLoc)
          .fixItRemove(Tok.getLoc());
        (void)consumeToken();
      } else {
        EllipsisLoc = consumeToken();
        EllipsisIdx = ElementsR.size() - 1;
      }
    }
    return makeParserSuccess();
  });

  if (EllipsisLoc.isValid() && ElementsR.empty()) {
    EllipsisLoc = SourceLoc();
  }

  if (EllipsisLoc.isInvalid())
    EllipsisIdx = ElementsR.size();

  // If there were any labels, figure out which labels should go into the type
  // representation.
  if (!Labels.empty()) {
    assert(Labels.size() == ElementsR.size());
    bool isFunctionType = Tok.isAny(tok::arrow, tok::kw_throws,
                                    tok::kw_rethrows);
    for (unsigned i : indices(ElementsR)) {
      auto &currentLabel = Labels[i];

      Identifier firstName = std::get<0>(currentLabel);
      SourceLoc firstNameLoc = std::get<1>(currentLabel);
      Identifier secondName = std::get<2>(currentLabel);
      SourceLoc secondNameLoc = std::get<3>(currentLabel);

      // True tuples have labels.
      if (!isFunctionType) {
        // If there were two names, complain.
        if (firstNameLoc.isValid() && secondNameLoc.isValid()) {
          auto diag = diagnose(firstNameLoc, diag::tuple_type_multiple_labels);
          if (firstName.empty()) {
            diag.fixItRemoveChars(firstNameLoc, ElementsR[i]->getStartLoc());
          } else {
            diag.fixItRemove(
              SourceRange(Lexer::getLocForEndOfToken(SourceMgr,firstNameLoc),
                          secondNameLoc));
          }
        }

        // Form the named type representation.
        ElementsR[i] = new (Context) NamedTypeRepr(firstName, ElementsR[i],
                                                   firstNameLoc);
        continue;
      }

      // If there was a first name, complain; arguments in function types are
      // always unlabeled.
      if (firstNameLoc.isValid() && !firstName.empty()) {
        auto diag = diagnose(firstNameLoc, diag::function_type_argument_label,
                             firstName);
        if (secondNameLoc.isInvalid())
          diag.fixItInsert(firstNameLoc, "_ ");
        else if (secondName.empty())
          diag.fixItRemoveChars(firstNameLoc, ElementsR[i]->getStartLoc());
        else
          diag.fixItReplace(SourceRange(firstNameLoc), "_");
      }

      if (firstNameLoc.isValid() || secondNameLoc.isValid()) {
        // Form the named parameter type representation.
        ElementsR[i] = new (Context) NamedTypeRepr(secondName, ElementsR[i],
                                                   secondNameLoc, firstNameLoc);
      }
    }
  }

  return makeParserResult(Status,
                          TupleTypeRepr::create(Context, ElementsR,
                                                SourceRange(LPLoc, RPLoc),
                                                EllipsisLoc, EllipsisIdx));
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
ParserResult<TypeRepr> Parser::parseTypeArray(TypeRepr *Base) {
  assert(Tok.isFollowingLSquare());
  Parser::StructureMarkerRAII ParsingArrayBound(*this, Tok);
  SourceLoc lsquareLoc = consumeToken();
  ArrayTypeRepr *ATR = nullptr;
  
  // Handle a postfix [] production, a common typo for a C-like array.

  // If we have something that might be an array size expression, parse it as
  // such, for better error recovery.
  if (Tok.isNot(tok::r_square)) {
    auto sizeEx = parseExprBasic(diag::expected_expr);
    if (sizeEx.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
    if (sizeEx.isNull())
      return makeParserErrorResult(Base);
  }
  
  SourceLoc rsquareLoc;
  if (parseMatchingToken(tok::r_square, rsquareLoc,
                         diag::expected_rbracket_array_type, lsquareLoc))
    return makeParserErrorResult(Base);

  // If we parsed something valid, diagnose it with a fixit to rewrite it to
  // Swift syntax.
  diagnose(lsquareLoc, diag::new_array_syntax)
    .fixItInsert(Base->getStartLoc(), "[")
    .fixItRemove(lsquareLoc);
  
  // Build a normal array slice type for recovery.
  ATR = new (Context) ArrayTypeRepr(Base,
                              SourceRange(Base->getStartLoc(), rsquareLoc));
  return makeParserResult(ATR);
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

  if (firstTy.hasCodeCompletion() || secondTy.hasCodeCompletion())
    return makeParserCodeCompletionStatus();

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
                                                      brackets));
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

//===----------------------------------------------------------------------===//
// Speculative type list parsing
//===----------------------------------------------------------------------===//

static bool isGenericTypeDisambiguatingToken(Parser &P) {
  auto &tok = P.Tok;
  switch (tok.getKind()) {
  default:
    return false;
  case tok::r_paren:
  case tok::r_square:
  case tok::l_brace:
  case tok::r_brace:
  case tok::period:
  case tok::period_prefix:
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
    return isGenericTypeDisambiguatingToken(*this);

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
  case tok::kw_Any:
      if (!canParseTypeIdentifier())
        return false;
      break;
  case tok::kw_protocol: // Deprecated composition syntax
  case tok::identifier:
    if (!canParseTypeIdentifierOrTypeComposition())
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
  
  // Handle type-function if we have an arrow or 'throws'/'rethrows' modifier.
  if (Tok.isAny(tok::kw_throws, tok::kw_rethrows)) {
    consumeToken();
    // "throws" or "rethrows" isn't a valid type without being followed by
    // a return.
    if (!Tok.is(tok::arrow))
      return false;
  }
  
  if (consumeIf(tok::arrow)) {
    if (!canParseType())
      return false;
    return true;
  }

  return true;
}

bool Parser::canParseTypeIdentifierOrTypeComposition() {
  if (Tok.is(tok::kw_protocol))
    return canParseOldStyleProtocolComposition();
  
  while (true) {
    if (!canParseTypeIdentifier())
      return false;
    
    if (Tok.isContextualPunctuator("&")) {
      consumeToken();
      continue;
    } else {
      return true;
    }
  }
}

bool Parser::canParseTypeIdentifier() {
  while (true) {
    if (!Tok.isAny(tok::identifier, tok::kw_Self, tok::kw_Any))
      return false;
    consumeToken();
    
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


bool Parser::canParseOldStyleProtocolComposition() {
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
      if (Tok.canBeArgumentLabel() && 
          (peekToken().is(tok::colon) || peekToken().canBeArgumentLabel())) {
        consumeToken();
        if (Tok.canBeArgumentLabel()) {
          consumeToken();
          if (!Tok.is(tok::colon)) return false;
        }
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

      if (Tok.isEllipsis())
        consumeToken();

    } while (consumeIf(tok::comma));
  }
  
  return consumeIf(tok::r_paren);
}



