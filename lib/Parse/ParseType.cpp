//===--- ParseType.cpp - Swift Language Parser for Types ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Type Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/SyntaxParsingContext.h"
#include "swift/Parse/ParsedSyntaxBuilders.h"
#include "swift/Parse/ParsedSyntaxRecorder.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace swift::syntax;

TypeRepr *Parser::applyAttributeToType(TypeRepr *ty,
                                       const TypeAttributes &attrs,
                                       ParamDecl::Specifier specifier,
                                       SourceLoc specifierLoc) {
  // Apply those attributes that do apply.
  if (!attrs.empty()) {
    ty = new (Context) AttributedTypeRepr(attrs, ty);
  }

  // Apply 'inout' or '__shared' or '__owned'
  if (specifierLoc.isValid()) {
    switch (specifier) {
    case ParamDecl::Specifier::Owned:
      ty = new (Context) OwnedTypeRepr(ty, specifierLoc);
      break;
    case ParamDecl::Specifier::InOut:
      ty = new (Context) InOutTypeRepr(ty, specifierLoc);
      break;
    case ParamDecl::Specifier::Shared:
      ty = new (Context) SharedTypeRepr(ty, specifierLoc);
      break;
    case ParamDecl::Specifier::Default:
      break;
    }
  }

  return ty;
}

/// Apply specifier and attributes to the parsed type.
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::applyAttributeToTypeSyntax(ParsedSyntaxResult<ParsedTypeSyntax> &&ty,
                                   Optional<ParsedTokenSyntax> specifier,
                                   Optional<ParsedAttributeListSyntax> attrs) {
  if (!attrs && !specifier)
    return std::move(ty);

  if (ty.isNull()) {
    SmallVector<ParsedSyntax, 2> junk;
    if (specifier)
      junk.emplace_back(std::move(*specifier));
    if (attrs)
      junk.emplace_back(std::move(*attrs));
    return makeParsedResult(
        ParsedSyntaxRecorder::makeUnknownType(junk, *SyntaxContext),
        ty.getStatus());
  }

  return makeParsedResult(
      ParsedSyntaxRecorder::makeAttributedType(
          std::move(specifier), std::move(attrs), ty.get(), *SyntaxContext),
      ty.getStatus());
}

/// Parse layout constraint for 'where' clause in '@_specialize' attribute
/// and in SIL.
///
///   layout-constraint:
///     identifier
///     identifier '(' integer-literal ')'
///     identifier '(' integer-literal ',' integer-literal ')'
ParsedSyntaxResult<ParsedLayoutConstraintSyntax>
Parser::parseLayoutConstraintSyntax() {
  assert(Tok.is(tok::identifier));
  ParsedLayoutConstraintSyntaxBuilder builder(*SyntaxContext);

  builder.useName(consumeTokenSyntax(tok::identifier));

  if (!Tok.isFollowingLParen())
    return makeParsedResult(builder.build());

  auto lParenLoc = Tok.getLoc();
  builder.useLeftParen(consumeTokenSyntax(tok::l_paren));

  auto parseTrivialConstraintBody = [&]() -> bool {
    int value;

    if (!Tok.is(tok::integer_literal) ||
        Tok.getText().getAsInteger(10, value) || value < 0) {
      diagnose(Tok, diag::layout_size_should_be_positive);
      return true;
    }
    builder.useSize(consumeTokenSyntax(tok::integer_literal));

    if (Tok.is(tok::comma)) {
      builder.useComma(consumeTokenSyntax(tok::comma));

      if (!Tok.is(tok::integer_literal) ||
          Tok.getText().getAsInteger(10, value) || value < 0) {
        diagnose(Tok, diag::layout_alignment_should_be_positive);
        return true;
      }
      builder.useAlignment(consumeTokenSyntax(tok::integer_literal));
    }
    return false;
  };

  if (parseTrivialConstraintBody()) {
    ignoreUntil(tok::r_paren);
    if (Tok.is(tok::r_paren))
      builder.useRightParen(consumeTokenSyntax(tok::r_paren));
  } else {
    SourceLoc rParenLoc;
    auto rParen = parseMatchingTokenSyntax(tok::r_paren, rParenLoc,
                             diag::expected_rparen_layout_constraint,
                             lParenLoc);
    if (rParen)
      builder.useRightParen(std::move(*rParen));
  }
  return makeParsedResult(builder.build());
}

/// parseTypeSimple
///   type-simple:
///     type-identifier
///     type-tuple
///     type-composition-deprecated
///     'Any'
///     type-simple '.Type'
///     type-simple '.Protocol'
///     type-simple '?'
///     type-simple '!'
///     type-collection
///     type-array
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeSimple(Diag<> MessageID, bool HandleCodeCompletion) {
  if (Tok.is(tok::kw_inout) ||
      (Tok.is(tok::identifier) && (Tok.getRawText().equals("__shared") ||
                                   Tok.getRawText().equals("__owned")))) {
    // Type specifier should already be parsed before here. This only happens
    // for construct like 'P1 & inout P2'.
    diagnose(Tok.getLoc(), diag::attr_only_on_parameters, Tok.getRawText());
    ignoreToken();
  }

  auto TypeLoc = leadingTriviaLoc();

  ParsedSyntaxResult<ParsedTypeSyntax> Result;
  switch (Tok.getKind()) {
  case tok::kw_Self:
  case tok::kw_Any:
  case tok::identifier:
    Result = parseTypeIdentifier();
    break;
  case tok::l_paren:
    Result = parseTypeTupleBody();
    break;
  case tok::code_complete: {
    if (!HandleCodeCompletion)
      break;
    ParsedTypeSyntax ty = ParsedSyntaxRecorder::makeCodeCompletionType(
        None, None, consumeTokenSyntax(), *SyntaxContext);
    return makeParsedCodeCompletion(std::move(ty));
  }
  case tok::l_square:
    Result = parseTypeCollection();
    break;
  case tok::kw_protocol:
    if (startsWithLess(peekToken())) {
      Result = parseOldStyleProtocolComposition();
      break;
    }
    LLVM_FALLTHROUGH;
  default:
    {
      auto diag = diagnose(Tok, MessageID);
      // If the next token is closing or separating, the type was likely forgotten
      if (Tok.isAny(tok::r_paren, tok::r_brace, tok::r_square, tok::arrow,
                    tok::equal, tok::comma, tok::semi))
        diag.fixItInsert(getEndOfPreviousLoc(), " <#type#>");
    }

    if (Tok.isKeyword() && !Tok.isAtStartOfLine()) {
      auto token = consumeTokenSyntax();
      ParsedTypeSyntax ty = ParsedSyntaxRecorder::makeUnknownType(
          {&token, 1}, *SyntaxContext);
      // Return success result because we recovered.
      return makeParsedResult(std::move(ty));
    }

    checkForInputIncomplete();
    return makeParsedError<ParsedTypeSyntax>();
  }

  // '.Type', '.Protocol', '?', '!', and '[]' still leave us with type-simple.
  while (Result.isSuccess()) {
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        (peekToken().isContextualKeyword("Type") ||
         peekToken().isContextualKeyword("Protocol"))) {
      Result = parseMetatypeType(Result.get());
      continue;
    }

    if (!Tok.isAtStartOfLine()) {
      if (isOptionalToken(Tok)) {
        Result = parseOptionalType(Result.get());
        continue;
      }
      if (isImplicitlyUnwrappedOptionalToken(Tok)) {
        Result = parseImplicitlyUnwrappedOptionalType(Result.get());
        continue;
      }
      // Parse legacy array types for migration.
      if (Tok.is(tok::l_square)) {
        Result = parseTypeArray(Result.get(), TypeLoc);
        continue;
      }
    }
    break;
  }

  return Result;
}

ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseSILBoxTypeSyntax(
    Optional<ParsedGenericParameterClauseListSyntax> generics) {
  ParsedSILBoxTypeSyntaxBuilder builder(*SyntaxContext);
  ParserStatus status;

  if (generics)
    builder.useGenericParameterClauses(std::move(*generics));

  // Parse '{'.
  builder.useLeftBrace(consumeTokenSyntax(tok::l_brace));

  // Parse comma separated field list.
  if (!Tok.is(tok::r_brace)) {
    bool hasNext = true;
    do {
      ParsedSILBoxTypeFieldSyntaxBuilder fieldBuilder(*SyntaxContext);

      // Parse 'let' or 'var'.
      if (!Tok.isAny(tok::kw_var, tok::kw_let)) {
        diagnose(Tok, diag::sil_box_expected_var_or_let);
        break;
      }
      fieldBuilder.useSpecifier(consumeTokenSyntax());

      // Parse the type.
      auto ty = parseTypeSyntax();
      status |= ty.getStatus();
      if (!ty.isNull())
        fieldBuilder.useType(ty.get());
      else
        fieldBuilder.useType(
            ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext));

      // Parse ','.
      hasNext = (status.isSuccess() && Tok.is(tok::comma));
      if (hasNext)
        fieldBuilder.useTrailingComma(consumeTokenSyntax());

      builder.addFieldsMember(fieldBuilder.build());
    } while (hasNext);
  }

  // Parse '}'.
  if (!Tok.is(tok::r_brace)) {
    diagnose(Tok, diag::sil_box_expected_r_brace);
    return makeParsedError(builder.build());
  }
  builder.useRightBrace(consumeTokenSyntax(tok::r_brace));

  // Parse the generic argument.
  if (startsWithLess(Tok)) {
    auto genericArgs = parseGenericArgumentClauseSyntax();
    status |= genericArgs.getStatus();
    if (!genericArgs.isNull())
      builder.useGenericArgumentClause(genericArgs.get());
  }

  return makeParsedResult(builder.build());
}

/// parseType
///   type:
///     attribute-list type-composition
///     attribute-list type-function
///     attribute-list sil-generic-function-type
///     sil-box-type
///
///   type-function:
///     '(' tuple-type-element-list ')' 'throws'? '->' type
///
///   sil-generic-function-type:
///     generic-parameter-clause-list type-function
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeSyntax(Diag<> MessageID, bool HandleCodeCompletion,
                        bool IsSILFuncDecl) {
  ParserStatus status;

  // Parse attributes.
  Optional<ParsedTokenSyntax> specifier;
  Optional<ParsedAttributeListSyntax> attrs;
  if (Tok.isAny(tok::at_sign, tok::kw_inout) ||
      (Tok.is(tok::identifier) && (Tok.getRawText().equals("__shared") ||
                                   Tok.getRawText().equals("__owned"))))
    status |= parseTypeAttributeListSyntax(specifier, attrs);

  // Parse generic parameters in SIL mode.
  Optional<ParsedGenericParameterClauseListSyntax> genericParams;
  SourceLoc genericParamsLoc = Tok.getLoc();
  if (isInSILMode())
    (void)parseSILGenericParamsSyntax(genericParams);

  // In SIL mode, parse box types { ... }.
  if (isInSILMode() && Tok.is(tok::l_brace)) {
    auto ty = parseSILBoxTypeSyntax(std::move(genericParams));
    return applyAttributeToTypeSyntax(std::move(ty), std::move(specifier),
                                      std::move(attrs));
  }

  auto startLoc = Tok.getLoc();
  // Parse the type.
  auto ty = parseTypeSimpleOrComposition(MessageID, HandleCodeCompletion);
  status |= ty.getStatus();
  auto endLoc = PreviousLoc;

  // Don't consume 'throws', if the next token is not '->', so we can emit a
  // more useful diagnostic when parsing a function decl.
  bool canBeFunctionTy =
      Tok.is(tok::arrow) ||
      (Tok.isAny(tok::kw_throws, tok::kw_rethrows, tok::kw_throw) &&
       peekToken().is(tok::arrow));

  // If the first type has an error, or this is not a function type, return the
  // first result.
  if (ty.isNull() || !canBeFunctionTy) {
    // Diagnose generic parameter for non-function types.
    if (!genericParams && !specifier && !attrs)
      return ty;

    if (genericParams) {
      SmallVector<ParsedSyntax, 2> junk;
      diagnose(genericParamsLoc, diag::generic_non_function);
      junk.push_back(std::move(*genericParams));
      if (!ty.isNull())
        junk.emplace_back(ty.get());
      ty = makeParsedResult(
          ParsedSyntaxRecorder::makeUnknownType(junk, *SyntaxContext),
          status);
    }

    return applyAttributeToTypeSyntax(std::move(ty), std::move(specifier),
                                      std::move(attrs));
  }

  // Parse a throws specifier.
  Optional<ParsedTokenSyntax> throws;
  if (Tok.isAny(tok::kw_throws, tok::kw_rethrows, tok::kw_throw) &&
      peekToken().is(tok::arrow)) {
    if (Tok.isNot(tok::kw_throws)) {
      // 'rethrows' is only allowed on function declarations for now.
      // 'throw' is probably a typo for 'throws'.
      Diag<> DiagID = Tok.is(tok::kw_rethrows) ? diag::rethrowing_function_type
                                               : diag::throw_in_function_type;
      diagnose(Tok.getLoc(), DiagID).fixItReplace(Tok.getLoc(), "throws");
      ignoreToken();
    } else {
      throws = consumeTokenSyntax();
    }
  }

  auto arrowLoc = Tok.getLoc();
  auto arrow = consumeTokenSyntax(tok::arrow);
  if (Tok.is(tok::kw_throws)) {
    Diag<> DiagID = diag::throws_in_wrong_position;
    diagnose(Tok.getLoc(), DiagID)
        .fixItInsert(arrowLoc, "throws ")
        .fixItRemove(Tok.getLoc());
    ignoreToken();
  }

  auto input = ty.get();
  auto result = parseTypeSyntax(diag::expected_type_function_result);
  status |= result.getStatus();

  ParsedFunctionTypeSyntaxBuilder builder(*SyntaxContext);
  if (auto tuple = input.getAs<ParsedTupleTypeSyntax>()) {
    assert(tuple->getRaw().isDeferredLayout());
    builder.useLeftParen(tuple->getDeferredLeftParen());
    builder.useArguments(tuple->getDeferredElements());
    builder.useRightParen(tuple->getDeferredRightParen());
  } else {
    builder.addArgumentsMember(ParsedSyntaxRecorder::makeTupleTypeElement(
        std::move(input), /*TrailingComma=*/None, *SyntaxContext));

    // Diagnose only if the result type is successfully parsed, to reduce the
    // noisy diagnostics.
    if (result.isSuccess()) {
      auto charRange = Lexer::getCharSourceRangeFromSourceRange(
          SourceMgr, {startLoc, endLoc});
      auto diag = diagnose(startLoc, diag::function_type_no_parens);
      if (SourceMgr.extractText(charRange) == "Void") {
        diag.fixItReplace(startLoc, "()");
      } else {
        diag.highlight(SourceRange(startLoc, endLoc));
        diag.fixItInsert(startLoc, "(");
        diag.fixItInsertAfter(endLoc, ")");
      }
    }
  }

  if (throws)
    builder.useThrowsOrRethrowsKeyword(std::move(*throws));
  builder.useArrow(std::move(arrow));
  if (!result.isNull())
    builder.useReturnType(result.get());
  else
    builder.useReturnType(
        ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext));

  ParsedFunctionTypeSyntax funcTy = builder.build();

  // Apply generic parameters if exists in SIL mode.
  if (genericParams) {
    auto silTy = ParsedSyntaxRecorder::makeSILFunctionType(
        std::move(*genericParams), std::move(funcTy), *SyntaxContext);
    return applyAttributeToTypeSyntax(
        makeParsedResult(std::move(silTy), status), std::move(specifier),
        std::move(attrs));
  }

  return applyAttributeToTypeSyntax(makeParsedResult(std::move(funcTy), status),
                                    std::move(specifier), std::move(attrs));
}

ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeSyntax() {
  return parseTypeSyntax(diag::expected_type);
}

ParserResult<TypeRepr> Parser::parseType(Diag<> MessageID,
                                         bool HandleCodeCompletion,
                                         bool IsSILFuncDecl) {
  auto leadingLoc = leadingTriviaLoc();
  auto result = parseTypeSyntax(MessageID, HandleCodeCompletion, IsSILFuncDecl);
  auto status = result.getStatus();
  if (result.isNull())
    return status;

  SyntaxContext->addSyntax(result.get());
  auto syntax = SyntaxContext->topNode<TypeSyntax>();
  auto tyR = Generator.generate(syntax, leadingLoc, IsSILFuncDecl);
  if (!tyR)
    status.setIsParseError();
  return makeParserResult(status, tyR);
}

ParserResult<TypeRepr> Parser::parseType() {
  return parseType(diag::expected_type);
}


ParserResult<TypeRepr> Parser::parseDeclResultType(Diag<> MessageID) {
  if (Tok.is(tok::code_complete)) {
    if (CodeCompletion)
      CodeCompletion->completeTypeDeclResultBeginning();
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionStatus();
  }

  auto result = parseType(MessageID);

  if (!result.isParseError() && Tok.is(tok::r_square)) {
    auto diag = diagnose(Tok, diag::extra_rbracket);
    diag.fixItInsert(result.get()->getStartLoc(), getTokenText(tok::l_square));
    consumeToken();
    return makeParserErrorResult(new (Context)
                                     ErrorTypeRepr(getTypeErrorLoc()));
  } else if (!result.isParseError() && Tok.is(tok::colon)) {
    auto colonTok = consumeToken();
    auto secondType = parseType(diag::expected_dictionary_value_type);

    auto diag = diagnose(colonTok, diag::extra_colon);
    diag.fixItInsert(result.get()->getStartLoc(), getTokenText(tok::l_square));
    if (!secondType.isParseError()) {
      if (Tok.is(tok::r_square)) {
        consumeToken();
      } else {
        diag.fixItInsertAfter(secondType.get()->getEndLoc(), getTokenText(tok::r_square));
      }
    }
    return makeParserErrorResult(new (Context)
                                     ErrorTypeRepr(getTypeErrorLoc()));
  }
  return result;
}

SourceLoc Parser::getTypeErrorLoc() const {
  // Use the same location as a missing close brace, etc.
  return getErrorOrMissingLoc();
}

ParsedSyntaxResult<ParsedGenericArgumentClauseSyntax>
Parser::parseGenericArgumentClauseSyntax() {
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  auto LAngleLoc = Tok.getLoc();
  ParsedGenericArgumentClauseSyntaxBuilder builder(*SyntaxContext);
  ParserStatus status;

  // Parse '<'.
  builder.useLeftAngleBracket(consumeStartingLessSyntax());

  bool hasNext = true;
  do {
    // Parse argument type.
    auto ty = parseTypeSyntax(diag::expected_type);
    status |= ty.getStatus();
    if (ty.isNull())
      break;
    ParsedGenericArgumentSyntaxBuilder argBuilder(*SyntaxContext);
    argBuilder.useArgumentType(ty.get());

    // Parse trailing comma: ','.
    if (Tok.is(tok::comma)) {
      argBuilder.useTrailingComma(consumeTokenSyntax());
    } else {
      hasNext = false;
    }
    builder.addArgumentsMember(argBuilder.build());
  } while (hasNext);

  // Parse '>'.
  if (startsWithGreater(Tok)) {
    builder.useRightAngleBracket(consumeStartingGreaterSyntax());
  } else {
    if (status.isSuccess()) {
      diagnose(Tok, diag::expected_rangle_generic_arg_list);
      diagnose(LAngleLoc, diag::opening_angle);
    }
    checkForInputIncomplete();
    status.setIsParseError();
    if (ignoreUntilGreaterInTypeList())
      builder.useRightAngleBracket(consumeStartingGreaterSyntax());
  }

  return makeParsedResult(builder.build(), status);
}

ParserStatus Parser::parseGenericArguments(SmallVectorImpl<TypeRepr *> &ArgsAST,
                                           SourceLoc &LAngleLoc,
                                           SourceLoc &RAngleLoc) {
  auto StartLoc = leadingTriviaLoc();
  auto ParsedClauseResult = parseGenericArgumentClauseSyntax();
  if (ParsedClauseResult.isNull())
    return ParsedClauseResult.getStatus();

  SyntaxContext->addSyntax(ParsedClauseResult.get());
  if (ParsedClauseResult.isError())
    return ParsedClauseResult.getStatus();

  auto Clause = SyntaxContext->topNode<GenericArgumentClauseSyntax>();
  LAngleLoc = Generator.generate(Clause.getLeftAngleBracket(), StartLoc);
  for (auto &&ArgAST : Generator.generate(Clause.getArguments(), StartLoc))
    ArgsAST.push_back(ArgAST);
  RAngleLoc = Generator.generate(Clause.getRightAngleBracket(), StartLoc);
  return makeParserSuccess();
}

/// parseTypeIdentifier
///
///   type-identifier:
///     identifier generic-args? ('.' identifier generic-args?)*
///
ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeIdentifier() {
  if (Tok.isNot(tok::identifier) && Tok.isNot(tok::kw_Self)) {
    // is this the 'Any' type
    if (Tok.is(tok::kw_Any))
      return parseAnyType();

    if (Tok.is(tok::code_complete)) {
      auto CCTok = consumeTokenSyntax(tok::code_complete);
      auto ty = ParsedSyntaxRecorder::makeCodeCompletionType(
          None, None, std::move(CCTok), *SyntaxContext);
      return makeParsedCodeCompletion(std::move(ty));
    }

    diagnose(Tok, diag::expected_identifier_for_type);

    // If there is a keyword at the start of a new line, we won't want to
    // skip it as a recovery but rather keep it.
    if (Tok.isKeyword() && !Tok.isAtStartOfLine()) {
      auto kwTok = consumeTokenSyntax();
      ParsedTypeSyntax ty =
          ParsedSyntaxRecorder::makeUnknownType({&kwTok, 1}, *SyntaxContext);
      return makeParsedError(std::move(ty));
    }

    return makeParsedError<ParsedTypeSyntax>();
  }

  SmallVector<ParsedSyntax, 0> Junk;

  auto parseComponent =
      [&](Optional<ParsedTokenSyntax> &Identifier,
          Optional<ParsedGenericArgumentClauseSyntax> &GenericArgs) {
        if (Tok.is(tok::kw_Self)) {
          Identifier = consumeIdentifierSyntax();
        } else {
          // FIXME: specialize diagnostic for 'Type': type cannot start with
          // 'metatype'
          // FIXME: offer a fixit: 'self' -> 'Self'
          Identifier =
              parseIdentifierSyntax(diag::expected_identifier_in_dotted_type);
        }

        if (!Identifier)
          return makeParserError();

        if (!startsWithLess(Tok))
          return makeParserSuccess();

        SmallVector<TypeRepr *, 4> GenericArgsAST;
        SourceLoc LAngleLoc, RAngleLoc;
        auto GenericArgsResult = parseGenericArgumentClauseSyntax();
        if (!GenericArgsResult.isNull())
          GenericArgs = GenericArgsResult.get();
        return GenericArgsResult.getStatus();
      };

  ParsedSyntaxResult<ParsedTypeSyntax> result;

  // Parse the base identifier.
  result = [&]() {
    Optional<ParsedTokenSyntax> identifier;
    Optional<ParsedGenericArgumentClauseSyntax> genericArgs;
    auto status = parseComponent(identifier, genericArgs);
    assert(identifier);
    return makeParsedResult(
        ParsedSyntaxRecorder::makeSimpleTypeIdentifier(
            std::move(*identifier), std::move(genericArgs), *SyntaxContext),
        status);
  }();

  // Parse member identifiers.
  while (result.isSuccess() && Tok.isAny(tok::period, tok::period_prefix)) {
    if (peekToken().isContextualKeyword("Type") ||
        peekToken().isContextualKeyword("Protocol"))
      break;

    // Parse '.'.
    auto period = consumeTokenSyntax();

    // Parse component;
    Optional<ParsedTokenSyntax> identifier;
    Optional<ParsedGenericArgumentClauseSyntax> genericArgs;
    auto status = parseComponent(identifier, genericArgs);
    if (identifier) {
      ParsedMemberTypeIdentifierSyntaxBuilder builder(*SyntaxContext);
      builder.useBaseType(result.get());
      builder.usePeriod(std::move(period));
      builder.useName(std::move(*identifier));
      if (genericArgs)
        builder.useGenericArgumentClause(std::move(*genericArgs));
      result = makeParsedResult(builder.build(), status);
      continue;
    }

    assert(!genericArgs);

    if (Tok.is(tok::code_complete)) {
      auto ty = ParsedSyntaxRecorder::makeCodeCompletionType(
          result.get(), std::move(period), consumeTokenSyntax(),
          *SyntaxContext);
      return makeParsedCodeCompletion(std::move(ty));
    }

    ParsedSyntax parts[] = {result.get(), std::move(period)};
    return makeParsedResult(
        ParsedSyntaxRecorder::makeUnknownType({parts, 2}, *SyntaxContext),
        status);
  }

  if (result.isSuccess() && Tok.is(tok::code_complete) &&
      !Tok.isAtStartOfLine()) {
    auto ty = ParsedSyntaxRecorder::makeCodeCompletionType(
        result.get(), None, consumeTokenSyntax(), *SyntaxContext);
    return makeParsedCodeCompletion(std::move(ty));
  }

  // Don't propagate malformed type as valid type.
  if (!result.isSuccess()) {
    auto ty = result.get();
    return makeParsedResult(
        ParsedSyntaxRecorder::makeUnknownType({&ty, 1}, *SyntaxContext),
        result.getStatus());
  }

  return result;
}

/// parseTypeSimpleOrComposition
///
///   type-composition:
///     'some'? type-simple
///     type-composition '&' type-simple
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeSimpleOrComposition(Diag<> MessageID,
                                     bool HandleCodeCompletion) {
  // Check for the opaque modifier.
  // This is only semantically allowed in certain contexts, but we parse it
  // generally for diagnostics and recovery.
  Optional<ParsedTokenSyntax> FirstSome;
  if (Tok.is(tok::identifier) && Tok.getRawText() == "some") {
    // Treat some as a keyword.
    TokReceiver->registerTokenKindChange(Tok.getLoc(), tok::contextual_keyword);
    FirstSome = consumeTokenSyntax();
  }

  auto ApplySome = [this](ParsedTypeSyntax Type,
                          Optional<ParsedTokenSyntax> Some) {
    return Some ? ParsedSyntaxRecorder::makeSomeType(
                      std::move(*Some), std::move(Type), *SyntaxContext)
                : std::move(Type);
  };

  // Parse the first type
  auto FirstTypeResult = parseTypeSimple(MessageID, HandleCodeCompletion);

  if (FirstTypeResult.isError())
    return FirstTypeResult;

  auto FirstType = FirstTypeResult.get();

  if (!Tok.isContextualPunctuator("&"))
    return makeParsedResult(
        ApplySome(std::move(FirstType), std::move(FirstSome)));

  SmallVector<ParsedCompositionTypeElementSyntax, 4> Elements;

  Optional<ParsedTokenSyntax> Ampersand = consumeTokenSyntax();
  auto FirstElement = ParsedSyntaxRecorder::makeCompositionTypeElement(
      std::move(FirstType), std::move(*Ampersand), *SyntaxContext);
  Elements.push_back(std::move(FirstElement));

  ParserStatus Status;

  do {
    // Diagnose invalid `some` after an ampersand.
    Optional<ParsedTokenSyntax> NextSome;
    if (Tok.is(tok::identifier) && Tok.getRawText() == "some") {
      auto NextSomeLoc = Tok.getLoc();
      NextSome = consumeTokenSyntax();
      // TODO: Fixit to move to beginning of composition.
      diagnose(NextSomeLoc, diag::opaque_mid_composition);
    }

    auto NextTypeResult = parseTypeSimple(diag::expected_identifier_for_type,
                                          HandleCodeCompletion);

    if (!NextTypeResult.isSuccess()) {
      Status |= NextTypeResult.getStatus();
      if (NextTypeResult.isNull())
        break;

      SmallVector<ParsedSyntax, 0> nodes;
      if (FirstSome)
        nodes.push_back(std::move(*FirstSome));
      std::move(Elements.begin(), Elements.end(), std::back_inserter(nodes));
      if (NextSome)
        nodes.push_back(std::move(*NextSome));
      nodes.push_back(NextTypeResult.get());

      auto ty = ParsedSyntaxRecorder::makeUnknownType(nodes, *SyntaxContext);
      return makeParsedResult(std::move(ty), Status);
    }

    auto NextType = ApplySome(NextTypeResult.get(), std::move(NextSome));
    Ampersand = Tok.isContextualPunctuator("&")
        ? consumeTokenSyntax()
        : llvm::Optional<ParsedTokenSyntax>();
    auto NextElement = ParsedSyntaxRecorder::makeCompositionTypeElement(
        std::move(NextType), std::move(Ampersand), *SyntaxContext);
    Elements.push_back(std::move(NextElement));
  } while (Ampersand);

  auto ElementList = ParsedSyntaxRecorder::makeCompositionTypeElementList(
      Elements, *SyntaxContext);
  auto Composition = ParsedSyntaxRecorder::makeCompositionType(
      std::move(ElementList), *SyntaxContext);
  return makeParsedResult(
      ApplySome(std::move(Composition), std::move(FirstSome)), Status);
}

ParserResult<TypeRepr> Parser::parseAnyTypeAST() {
  auto AnyLoc = leadingTriviaLoc();
  auto ParsedAny = parseAnyType().get();
  SyntaxContext->addSyntax(std::move(ParsedAny));
  auto Any = SyntaxContext->topNode<SimpleTypeIdentifierSyntax>();
  return makeParserResult(Generator.generate(Any, AnyLoc));
}

ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseAnyType() {
  auto Any = consumeTokenSyntax(tok::kw_Any);
  auto Type = ParsedSyntaxRecorder::makeSimpleTypeIdentifier(
      std::move(Any), llvm::None, *SyntaxContext);
  return makeParsedResult(std::move(Type));
}

/// parseOldStyleProtocolComposition
///   type-composition-deprecated:
///     'protocol' '<' '>'
///     'protocol' '<' type-composition-list-deprecated '>'
///
///   type-composition-list-deprecated:
///     type-identifier
///     type-composition-list-deprecated ',' type-identifier
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseOldStyleProtocolComposition() {
  // Defer all nodes so that we can de-structure the composed types in case we
  // need to emit a diagnostic (below).
  DeferringContextRAII Deferring(*SyntaxContext);

  SmallVector<ParsedSyntax, 0> Junk;

  auto ProtocolLoc = Tok.getLoc();
  auto Protocol = consumeTokenSyntax();
  auto LAngleLoc = Tok.getLoc();
  auto LAngle = consumeStartingLessSyntax();

  Junk.push_back(Protocol.copyDeferred());
  Junk.push_back(LAngle.copyDeferred());

  // Parse the type-composition-list.
  ParserStatus Status;
  SmallVector<ParsedTypeSyntax, 4> Protocols;
  Optional<ParsedTokenSyntax> Comma;
  bool IsEmpty = startsWithGreater(Tok);
  if (!IsEmpty) {
    do {
      bool IsAny = Tok.getKind() == tok::kw_Any;
      auto TypeResult = parseTypeIdentifier();
      Status |= TypeResult.getStatus();
      if (!TypeResult.isNull()) {
        auto Type = TypeResult.get();
        Junk.push_back(Type.copyDeferred());
        if (!IsAny)
          Protocols.push_back(std::move(Type));
      }
      Comma = consumeTokenSyntaxIf(tok::comma);
      if (Comma)
        Junk.push_back(Comma->copyDeferred());
    } while (Comma);
  }

  // Check for the terminating '>'.
  Optional<SourceLoc> RAngleLoc;
  if (startsWithGreater(Tok)) {
    RAngleLoc = Tok.getLoc();
    auto RAngle = consumeStartingGreaterSyntax();
    Junk.push_back(RAngle.copyDeferred());
  } else {
    if (Status.isSuccess()) {
      diagnose(Tok, diag::expected_rangle_protocol);
      diagnose(LAngleLoc, diag::opening_angle);
      Status.setIsParseError();
    }

    SmallVector<ParsedSyntax, 4> RAngleJunk;
    // Skip until we hit the '>'.
    skipUntilGreaterInTypeListSyntax(RAngleJunk, /*protocolComposition=*/true);
    for (auto &&Piece : RAngleJunk)
      Junk.push_back(Piece.copyDeferred());
  }

  if (Status.isSuccess()) {
    SmallString<32> replacement;
    if (Protocols.empty()) {
      replacement = "Any";
    } else {
      auto extractText = [&](ParsedTypeSyntax &Type) -> StringRef {
        auto SourceRange = Type.getRaw()
          .getDeferredRange(/*includeTrivia=*/false);
        return SourceMgr.extractText(SourceRange);
      };
      auto Begin = Protocols.begin();
      replacement += extractText(*Begin);
      while (++Begin != Protocols.end()) {
        replacement += " & ";
        replacement += extractText(*Begin);
      }
    }

    if (Protocols.size() > 1) {
      // Need parenthesis if the next token looks like postfix TypeRepr.
      // i.e. '?', '!', '.Type', '.Protocol'
      bool needParen = false;
      needParen |= !Tok.isAtStartOfLine() &&
          (isOptionalToken(Tok) || isImplicitlyUnwrappedOptionalToken(Tok));
      needParen |= Tok.isAny(tok::period, tok::period_prefix);
      if (needParen) {
        replacement.insert(replacement.begin(), '(');
        replacement += ")";
      }
    }

    // Copy split token after '>' to the replacement string.
    // FIXME: lexer should smartly separate '>' and trailing contents like '?'.
    StringRef TrailingContent = L->getTokenAt(*RAngleLoc).getRange().str().
      substr(1);
    if (!TrailingContent.empty())
      replacement += TrailingContent;

    // Replace 'protocol<T1, T2>' with 'T1 & T2'
    diagnose(ProtocolLoc,
      IsEmpty              ? diag::deprecated_any_composition :
      Protocols.size() > 1 ? diag::deprecated_protocol_composition :
                             diag::deprecated_protocol_composition_single)
      .highlight({ProtocolLoc, *RAngleLoc})
      .fixItReplace({ProtocolLoc, *RAngleLoc}, replacement);
  }

  auto Unknown = ParsedSyntaxRecorder::makeUnknownType(Junk, *SyntaxContext);
  return makeParsedResult(std::move(Unknown));
}

/// parseTypeTupleBody
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)* '...'?
///   type-tuple-element:
///     identifier? identifier ':' type
///     type
ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeTupleBody() {
  // Force the context to create deferred nodes, as we might need to
  // de-structure the tuple type to create a function type.
  DeferringContextRAII Deferring(*SyntaxContext);
  Parser::StructureMarkerRAII ParsingTypeTuple(*this, Tok);

  if (ParsingTypeTuple.isFailed())
    return makeParsedError<ParsedTypeSyntax>();

  SmallVector<ParsedSyntax, 0> Junk;

  auto LParenLoc = Tok.getLoc();
  auto LParen = consumeTokenSyntax(tok::l_paren);

  Junk.push_back(LParen.copyDeferred());

  SmallVector<ParsedTupleTypeElementSyntax, 4> Elements;
  SmallVector<std::tuple<SourceLoc, SourceLoc, SourceLoc>, 4> ElementsLoc;
  SourceLoc FirstEllipsisLoc;

  Optional<ParsedTokenSyntax> Comma;

  SourceLoc RParenLoc;
  Optional<ParsedTokenSyntax> RParen;

  ParserStatus Status =
      parseListSyntax(tok::r_paren, LParenLoc, Comma, RParenLoc, RParen, Junk,
                      false, diag::expected_rparen_tuple_type_list, [&]() {
    Optional<BacktrackingScope> Backtracking;
    SmallVector<ParsedSyntax, 0> LocalJunk;

    // 'inout' here can be a obsoleted use of the marker in an argument list,
    // consume it in backtracking context so we can determine it's really a
    // deprecated use of it.
    SourceLoc InOutLoc;
    Optional<ParsedTokenSyntax> InOut;
    bool IsInOutObsoleted = false;
    if (Tok.is(tok::kw_inout)) {
      InOutLoc = Tok.getLoc();
      InOut = consumeTokenSyntax(tok::kw_inout);
      IsInOutObsoleted = true;

      LocalJunk.push_back(InOut->copyDeferred());
    }

    // If the label is "some", this could end up being an opaque type
    // description if there's `some <identifier>` without a following colon,
    // so we may need to backtrack as well.
    if (Tok.getText().equals("some")) {
      Backtracking.emplace(*this);
    }

    // If the tuple element starts with a potential argument label followed by a
    // ':' or another potential argument label, then the identifier is an
    // element tag, and it is followed by a type annotation.
    Optional<ParsedTokenSyntax> Name;
    Optional<ParsedTokenSyntax> SecondName;
    Optional<ParsedTokenSyntax> Colon;
    SourceLoc NameLoc;
    SourceLoc SecondNameLoc;
    if (Tok.canBeArgumentLabel() &&
        (peekToken().is(tok::colon) || peekToken().canBeArgumentLabel())) {
      // Consume a name.
      NameLoc = Tok.getLoc();
      Name = consumeArgumentLabelSyntax();

      // If there is a second name, consume it as well.
      if (Tok.canBeArgumentLabel()) {
        SecondNameLoc = Tok.getLoc();
        SecondName = consumeArgumentLabelSyntax();
      }

      // Consume the ':'.
      if ((Colon = consumeTokenSyntaxIf(tok::colon))) {
        // If we succeed, then we successfully parsed a label.
        if (Backtracking)
          Backtracking->cancelBacktrack();
        // Otherwise, if we can't backtrack to parse this as a type,
        // this is a syntax error.
      } else {
        if (!Backtracking)
          diagnose(Tok, diag::expected_parameter_colon);
        NameLoc = SourceLoc();
        SecondNameLoc = SourceLoc();
      }
    } else if (InOut) {
      // If we don't have labels, 'inout' is not a obsoleted use.
      IsInOutObsoleted = false;
    }

    if (!Backtracking || !Backtracking->willBacktrack()) {
      if (Name)
        LocalJunk.push_back(Name->copyDeferred());
      if (SecondName)
        LocalJunk.push_back(SecondName->copyDeferred());
      if (Colon)
        LocalJunk.push_back(Colon->copyDeferred());
    } else if (Backtracking && Backtracking->willBacktrack()) {
      Name.reset();
      SecondName.reset();
      assert(!Colon.hasValue());
    }
    Backtracking.reset();

    // Parse the type annotation.
    auto TypeLoc = Tok.getLoc();
    auto ty = parseTypeSyntax(diag::expected_type);
    if (ty.isError()) {
      std::move(LocalJunk.begin(), LocalJunk.end(), std::back_inserter(Junk));
      if (!ty.isNull())
        Junk.push_back(ty.get());
      skipListUntilDeclRBraceSyntax(Junk, LParenLoc, tok::r_paren, tok::comma);
      return ty.getStatus();
    }

    if (InOut) {
      if (IsInOutObsoleted) {
        bool IsTypeAlreadyAttributed = false;
        if (auto AttributedType = ty.getAs<ParsedAttributedTypeSyntax>()) {
          IsTypeAlreadyAttributed =
              AttributedType->getDeferredSpecifier().hasValue();
          ty = makeParsedResult(std::move(*AttributedType), ty.getStatus());
        }
        if (IsTypeAlreadyAttributed) {
          // If the parsed type is already attributed, suggest removing `inout`.
          diagnose(Tok, diag::parameter_specifier_repeated)
              .fixItRemove(InOutLoc);
        } else {
          diagnose(InOutLoc, diag::parameter_specifier_as_attr_disallowed, "inout")
              .fixItRemove(InOutLoc)
              .fixItInsert(TypeLoc, "inout ");
        }
      } else {
        // Apply 'inout' to the parsed type.
        ParsedAttributedTypeSyntaxBuilder builder(*SyntaxContext);
        ty = applyAttributeToTypeSyntax(std::move(ty), std::move(InOut), None);
        InOut.reset();
      }
    }

    Optional<ParsedTokenSyntax> ElementEllipsis;
    if (Tok.isEllipsis()) {
      Tok.setKind(tok::ellipsis);
      auto ElementEllipsisLoc = Tok.getLoc();
      ElementEllipsis = consumeTokenSyntax();
      if (!FirstEllipsisLoc.isValid()) {
        FirstEllipsisLoc = ElementEllipsisLoc;
      } else {
        diagnose(ElementEllipsisLoc, diag::multiple_ellipsis_in_tuple)
            .highlight(FirstEllipsisLoc)
            .fixItRemove(ElementEllipsisLoc);
      }
    }

    Optional<ParsedTokenSyntax> Equal;
    Optional<ParsedInitializerClauseSyntax> Initializer;
    if (Tok.is(tok::equal)) {
      auto EqualLoc = Tok.getLoc();
      Equal = consumeTokenSyntax(tok::equal);
      auto Init = parseExpr(diag::expected_init_value);
      auto InFlight = diagnose(EqualLoc, diag::tuple_type_init);
      if (Init.isNonNull())
        InFlight.fixItRemove(SourceRange(EqualLoc, Init.get()->getEndLoc()));
      Initializer = ParsedSyntaxRecorder::makeInitializerClause(
          std::move(*Equal),
          std::move(*SyntaxContext->popIf<ParsedExprSyntax>()),
          *SyntaxContext);
    }

    Comma = consumeTokenSyntaxIf(tok::comma);

    auto Element = ParsedSyntaxRecorder::makeTupleTypeElement(
        std::move(InOut), std::move(Name), std::move(SecondName),
        std::move(Colon), ty.get(), std::move(ElementEllipsis),
        std::move(Initializer), std::move(Comma), *SyntaxContext);

    Junk.push_back(Element.copyDeferred());

    Elements.push_back(std::move(Element));
    ElementsLoc.emplace_back(NameLoc, SecondNameLoc, TypeLoc);

    return makeParserSuccess();
  });

  if (!Status.isSuccess()) {
    if (RParen)
      Junk.push_back(std::move(*RParen));
    auto ty = ParsedSyntaxRecorder::makeUnknownType(Junk, *SyntaxContext);
    return makeParsedResult(std::move(ty), Status);
  }

  bool IsFunctionType = Tok.isAny(tok::arrow, tok::kw_throws, tok::kw_rethrows);

  auto GetNameText = [this](Optional<ParsedTokenSyntax> Name) {
    return !Name ? StringRef()
                 : SourceMgr.extractText(
                       Name->getRaw().getDeferredTokenRange(),
                       L->getBufferID());
  };

  if (!IsFunctionType) {
    for (unsigned i = 0; i < Elements.size(); i++) {
      // true tuples have labels
      auto &Element = Elements[i];
      SourceLoc NameLoc, SecondNameLoc, TypeLoc;
      std::tie(NameLoc, SecondNameLoc, TypeLoc) = ElementsLoc[i];
      // If there were two names, complain.
      if (NameLoc.isValid() && SecondNameLoc.isValid()) {
        auto Diag = diagnose(NameLoc, diag::tuple_type_multiple_labels);
        auto Name = Element.getDeferredName();
        auto NameText = SourceMgr.extractText(
            Name->getRaw().getDeferredTokenRange(),
            L->getBufferID());
        if (NameText == "_") {
          Diag.fixItRemoveChars(NameLoc, TypeLoc);
        } else {
          Diag.fixItRemove(SourceRange(
              Lexer::getLocForEndOfToken(SourceMgr, NameLoc), SecondNameLoc));
        }
      }
    }
  } else {
    for (unsigned i = 0; i < Elements.size(); i++) {
      // If there was a first name, complain; arguments in function types are
      // always unlabeled.
      auto &Element = Elements[i];
      SourceLoc NameLoc, SecondNameLoc, TypeLoc;
      std::tie(NameLoc, SecondNameLoc, TypeLoc) = ElementsLoc[i];
      if (NameLoc.isValid()) {
        auto NameText = GetNameText(Element.getDeferredName());
        if (NameText != "_") {
          auto NameIdentifier = Context.getIdentifier(NameText);
          auto Diag = diagnose(NameLoc, diag::function_type_argument_label,
                               NameIdentifier);
          auto SecondNameText = GetNameText(Element.getDeferredSecondName());
          if (SecondNameLoc.isInvalid())
            Diag.fixItInsert(NameLoc, "_ ");
          else if (SecondNameText == "_")
            Diag.fixItRemoveChars(NameLoc, TypeLoc);
          else
            Diag.fixItReplace(SourceRange(NameLoc), "_");
        }
      }
    }
  }

  auto ElementList =
      ParsedSyntaxRecorder::makeTupleTypeElementList(Elements, *SyntaxContext);

  auto TupleType = ParsedSyntaxRecorder::makeTupleType(
      std::move(LParen), std::move(ElementList), std::move(*RParen),
      *SyntaxContext);

  return makeParsedResult(std::move(TupleType));
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
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeArray(ParsedTypeSyntax Base, SourceLoc BaseLoc) {
  assert(Tok.isFollowingLSquare());
  auto LSquareLoc = Tok.getLoc();
  ignoreToken(tok::l_square);

  // Ignore integer literal between '[' and ']'
  ignoreIf(tok::integer_literal);

  SourceLoc RSquareLoc;
  auto RSquare = parseMatchingTokenSyntax(
      tok::r_square, RSquareLoc, diag::expected_rbracket_array_type, LSquareLoc);

  if (RSquare) {
    // If we parsed something valid, diagnose it with a fixit to rewrite it to
    // Swift syntax.
    diagnose(LSquareLoc, diag::new_array_syntax)
        .fixItInsert(BaseLoc, "[")
        .fixItRemoveChars(LSquareLoc, RSquareLoc);
  }

  ParsedArrayTypeSyntaxBuilder builder(*SyntaxContext);
  ParserStatus status;

  builder.useElementType(std::move(Base));
  if (RSquare) {
    builder.useRightSquareBracket(std::move(*RSquare));
  } else {
    status.setIsParseError();
  }

  return makeParsedResult(builder.build(), status);
}

/// Parse a collection type.
///   type-simple:
///     '[' type ']'
///     '[' type ':' type ']'
ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeCollection() {
  ParserStatus Status;
  assert(Tok.is(tok::l_square));
  Parser::StructureMarkerRAII parsingCollection(*this, Tok);
  auto LSquareLoc = Tok.getLoc();
  auto LSquare = consumeTokenSyntax(tok::l_square);

  auto ElementTypeResult = parseTypeSyntax(diag::expected_element_type);
  Status |= ElementTypeResult.getStatus();
  auto ElementType = ElementTypeResult.getOrNull();
  if (!ElementType)
    ElementType = ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext);

  Optional<ParsedTokenSyntax> Colon;
  Optional<ParsedTypeSyntax> ValueType;

  if (Tok.is(tok::colon)) {
    Colon = consumeTokenSyntax(tok::colon);
    auto ValueTypeResult =
        parseTypeSyntax(diag::expected_dictionary_value_type);
    ValueType = ValueTypeResult.getOrNull();
    if (!ValueType)
      ValueType = ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext);
    Status |= ValueTypeResult.getStatus();
  }

  auto Diag = Colon ? diag::expected_rbracket_dictionary_type
                    : diag::expected_rbracket_array_type;

  SourceLoc RSquareLoc;
  auto RSquare =
      parseMatchingTokenSyntax(tok::r_square, RSquareLoc, Diag, LSquareLoc);
  if (!RSquare)
    Status.setIsParseError();

  if (Colon) {
    ParsedDictionaryTypeSyntaxBuilder builder(*SyntaxContext);
    builder.useLeftSquareBracket(std::move(LSquare));
    builder.useKeyType(std::move(*ElementType));
    builder.useColon(std::move(*Colon));
    builder.useValueType(std::move(*ValueType));
    if (RSquare)
      builder.useRightSquareBracket(std::move(*RSquare));
    return makeParsedResult(builder.build(), Status);
  } else {
    ParsedArrayTypeSyntaxBuilder builder(*SyntaxContext);
    builder.useLeftSquareBracket(std::move(LSquare));
    builder.useElementType(std::move(*ElementType));
    if (RSquare)
      builder.useRightSquareBracket(std::move(*RSquare));
    return makeParsedResult(builder.build(), Status);
  }
}

ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseMetatypeType(ParsedTypeSyntax Base) {
  auto Period = consumeTokenSyntax(); // tok::period or tok::period_prefix
  auto Keyword = consumeTokenSyntax(tok::identifier); // "Type" or "Protocol"
  auto MetatypeType = ParsedSyntaxRecorder::makeMetatypeType(
      std::move(Base), std::move(Period), std::move(Keyword), *SyntaxContext);
  return makeParsedResult(std::move(MetatypeType));
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

ParsedTokenSyntax Parser::consumeOptionalTokenSyntax() {
  assert(isOptionalToken(Tok) && "not a '?' token?!");
  return consumeStartingCharacterOfCurrentTokenSyntax(tok::question_postfix, 1);
}

SourceLoc Parser::consumeOptionalToken() {
  assert(isOptionalToken(Tok) && "not a '?' token?!");
  return consumeStartingCharacterOfCurrentToken(tok::question_postfix);
}

ParsedTokenSyntax Parser::consumeImplicitlyUnwrappedOptionalTokenSyntax() {
  assert(isImplicitlyUnwrappedOptionalToken(Tok) && "not a '!' token?!");
  // If the text of the token is just '!', grab the next token.
  return consumeStartingCharacterOfCurrentTokenSyntax(tok::exclaim_postfix, 1);
}

SourceLoc Parser::consumeImplicitlyUnwrappedOptionalToken() {
  assert(isImplicitlyUnwrappedOptionalToken(Tok) && "not a '!' token?!");
  // If the text of the token is just '!', grab the next token.
  return consumeStartingCharacterOfCurrentToken(tok::exclaim_postfix);
}

ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseOptionalType(ParsedTypeSyntax Base) {
  auto Question = consumeOptionalTokenSyntax();
  auto Optional = ParsedSyntaxRecorder::makeOptionalType(
      std::move(Base), std::move(Question), *SyntaxContext);
  return makeParsedResult(std::move(Optional));
}

ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseImplicitlyUnwrappedOptionalType(ParsedTypeSyntax Base) {
  auto Exclamation = consumeImplicitlyUnwrappedOptionalTokenSyntax();
  auto Unwrapped = ParsedSyntaxRecorder::makeImplicitlyUnwrappedOptionalType(
      std::move(Base), std::move(Exclamation), *SyntaxContext);
  return makeParsedResult(std::move(Unwrapped));
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
  case tok::colon:
    return true;

  case tok::oper_binary_spaced:
    if (tok.getText() == "&")
      return true;

    LLVM_FALLTHROUGH;
  case tok::oper_binary_unspaced:
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
  // Accept 'inout' at for better recovery.
  consumeIf(tok::kw_inout);

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

        // Parse a type.
        if (!canParseType())
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
      if (!canParseType())
        return false;

      if (Tok.isEllipsis())
        consumeToken();

    } while (consumeIf(tok::comma));
  }
  
  return consumeIf(tok::r_paren);
}
