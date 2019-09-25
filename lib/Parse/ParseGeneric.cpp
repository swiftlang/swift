//===--- ParseGeneric.cpp - Swift Language Parser for Generics ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Generic Parsing
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Parse/ParsedSyntaxBuilders.h"
#include "swift/Parse/ParsedSyntaxRecorder.h"
#include "swift/Parse/SyntaxParsingContext.h"

using namespace swift;
using namespace swift::syntax;

/// Parse a list of generic parameters.
///
///   generic-parameter-clause-list:
///     generic-parameter-clause generic-parameter-clause*
ParserStatus Parser::parseSILGenericParamsSyntax(
    Optional<ParsedGenericParameterClauseListSyntax> &result) {
  assert(isInSILMode());
  ParserStatus status;
  if (!startsWithLess(Tok))
    return status;

  SmallVector<ParsedGenericParameterClauseSyntax, 1> clauses;
  do {
    auto result = parseGenericParameterClauseSyntax();
    status |= result.getStatus();
    if (!result.isNull())
      clauses.push_back(result.get());
  } while (startsWithLess(Tok));

  result = ParsedSyntaxRecorder::makeGenericParameterClauseList(clauses,
                                                                *SyntaxContext);
  return status;
}

/// Parse a sequence of generic parameters, e.g. '<T: Comparable, U: Container>'
/// along with an optional requires clause.
///
///   generic-parameter-clause:
///     '<' generic-paramter (',' generic-parameter)* where-clause? '>'
///
///   generic-parameter:
///     identifier
///     identifier ':' type
ParsedSyntaxResult<ParsedGenericParameterClauseSyntax>
Parser::parseGenericParameterClauseSyntax() {
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  ParsedGenericParameterClauseSyntaxBuilder builder(*SyntaxContext);
  ParserStatus status;

  // Parse '<'.
  SourceLoc LAngleLoc = Tok.getLoc();
  builder.useLeftAngleBracket(consumeStartingLessSyntax());

  // Parse parameters.
  bool hasNext = true;
  do {
    ParsedGenericParameterSyntaxBuilder paramBuilder(*SyntaxContext);

    // Parse attributes.
    // TODO: Implement syntax attribute parsing.
    DeclAttributes attrsAST;
    parseDeclAttributeList(attrsAST);
    auto attrs = SyntaxContext->popIf<ParsedAttributeListSyntax>();
    if (attrs) {
      paramBuilder.useAttributes(std::move(*attrs));
      Generator.addDeclAttributes(attrsAST, attrsAST.getStartLoc());
    }

    // Parse the name of the parameter.
    auto ident = Context.getIdentifier(Tok.getText());
    auto name = parseIdentifierSyntax(diag::expected_generics_parameter_name);
    if (!name) {
      status.setIsParseError();
      break;
    }
    paramBuilder.useName(std::move(*name));

    // Parse the ':' followed by a type.
    if (Tok.is(tok::colon)) {
      paramBuilder.useColon(consumeTokenSyntax(tok::colon));
      if (Tok.isAny(tok::identifier, tok::code_complete, tok::kw_protocol,
                    tok::kw_Any)) {
        auto tyResult = parseTypeSyntax();
        status |= tyResult.getStatus();
        if (auto ty = tyResult.getOrNull())
          paramBuilder.useInheritedType(std::move(*ty));
      } else {
        if (Tok.is(tok::kw_class)) {
          diagnose(Tok, diag::unexpected_class_constraint);
          diagnose(Tok, diag::suggest_anyobject)
              .fixItReplace(Tok.getLoc(), "AnyObject");
          Tok.setKind(tok::identifier);
          auto ty = ParsedSyntaxRecorder::makeSimpleTypeIdentifier(
              consumeTokenSyntax(), None, *SyntaxContext);
          paramBuilder.useInheritedType(std::move(ty));
        } else {
          diagnose(Tok, diag::expected_generics_type_restriction, ident);

          paramBuilder.useInheritedType(
              ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext));
        }
        status.setIsParseError();
      }
    }

    // Parse ','
    hasNext = Tok.is(tok::comma);
    if (hasNext)
      paramBuilder.useTrailingComma(consumeTokenSyntax(tok::comma));

    builder.addGenericParameterListMember(paramBuilder.build());
  } while (hasNext);

  // Parse optional where clause.
  SourceLoc whereLoc;
  if (Tok.is(tok::kw_where)) {
    SmallVector<RequirementRepr, 2> requirementAST;
    bool FirstTypeInComplete = false;
    auto where = parseGenericWhereClauseSyntax(FirstTypeInComplete);
    builder.useObsoletedWhereClause(where.get());
  }

  // Parse the closing '>'.
  if (startsWithGreater(Tok)) {
    builder.useRightAngleBracket(consumeStartingGreaterSyntax());
  } else {
    if (!status.isError()) {
      diagnose(Tok, diag::expected_rangle_generics_param);
      diagnose(LAngleLoc, diag::opening_angle);
    }

    // Skip until we hit the '>'.
    if (ignoreUntilGreaterInTypeList())
      builder.useRightAngleBracket(consumeStartingGreaterSyntax());
    status.setIsParseError();
  }

  return makeParsedResult(builder.build(), status);
}

ParserResult<GenericParamList> Parser::parseGenericParameters() {
  auto loc = leadingTriviaLoc();
  auto syntaxResult = parseGenericParameterClauseSyntax();
  if (syntaxResult.isNull())
    return syntaxResult.getStatus();
  SyntaxContext->addSyntax(syntaxResult.get());

  auto clause = SyntaxContext->topNode<GenericParameterClauseSyntax>();
  if (clause.getGenericParameterList().empty())
    return nullptr;
  return makeParserResult(syntaxResult.getStatus(),
                          Generator.generate(clause, loc));
}

ParserResult<GenericParamList> Parser::maybeParseGenericParams() {
  if (!startsWithLess(Tok))
    return nullptr;
  return parseGenericParameters();
}

ParserResult<GenericParamList> Parser::parseSILGenericParams() {
  assert(isInSILMode());
  auto loc = leadingTriviaLoc();
  Optional<ParsedGenericParameterClauseListSyntax> result;
  auto status = parseSILGenericParamsSyntax(result);
  if (!result.hasValue()) {
    status.setIsParseError();
    return status;
  }

  SyntaxContext->addSyntax(std::move(*result));
  auto list = SyntaxContext->topNode<GenericParameterClauseListSyntax>();
  auto ret = Generator.generate(list, loc);
  if (!ret)
    return nullptr;
  return makeParserResult(status, ret);
}

void
Parser::diagnoseWhereClauseInGenericParamList(const GenericParamList *
                                              GenericParams) {
  if (GenericParams == nullptr || GenericParams->getWhereLoc().isInvalid())
    return;



  auto WhereRangeInsideBrackets = GenericParams->getWhereClauseSourceRange();

  // Move everything immediately following the last generic parameter
  // as written all the way to the right angle bracket (">")
  auto LastGenericParam = GenericParams->getParams().back();
  auto EndOfLastGenericParam =
      Lexer::getLocForEndOfToken(SourceMgr, LastGenericParam->getEndLoc());

  CharSourceRange RemoveWhereRange { SourceMgr,
    EndOfLastGenericParam,
    GenericParams->getRAngleLoc()
  };

  auto WhereCharRange =
    Lexer::getCharSourceRangeFromSourceRange(SourceMgr,
                                  GenericParams->getWhereClauseSourceRange());

  SmallString<64> Buffer;
  llvm::raw_svector_ostream WhereClauseText(Buffer);
  WhereClauseText << SourceMgr.extractText(Tok.is(tok::kw_where)
                                           ? WhereCharRange
                                           : RemoveWhereRange);

  // If, for some reason, there was a where clause in both locations, we're
  // adding to the list of requirements, so tack on a comma here before
  // inserting it at the head of the later where clause.
  if (Tok.is(tok::kw_where))
    WhereClauseText << ',';

  auto Diag = diagnose(WhereRangeInsideBrackets.Start,
                       diag::where_inside_brackets);

  Diag.fixItRemoveChars(RemoveWhereRange.getStart(),
                        RemoveWhereRange.getEnd());

  if (Tok.is(tok::kw_where)) {
    Diag.fixItReplace(Tok.getLoc(), WhereClauseText.str());
  } else {
    Diag.fixItInsert(Lexer::getLocForEndOfToken(SourceMgr, PreviousLoc),
                     WhereClauseText.str());
  }
}

/// Parse a 'where' clause, which places additional constraints on generic
/// parameters or types based on them.
///
///   where-clause:
///     'where' generic-requirement (',' generic-requirement) *
///
///   generic-requirement:
///     conformance-requirement
///     same-type-requirement
///     layout-requirement
///
///   conformance-requirement:
///     type ':' type
///
///   same-type-requirement:
///     type '==' type
///
///   layout-requirement:
///     type ':' layout-constraint
ParsedSyntaxResult<ParsedGenericWhereClauseSyntax>
Parser::parseGenericWhereClauseSyntax(bool &FirstTypeInComplete,
                                      bool allowLayoutConstraints) {
  ParsedGenericWhereClauseSyntaxBuilder builder(*SyntaxContext);
  ParserStatus status;

  // Parse 'where'.
  builder.useWhereKeyword(consumeTokenSyntax(tok::kw_where));

  bool hasNext = true;
  do {
    auto firstType = parseTypeSyntax();
    status |= firstType.getStatus();
    FirstTypeInComplete = firstType.hasCodeCompletion();
    if (firstType.isNull())
      break;

    ParsedGenericRequirementSyntaxBuilder elementBuilder(*SyntaxContext);

    if (Tok.is(tok::colon)) {
      auto colon = consumeTokenSyntax(tok::colon);

      if (Tok.is(tok::identifier) &&
          getLayoutConstraint(Context.getIdentifier(Tok.getText()), Context)
              ->isKnownLayout()) {
        // Layout constraint.
        ParsedLayoutRequirementSyntaxBuilder layoutReqBuilder(*SyntaxContext);
        layoutReqBuilder.useLeftTypeIdentifier(firstType.get());
        layoutReqBuilder.useColon(std::move(colon));
        SourceLoc layoutLoc = Tok.getLoc();
        auto layout = parseLayoutConstraintSyntax();
        status |= layout.getStatus();

        if (!allowLayoutConstraints && !isInSILMode())
          diagnose(layoutLoc,
                   diag::layout_constraints_only_inside_specialize_attr);
        assert(!layout.isNull());
        layoutReqBuilder.useLayoutConstraint(layout.get());
        elementBuilder.useBody(layoutReqBuilder.build());
      } else {
        // Conformance requirement.
        ParsedConformanceRequirementSyntaxBuilder conformanceReqBuilder(
            *SyntaxContext);
        conformanceReqBuilder.useLeftTypeIdentifier(firstType.get());
        conformanceReqBuilder.useColon(std::move(colon));
        auto secondType = parseTypeSyntax();
        status |= secondType.getStatus();
        if (!secondType.isNull())
          conformanceReqBuilder.useRightTypeIdentifier(secondType.get());
        else
          conformanceReqBuilder.useRightTypeIdentifier(
              ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext));
        elementBuilder.useBody(conformanceReqBuilder.build());
      }
    } else if ((Tok.isAnyOperator() && Tok.getText() == "==") ||
               Tok.is(tok::equal)) {
      // Same type requirement.
      ParsedSameTypeRequirementSyntaxBuilder sametypeReqBuilder(*SyntaxContext);
      sametypeReqBuilder.useLeftTypeIdentifier(firstType.get());
      if (Tok.is(tok::equal)) {
        diagnose(Tok, diag::requires_single_equal)
            .fixItReplace(SourceRange(Tok.getLoc()), "==");
        ignoreToken();
      } else {
        sametypeReqBuilder.useEqualityToken(consumeTokenSyntax());
      }

      auto secondType = parseTypeSyntax();
      status |= secondType.getStatus();
      if (!secondType.isNull())
        sametypeReqBuilder.useRightTypeIdentifier(secondType.get());
      else
        sametypeReqBuilder.useRightTypeIdentifier(
            ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext));
      elementBuilder.useBody(sametypeReqBuilder.build());
    } else {
      diagnose(Tok, diag::expected_requirement_delim);
      status.setIsParseError();

      // Fallback to conformance requirement with missing right type.
      ParsedConformanceRequirementSyntaxBuilder conformanceReqBuilder(
          *SyntaxContext);
      conformanceReqBuilder.useLeftTypeIdentifier(firstType.get());
      conformanceReqBuilder.useRightTypeIdentifier(
          ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext));
      elementBuilder.useBody(conformanceReqBuilder.build());
    }

    // Parse ','.
    hasNext = (status.isSuccess() && Tok.is(tok::comma));
    if (hasNext)
      elementBuilder.useTrailingComma(consumeTokenSyntax());

    builder.addRequirementListMember(elementBuilder.build());
  } while (hasNext && status.isSuccess());

  return makeParsedResult(builder.build(), status);
}

ParserStatus Parser::parseGenericWhereClause(
    SourceLoc &whereLoc, SmallVectorImpl<RequirementRepr> &requirements,
    bool &FirstTypeInComplete, bool AllowLayoutConstraints) {
  auto loc = leadingTriviaLoc();
  auto syntaxResult = parseGenericWhereClauseSyntax(FirstTypeInComplete,
                                                    AllowLayoutConstraints);
  if (syntaxResult.isNull())
    return syntaxResult.getStatus();

  SyntaxContext->addSyntax(syntaxResult.get());
  auto clause = SyntaxContext->topNode<GenericWhereClauseSyntax>();

  whereLoc = Generator.generate(clause.getWhereKeyword(), loc);
  requirements.reserve(clause.getRequirementList().size());
  for (auto elem : clause.getRequirementList()) {
    if (auto req = Generator.generate(elem, loc))
      requirements.push_back(*req);
  }

  return syntaxResult.getStatus();
}

/// Parse a free-standing where clause attached to a declaration, adding it to
/// a generic parameter list that may (or may not) already exist.
ParserStatus Parser::
parseFreestandingGenericWhereClause(GenericParamList *genericParams,
                                    WhereClauseKind kind) {
  assert(Tok.is(tok::kw_where) && "Shouldn't call this without a where");
  
  // Push the generic arguments back into a local scope so that references will
  // find them.
  Scope S(this, ScopeKind::Generics);
  
  if (genericParams)
    for (auto pd : genericParams->getParams())
      addToScope(pd);
  
  SmallVector<RequirementRepr, 4> Requirements;
  SourceLoc WhereLoc;
  bool FirstTypeInComplete;
  auto result = parseGenericWhereClause(WhereLoc, Requirements,
                                        FirstTypeInComplete);
  if (result.shouldStopParsing() || Requirements.empty())
    return result;

  if (!genericParams)
    diagnose(WhereLoc, diag::where_without_generic_params, unsigned(kind));
  else
    genericParams->addTrailingWhereClause(Context, WhereLoc, Requirements);
  return ParserStatus();
}

/// Parse a where clause after a protocol or associated type declaration.
ParserStatus Parser::parseProtocolOrAssociatedTypeWhereClause(
    TrailingWhereClause *&trailingWhere, bool isProtocol) {
  assert(Tok.is(tok::kw_where) && "Shouldn't call this without a where");
  SourceLoc whereLoc;
  SmallVector<RequirementRepr, 4> requirements;
  bool firstTypeInComplete;
  auto whereStatus =
      parseGenericWhereClause(whereLoc, requirements, firstTypeInComplete);
  if (whereStatus.isSuccess()) {
    trailingWhere =
        TrailingWhereClause::create(Context, whereLoc, requirements);
  } else if (whereStatus.hasCodeCompletion()) {
    return whereStatus;
  }

  return ParserStatus();
}
