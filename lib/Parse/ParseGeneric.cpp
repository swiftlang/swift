//===--- ParseGeneric.cpp - Swift Language Parser for Generics ------------===//
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
// Generic Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Parse/Lexer.h"
using namespace swift;

/// parseGenericParameters - Parse a sequence of generic parameters, e.g.,
/// < T : Comparable, U : Container> along with an optional requires clause.
///
///   generic-params:
///     '<' generic-param (',' generic-param)* where-clause? '>'
///
///   generic-param:
///     identifier
///     identifier ':' type-identifier
///     identifier ':' type-composition
///
/// When parsing the generic parameters, this routine establishes a new scope
/// and adds those parameters to the scope.
ParserResult<GenericParamList> Parser::parseGenericParameters() {
  // Parse the opening '<'.
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  return parseGenericParameters(consumeStartingLess());
}

ParserResult<GenericParamList>
Parser::parseGenericParameters(SourceLoc LAngleLoc) {
  // Parse the generic parameter list.
  SmallVector<GenericTypeParamDecl *, 4> GenericParams;
  bool Invalid = false;
  do {
    // Note that we're parsing a declaration.
    StructureMarkerRAII ParsingDecl(*this, Tok.getLoc(),
                                    StructureMarkerKind::Declaration);

    // Parse attributes.
    DeclAttributes attributes;
    if (Tok.hasComment())
      attributes.add(new (Context) RawDocCommentAttr(Tok.getCommentRange()));
    bool foundCCTokenInAttr;
    parseDeclAttributeList(attributes, foundCCTokenInAttr);

    // Parse the name of the parameter.
    Identifier Name;
    SourceLoc NameLoc;
    if (parseIdentifier(Name, NameLoc,
                        diag::expected_generics_parameter_name)) {
      Invalid = true;
      break;
    }

    // Parse the ':' followed by a type.
    SmallVector<TypeLoc, 1> Inherited;
    if (Tok.is(tok::colon)) {
      (void)consumeToken();
      ParserResult<TypeRepr> Ty;
      
      if (Tok.isAny(tok::identifier, tok::code_complete, tok::kw_protocol, tok::kw_Any)) {
        Ty = parseTypeForInheritance(diag::expected_identifier_for_type,
                                     diag::expected_ident_type_in_inheritance);
      } else if (Tok.is(tok::kw_class)) {
        diagnose(Tok, diag::unexpected_class_constraint);
        diagnose(Tok, diag::suggest_anyobject, Name)
          .fixItReplace(Tok.getLoc(), "AnyObject");
        consumeToken();
        Invalid = true;
      } else {
        diagnose(Tok, diag::expected_generics_type_restriction, Name);
        Invalid = true;
      }

      if (Ty.hasCodeCompletion())
        return makeParserCodeCompletionStatus();

      if (Ty.isNonNull())
        Inherited.push_back(Ty.get());
    }

    // We always create generic type parameters with a depth of zero.
    // Semantic analysis fills in the depth when it processes the generic
    // parameter list.
    auto Param = new (Context) GenericTypeParamDecl(CurDeclContext, Name,
                                                    NameLoc,
                                                    /*Bogus depth=*/0xFFFF,
                                                    GenericParams.size());
    if (!Inherited.empty())
      Param->setInherited(Context.AllocateCopy(Inherited));
    GenericParams.push_back(Param);

    // Attach attributes.
    Param->getAttrs() = attributes;

    // Add this parameter to the scope.
    addToScope(Param);

    // Parse the comma, if the list continues.
  } while (consumeIf(tok::comma));

  // Parse the optional where-clause.
  SourceLoc WhereLoc;
  SmallVector<RequirementRepr, 4> Requirements;
  bool FirstTypeInComplete;
  if (Tok.is(tok::kw_where) &&
      parseGenericWhereClause(WhereLoc, Requirements,
                              FirstTypeInComplete).isError()) {
    Invalid = true;
  }
  
  // Parse the closing '>'.
  SourceLoc RAngleLoc;
  if (startsWithGreater(Tok)) {
    RAngleLoc = consumeStartingGreater();
  } else {
    if (!Invalid) {
      diagnose(Tok, diag::expected_rangle_generics_param);
      diagnose(LAngleLoc, diag::opening_angle);
      Invalid = true;
    }

    // Skip until we hit the '>'.
    RAngleLoc = skipUntilGreaterInTypeList();
  }

  if (GenericParams.empty())
    return nullptr;

  return makeParserResult(GenericParamList::create(Context, LAngleLoc,
                                                   GenericParams, WhereLoc,
                                                   Requirements, RAngleLoc));
}

ParserResult<GenericParamList> Parser::maybeParseGenericParams() {
  if (!startsWithLess(Tok))
    return nullptr;

  if (!isInSILMode())
    return parseGenericParameters();

  // In SIL mode, we can have multiple generic parameter lists, with the
  // first one being the outmost generic parameter list.
  GenericParamList *gpl = nullptr, *outer_gpl = nullptr;
  do {
    gpl = parseGenericParameters().getPtrOrNull();
    if (!gpl)
      return nullptr;

    if (outer_gpl)
      gpl->setOuterParameters(outer_gpl);
    outer_gpl = gpl;
  } while (startsWithLess(Tok));
  return makeParserResult(gpl);
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

/// parseGenericWhereClause - Parse a 'where' clause, which places additional
/// constraints on generic parameters or types based on them.
///
///   where-clause:
///     'where' requirement (',' requirement) *
///
///   requirement:
///     conformance-requirement
///     same-type-requirement
///
///   conformance-requirement:
///     type-identifier ':' type-identifier
///     type-identifier ':' type-composition
///
///   same-type-requirement:
///     type-identifier '==' type
ParserStatus Parser::parseGenericWhereClause(
               SourceLoc &WhereLoc,
               SmallVectorImpl<RequirementRepr> &Requirements,
               bool &FirstTypeInComplete,
               bool AllowLayoutConstraints) {
  ParserStatus Status;
  // Parse the 'where'.
  WhereLoc = consumeToken(tok::kw_where);
  FirstTypeInComplete = false;
  do {
    // Parse the leading type-identifier.
    ParserResult<TypeRepr> FirstType = parseTypeIdentifier();
    if (FirstType.isNull()) {
      Status.setIsParseError();
      if (FirstType.hasCodeCompletion()) {
        Status.setHasCodeCompletion();
        FirstTypeInComplete = true;
      }
      break;
    }

    if (Tok.is(tok::colon)) {
      // A conformance-requirement.
      SourceLoc ColonLoc = consumeToken();

      if (Tok.is(tok::identifier) &&
          getLayoutConstraint(Context.getIdentifier(Tok.getText()), Context)
              ->isKnownLayout()) {
        // Parse a layout constraint.
        auto LayoutName = Context.getIdentifier(Tok.getText());
        auto LayoutLoc = consumeToken();
        auto LayoutInfo = parseLayoutConstraint(LayoutName);
        if (!LayoutInfo->isKnownLayout()) {
          // There was a bug in the layout constraint.
          Status.setIsParseError();
        }
        auto Layout = LayoutInfo;
        // Types in SIL mode may contain layout constraints.
        if (!AllowLayoutConstraints && !isInSILMode()) {
          diagnose(LayoutLoc,
                   diag::layout_constraints_only_inside_specialize_attr);
        } else {
          // Add the layout requirement.
          Requirements.push_back(RequirementRepr::getLayoutConstraint(
              FirstType.get(), ColonLoc,
              LayoutConstraintLoc(Layout, LayoutLoc)));
        }
      } else {
        // Parse the protocol or composition.
        ParserResult<TypeRepr> Protocol =
            parseTypeForInheritance(diag::expected_identifier_for_type,
                                    diag::expected_ident_type_in_inheritance);

        if (Protocol.isNull()) {
          Status.setIsParseError();
          if (Protocol.hasCodeCompletion())
            Status.setHasCodeCompletion();
          break;
        }

        // Add the requirement.
        Requirements.push_back(RequirementRepr::getTypeConstraint(
            FirstType.get(), ColonLoc, Protocol.get()));
      }
    } else if ((Tok.isAnyOperator() && Tok.getText() == "==") ||
               Tok.is(tok::equal)) {
      // A same-type-requirement
      if (Tok.is(tok::equal)) {
        diagnose(Tok, diag::requires_single_equal)
          .fixItReplace(SourceRange(Tok.getLoc()), "==");
      }
      SourceLoc EqualLoc = consumeToken();

      // Parse the second type.
      ParserResult<TypeRepr> SecondType = parseType();
      if (SecondType.isNull()) {
        Status.setIsParseError();
        if (SecondType.hasCodeCompletion())
          Status.setHasCodeCompletion();
        break;
      }

      // Add the requirement
      Requirements.push_back(RequirementRepr::getSameType(FirstType.get(),
                                                      EqualLoc,
                                                      SecondType.get()));
    } else {
      diagnose(Tok, diag::expected_requirement_delim);
      Status.setIsParseError();
      break;
    }
    // If there's a comma, keep parsing the list.
  } while (consumeIf(tok::comma));

  if (Requirements.empty())
    WhereLoc = SourceLoc();

  return Status;
}


/// Parse a free-standing where clause attached to a declaration, adding it to
/// a generic parameter list that may (or may not) already exist.
ParserStatus Parser::
parseFreestandingGenericWhereClause(GenericParamList *&genericParams,
                                    WhereClauseKind kind) {
  assert(Tok.is(tok::kw_where) && "Shouldn't call this without a where");
  
  // Push the generic arguments back into a local scope so that references will
  // find them.
  Scope S(this, ScopeKind::Generics);
  
  if (genericParams)
    for (auto pd : genericParams->getParams())
      addToScope(pd);
  
  SmallVector<RequirementRepr, 4> Requirements;
  if (genericParams)
    Requirements.append(genericParams->getRequirements().begin(),
                        genericParams->getRequirements().end());
  
  SourceLoc WhereLoc;
  bool FirstTypeInComplete;
  auto result = parseGenericWhereClause(WhereLoc, Requirements,
                                        FirstTypeInComplete);
  if (result.shouldStopParsing() || Requirements.empty())
    return result;

  if (!genericParams)
    diagnose(WhereLoc, diag::where_without_generic_params, unsigned(kind));
  else
    genericParams = GenericParamList::create(Context,
                                             genericParams->getLAngleLoc(),
                                             genericParams->getParams(),
                                             WhereLoc, Requirements,
                                             genericParams->getRAngleLoc());
  return ParserStatus();
}

