//===--- ParseGeneric.cpp - Swift Language Parser for Generics ------------===//
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
      if (Tok.getKind() == tok::identifier ||
          Tok.getKind() == tok::code_complete) {
        Ty = parseTypeIdentifier();
      } else if (Tok.getKind() == tok::kw_protocol) {
        Ty = parseTypeComposition();
      } else if (Tok.getKind() == tok::kw_class) {
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
                                                    NameLoc, /*Depth=*/0,
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
  if (!startsWithGreater(Tok)) {
    if (!Invalid) {
      diagnose(Tok, diag::expected_rangle_generics_param);
      diagnose(LAngleLoc, diag::opening_angle);
      
      Invalid = true;
    }
    
    // Skip until we hit the '>'.
    skipUntilGreaterInTypeList();
    if (startsWithGreater(Tok))
      RAngleLoc = consumeStartingGreater();
    else
      RAngleLoc = Tok.getLoc();
  } else {
    RAngleLoc = consumeStartingGreater();
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
               bool &FirstTypeInComplete) {
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

      // Parse the protocol or composition.
      ParserResult<TypeRepr> Protocol;
      if (Tok.is(tok::kw_protocol)) {
        Protocol = parseTypeComposition();
      } else {
        Protocol = parseTypeIdentifier();
      }
      if (Protocol.isNull()) {
        Status.setIsParseError();
        if (Protocol.hasCodeCompletion())
          Status.setHasCodeCompletion();
        break;
      }

      // Add the requirement.
      Requirements.push_back(RequirementRepr::getTypeConstraint(FirstType.get(),
                                                     ColonLoc, Protocol.get()));
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

  return Status;
}
