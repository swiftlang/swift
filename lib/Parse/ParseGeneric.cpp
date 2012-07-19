//===--- ParseGeneric.cpp - Swift Language Parser for Generics ------------===//
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
// Generic Parsing and AST Building
//
//===----------------------------------------------------------------------===//
#include "Parser.h"
#include "swift/AST/Diagnostics.h"
#include "swift/Parse/Lexer.h"
using namespace swift;

/// parseGenericParameters - Parse a sequence of generic parameters, e.g.,
/// < T : Comparable, U : Container> along with an optional requires clause.
///
///   generic-params:
///     '<' generic-param (',' generic-param)? requires-clause? '>'
///
///   generic-param:
///     identifier
///     identifier ':' type-identifier
///     identifier ':' type-composition
///
/// When parsing the generic parameters, this routine establishes a new scope
/// and adds those parameters to the scope.
GenericParamList *Parser::parseGenericParameters(Optional<Scope>& scope,
                                                 bool AllowLookup) {
  // Parse the opening '<'.
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  SourceLoc LAngleLoc = consumeStartingLess();

  // Parse the generic parameter list.
  // FIXME: Allow a bare 'requires' clause with no generic parameters?
  SmallVector<GenericParam, 4> GenericParams;
  bool Invalid = false;
  while (true) {
    // Parse the name of the parameter.
    Identifier Name;
    SourceLoc NameLoc = Tok.getLoc();
    if (parseIdentifier(Name, diag::expected_generics_parameter_name)) {
      Invalid = true;
      break;
    }

    // Parse the ':' followed by a type.
    SmallVector<Type, 1> Inherited;
    if (Tok.is(tok::colon)) {
      (void)consumeToken();
      Type Ty;
      TypeLoc *TyLoc = nullptr;
      if (Tok.getKind() == tok::identifier) {
        parseTypeIdentifier(Ty, TyLoc);
      } else if (Tok.getKind() == tok::kw_protocol) {
        parseTypeComposition(Ty, TyLoc);
      } else {
        diagnose(Tok.getLoc(), diag::expected_generics_type_restriction, Name);
        Invalid = true;
      }

      if (Ty)
        Inherited.push_back(Ty);
    }

    // FIXME: Bad location info here
    TypeAliasDecl *Param
      = new (Context) TypeAliasDecl(NameLoc, Name, NameLoc, Type(), nullptr,
                                    CurDeclContext,
                                    Context.AllocateCopy(Inherited));
    GenericParams.push_back(Param);

    // If we haven't built a scope yet, do so now.
    if (!scope) {
      scope.emplace(this, AllowLookup);
    }

    // Add this parameter to the scope.
    ScopeInfo.addToScope(Param);

    // Parse the comma, if the list continues.
    if (Tok.is(tok::comma)) {
      consumeToken();
      continue;
    }

    break;
  }

  // Parse the optional requires-clause.
  SourceLoc RequiresLoc;
  SmallVector<Requirement, 4> Requirements;
  if (Tok.is(tok::kw_requires) &&
      parseRequiresClause(RequiresLoc, Requirements)) {
    Invalid = true;
  }
  
  // Parse the closing '>'.
  SourceLoc RAngleLoc;
  if (!startsWithGreater(Tok)) {
    if (!Invalid) {
      diagnose(Tok.getLoc(), diag::expected_rangle_generics_param);
      diagnose(LAngleLoc, diag::opening_angle);
      
      Invalid = true;
    }
    
    // Skip until we hit the '>'.
    skipUntilAnyOperator();
    if (startsWithGreater(Tok))
      RAngleLoc = consumeStartingGreater();
    else
      RAngleLoc = Tok.getLoc();
  } else {
    RAngleLoc = consumeStartingGreater();
  }

  if (GenericParams.empty())
    return nullptr;

  return GenericParamList::create(Context, LAngleLoc, GenericParams,
                                  RequiresLoc, Requirements, RAngleLoc);
}

GenericParamList *Parser::maybeParseGenericParams(Optional<Scope>& scope,
                                                  bool AllowLookup) {
  if (!startsWithLess(Tok))
    return nullptr;

  return parseGenericParameters(scope, AllowLookup);
}

/// parseRequiresClause - Parse a requires clause, which places additional
/// constraints on generic parameters or types based on them.
///
///   requires-clause:
///     'requires' requirement (',' requirement) *
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
///     type-identifier '==' type-identifier
bool Parser::parseRequiresClause(SourceLoc &RequiresLoc,
                                 SmallVectorImpl<Requirement> &Requirements) {
  // Parse the 'requires'.
  RequiresLoc = consumeToken(tok::kw_requires);
  bool Invalid = false;
  while (true) {
    // Parse the leading type-identifier.
    // FIXME: Dropping TypeLocs left and right.
    Type FirstType;
    TypeLoc *FirstTypeLoc = nullptr;
    if (parseTypeIdentifier(FirstType, FirstTypeLoc)) {
      Invalid = true;
      break;
    }

    if (Tok.is(tok::colon)) {
      // A conformance-requirement.
      SourceLoc ColonLoc = consumeToken();

      // Parse the protocol or composition.
      Type Protocol;
      TypeLoc *ProtocolLoc = nullptr;
      if (Tok.is(tok::kw_protocol)) {
        if (parseTypeComposition(Protocol, ProtocolLoc)) {
          Invalid = true;
          break;
        }
      } else if (parseTypeIdentifier(Protocol, ProtocolLoc)) {
        Invalid = true;
        break;
      }

      // Add the requirement.
      Requirements.push_back(Requirement::getConformance(FirstType, ColonLoc,
                                                         Protocol));

      // If there's a comma, keep parsing the list.
      if (Tok.is(tok::comma)) {
        consumeToken();
        continue;
      }

      break;
    }

    if ((Tok.isAnyOperator() && Tok.getText() == "==") || Tok.is(tok::equal)) {
      // A same-type-requirement
      if (Tok.is(tok::equal)) {
        // FIXME: Fix-It here!
        diagnose(Tok, diag::requires_single_equal);
      }
      SourceLoc EqualLoc = consumeToken();

      // Parse the second type.
      Type SecondType;
      TypeLoc *SecondTypeLoc = nullptr;
      if (parseTypeIdentifier(SecondType, SecondTypeLoc)) {
        Invalid = true;
        break;
      }

      // Add the requirement
      Requirements.push_back(Requirement::getSameType(FirstType, EqualLoc,
                                                      SecondType));

      // If there's a comma, keep parsing the list.
      if (Tok.is(tok::comma)) {
        consumeToken();
        continue;
      }

      break;
    }

    diagnose(Tok, diag::expected_requirement_delim);
    break;
  }

  return Invalid;
}
