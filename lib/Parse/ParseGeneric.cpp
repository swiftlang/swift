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
/// < T : Comparable, U : Container>
///
///   generic-params:
///     '<' generic-param (',' generic-param)? '>'
///
///   generic-param:
///     identifier
///     identifier ':' type-identifier
///     identifier ':' type-composition
GenericParamList *Parser::parseGenericParameters() {
  // Parse the opening '<'.
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  SourceLoc LAngleLoc = consumeStartingLess();

  // Parse the generic parameter list.
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
      = new (Context) TypeAliasDecl(NameLoc, Name, NameLoc, Type(),
                                    CurDeclContext,
                                    Context.AllocateCopy(Inherited));
    GenericParams.push_back(Param);
    
    // Parse the comma, if the list continues.
    if (Tok.is(tok::comma)) {
      consumeToken();
      continue;
    }

    break;
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

  return GenericParamList::create(Context, LAngleLoc, GenericParams, RAngleLoc);
}

GenericParamList *Parser::maybeParseGenericParamsIntoScope(
                              Optional<Scope>& scope,
                              bool AllowLookup) {
  if (!startsWithLess(Tok))
    return nullptr;

  GenericParamList *GenericParams = parseGenericParameters();

  // If there were any generic parameters, introduce the new scope to
  // hold those generic parameters.
  if (GenericParams) {
    scope.emplace(this, AllowLookup);
    
    for (auto Param : *GenericParams) {
      ScopeInfo.addToScope(Param.getDecl());
    }
  }

  return GenericParams;
}
