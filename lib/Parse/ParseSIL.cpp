//===--- ParseSIL.cpp - SIL File Parsing logic ----------------------------===//
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

#include "Parser.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/SILModule.h"
using namespace swift;


/// parseSILLinkage - Parse a linkage specifier if present.
///   sil-linkage:
///     /*empty*/           // defaults to external linkage.
///     'internal'
///     'clang_thunk'
static bool parseSILLinkage(SILLinkage &Result, Parser &P) {
  if (P.Tok.isNot(tok::identifier)) {
    Result = SILLinkage::External;
  } else if (P.Tok.getText() == "internal") {
    Result = SILLinkage::Internal;
    P.consumeToken(tok::identifier);
  } else if (P.Tok.getText() == "clang_thunk") {
    Result = SILLinkage::ClangThunk;
    P.consumeToken(tok::identifier);
  } else {
    P.diagnose(P.Tok, diag::expected_sil_linkage_or_function);
    return true;
  }
  return false;
}


///   decl-sil:   [[only in SIL mode]]
///     'sil' sil-linkage '@' identifier ':' sil-type decl-sil-body
///   decl-sil-body:
///     '{' sil-basic-block+ '}'
bool Parser::parseDeclSIL() {
  // Inform the lexer that we're lexing the body of the SIL declaration.  Do
  // this before we consume the 'sil' token so that all later tokens are
  // properly handled.
  Lexer::SILBodyRAII Tmp(*L);

  consumeToken(tok::kw_sil);

  SILLinkage FnLinkage;
  Identifier FnName;
  SILType FnType;

  if (parseSILLinkage(FnLinkage, *this) ||
      parseToken(tok::sil_at_sign, diag::expected_sil_function_name) ||
      parseIdentifier(FnName, diag::expected_sil_function_name) ||
      parseToken(tok::colon, diag::expected_sil_type) ||
      parseSILType(FnType))
    return true;

  SILFunction *Fn = new (*SIL) SILFunction(*SIL, FnLinkage,
                                           FnName.str(), FnType);
  (void)Fn;

  // Now that we have a SILFunction parse the body, if present.

  SourceLoc LBraceLoc = Tok.getLoc();
  if (consumeIf(tok::l_brace)) {
    // TODO: parse the body.

    SourceLoc RBraceLoc;
    parseMatchingToken(tok::r_brace, RBraceLoc, diag::expected_sil_rbrace,
                       LBraceLoc);
  }


  // TODO: Verify it is a function type.

  return false;
}

///   sil-type:
///     '$' sil-type-attributes? '*'? type
///   sil-type-attributes:
///     '[' sil-type-attribute (',' sil-type-attribute)* ']'
///   sil-type-attribute:
///     'sil_sret'
///     'sil_uncurry' '=' integer_literal
///
bool Parser::parseSILType(SILType &Result) {
  bool IsSRet = false;
  unsigned UncurryLevel = 0;

  // If we have sil-type-attribute list, parse it.
  if (Tok.is(tok::l_square) && peekToken().is(tok::identifier) &&
      peekToken().getText().startswith("sil_")) {
    SourceLoc LeftLoc = Tok.getLoc(), RightLoc;

    // The attribute list is always guaranteed to have at least one attribute.
    do {
      consumeToken();

      SourceLoc AttrTokenLoc = Tok.getLoc();
      Identifier AttrToken;
      if (parseIdentifier(AttrToken,
                          diag::expected_identifier_sil_type_attributes))
        return true;

      if (AttrToken.str() == "sil_sret") {
        IsSRet = true;
        consumeToken(tok::identifier);
      } else if (AttrToken.str() == "sil_uncurry") {
        if (parseToken(tok::identifier, diag::malformed_sil_uncurry_attribute)||
            parseToken(tok::equal, diag::malformed_sil_uncurry_attribute))
          return true;
        if (Tok.isNot(tok::integer_literal) ||
            Tok.getText().getAsInteger(10, UncurryLevel)) {
          diagnose(Tok, diag::malformed_sil_uncurry_attribute);
          return true;
        }

        consumeToken(tok::integer_literal);
      } else {
        diagnose(AttrTokenLoc, diag::unknown_sil_type_attribute);
        return true;
      }

      // Continue parsing the next token.
    } while (Tok.is(tok::comma));

    if (parseMatchingToken(tok::r_square, RightLoc,
                           diag::expected_bracket_sil_type_attributes, LeftLoc))
      return true;
  }

  // If we have a '*', then this is an address type.
  bool isAddress = false;
  if (Tok.isAnyOperator() && Tok.getText() == "*") {
    isAddress = true;
    consumeToken();
  }

  TypeLoc Ty;
  if (parseToken(tok::sil_dollar, diag::expected_sil_type) ||
      parseType(Ty, diag::expected_sil_type))
    return true;

  // FIXME: Stop using TypeConverter when SILType for functions doesn't contain
  // SILTypes itself.
  (void)IsSRet;
  Result = SIL->Types.getLoweredType(Ty.getType(), UncurryLevel);
  
//  performNameBinding(TU);
//  performTypeChecking(TU);

  
  return false;
}
