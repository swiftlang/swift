//===--- ParsePattern.cpp - Swift Language Parser for Patterns ------------===//
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
// Pattern Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ExprHandle.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;

/// \brief Determine the kind of a default argument given a parsed
/// expression that has not yet been type-checked.
static DefaultArgumentKind getDefaultArgKind(ExprHandle *init) {
  if (!init || !init->getExpr())
    return DefaultArgumentKind::None;

  auto magic = dyn_cast<MagicIdentifierLiteralExpr>(init->getExpr());
  if (!magic)
    return DefaultArgumentKind::Normal;

  switch (magic->getKind()) {
  case MagicIdentifierLiteralExpr::Column:
    return DefaultArgumentKind::Column;
  case MagicIdentifierLiteralExpr::File:
    return DefaultArgumentKind::File;
  case MagicIdentifierLiteralExpr::Line:
    return DefaultArgumentKind::Line;
  case MagicIdentifierLiteralExpr::Function:
    return DefaultArgumentKind::Function;
  }
}

static void recoverFromBadSelectorArgument(Parser &P) {
  while (P.Tok.isNot(tok::eof) && P.Tok.isNot(tok::r_paren) &&
         P.Tok.isNot(tok::l_brace) && P.Tok.isNot(tok::r_brace) &&
         !P.isStartOfStmt(P.Tok) &&
         !P.isStartOfDecl(P.Tok, P.peekToken())) {
    P.skipSingle();
  }
  P.consumeIf(tok::r_paren);
}

void Parser::DefaultArgumentInfo::setFunctionContext(DeclContext *DC) {
  assert(DC->isLocalContext());
  for (auto context : ParsedContexts) {
    context->changeFunction(DC);
  }
}

static ParserStatus parseDefaultArgument(Parser &P,
                                   Parser::DefaultArgumentInfo *defaultArgs,
                                   unsigned argIndex,
                                   ExprHandle *&init) {
  SourceLoc equalLoc = P.consumeToken(tok::equal);

  // Enter a fresh default-argument context with a meaningless parent.
  // We'll change the parent to the function later after we've created
  // that declaration.
  auto initDC =
    P.Context.createDefaultArgumentContext(P.CurDeclContext, argIndex);
  Parser::ParseFunctionBody initScope(P, initDC);

  ParserResult<Expr> initR = P.parseExpr(diag::expected_init_value);

  // Give back the default-argument context if we didn't need it.
  if (!initScope.hasClosures()) {
    P.Context.destroyDefaultArgumentContext(initDC);

  // Otherwise, record it if we're supposed to accept default
  // arguments here.
  } else if (defaultArgs) {
    defaultArgs->ParsedContexts.push_back(initDC);
  }

  if (!defaultArgs) {
    auto inFlight = P.diagnose(equalLoc, diag::non_func_decl_pattern_init);
    if (initR.isNonNull())
      inFlight.fixItRemove(SourceRange(equalLoc, initR.get()->getEndLoc()));
  }

  if (initR.hasCodeCompletion()) {
    recoverFromBadSelectorArgument(P);
    return makeParserCodeCompletionStatus();
  }
  if (initR.isNull()) {
    recoverFromBadSelectorArgument(P);
    return makeParserError();
  }

  init = ExprHandle::get(P.Context, initR.get());
  return ParserStatus();
}

/// Given a pattern "P" based on a pattern atom (either an identifer or _
/// pattern), rebuild and return the nested pattern around another root that
/// replaces the atom.
static Pattern *rebuildImplicitPatternAround(const Pattern *P, Pattern *NewRoot,
                                             ASTContext &C) {
  // We'll return a cloned copy of the pattern.
  Pattern *Result = P->clone(C, /*isImplicit*/true);

  class ReplaceRoot : public ASTWalker {
    Pattern *NewRoot;
  public:
    ReplaceRoot(Pattern *NewRoot) : NewRoot(NewRoot) {}

    // If we find a typed pattern, replace its subpattern with the NewRoot and
    // return.
    std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
      if (auto *TP = dyn_cast<TypedPattern>(P)) {
        TP->setSubPattern(NewRoot);
        return { false, TP };
      }
      
      return { true, P };
    }

    // If we get down to a named pattern "x" or any pattern "_", replace it
    // with our root.
    Pattern *walkToPatternPost(Pattern *P) override {
      if (isa<NamedPattern>(P) || isa<AnyPattern>(P))
        return NewRoot;
      return P;
    }
  };
  
  return Result->walk(ReplaceRoot(NewRoot));
}

/// Parse a single argument, the leading token is expected to be a (.
static ParserResult<Pattern> parseArgument(Parser &P, Identifier leadingIdent,
                                     Parser::DefaultArgumentInfo *defaultArgs) {
  // Consume the (.
  Parser::StructureMarkerRAII ParsingArgument(P, P.Tok);
  SourceLoc LPLoc = P.consumeToken(tok::l_paren);

  // Decide if this is a singular unnamed argument (e.g. "foo(Int)" or if
  // it is a standard tuple body pattern (e.g. "foo(x : Int)").  The former is
  // shorthand where the elements get the name of the selector chunk.  The
  // later includes the names are specified for each piece.

  // Before doing a speculative parse, check for the common cases of
  // "identifier:" (always named), "identifier)" (always unnamed), and
  // "identifier =" (always unnamed).  We know to parse the second identifier in
  // "identifier(identifier = ...)" as a type because all arguments are required
  // to have types.
  bool isImpliedNameArgument;

  if (P.Tok.is(tok::identifier) && P.peekToken().is(tok::colon))
    isImpliedNameArgument = false;
  else if (P.Tok.is(tok::identifier) && P.peekToken().is(tok::r_paren))
    isImpliedNameArgument = true;
  else if (P.Tok.is(tok::identifier) && P.peekToken().is(tok::equal))
    isImpliedNameArgument = true;
  else if (P.Tok.is(tok::l_paren)) {
    // Nested tuple values like "(a : Int, b: Int)" destructure the argument
    // further, so never parse them as an implied name.  However, function types
    // like "() -> Int" are implied name arguments.  Speculatively parse to
    // disambiguate the cases.

    // Otherwise, we do a full speculative parse to determine this.
    Parser::BacktrackingScope backtrack(P);
    P.consumeToken(tok::l_paren);
    
    isImpliedNameArgument = P.canParseTypeTupleBody() &&
        P.Tok.isNot(tok::r_paren) && P.Tok.isNot(tok::colon) &&
        P.Tok.isNot(tok::comma);
  } else {
    // Otherwise, we do a full speculative parse to determine this.
    Parser::BacktrackingScope backtrack(P);

    // Allow "identifier(inout Type)"
    if (P.Tok.isContextualKeyword("inout"))
      P.consumeToken(tok::identifier);

    // This is type-only if it is a valid type followed by an r_paren or equal.
    isImpliedNameArgument = P.canParseType();
    if (isImpliedNameArgument)
      isImpliedNameArgument = P.Tok.is(tok::r_paren) || P.Tok.is(tok::equal);
  }

  // If this is a standard tuple, parse it.
  if (!isImpliedNameArgument)
    return P.parsePatternTupleAfterLP(/*IsLet*/true, /*IsArgList*/true,
                                      LPLoc, /*DefArgs=*/defaultArgs);

  SourceLoc ArgStartLoc = P.Tok.getLoc();
  
  // Create the patterns for the identifier.
  Pattern *Name;
  if (leadingIdent.empty())
    Name = new (P.Context) AnyPattern(ArgStartLoc, /*Implicit=*/true);
  else
    Name = P.createBindingFromPattern(ArgStartLoc, leadingIdent, /*isLet*/true);
  Name->setImplicit();


  ParserStatus EltStatus;
  Optional<TuplePatternElt> elto;
  std::tie(EltStatus, elto) =
    P.parsePatternTupleElement(/*isLet*/true, /*isArgumentList*/true,
                               Name, defaultArgs);

  if (EltStatus.hasCodeCompletion())
    return makeParserCodeCompletionResult<Pattern>();
  if (!elto)
    return makeParserError();
  TuplePatternElt elt = elto.getValue();

  // If we found our r_paren, we're done.
  SourceLoc RPLoc = P.Tok.getLoc();
  if (P.consumeIf(tok::r_paren)) {
    auto *Res = TuplePattern::createSimple(P.Context, LPLoc, elt, RPLoc);
    return makeParserResult(Res);
  }
  
  // If not, we must have a default value, and we haven't validated that there
  // is a single argument above (because we don't have a "canParseExpr" to check
  // that the default value is valid).
  //
  // If we have a ",", then reject the code with a specific error and recover.
  // Otherwise, emit a generic error.
  if (!P.consumeIf(tok::comma)) {
    P.diagnose(P.Tok, diag::expected_rparen_parameter);
    return makeParserError();
  }
  
  P.diagnose(ArgStartLoc, diag::implied_name_multiple_parameters)
    .fixItInsert(ArgStartLoc, "_: ");
  return P.parsePatternTupleAfterLP(/*IsLet*/true, /*IsArgList*/true,
                                    LPLoc, /*DefArgs=*/defaultArgs);
}


static ParserStatus
parseSelectorArgument(Parser &P,
                      SmallVectorImpl<Identifier> &namePieces,
                      SmallVectorImpl<TuplePatternElt> &argElts,
                      SmallVectorImpl<TuplePatternElt> &bodyElts,
                      Parser::DefaultArgumentInfo &defaultArgs,
                      SourceLoc &rp) {
  ParserResult<Pattern> ArgPatternRes = P.parsePatternIdentifier(true);
  assert(ArgPatternRes.isNonNull() &&
         "selector argument did not start with an identifier or _!");
  Pattern *ArgPattern = ArgPatternRes.get();
  ArgPattern->setImplicit();

  Identifier leadingIdent;

  // Check that a selector name isn't used multiple times, which would
  // lead to the function type having multiple arguments with the same name.
  if (NamedPattern *name = dyn_cast<NamedPattern>(ArgPattern)) {
    VarDecl *decl = name->getDecl();
    decl->setImplicit();
    leadingIdent = name->getDecl()->getName();
  } else {
    // If the selector is named "_", then we ignore it.
    assert(isa<AnyPattern>(ArgPattern) && "Unexpected selector pattern");
  }
  
  namePieces.push_back(leadingIdent);
  
  if (!P.Tok.is(tok::l_paren)) {
    P.diagnose(P.Tok, diag::func_selector_without_paren);
    return makeParserError();
  }

  auto PatternRes = parseArgument(P, leadingIdent, &defaultArgs);
  if (PatternRes.hasCodeCompletion())
    return PatternRes;
  if (PatternRes.isNull()) {
    if (PatternRes.isParseError())
      recoverFromBadSelectorArgument(P);
    return PatternRes;
  }
  
  // The result of parsing a '(' pattern is either a ParenPattern or a
  // TuplePattern.
  if (auto *PP = dyn_cast<ParenPattern>(PatternRes.get())) {
    bodyElts.push_back(TuplePatternElt(PP->getSubPattern(), /*init*/nullptr,
                                       DefaultArgumentKind::None));
    // Return the ')' location.
    rp = PP->getRParenLoc();
  } else {
    auto *TP = cast<TuplePattern>(PatternRes.get());
    
    // Reject tuple patterns that aren't a single argument.
    if (TP->getNumFields() != 1 || TP->hasVararg()) {
      P.diagnose(TP->getLParenLoc(), diag::func_selector_with_not_one_argument);
      return makeParserError();
    }

    bodyElts.push_back(TP->getFields()[0]);

    // Return the ')' location.
    rp = TP->getRParenLoc();
  }
  

  TuplePatternElt &TPE = bodyElts.back();
  ArgPattern = rebuildImplicitPatternAround(TPE.getPattern(), ArgPattern,
                                            P.Context);
  
  argElts.push_back(TuplePatternElt(ArgPattern, TPE.getInit(),
                                    getDefaultArgKind(TPE.getInit())));
  return makeParserSuccess();
}

static Pattern *getFirstSelectorPattern(ASTContext &Context,
                                        const Pattern *argPattern,
                                        SourceLoc loc) {
  Pattern *any = new (Context) AnyPattern(loc, /*Implicit=*/true);
  return rebuildImplicitPatternAround(argPattern, any, Context);
}


static ParserStatus
parseSelectorFunctionArguments(Parser &P,
                               SmallVectorImpl<Identifier> &NamePieces,
                               SmallVectorImpl<Pattern *> &ArgPatterns,
                               SmallVectorImpl<Pattern *> &BodyPatterns,
                               Parser::DefaultArgumentInfo &DefaultArgs,
                               Pattern *FirstPattern) {
  SourceLoc LParenLoc;
  SourceLoc RParenLoc;
  SmallVector<TuplePatternElt, 8> ArgElts;
  SmallVector<TuplePatternElt, 8> BodyElts;

  // For the argument pattern, try to convert the first parameter pattern to
  // an anonymous AnyPattern of the same type as the body parameter.
  if (ParenPattern *FirstParen = dyn_cast<ParenPattern>(FirstPattern)) {
    BodyElts.push_back(TuplePatternElt(FirstParen->getSubPattern()));
    LParenLoc = FirstParen->getLParenLoc();
    RParenLoc = FirstParen->getRParenLoc();
    ArgElts.push_back(TuplePatternElt(
        getFirstSelectorPattern(P.Context,
                                FirstParen->getSubPattern(),
                                FirstParen->getLoc())));
  } else if (TuplePattern *FirstTuple = dyn_cast<TuplePattern>(FirstPattern)) {
    LParenLoc = FirstTuple->getLParenLoc();
    RParenLoc = FirstTuple->getRParenLoc();
    if (FirstTuple->getNumFields() != 1)
      P.diagnose(P.Tok, diag::func_selector_with_not_one_argument);

    if (FirstTuple->getNumFields() >= 1) {
      const TuplePatternElt &FirstElt = FirstTuple->getFields()[0];
      BodyElts.push_back(FirstElt);
      ArgElts.push_back(TuplePatternElt(
          getFirstSelectorPattern(P.Context,
                                  FirstElt.getPattern(),
                                  FirstTuple->getLoc()),
        FirstElt.getInit(),
        FirstElt.getDefaultArgKind()));
    } else {
      // Recover by creating a '(_: ())' pattern.
      TuplePatternElt FirstElt(
          new (P.Context) TypedPattern(
              new (P.Context) AnyPattern(FirstTuple->getLParenLoc()),
              TupleTypeRepr::create(P.Context, {},
                                    FirstTuple->getSourceRange(),
                                    SourceLoc())));
      BodyElts.push_back(FirstElt);
      ArgElts.push_back(FirstElt);
    }
  } else
    llvm_unreachable("unexpected function argument pattern!");

  assert(!ArgElts.empty() && !BodyElts.empty());

  // Parse additional selectors as long as we can.
  ParserStatus Status;
  for (;;) {
    if (P.isAtStartOfBindingName()) {
      Status |= parseSelectorArgument(P, NamePieces,
                                      ArgElts, BodyElts, DefaultArgs,
                                      RParenLoc);
      continue;
    }
    if (P.Tok.is(tok::l_paren)) {
      P.diagnose(P.Tok, diag::func_selector_with_curry);
      // FIXME: better recovery: just parse a tuple instead of skipping tokens.
      P.skipUntilDeclRBrace(tok::l_brace);
      Status.setIsParseError();
    }
    break;
  }

  ArgPatterns.push_back(
      TuplePattern::create(P.Context, LParenLoc, ArgElts, RParenLoc,
                           /*hasVarArg=*/false,SourceLoc(), /*Implicit=*/true));
  BodyPatterns.push_back(
      TuplePattern::create(P.Context, LParenLoc, BodyElts, RParenLoc));
  return Status;
}

/// Parse function arguments.
///   func-arguments:
///     curried-arguments | selector-arguments
///   curried-arguments:
///     pattern-tuple+
///   selector-arguments:
///     '(' selector-element ')' (identifier '(' selector-element ')')+
///   selector-element:
///      identifier '(' pattern-atom (':' type-annotation)? ('=' expr)? ')'
///
ParserStatus
Parser::parseFunctionArguments(SmallVectorImpl<Identifier> &NamePieces,
                               SmallVectorImpl<Pattern *> &ArgPatterns,
                               SmallVectorImpl<Pattern *> &BodyPatterns,
                               DefaultArgumentInfo &DefaultArgs,
                               bool &HasSelectorStyleSignature) {
  // Parse all of the selector arguments or curried argument pieces.

  // Parse the first function argument clause.
  ParserResult<Pattern> ArgPattern =
    parseArgument(*this, NamePieces.front(), &DefaultArgs);

  if (ArgPattern.isNull() || ArgPattern.hasCodeCompletion())
    return ArgPattern;

  // If we've reached the end of the first argument, decide whether this is
  // a curried argument list, a selector style argument list, or if we're
  // done.
  HasSelectorStyleSignature = isAtStartOfBindingName();

  if (HasSelectorStyleSignature) {
    // This looks like a selector-style argument.  Try to convert the first
    // argument pattern into a single argument type and parse subsequent
    // selector forms.
    return ParserStatus(ArgPattern) |
      parseSelectorFunctionArguments(*this, NamePieces,
                                     ArgPatterns, BodyPatterns,
                                     DefaultArgs, ArgPattern.get());
  }

  ParserStatus Result;
  while (1) {
    ArgPatterns.push_back(ArgPattern.get());
    BodyPatterns.push_back(ArgPattern.get());
    Result |= ArgPattern;

    // If we're out of pieces, we're done.
    if (Tok.isNot(tok::l_paren))
      break;

    ArgPattern = parsePatternTuple(/*IsLet*/true, /*IsArgList*/true, nullptr);

    if (ArgPattern.isNull() || ArgPattern.hasCodeCompletion())
      return ArgPattern;
  }

  return Result;
}

/// parseFunctionSignature - Parse a function definition signature.
///   func-signature:
///     func-arguments func-signature-result?
///   func-signature-result:
///     '->' type-annotation
///
/// Note that this leaves retType as null if unspecified.
ParserStatus
Parser::parseFunctionSignature(Identifier SimpleName,
                               DeclName &FullName,
                               SmallVectorImpl<Pattern *> &argPatterns,
                               SmallVectorImpl<Pattern *> &bodyPatterns,
                               DefaultArgumentInfo &defaultArgs,
                               TypeRepr *&retType,
                               bool &HasSelectorStyleSignature) {
  HasSelectorStyleSignature = false;
  
  SmallVector<Identifier, 4> NamePieces;
  NamePieces.push_back(SimpleName);
  FullName = SimpleName;
  
  ParserStatus Status;
  // We force first type of a func declaration to be a tuple for consistency.
  if (Tok.is(tok::l_paren)) {
    Status = parseFunctionArguments(NamePieces, argPatterns, bodyPatterns,
                                    defaultArgs, HasSelectorStyleSignature);
    
    FullName = DeclName(Context, NamePieces);
    
    if (bodyPatterns.empty()) {
      // If we didn't get anything, add a () pattern to avoid breaking
      // invariants.
      assert(Status.hasCodeCompletion() || Status.isError());
      bodyPatterns.push_back(TuplePattern::create(Context, Tok.getLoc(),
                                                  {}, Tok.getLoc()));
      argPatterns.push_back(bodyPatterns.back());
    }

  } else {
    diagnose(Tok, diag::func_decl_without_paren);
    Status = makeParserError();

    // Recover by creating a '() -> ?' signature.
    auto *EmptyTuplePattern =
        TuplePattern::create(Context, Tok.getLoc(), {}, Tok.getLoc());
    argPatterns.push_back(EmptyTuplePattern);
    bodyPatterns.push_back(EmptyTuplePattern);
  }

  // If there's a trailing arrow, parse the rest as the result type.
  if (Tok.is(tok::arrow) || Tok.is(tok::colon)) {
    if (!consumeIf(tok::arrow)) {
      // FixIt ':' to '->'.
      diagnose(Tok, diag::func_decl_expected_arrow)
          .fixItReplace(SourceRange(Tok.getLoc()), "->");
      consumeToken(tok::colon);
    }

    ParserResult<TypeRepr> ResultType =
        parseTypeAnnotation(diag::expected_type_function_result);
    if (ResultType.hasCodeCompletion())
      return ResultType;
    retType = ResultType.getPtrOrNull();
    if (!retType) {
      Status.setIsParseError();
      return Status;
    }
  } else {
    // Otherwise, we leave retType null.
    retType = nullptr;
  }

  return Status;
}

ParserStatus
Parser::parseConstructorArguments(Pattern *&ArgPattern, Pattern *&BodyPattern,
                                  DefaultArgumentInfo &DefaultArgs,
                                  bool &HasSelectorStyleSignature) {
  HasSelectorStyleSignature = false;

  // It's just a pattern. Parse it.
  if (Tok.is(tok::l_paren)) {
    ParserResult<Pattern> Params =
      parsePatternTuple(/*IsLet*/ true, /*IsArgList*/ true, &DefaultArgs);

    // If we failed to parse the pattern, create an empty tuple to recover.
    if (Params.isNull()) {
      Params = makeParserResult(Params,
          TuplePattern::createSimple(Context, Tok.getLoc(), {}, Tok.getLoc()));
    }

    ArgPattern = Params.get();
    BodyPattern = ArgPattern->clone(Context);
    return Params;
  }

  if (!isAtStartOfBindingName()) {
    // Complain that we expected '(' or a parameter name.
    {
      auto diag = diagnose(Tok, diag::expected_lparen_initializer);
      if (Tok.is(tok::l_brace))
        diag.fixItInsert(Tok.getLoc(), "() ");
    }

    // Create an empty tuple to recover.
    ArgPattern = TuplePattern::createSimple(Context, Tok.getLoc(), {},
                                            Tok.getLoc());
    BodyPattern = ArgPattern->clone(Context);
    return makeParserError();
  }

  // We have the start of a binding name, so this is a selector-style
  // declaration.
  HasSelectorStyleSignature = true;

  // This is not a parenthesis, but we should provide a reasonable source range
  // for parameters.
  SourceLoc LParenLoc = Tok.getLoc();

  // Parse additional selectors as long as we can.
  ParserStatus Status;
  SmallVector<TuplePatternElt, 4> ArgElts;
  SmallVector<TuplePatternElt, 4> BodyElts;
  SourceLoc RParenLoc;
  // NB: Not actually used to form the initializer name.
  SmallVector<Identifier, 4> NamePieces;
  
  for (;;) {
    if (isAtStartOfBindingName()) {
      Status |= parseSelectorArgument(*this, NamePieces, ArgElts, BodyElts,
                                      DefaultArgs, RParenLoc);
      continue;
    }

    if (Tok.is(tok::l_paren)) {
      // FIXME: Should we assume this is '_'?
      diagnose(Tok, diag::func_selector_with_curry);
      // FIXME: better recovery: just parse a tuple instead of skipping tokens.
      skipUntilDeclRBrace(tok::l_brace);
      Status.setIsParseError();
    }
    break;
  }

  ArgPattern = TuplePattern::create(Context, LParenLoc, ArgElts, RParenLoc);
  BodyPattern = TuplePattern::create(Context, LParenLoc, BodyElts,
                                     RParenLoc);
  return Status;
}

/// Parse a pattern.
///   pattern ::= pattern-atom
///   pattern ::= pattern-atom ':' type-annotation
///   pattern ::= 'var' pattern
///   pattern ::= 'let' pattern
ParserResult<Pattern> Parser::parsePattern(bool isLet) {
  // If this is a let or var pattern parse it.
  if (Tok.is(tok::kw_let) || Tok.is(tok::kw_var))
    return parsePatternVarOrLet();
  
  // First, parse the pattern atom.
  ParserResult<Pattern> Result = parsePatternAtom(isLet);

  // Now parse an optional type annotation.
  if (consumeIf(tok::colon)) {
    if (Result.isNull()) {
      // Recover by creating AnyPattern.
      Result = makeParserErrorResult(new (Context) AnyPattern(PreviousLoc));
    }

    ParserResult<TypeRepr> Ty = parseTypeAnnotation();
    if (Ty.hasCodeCompletion())
      return makeParserCodeCompletionResult<Pattern>();

    if (Ty.isNull())
      Ty = makeParserResult(new (Context) ErrorTypeRepr(PreviousLoc));

    Result = makeParserResult(Result,
        new (Context) TypedPattern(Result.get(), Ty.get()));
  }

  return Result;
}

ParserResult<Pattern> Parser::parsePatternVarOrLet() {
  assert((Tok.is(tok::kw_let) || Tok.is(tok::kw_var)) && "expects let or var");
  bool isLet = Tok.is(tok::kw_let);
  SourceLoc varLoc = consumeToken();

  // 'var' and 'let' patterns shouldn't nest.
  if (InVarOrLetPattern)
    diagnose(varLoc, diag::var_pattern_in_var, unsigned(isLet));

  // In our recursive parse, remember that we're in a var/let pattern.
  llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
    T(InVarOrLetPattern, isLet ? IVOLP_InLet : IVOLP_InVar);

  ParserResult<Pattern> subPattern = parsePattern(isLet);
  if (subPattern.hasCodeCompletion())
    return makeParserCodeCompletionResult<Pattern>();
  if (subPattern.isNull())
    return nullptr;
  return makeParserResult(new (Context) VarPattern(varLoc, subPattern.get()));
}

/// \brief Determine whether this token can start a binding name, whether an
/// identifier or the special discard-value binding '_'.
bool Parser::isAtStartOfBindingName() {
  return Tok.is(tok::kw__)
    || (Tok.is(tok::identifier) && !isStartOfDecl(Tok, peekToken()));
}

Pattern *Parser::createBindingFromPattern(SourceLoc loc, Identifier name,
                                          bool isLet) {
  auto *var = new (Context) VarDecl(/*static*/ false, /*IsLet*/ isLet,
                                    loc, name, Type(), CurDeclContext);
  return new (Context) NamedPattern(var);
}

/// Parse an identifier as a pattern.
ParserResult<Pattern> Parser::parsePatternIdentifier(bool isLet) {
  SourceLoc loc = Tok.getLoc();
  if (consumeIf(tok::kw__)) {
    return makeParserResult(new (Context) AnyPattern(loc));
  }
  
  StringRef text = Tok.getText();
  if (consumeIf(tok::identifier)) {
    Identifier ident = Context.getIdentifier(text);
    return makeParserResult(createBindingFromPattern(loc, ident, isLet));
  }

  return nullptr;
}

/// Parse a pattern "atom", meaning the part that precedes the
/// optional type annotation.
///
///   pattern-atom ::= identifier
///   pattern-atom ::= '_'
///   pattern-atom ::= pattern-tuple
ParserResult<Pattern> Parser::parsePatternAtom(bool isLet) {
  switch (Tok.getKind()) {
  case tok::l_paren:
    return parsePatternTuple(isLet, /*IsArgList*/false,/*DefaultArgs*/nullptr);

  case tok::identifier:
  case tok::kw__:
    return parsePatternIdentifier(isLet);

  case tok::code_complete:
    // Just eat the token and return an error status, *not* the code completion
    // status.  We can not code complete anything here -- we expect an
    // identifier.
    consumeToken(tok::code_complete);
    return nullptr;

  default:
    if (Tok.isKeyword() &&
        (peekToken().is(tok::colon) || peekToken().is(tok::equal))) {
      diagnose(Tok, diag::expected_pattern_is_keyword, Tok.getText());
      SourceLoc Loc = Tok.getLoc();
      consumeToken();
      return makeParserErrorResult(new (Context) AnyPattern(Loc));
    }
    diagnose(Tok, diag::expected_pattern);
    return nullptr;
  }
}

std::pair<ParserStatus, Optional<TuplePatternElt>>
Parser::parsePatternTupleElement(bool isLet, bool isArgumentList,
                                 Pattern *ImplicitName,
                                 DefaultArgumentInfo *defaultArgs) {
  
  // Function argument lists can have "inout" applied to TypedPatterns in their
  // arguments.
  SourceLoc InOutLoc;
  if (isArgumentList && Tok.isContextualKeyword("inout"))
    InOutLoc = consumeToken(tok::identifier);
  
  unsigned defaultArgIndex = (defaultArgs ? defaultArgs->NextIndex++ : 0);

  // Parse the pattern.
  ParserResult<Pattern> pattern;

  // If this is a normal tuple value, parse it as a pattern.  If it is a "type
  // only" case (e.g. an implicitly named selector argument) then parse a type
  // and build the pattern around it.
  if (!ImplicitName) {
    pattern = parsePattern(isLet);
    if (pattern.hasCodeCompletion())
      return std::make_pair(makeParserCodeCompletionStatus(), Nothing);
    if (pattern.isNull())
      return std::make_pair(makeParserError(), Nothing);
  } else {
    ParserResult<TypeRepr> Ty = parseTypeAnnotation();
    if (Ty.hasCodeCompletion())
      return std::make_pair(makeParserCodeCompletionStatus(), Nothing);
    if (Ty.isNull())
      return std::make_pair(makeParserError(), Nothing);

    // Build this as typed_pattern(name).
    pattern = makeParserResult(new (Context) TypedPattern(ImplicitName,
                                                          Ty.get()));
  }

  // Parse the optional initializer.
  ExprHandle *init = nullptr;
  if (Tok.is(tok::equal))
    parseDefaultArgument(*this, defaultArgs, defaultArgIndex, init);

  // If this is an inout function argument, validate that the sub-pattern is
  // a TypedPattern.
  if (InOutLoc.isValid()) {
    if (auto *TP = dyn_cast<TypedPattern>(pattern.get())) {
      // Change the TypeRep of the underlying typed pattern to be an inout
      // typerep.
      TypeLoc &LocInfo = TP->getTypeLoc();
      LocInfo = TypeLoc(new (Context) InOutTypeRepr(LocInfo.getTypeRepr(),
                                                    InOutLoc));
    } else if (isa<VarPattern>(pattern.get())) {
      diagnose(InOutLoc, diag::inout_varpattern);
    } else {
      diagnose(InOutLoc, diag::inout_must_have_type);
    }
  }
  
  return std::make_pair(
      makeParserSuccess(),
      TuplePatternElt(pattern.get(), init, getDefaultArgKind(init)));
}

ParserResult<Pattern> Parser::parsePatternTuple(bool isLet, bool isArgumentList,
                                                DefaultArgumentInfo *defaults) {
  StructureMarkerRAII ParsingPatternTuple(*this, Tok);
  SourceLoc LPLoc = consumeToken(tok::l_paren);
  return parsePatternTupleAfterLP(isLet, isArgumentList, LPLoc, defaults);
}

/// Parse a tuple pattern.  The leading left paren has already been consumed and
/// we are looking at the next token.  LPLoc specifies its location.
///
///   pattern-tuple:
///     '(' pattern-tuple-body? ')'
///   pattern-tuple-body:
///     pattern-tuple-element (',' pattern-tuple-body)*
ParserResult<Pattern>
Parser::parsePatternTupleAfterLP(bool isLet, bool isArgumentList,
                                 SourceLoc LPLoc,
                                 DefaultArgumentInfo *defaults) {
  SourceLoc RPLoc, EllipsisLoc;

  auto diagToUse = isArgumentList ? diag::expected_rparen_parameter
                                  : diag::expected_rparen_tuple_pattern_list;
  
  // Parse all the elements.
  SmallVector<TuplePatternElt, 8> elts;
  ParserStatus ListStatus =
    parseList(tok::r_paren, LPLoc, RPLoc, tok::comma, /*OptionalSep=*/false,
              /*AllowSepAfterLast=*/false, diagToUse, [&] () -> ParserStatus {
    // Parse the pattern tuple element.
    ParserStatus EltStatus;
    Optional<TuplePatternElt> elt;
    std::tie(EltStatus, elt) = parsePatternTupleElement(isLet, isArgumentList,
                                                        nullptr, defaults);
    if (EltStatus.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
    if (!elt)
      return makeParserError();

    // Add this element to the list.
    elts.push_back(*elt);

    // If there is no ellipsis, we're done with the element.
    if (Tok.isNotEllipsis())
      return makeParserSuccess();
    SourceLoc ellLoc = consumeToken();

    // An element cannot have both an initializer and an ellipsis.
    if (elt->getInit()) {
      diagnose(ellLoc, diag::tuple_ellipsis_init)
        .highlight(elt->getInit()->getExpr()->getSourceRange());
      // Return success since the error was semantic, and the caller should not
      // attempt recovery.
      return makeParserSuccess();
    }

    // An ellipsis element shall have a specified element type.
    // FIXME: This seems unnecessary.
    TypedPattern *typedPattern = dyn_cast<TypedPattern>(elt->getPattern());
    if (!typedPattern) {
      diagnose(ellLoc, diag::untyped_pattern_ellipsis)
        .highlight(elt->getPattern()->getSourceRange());
      // Return success so that the caller does not attempt recovery -- it
      // should have already happened when we were parsing the tuple element.
      return makeParserSuccess();
    }

    // Variadic elements must come last.
    // FIXME: Unnecessary restriction. It makes conversion more interesting,
    // but is not complicated to support.
    if (Tok.is(tok::r_paren)) {
      EllipsisLoc = ellLoc;
    } else {
      diagnose(ellLoc, diag::ellipsis_pattern_not_at_end);
    }

    return makeParserSuccess();
  });

  return makeParserResult(ListStatus, TuplePattern::createSimple(
                                          Context, LPLoc, elts, RPLoc,
                                          EllipsisLoc.isValid(), EllipsisLoc));
}

ParserResult<Pattern> Parser::parseMatchingPattern() {
  // TODO: Since we expect a pattern in this position, we should optimistically
  // parse pattern nodes for productions shared by pattern and expression
  // grammar. For short-term ease of initial implementation, we always go
  // through the expr parser for ambiguious productions.

  // Parse productions that can only be patterns.
  // matching-pattern ::= matching-pattern-var
  if (Tok.is(tok::kw_var) || Tok.is(tok::kw_let))
    return parseMatchingPatternVarOrVal();

  // matching-pattern ::= 'is' type
  if (Tok.is(tok::kw_is))
    return parseMatchingPatternIs();

  // matching-pattern ::= expr
  // Fall back to expression parsing for ambiguous forms. Name lookup will
  // disambiguate.
  ParserResult<Expr> subExpr = parseExpr(diag::expected_pattern);
  if (subExpr.hasCodeCompletion())
    return makeParserCodeCompletionStatus();
  if (subExpr.isNull())
    return nullptr;
  
  return makeParserResult(new (Context) ExprPattern(subExpr.get()));
}

ParserResult<Pattern> Parser::parseMatchingPatternVarOrVal() {
  assert((Tok.is(tok::kw_let) || Tok.is(tok::kw_var)) && "expects val or var");
  bool isVal = Tok.is(tok::kw_let);
  SourceLoc varLoc = consumeToken();

  // 'var' and 'let' patterns shouldn't nest.
  if (InVarOrLetPattern)
    diagnose(varLoc, diag::var_pattern_in_var, unsigned(isVal));

  // In our recursive parse, remember that we're in a var/let pattern.
  llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
    T(InVarOrLetPattern, isVal ? IVOLP_InLet : IVOLP_InVar);

  ParserResult<Pattern> subPattern = parseMatchingPattern();
  if (subPattern.isNull())
    return nullptr;
  return makeParserResult(new (Context) VarPattern(varLoc, subPattern.get()));
}

// matching-pattern ::= 'is' type
ParserResult<Pattern> Parser::parseMatchingPatternIs() {
  SourceLoc isLoc = consumeToken(tok::kw_is);
  ParserResult<TypeRepr> castType = parseType();
  if (castType.isNull() || castType.hasCodeCompletion())
    return nullptr;
  return makeParserResult(new (Context) IsaPattern(isLoc, castType.get()));
}

bool Parser::isOnlyStartOfMatchingPattern() {
  return Tok.is(tok::kw_var) || Tok.is(tok::kw_let) || Tok.is(tok::kw_is);
}
