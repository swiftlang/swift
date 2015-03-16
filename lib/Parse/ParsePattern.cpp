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

#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Parser.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ExprHandle.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/ADT/SmallString.h"
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
  case MagicIdentifierLiteralExpr::DSOHandle:
    return DefaultArgumentKind::DSOHandle;
  }
}

static void recoverFromBadSelectorArgument(Parser &P) {
  while (P.Tok.isNot(tok::eof) && P.Tok.isNot(tok::r_paren) &&
         P.Tok.isNot(tok::l_brace) && P.Tok.isNot(tok::r_brace) &&
         !P.isStartOfStmt() && !P.isStartOfDecl()) {
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
  } else {
    defaultArgs->HasDefaultArgument = true;
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

/// Determine whether we are at the start of a parameter name when
/// parsing a parameter.
static bool startsParameterName(Parser &parser, bool isClosure) {
  // '_' cannot be a type, so it must be a parameter name.
  if (parser.Tok.is(tok::kw__))
    return true;

  // To have a parameter name here, we need a name.
  if (!parser.Tok.is(tok::identifier))
    return false;

  // If the next token is another identifier, '_', or ':', this is a name.
  if (parser.peekToken().isAny(tok::identifier, tok::kw__, tok::colon))
    return true;

  // The identifier could be a name or it could be a type. In a closure, we
  // assume it's a name, because the type can be inferred. Elsewhere, we
  // assume it's a type.
  return isClosure;
}

ParserStatus
Parser::parseParameterClause(SourceLoc &leftParenLoc,
                             SmallVectorImpl<ParsedParameter> &params,
                             SourceLoc &rightParenLoc,
                             DefaultArgumentInfo *defaultArgs,
                             ParameterContextKind paramContext) {
  assert(params.empty() && leftParenLoc.isInvalid() &&
         rightParenLoc.isInvalid() && "Must start with empty state");

  // Consume the starting '(';
  leftParenLoc = consumeToken(tok::l_paren);

  // Trivial case: empty parameter list.
  if (Tok.is(tok::r_paren)) {
    rightParenLoc = consumeToken(tok::r_paren);
    return ParserStatus();
  }

  // Parse the parameter list.
  bool isClosure = paramContext == ParameterContextKind::Closure;
  return parseList(tok::r_paren, leftParenLoc, rightParenLoc, tok::comma,
                      /*OptionalSep=*/false, /*AllowSepAfterLast=*/false,
                      diag::expected_rparen_parameter,
                      [&]() -> ParserStatus {
    ParsedParameter param;
    ParserStatus status;
    SourceLoc StartLoc = Tok.getLoc();

    unsigned defaultArgIndex = defaultArgs? defaultArgs->NextIndex++ : 0;

    // Attributes.
    bool FoundCCToken;
    parseDeclAttributeList(param.Attrs, FoundCCToken,
                          /*stop at type attributes*/true, true);
    if (FoundCCToken) {
      if (CodeCompletion) {
        CodeCompletion->completeDeclAttrKeyword(nullptr, isInSILMode(), true);
      } else {
        status |= makeParserCodeCompletionStatus();
      }
    }

    // ('inout' | 'let' | 'var')?
    if (Tok.is(tok::kw_inout)) {
      param.LetVarInOutLoc = consumeToken();
      param.SpecifierKind = ParsedParameter::InOut;
    } else if (Tok.is(tok::kw_let)) {
      param.LetVarInOutLoc = consumeToken();
      param.SpecifierKind = ParsedParameter::Let;
    } else if (Tok.is(tok::kw_var)) {
      param.LetVarInOutLoc = consumeToken();
      param.SpecifierKind = ParsedParameter::Var;
    }

    // Redundant specifiers are fairly common, recognize, reject, and recover
    // from this gracefully.
    if (Tok.isAny(tok::kw_inout, tok::kw_let, tok::kw_var)) {
      diagnose(Tok, diag::parameter_inout_var_let)
        .fixItRemove(Tok.getLoc());
      consumeToken();
    }

    // '#'?
    if (Tok.is(tok::pound))
      param.PoundLoc = consumeToken(tok::pound);

    if (param.PoundLoc.isValid() || startsParameterName(*this, isClosure)) {
      // identifier-or-none for the first name
      if (Tok.is(tok::identifier)) {
        param.FirstName = Context.getIdentifier(Tok.getText());
        param.FirstNameLoc = consumeToken();

        // Operators can not have API names.
        if (paramContext == ParameterContextKind::Operator &&
            param.PoundLoc.isValid()) {
          diagnose(param.PoundLoc, 
                   diag::parameter_operator_keyword_argument)
            .fixItRemove(param.PoundLoc);
          param.PoundLoc = SourceLoc();
        }
      } else if (Tok.is(tok::kw__)) {
        // A back-tick cannot precede an empty name marker.
        if (param.PoundLoc.isValid()) {
          diagnose(Tok, diag::parameter_backtick_empty_name)
            .fixItRemove(param.PoundLoc);
          param.PoundLoc = SourceLoc();
        }

        param.FirstNameLoc = consumeToken();
      } else {
        assert(param.PoundLoc.isValid() && "startsParameterName() lied");
        diagnose(Tok, diag::parameter_backtick_missing_name);
        param.FirstNameLoc = param.PoundLoc;
        param.PoundLoc = SourceLoc();
      }

      // identifier-or-none? for the second name
      if (Tok.is(tok::identifier)) {
        param.SecondName = Context.getIdentifier(Tok.getText());
        param.SecondNameLoc = consumeToken();
      } else if (Tok.is(tok::kw__)) {
        param.SecondNameLoc = consumeToken();
      }

      // Operators can not have API names.
      if (paramContext == ParameterContextKind::Operator &&
          param.SecondNameLoc.isValid()) {
        diagnose(param.FirstNameLoc, 
                 diag::parameter_operator_keyword_argument)
          .fixItRemoveChars(param.FirstNameLoc, param.SecondNameLoc);
        param.FirstName = param.SecondName;
        param.FirstNameLoc = param.SecondNameLoc;
        param.SecondName = Identifier();
        param.SecondNameLoc = SourceLoc();
      }

      // Cannot have a back-tick and two names.
      if (param.PoundLoc.isValid() && param.SecondNameLoc.isValid()) {
        diagnose(param.PoundLoc, diag::parameter_backtick_two_names)
          .fixItRemove(param.PoundLoc);
        param.PoundLoc = SourceLoc();
      }

      // If we have two equivalent names, suggest using the back-tick.
      if (param.FirstNameLoc.isValid() && param.SecondNameLoc.isValid() &&
          param.FirstName == param.SecondName) {
        StringRef name;
        if (param.FirstName.empty())
          name = "_";
        else
          name = param.FirstName.str();

        SourceLoc afterFirst = Lexer::getLocForEndOfToken(Context.SourceMgr,
                                                          param.FirstNameLoc);
        diagnose(param.FirstNameLoc, diag::parameter_two_equivalent_names,
                 name)
          .fixItInsert(param.FirstNameLoc, "#")
          .fixItRemove(SourceRange(afterFirst, param.SecondNameLoc));
      }

      // (':' type)?
      if (Tok.is(tok::colon)) {
        param.ColonLoc = consumeToken();

        // Special case handling of @autoclosure attribute on the type, which
        // was supported in Swift 1.0 and 1.1, but removed in Swift 1.2 (moved
        // to a decl attribute).
        if (Tok.is(tok::at_sign) &&
            peekToken().isContextualKeyword("autoclosure")) {
          SourceLoc AtLoc = consumeToken(tok::at_sign);
          SourceLoc ACLoc = consumeToken(tok::identifier);
          diagnose(AtLoc, diag::autoclosure_is_decl_attribute)
            .fixItRemove(SourceRange(AtLoc, ACLoc))
            .fixItInsert(StartLoc, "@autoclosure ");
          param.Attrs.add(new (Context) AutoClosureAttr(AtLoc, ACLoc,
                                                        /*escaping=*/false));
        }

        auto type = parseType(diag::expected_parameter_type);
        status |= type;
        param.Type = type.getPtrOrNull();
        // Only allow 'inout' before the parameter name.
        if (auto InOutTy = dyn_cast_or_null<InOutTypeRepr>(param.Type)) {
          SourceLoc InOutLoc = InOutTy->getInOutLoc();
          SourceLoc NameLoc = param.FirstNameLoc;
          diagnose(InOutLoc, diag::inout_must_appear_before_param)
              .fixItRemove(InOutLoc)
              .fixItInsert(NameLoc, "inout ");
          param.Type = InOutTy->getBase();
        }
      }
    } else {
      auto type = parseType(diag::expected_parameter_type);
      status |= type;
      param.Type = type.getPtrOrNull();
    }

    // '...'?
    if (Tok.isEllipsis()) {
      param.EllipsisLoc = consumeToken();
    }

    // ('=' expr)?
    if (Tok.is(tok::equal)) {
      param.EqualLoc = Tok.getLoc();
      status |= parseDefaultArgument(*this, defaultArgs, defaultArgIndex,
                                     param.DefaultArg);

      // A default argument implies that the name is API, making the
      // back-tick redundant.
      if (param.PoundLoc.isValid()) {
        diagnose(param.PoundLoc, diag::parameter_backtick_default_arg)
          .fixItRemove(param.PoundLoc);
        param.PoundLoc = SourceLoc();
      }

      if (param.EllipsisLoc.isValid()) {
        // The range of the complete default argument.
        SourceRange defaultArgRange;
        if (param.DefaultArg) {
          if (auto init = param.DefaultArg->getExpr()) {
            defaultArgRange = SourceRange(param.EllipsisLoc, init->getEndLoc());
          }
        }

        diagnose(param.EqualLoc, diag::parameter_vararg_default)
          .highlight(param.EllipsisLoc)
          .fixItRemove(defaultArgRange);
      }
    }

    // If we haven't made progress, don't add the param.
    if (Tok.getLoc() == StartLoc)
      return status;

    params.push_back(param);
    return status;
  });
}

/// Map parsed parameters to argument and body patterns.
///
/// \returns the pattern describing the parsed parameters.
static Pattern*
mapParsedParameters(Parser &parser,
                    SourceLoc leftParenLoc,
                    MutableArrayRef<Parser::ParsedParameter> params,
                    SourceLoc rightParenLoc,
                    bool isFirstParameterClause,
                    SmallVectorImpl<Identifier> *argNames,
                    Parser::ParameterContextKind paramContext) {
  auto &ctx = parser.Context;

  // Local function to create a pattern for a single parameter.
  auto createParamPattern = [&](SourceLoc &letVarInOutLoc,
                        Parser::ParsedParameter::SpecifierKindTy &specifierKind,
                                Identifier argName, SourceLoc argNameLoc,
                                Identifier paramName, SourceLoc paramNameLoc,
                                TypeRepr *type,
                                const DeclAttributes &Attrs) -> Pattern * {
    // Create the parameter based on the name.
    Pattern *param;
    ParamDecl *var = nullptr;
    // Create a variable to capture this.
    var = new (ctx) ParamDecl(specifierKind == Parser::ParsedParameter::Let,
                              argNameLoc, argName,
                              paramNameLoc, paramName, Type(),
                              parser.CurDeclContext);
    var->getAttrs() = Attrs;
    if (argNameLoc.isInvalid() && paramNameLoc.isInvalid())
      var->setImplicit();
    param = new (ctx) NamedPattern(var);

    // If a type was provided, create the typed pattern.
    if (type) {
      // If 'inout' was specified, turn the type into an in-out type.
      if (specifierKind == Parser::ParsedParameter::InOut)
        type = new (ctx) InOutTypeRepr(type, letVarInOutLoc);

      param = new (ctx) TypedPattern(param, type);
    } else if (specifierKind == Parser::ParsedParameter::InOut) {
      parser.diagnose(letVarInOutLoc, diag::inout_must_have_type);
      letVarInOutLoc = SourceLoc();
      specifierKind = Parser::ParsedParameter::Let;
    }

    // If 'var' or 'let' was specified explicitly, create a pattern for it.
    if (specifierKind != Parser::ParsedParameter::InOut &&
        letVarInOutLoc.isValid()) {
      bool isLet = specifierKind == Parser::ParsedParameter::Let;
      param = new (ctx) VarPattern(letVarInOutLoc, isLet, param);
    }

    if (var)
      var->setParamParentPattern(param);
    return param;
  };

  // Collect the elements of the tuple patterns for argument and body
  // parameters.
  SmallVector<TuplePatternElt, 4> elements;
  SourceLoc ellipsisLoc;
  bool isFirstParameter = true;
  for (auto &param : params) {
    // Whether the provided name is API by default depends on the parameter
    // context.
    bool isKeywordArgumentByDefault;
    switch (paramContext) {
    case Parser::ParameterContextKind::Function:
    case Parser::ParameterContextKind::Closure:
    case Parser::ParameterContextKind::Subscript:
    case Parser::ParameterContextKind::Operator:
      isKeywordArgumentByDefault = !isFirstParameterClause;
      break;

    case Parser::ParameterContextKind::Initializer:
      isKeywordArgumentByDefault = true;
      break;

    case Parser::ParameterContextKind::Method:
      isKeywordArgumentByDefault = !isFirstParameterClause || !isFirstParameter;
      break;
    }

    // The presence of a default argument implies that this argument
    // is a keyword argument.
    if (param.DefaultArg)
      isKeywordArgumentByDefault = true;

    // Local function that checks the argument and parameter name to see if they
    // are permissible, which may result in changes.
    auto checkArgNames = [&](Identifier argName, Identifier paramName)
                           -> std::pair<Identifier, Identifier> {
      // If the first parameter of a method or initializer is a keyword argument
      // and starts with "with", note that the "with" is implied.
      if (!argName.empty() && isFirstParameter && isFirstParameterClause &&
          (paramContext == Parser::ParameterContextKind::Method ||
           paramContext == Parser::ParameterContextKind::Initializer) &&
          argName.str().size() > 4 &&
          camel_case::getFirstWord(argName.str()) == "with") {

        // Compute the name that remains once we drop "with".
        llvm::SmallString<16> buffer;
        StringRef remainingName
          = camel_case::toLowercaseWord(argName.str().substr(4), buffer);

        // Figure out where to emit the diagnostic.
        bool isInitializer
          = paramContext == Parser::ParameterContextKind::Initializer;
        auto diag = parser.diagnose(param.FirstNameLoc,
                                    diag::implied_with_parameter,
                                    isInitializer, remainingName);

        // Emit a Fix-It to patch this up.
        if (param.SecondNameLoc.isInvalid()) {
          // There was no second name, so changing this involves introducing
          // a separate keyword argument name without the "with".
          llvm::SmallString<16> replacementText;
          replacementText += remainingName;
          replacementText += " ";
          diag.fixItInsert(param.FirstNameLoc, replacementText);

          // If there was a back-tick, remove it: we have two names now.
          if (param.PoundLoc.isValid())
            diag.fixItRemove(param.PoundLoc);
        } else if (!param.SecondName.empty() &&
                   param.SecondName.str() == remainingName) {
          // Both names were specified and the second one matches what we get
          // by dropping "with", so just zap the first name from the source.
          diag.fixItRemoveChars(param.FirstNameLoc, param.SecondNameLoc);

          // If this isn't a keyword argument by default, add a back-tick back.
          if (!isKeywordArgumentByDefault) {
            diag.fixItInsert(param.SecondNameLoc, "#");
          }
        } else {
          // There were two names and the second one doesn't match, so just fix
          // the first name by dropping the "with".
          diag.fixItReplace(param.FirstNameLoc, remainingName);
        }

        // Update the argument name.
        argName = parser.Context.getIdentifier(remainingName);
      }

      return {argName, paramName};
    };

    // Create the pattern.
    Pattern *pattern;
    Identifier argName;
    Identifier paramName;
    if (param.SecondNameLoc.isValid()) {
      std::tie(argName, paramName) = checkArgNames(param.FirstName,
                                                   param.SecondName);

      // Both names were provided, so pass them in directly.
      pattern = createParamPattern(param.LetVarInOutLoc, param.SpecifierKind,
                                   argName, param.FirstNameLoc,
                                   paramName, param.SecondNameLoc,
                                   param.Type, param.Attrs);

      // If the first name is empty and this parameter would not have been
      // an API name by default, complain.
      if (param.FirstName.empty() && !isKeywordArgumentByDefault) {
        parser.diagnose(param.FirstNameLoc,
                        diag::parameter_extraneous_empty_name,
                        param.SecondName)
          .fixItRemoveChars(param.FirstNameLoc, param.SecondNameLoc);

        param.FirstNameLoc = SourceLoc();
      }
    } else {
      // If it's an API name by default, or there was a back-tick, we have an
      // API name.
      if (isKeywordArgumentByDefault || param.PoundLoc.isValid()) {
        argName = param.FirstName;

        // If both are true, warn that the back-tick is unnecessary.
        if (isKeywordArgumentByDefault && param.PoundLoc.isValid()) {
          parser.diagnose(param.PoundLoc,
                          diag::parameter_extraneous_backtick, argName)
            .fixItRemove(param.PoundLoc);
        }
      }

      std::tie(argName, paramName) = checkArgNames(argName, param.FirstName);

      pattern = createParamPattern(param.LetVarInOutLoc, param.SpecifierKind,
                                   argName, SourceLoc(),
                                   param.FirstName, param.FirstNameLoc,
                                   param.Type, param.Attrs);
    }

    // If this parameter had an ellipsis, check whether it's the last parameter.
    if (param.EllipsisLoc.isValid()) {
      if (&param != &params.back()) {
        parser.diagnose(param.EllipsisLoc, diag::parameter_ellipsis_not_at_end)
          .fixItRemove(param.EllipsisLoc);
        param.EllipsisLoc = SourceLoc();
      } else if (!isa<TypedPattern>(pattern)) {
        parser.diagnose(param.EllipsisLoc, diag::untyped_pattern_ellipsis)
          .highlight(pattern->getSourceRange());
      } else {
        ellipsisLoc = param.EllipsisLoc;
      }
    }

    // Default arguments are only permitted on the first parameter clause.
    if (param.DefaultArg && !isFirstParameterClause) {
      parser.diagnose(param.EqualLoc, diag::non_func_decl_pattern_init)
        .fixItRemove(SourceRange(param.EqualLoc,
                                 param.DefaultArg->getExpr()->getEndLoc()));
    }

    // Create the tuple pattern elements.
    auto defArgKind = getDefaultArgKind(param.DefaultArg);
    elements.push_back(TuplePatternElt(pattern, param.DefaultArg, defArgKind));

    if (argNames)
      argNames->push_back(argName);

    isFirstParameter = false;
  }

  return TuplePattern::createSimple(ctx, leftParenLoc, elements,
                                    rightParenLoc, ellipsisLoc.isValid(),
                                    ellipsisLoc);
}

/// Parse a single parameter-clause.
ParserResult<Pattern> Parser::parseSingleParameterClause(
                                ParameterContextKind paramContext,
                                SmallVectorImpl<Identifier> *namePieces) {
  ParserStatus status;
  SmallVector<ParsedParameter, 4> params;
  SourceLoc leftParenLoc, rightParenLoc;
  
  // Parse the parameter clause.
  status |= parseParameterClause(leftParenLoc, params, rightParenLoc,
                                 /*defaultArgs=*/nullptr, paramContext);
  
  // Turn the parameter clause into argument and body patterns.
  auto pattern = mapParsedParameters(*this, leftParenLoc, params,
                                     rightParenLoc, true, namePieces,
                                     paramContext);

  return makeParserResult(status, pattern);
}

/// Parse function arguments.
///   func-arguments:
///     curried-arguments | selector-arguments
///   curried-arguments:
///     parameter-clause+
///   selector-arguments:
///     '(' selector-element ')' (identifier '(' selector-element ')')+
///   selector-element:
///      identifier '(' pattern-atom (':' type)? ('=' expr)? ')'
///
ParserStatus
Parser::parseFunctionArguments(SmallVectorImpl<Identifier> &NamePieces,
                               SmallVectorImpl<Pattern *> &BodyPatterns,
                               ParameterContextKind paramContext,
                               DefaultArgumentInfo &DefaultArgs) {
  // Parse parameter-clauses.
  ParserStatus status;
  bool isFirstParameterClause = true;
  while (Tok.is(tok::l_paren)) {
    SmallVector<ParsedParameter, 4> params;
    SourceLoc leftParenLoc, rightParenLoc;

    // Parse the parameter clause.
    status |= parseParameterClause(leftParenLoc, params, rightParenLoc,
                                   &DefaultArgs, paramContext);

    // Turn the parameter clause into argument and body patterns.
    auto pattern = mapParsedParameters(*this, leftParenLoc, params,
                                       rightParenLoc, 
                                       isFirstParameterClause,
                                       isFirstParameterClause ? &NamePieces
                                                              : nullptr,
                                       paramContext);
    BodyPatterns.push_back(pattern);
    isFirstParameterClause = false;
  }

  return status;
}

/// Parse a function definition signature.
///   func-signature:
///     func-arguments func-signature-result?
///   func-signature-result:
///     '->' type
///
/// Note that this leaves retType as null if unspecified.
ParserStatus
Parser::parseFunctionSignature(Identifier SimpleName,
                               DeclName &FullName,
                               SmallVectorImpl<Pattern *> &bodyPatterns,
                               DefaultArgumentInfo &defaultArgs,
                               TypeRepr *&retType) {
  SmallVector<Identifier, 4> NamePieces;
  NamePieces.push_back(SimpleName);
  FullName = SimpleName;
  
  ParserStatus Status;
  // We force first type of a func declaration to be a tuple for consistency.
  if (Tok.is(tok::l_paren)) {
    ParameterContextKind paramContext;
    if (SimpleName.isOperator())
      paramContext = ParameterContextKind::Operator;
    else if (CurDeclContext->isTypeContext())
      paramContext = ParameterContextKind::Method;
    else
      paramContext = ParameterContextKind::Function;

    Status = parseFunctionArguments(NamePieces, bodyPatterns, paramContext,
                                    defaultArgs);
    FullName = DeclName(Context, SimpleName, 
                        llvm::makeArrayRef(NamePieces.begin() + 1,
                                           NamePieces.end()));

    if (bodyPatterns.empty()) {
      // If we didn't get anything, add a () pattern to avoid breaking
      // invariants.
      assert(Status.hasCodeCompletion() || Status.isError());
      bodyPatterns.push_back(TuplePattern::create(Context, Tok.getLoc(),
                                                  {}, Tok.getLoc()));
    }
  } else {
    diagnose(Tok, diag::func_decl_without_paren);
    Status = makeParserError();

    // Recover by creating a '() -> ?' signature.
    auto *EmptyTuplePattern =
        TuplePattern::create(Context, PreviousLoc, {}, PreviousLoc);
    bodyPatterns.push_back(EmptyTuplePattern);
    FullName = DeclName(Context, SimpleName, { });
  }

  // If there's a trailing arrow, parse the rest as the result type.
  if (Tok.isAny(tok::arrow, tok::colon)) {
    if (!consumeIf(tok::arrow)) {
      // FixIt ':' to '->'.
      diagnose(Tok, diag::func_decl_expected_arrow)
          .fixItReplace(SourceRange(Tok.getLoc()), "->");
      consumeToken(tok::colon);
    }

    ParserResult<TypeRepr> ResultType =
      parseType(diag::expected_type_function_result);
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
Parser::parseConstructorArguments(DeclName &FullName, Pattern *&BodyPattern,
                                  DefaultArgumentInfo &DefaultArgs) {
  // If we don't have the leading '(', complain.
  if (!Tok.is(tok::l_paren)) {
    // Complain that we expected '('.
    {
      auto diag = diagnose(Tok, diag::expected_lparen_initializer);
      if (Tok.is(tok::l_brace))
        diag.fixItInsert(Tok.getLoc(), "() ");
    }

    // Create an empty tuple to recover.
    BodyPattern = TuplePattern::createSimple(Context, PreviousLoc, {},
                                             PreviousLoc, false,
                                             SourceLoc(), true);
    FullName = DeclName(Context, Context.Id_init, { });
    return makeParserError();
  }

  // Parse the parameter-clause.
  SmallVector<ParsedParameter, 4> params;
  SourceLoc leftParenLoc, rightParenLoc;
  
  // Parse the parameter clause.
  ParserStatus status 
    = parseParameterClause(leftParenLoc, params, rightParenLoc,
                           &DefaultArgs, ParameterContextKind::Initializer);

  // Turn the parameter clause into argument and body patterns.
  llvm::SmallVector<Identifier, 2> namePieces;
  BodyPattern = mapParsedParameters(*this, leftParenLoc, params,
                                    rightParenLoc, 
                                    /*isFirstParameterClause=*/true,
                                    &namePieces,
                                    ParameterContextKind::Initializer);

  FullName = DeclName(Context, Context.Id_init, namePieces);
  return status;
}


/// Parse a pattern with an optional type annotation.
///
///  typed-pattern ::= pattern (':' type)?
///
ParserResult<Pattern> Parser::parseTypedPattern() {
  auto result = parsePattern();
  
  // Now parse an optional type annotation.
  if (consumeIf(tok::colon)) {
    if (result.isNull())  // Recover by creating AnyPattern.
      result = makeParserErrorResult(new (Context) AnyPattern(PreviousLoc));
    
    ParserResult<TypeRepr> Ty = parseType();
    if (Ty.hasCodeCompletion())
      return makeParserCodeCompletionResult<Pattern>();
    if (Ty.isNull())
      Ty = makeParserResult(new (Context) ErrorTypeRepr(PreviousLoc));
    
    result = makeParserResult(result,
                            new (Context) TypedPattern(result.get(), Ty.get()));
  }
  
  return result;
}

/// Parse a pattern.
///   pattern ::= identifier
///   pattern ::= '_'
///   pattern ::= pattern-tuple
///   pattern ::= 'var' pattern
///   pattern ::= 'let' pattern
///
ParserResult<Pattern> Parser::parsePattern() {
  switch (Tok.getKind()) {
  case tok::l_paren:
    return parsePatternTuple();
    
  case tok::kw__:
    return makeParserResult(new (Context) AnyPattern(consumeToken(tok::kw__)));
    
  case tok::identifier: {
    Identifier name;
    SourceLoc loc = consumeIdentifier(&name);
    bool isLet = InVarOrLetPattern != IVOLP_InVar;
    return makeParserResult(createBindingFromPattern(loc, name, isLet));
  }
    
  case tok::code_complete:
    // Just eat the token and return an error status, *not* the code completion
    // status.  We can not code complete anything here -- we expect an
    // identifier.
    consumeToken(tok::code_complete);
    return nullptr;
    
  case tok::kw_var:
  case tok::kw_let: {
    bool isLet = Tok.is(tok::kw_let);
    SourceLoc varLoc = consumeToken();
    
    // 'var' and 'let' patterns shouldn't nest.
    if (InVarOrLetPattern == IVOLP_InLet ||
        InVarOrLetPattern == IVOLP_InVar)
      diagnose(varLoc, diag::var_pattern_in_var, unsigned(isLet));
    
    // 'let' isn't valid inside an implicitly immutable context, but var is.
    if (isLet && InVarOrLetPattern == IVOLP_ImplicitlyImmutable)
      diagnose(varLoc, diag::let_pattern_in_immutable_context);
    
    // In our recursive parse, remember that we're in a var/let pattern.
    llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
    T(InVarOrLetPattern, isLet ? IVOLP_InLet : IVOLP_InVar);
    
    ParserResult<Pattern> subPattern = parsePattern();
    if (subPattern.hasCodeCompletion())
      return makeParserCodeCompletionResult<Pattern>();
    if (subPattern.isNull())
      return nullptr;
    return makeParserResult(new (Context) VarPattern(varLoc, isLet,
                                                     subPattern.get()));
  }
      
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

Pattern *Parser::createBindingFromPattern(SourceLoc loc, Identifier name,
                                          bool isLet) {
  VarDecl *var;
  if (ArgumentIsParameter) {
    var = new (Context) ParamDecl(isLet, loc, name, loc, name, Type(),
                                  CurDeclContext);
  } else {
    var = new (Context) VarDecl(/*static*/ false, /*IsLet*/ isLet,
                                loc, name, Type(), CurDeclContext);
  }
  return new (Context) NamedPattern(var);
}

std::pair<ParserStatus, Optional<TuplePatternElt>>
Parser::parsePatternTupleElement() {
  // Parse the pattern.
  ParserResult<Pattern>  pattern = parsePattern();
  if (pattern.hasCodeCompletion())
    return std::make_pair(makeParserCodeCompletionStatus(), None);
  if (pattern.isNull())
    return std::make_pair(makeParserError(), None);

  // We don't accept initializers here, but parse one if it's there
  // for recovery purposes.
  ExprHandle *init = nullptr;
  if (Tok.is(tok::equal))
    parseDefaultArgument(*this, nullptr, 0, init);

  return std::make_pair(
      makeParserSuccess(),
        TuplePatternElt(pattern.get(), nullptr, DefaultArgumentKind::None));
}

ParserResult<Pattern> Parser::parsePatternTuple() {
  StructureMarkerRAII ParsingPatternTuple(*this, Tok);
  SourceLoc LPLoc = consumeToken(tok::l_paren);
  return parsePatternTupleAfterLP(LPLoc);
}

/// Parse a tuple pattern.  The leading left paren has already been consumed and
/// we are looking at the next token.  LPLoc specifies its location.
///
///   pattern-tuple:
///     '(' pattern-tuple-body? ')'
///   pattern-tuple-body:
///     pattern-tuple-element (',' pattern-tuple-body)*
ParserResult<Pattern>
Parser::parsePatternTupleAfterLP(SourceLoc LPLoc) {
  SourceLoc RPLoc, EllipsisLoc;

  // Parse all the elements.
  SmallVector<TuplePatternElt, 8> elts;
  ParserStatus ListStatus =
    parseList(tok::r_paren, LPLoc, RPLoc, tok::comma, /*OptionalSep=*/false,
              /*AllowSepAfterLast=*/false,
              diag::expected_rparen_tuple_pattern_list, [&] () -> ParserStatus {
    // Parse the pattern tuple element.
    ParserStatus EltStatus;
    Optional<TuplePatternElt> elt;
    std::tie(EltStatus, elt) = parsePatternTupleElement();
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

    // An ellipsis element shall have a specified element type.
    TypedPattern *typedPattern = dyn_cast<TypedPattern>(elt->getPattern());
    if (!typedPattern) {
      diagnose(ellLoc, diag::untyped_pattern_ellipsis)
        .highlight(elt->getPattern()->getSourceRange());
      // Return success so that the caller does not attempt recovery -- it
      // should have already happened when we were parsing the tuple element.
      return makeParserSuccess();
    }

    // Variadic elements must come last.
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

/// Parse a pattern with an optional type annotation.
///
///  typed-pattern ::= mattching-pattern (':' type)?
///
ParserResult<Pattern> Parser::parseTypedMatchingPattern() {
  auto result = parseMatchingPattern();
  
  // Now parse an optional type annotation.
  if (consumeIf(tok::colon)) {
    if (result.isNull())  // Recover by creating AnyPattern.
      result = makeParserErrorResult(new (Context) AnyPattern(PreviousLoc));
    
    ParserResult<TypeRepr> Ty = parseType();
    if (Ty.hasCodeCompletion())
      return makeParserCodeCompletionResult<Pattern>();
    if (Ty.isNull())
      Ty = makeParserResult(new (Context) ErrorTypeRepr(PreviousLoc));
    
    result = makeParserResult(result,
                              new (Context) TypedPattern(result.get(), Ty.get()));
  }
  
  return result;
}

/// matching-pattern ::= 'is' type
/// matching-pattern ::= matching-pattern-var
/// matching-pattern ::= expr
///
ParserResult<Pattern> Parser::parseMatchingPattern() {
  // TODO: Since we expect a pattern in this position, we should optimistically
  // parse pattern nodes for productions shared by pattern and expression
  // grammar. For short-term ease of initial implementation, we always go
  // through the expr parser for ambiguious productions.

  // Parse productions that can only be patterns.
  if (Tok.isAny(tok::kw_var, tok::kw_let)) {
    assert(Tok.isAny(tok::kw_let, tok::kw_var) && "expects var or let");
    bool isLet = Tok.is(tok::kw_let);
    SourceLoc varLoc = consumeToken();
    
    return parseMatchingPatternAsLetOrVar(isLet, varLoc);
  }
  
  // matching-pattern ::= 'is' type
  if (Tok.is(tok::kw_is)) {
    SourceLoc isLoc = consumeToken(tok::kw_is);
    ParserResult<TypeRepr> castType = parseType();
    if (castType.isNull() || castType.hasCodeCompletion())
      return nullptr;
    return makeParserResult(new (Context) IsPattern(isLoc, castType.get(),
                                                    nullptr));
  }

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

ParserResult<Pattern> Parser::parseMatchingPatternAsLetOrVar(bool isLet,
                                                             SourceLoc varLoc) {
  // 'var' and 'let' patterns shouldn't nest.
  if (InVarOrLetPattern == IVOLP_InLet ||
      InVarOrLetPattern == IVOLP_InVar)
    diagnose(varLoc, diag::var_pattern_in_var, unsigned(isLet));
  
  // 'let' isn't valid inside an implicitly immutable context, but var is.
  if (isLet && InVarOrLetPattern == IVOLP_ImplicitlyImmutable)
    diagnose(varLoc, diag::let_pattern_in_immutable_context);

  // In our recursive parse, remember that we're in a var/let pattern.
  llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
    T(InVarOrLetPattern, isLet ? IVOLP_InLet : IVOLP_InVar);

  ParserResult<Pattern> subPattern = parseMatchingPattern();
  if (subPattern.isNull())
    return nullptr;
  return makeParserResult(new (Context) VarPattern(varLoc, isLet,
                                                   subPattern.get()));
}


// Swift 1.x supported irrefutable patterns that unwrapped optionals and
// allowed type annotations.  We specifically handle the common cases of
// "if let x = foo()" and "if let x : AnyObject = foo()" for good QoI and
// migration.  These are not valid modern 'if let' sequences: the former
// isn't refutable, and the later isn't a valid matching pattern.
//
ParserResult<Pattern> Parser::
parseSwift1IfLetPattern(bool isLet, SourceLoc VarLoc) {
  assert(Tok.is(tok::identifier) && peekToken().isAny(tok::equal, tok::colon) &&
         "caller didn't check our requirements");
  
  Identifier Name;
  SourceLoc idLoc = consumeIdentifier(&Name);
  
  // Produce a nice diagnostic - complain about (and rewrite to 'x?').
  diagnose(idLoc, diag::expected_refutable_pattern_maybe_optional)
  .fixItInsertAfter(idLoc, "?");
  
  // If the type annotation is present, parse it and error.
  SourceLoc ColonLoc;
  if (consumeIf(tok::colon, ColonLoc)) {
    auto type = parseType();
    if (type.hasCodeCompletion())
      return makeParserCodeCompletionResult<Pattern>();
    if (type.isNull())
      return nullptr;
    
    diagnose(idLoc, diag::refutable_pattern_no_type_annotation)
    .fixItRemove(SourceRange(ColonLoc, type.get()->getEndLoc()));
  }
  
  // Return a pattern of "let x?".
  auto result = createBindingFromPattern(idLoc, Name, isLet);
  result = new (Context) OptionalSomePattern(result, idLoc, true);
  result = new (Context) VarPattern(VarLoc, isLet, result);
  
  return makeParserResult(result);
}

bool Parser::isOnlyStartOfMatchingPattern() {
  return Tok.isAny(tok::kw_var, tok::kw_let, tok::kw_is);
}


static bool canParsePatternTuple(Parser &P);

///   pattern ::= identifier
///   pattern ::= '_'
///   pattern ::= pattern-tuple
///   pattern ::= 'var' pattern
///   pattern ::= 'let' pattern
static bool canParsePattern(Parser &P) {
  switch (P.Tok.getKind()) {
  case tok::identifier:
  case tok::kw__:
    P.consumeToken();
    return true;
  case tok::kw_let:
  case tok::kw_var:
    P.consumeToken();
    return canParsePattern(P);
  case tok::l_paren:
    return canParsePatternTuple(P);

  default:
    return false;
  }
}


static bool canParsePatternTuple(Parser &P) {
  if (!P.consumeIf(tok::l_paren)) return false;

  if (P.Tok.isNot(tok::r_paren)) {
    do {
      if (!canParsePattern(P)) return false;
    } while (P.consumeIf(tok::comma));
  }

  return P.consumeIf(tok::r_paren);
}

///  typed-pattern ::= pattern (':' type)?
///
bool Parser::canParseTypedPattern() {
  if (!canParsePattern(*this)) return false;
  
  if (consumeIf(tok::colon))
    return canParseType();
  return true;
}


