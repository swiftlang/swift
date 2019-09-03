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

LayoutConstraint Parser::parseLayoutConstraint(Identifier LayoutConstraintID) {
  LayoutConstraint layoutConstraint =
      getLayoutConstraint(LayoutConstraintID, Context);
  assert(layoutConstraint->isKnownLayout() &&
         "Expected layout constraint definition");

  if (!layoutConstraint->isTrivial())
    return layoutConstraint;

  SourceLoc LParenLoc;
  if (!consumeIf(tok::l_paren, LParenLoc)) {
    // It is a trivial without any size constraints.
    return LayoutConstraint::getLayoutConstraint(LayoutConstraintKind::Trivial,
                                                 Context);
  }

  int size = 0;
  int alignment = 0;

  auto ParseTrivialLayoutConstraintBody = [&] () -> bool {
    // Parse the size and alignment.
    if (Tok.is(tok::integer_literal)) {
      if (Tok.getText().getAsInteger(10, size)) {
        diagnose(Tok.getLoc(), diag::layout_size_should_be_positive);
        return true;
      }
      consumeToken();
      if (consumeIf(tok::comma)) {
        // parse alignment.
        if (Tok.is(tok::integer_literal)) {
          if (Tok.getText().getAsInteger(10, alignment)) {
            diagnose(Tok.getLoc(), diag::layout_alignment_should_be_positive);
            return true;
          }
          consumeToken();
        } else {
          diagnose(Tok.getLoc(), diag::layout_alignment_should_be_positive);
          return true;
        }
      }
    } else {
      diagnose(Tok.getLoc(), diag::layout_size_should_be_positive);
      return true;
    }
    return false;
  };

  if (ParseTrivialLayoutConstraintBody()) {
    // There was an error during parsing.
    skipUntil(tok::r_paren);
    consumeIf(tok::r_paren);
    return LayoutConstraint::getUnknownLayout();
  }

  if (!consumeIf(tok::r_paren)) {
    // Expected a closing r_paren.
    diagnose(Tok.getLoc(), diag::expected_rparen_layout_constraint);
    consumeToken();
    return LayoutConstraint::getUnknownLayout();
  }

  if (size < 0) {
    diagnose(Tok.getLoc(), diag::layout_size_should_be_positive);
    return LayoutConstraint::getUnknownLayout();
  }

  if (alignment < 0) {
    diagnose(Tok.getLoc(), diag::layout_alignment_should_be_positive);
    return LayoutConstraint::getUnknownLayout();
  }

  // Otherwise it is a trivial layout constraint with
  // provided size and alignment.
  return LayoutConstraint::getLayoutConstraint(layoutConstraint->getKind(), size,
                                               alignment, Context);
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
Parser::TypeResult Parser::parseTypeSimple(Diag<> MessageID,
                                           bool HandleCodeCompletion) {
  if (Tok.is(tok::kw_inout) ||
      (Tok.is(tok::identifier) && (Tok.getRawText().equals("__shared") ||
                                   Tok.getRawText().equals("__owned")))) {
    // Type specifier should already be parsed before here. This only happens
    // for construct like 'P1 & inout P2'.
    diagnose(Tok.getLoc(), diag::attr_only_on_parameters, Tok.getRawText());
    consumeToken();
  }

  auto TypeLoc = leadingTriviaLoc();

  Optional<TypeResult> Result;
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
    if (CodeCompletion)
      CodeCompletion->completeTypeSimpleBeginning();
    // Eat the code completion token because we handled it.
    auto Token = consumeTokenSyntax(tok::code_complete);
    return makeParsedCodeCompletion<ParsedTypeSyntax>({Token});
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
      auto Token = consumeTokenSyntax();
      return makeParsedError<ParsedTypeSyntax>({Token});
    }

    checkForInputIncomplete();
    return makeParsedErrorEmpty<ParsedTypeSyntax>();
  }

  // '.Type', '.Protocol', '?', '!', and '[]' still leave us with type-simple.
  while (Result->isSuccess()) {
    auto PrevType = Result->getResult();

    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        (peekToken().isContextualKeyword("Type") ||
         peekToken().isContextualKeyword("Protocol"))) {
      Result = parseMetatypeType(PrevType);
      continue;
    }

    if (!Tok.isAtStartOfLine()) {
      if (isOptionalToken(Tok)) {
        Result = parseOptionalType(PrevType);
        continue;
      }
      if (isImplicitlyUnwrappedOptionalToken(Tok)) {
        Result = parseImplicitlyUnwrappedOptionalType(PrevType);
        continue;
      }
      // Parse legacy array types for migration.
      if (Tok.is(tok::l_square)) {
        Result = parseTypeArray(PrevType, TypeLoc);
        continue;
      }
    }
    break;
  }

  return *Result;
}

Parser::TypeASTResult Parser::parseType() {
  return parseType(diag::expected_type);
}

Parser::TypeASTResult Parser::parseSILBoxType(GenericParamList *generics,
                                              const TypeAttributes &attrs,
                                              Optional<Scope> &GenericsScope) {
  auto LBraceLoc = consumeToken(tok::l_brace);
  
  SmallVector<SILBoxTypeRepr::Field, 4> Fields;
  if (!Tok.is(tok::r_brace)) {
    for (;;) {
      bool Mutable;
      if (Tok.is(tok::kw_var)) {
        Mutable = true;
      } else if (Tok.is(tok::kw_let)) {
        Mutable = false;
      } else {
        diagnose(Tok, diag::sil_box_expected_var_or_let);
        return makeParserError();
      }
      SourceLoc VarOrLetLoc = consumeToken();
      
      auto fieldTy = parseType();
      if (!fieldTy.getPtrOrNull())
        return makeParserError();
      Fields.push_back({VarOrLetLoc, Mutable, fieldTy.get()});
      
      if (consumeIf(tok::comma))
        continue;
      
      break;
    }
  }
  
  if (!Tok.is(tok::r_brace)) {
    diagnose(Tok, diag::sil_box_expected_r_brace);
    return makeParserError();
  }
  
  auto RBraceLoc = consumeToken(tok::r_brace);
  
  // The generic arguments are taken from the enclosing scope. Pop the
  // box layout's scope now.
  GenericsScope.reset();
  
  SourceLoc LAngleLoc, RAngleLoc;
  SmallVector<TypeRepr*, 4> Args;
  if (Tok.isContextualPunctuator("<")) {
    LAngleLoc = consumeToken();
    for (;;) {
      auto argTy = parseType();
      if (!argTy.getPtrOrNull())
        return makeParserError();
      Args.push_back(argTy.get());
      if (consumeIf(tok::comma))
        continue;
      break;
    }
    if (!Tok.isContextualPunctuator(">")) {
      diagnose(Tok, diag::sil_box_expected_r_angle);
      return makeParserError();
    }
    
    RAngleLoc = consumeToken();
  }

  auto SILType = SILBoxTypeRepr::create(Context, generics, LBraceLoc, Fields,
                                        RBraceLoc, LAngleLoc, Args, RAngleLoc);

  auto AttributedType = applyAttributeToType(
      SILType, attrs, ParamDecl::Specifier::Owned, SourceLoc());

  return makeParserResult(AttributedType);
}

/// parseType
///   type:
///     attribute-list type-composition
///     attribute-list type-function
///
///   type-function:
///     type-composition 'throws'? '->' type
///
Parser::TypeASTResult Parser::parseType(Diag<> MessageID,
                                        bool HandleCodeCompletion,
                                        bool IsSILFuncDecl) {
  // Start a context for creating type syntax.
  SyntaxParsingContext TypeParsingContext(SyntaxContext,
                                          SyntaxContextKind::Type);
  auto TypeLoc = Tok.getLoc();

  // Parse attributes.
  ParamDecl::Specifier specifier;
  SourceLoc specifierLoc;
  TypeAttributes attrs;
  parseTypeAttributeList(specifier, specifierLoc, attrs);

  Optional<Scope> GenericsScope;

  // Parse generic parameters in SIL mode.
  GenericParamList *generics = nullptr;
  if (isInSILMode()) {
    // If this is part of a sil function decl, generic parameters are visible in
    // the function body; otherwise, they are visible when parsing the type.
    if (!IsSILFuncDecl)
      GenericsScope.emplace(this, ScopeKind::Generics);
    generics = maybeParseGenericParams().getPtrOrNull();
  }
  
  // In SIL mode, parse box types { ... }.
  if (isInSILMode() && Tok.is(tok::l_brace)) {
    auto SILBoxType = parseSILBoxType(generics, attrs, GenericsScope);
    Generator.addType(SILBoxType.getPtrOrNull(), TypeLoc);
    return SILBoxType;
  }

  auto RealTypeLoc = leadingTriviaLoc();

  ParserResult<TypeRepr> ty =
    parseTypeSimpleOrCompositionAST(MessageID, HandleCodeCompletion);
  if (ty.hasCodeCompletion())
    return makeParserCodeCompletionResult<TypeRepr>();
  if (ty.isNull())
    return nullptr;
  auto tyR = ty.get();

  // Parse a throws specifier.
  // Don't consume 'throws', if the next token is not '->', so we can emit a
  // more useful diagnostic when parsing a function decl.
  Optional<ParsedTokenSyntax> Throws;
  if (Tok.isAny(tok::kw_throws, tok::kw_rethrows, tok::kw_throw) &&
      peekToken().is(tok::arrow)) {
    if (Tok.isNot(tok::kw_throws)) {
      // 'rethrows' is only allowed on function declarations for now.
      // 'throw' is probably a typo for 'throws'.
      Diag<> DiagID = Tok.is(tok::kw_rethrows) ?
        diag::rethrowing_function_type : diag::throw_in_function_type;
      diagnose(Tok.getLoc(), DiagID)
        .fixItReplace(Tok.getLoc(), "throws");
    }

    Throws = consumeTokenSyntax();
  }

  if (Tok.is(tok::arrow)) {
    auto InputNode = SyntaxContext->popIf<ParsedTypeSyntax>().getValue();
    // Handle type-function if we have an arrow.
    auto ArrowLoc = Tok.getLoc();
    auto Arrow = consumeTokenSyntax();
    if (Tok.is(tok::kw_throws)) {
      Diag<> DiagID = diag::throws_in_wrong_position;
      diagnose(Tok.getLoc(), DiagID)
          .fixItInsert(ArrowLoc, "throws ")
          .fixItRemove(Tok.getLoc());
      Throws = consumeTokenSyntax();
    }
    ParserResult<TypeRepr> SecondHalf =
        parseType(diag::expected_type_function_result);
    auto SecondTy = SyntaxContext->popIf<ParsedTypeSyntax>();
    if (SecondHalf.isParseError()) {
      SyntaxContext->addSyntax(InputNode);
      if (Throws)
        SyntaxContext->addSyntax(*Throws);
      SyntaxContext->addSyntax(Arrow);
      if (SecondTy)
        SyntaxContext->addSyntax(*SecondTy);
      if (SecondHalf.hasCodeCompletion())
        return makeParserCodeCompletionResult<TypeRepr>();
      if (SecondHalf.isNull())
        return nullptr;
    }

    ParsedFunctionTypeSyntaxBuilder Builder(*SyntaxContext);
    bool isVoid = false;
    if (auto TupleTypeNode = InputNode.getAs<ParsedTupleTypeSyntax>()) {
      // Decompose TupleTypeSyntax and repack into FunctionType.
      auto LeftParen = TupleTypeNode->getDeferredLeftParen();
      auto Arguments = TupleTypeNode->getDeferredElements();
      auto RightParen = TupleTypeNode->getDeferredRightParen();
      Builder
        .useLeftParen(LeftParen)
        .useArguments(Arguments)
        .useRightParen(RightParen);
    } else {
      // FIXME(syntaxparse): Extract 'Void' text from recoreded node.
      if (const auto Void = dyn_cast<SimpleIdentTypeRepr>(tyR))
        isVoid =  (Void->getIdentifier().str() == "Void");

      if (isVoid) {
        diagnose(tyR->getStartLoc(), diag::function_type_no_parens)
          .fixItReplace(tyR->getStartLoc(), "()");
      } else {
        diagnose(tyR->getStartLoc(), diag::function_type_no_parens)
          .highlight(tyR->getSourceRange())
          .fixItInsert(tyR->getStartLoc(), "(")
          .fixItInsertAfter(tyR->getEndLoc(), ")");
      }
      Builder.addArgumentsMember(ParsedSyntaxRecorder::makeTupleTypeElement(
          InputNode, /*TrailingComma=*/None, *SyntaxContext));
    }

    Builder.useReturnType(*SecondTy);
    if (Throws)
      Builder.useThrowsOrRethrowsKeyword(*Throws);
    Builder.useArrow(Arrow);
    Builder.useReturnType(*SecondTy);

    SyntaxContext->addSyntax(Builder.build());

    auto FunctionType = SyntaxContext->topNode<FunctionTypeSyntax>();
    tyR = Generator.generate(FunctionType, RealTypeLoc);

    if (generics || isVoid) {
      auto FunctionTypeAST = cast<FunctionTypeRepr>(tyR);

      // TODO(syntaxparse): Represent 'Void -> ()' in libSyntax?
      auto argsTyR = FunctionTypeAST->getArgsTypeRepr();
      if (isVoid)
        argsTyR = TupleTypeRepr::createEmpty(Context, tyR->getSourceRange());

      // TODO(syntaxparse): Represent SIL generic type in libSyntax.
      tyR = new (Context) FunctionTypeRepr(
          generics, argsTyR, FunctionTypeAST->getThrowsLoc(),
          FunctionTypeAST->getArrowLoc(), FunctionTypeAST->getResultTypeRepr());
    }
  } else if (generics) {
    // Only function types may be generic.
    auto brackets = generics->getSourceRange();
    diagnose(brackets.Start, diag::generic_non_function);
    GenericsScope.reset();

    // Forget any generic parameters we saw in the type.
    class EraseTypeParamWalker : public ASTWalker {
    public:
      bool walkToTypeReprPre(TypeRepr *T) override {
        if (auto ident = dyn_cast<ComponentIdentTypeRepr>(T)) {
          if (auto decl = ident->getBoundDecl()) {
            if (auto genericParam = dyn_cast<GenericTypeParamDecl>(decl))
              ident->overwriteIdentifier(genericParam->getName());
          }
        }
        return true;
      }

    } walker;

    if (tyR)
      tyR->walk(walker);
  }
  if (specifierLoc.isValid() || !attrs.empty())
    SyntaxContext->setCreateSyntax(SyntaxKind::AttributedType);

  auto attributedType = applyAttributeToType(tyR, attrs, specifier, specifierLoc);

  Generator.addType(attributedType, TypeLoc);

  return makeParserResult(attributedType);
}

Parser::TypeASTResult Parser::parseDeclResultType(Diag<> MessageID) {
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
  auto LAngle = consumeStartingLessSyntax();

  SmallVector<ParsedGenericArgumentSyntax, 4> Args;
  SmallVector<ParsedSyntax, 0> Junk;
  Junk.push_back(LAngle);

  while (true) {
    ParserResult<TypeRepr> Ty = parseType(diag::expected_type);
    auto Type = SyntaxContext->popIf<ParsedTypeSyntax>();
    if (Ty.isParseError() || Ty.hasCodeCompletion()) {
      Junk.append(Args.begin(), Args.end());
      if (Type)
        Junk.push_back(*Type);
      skipUntilGreaterInTypeListSyntax(Junk);
      return makeParsedResult<ParsedGenericArgumentClauseSyntax>(Junk, Ty.getStatus());
    }
    auto Comma = consumeTokenSyntaxIf(tok::comma);
    auto Arg = ParsedSyntaxRecorder::makeGenericArgument(*Type, Comma, *SyntaxContext);
    Args.push_back(Arg);
    if (!Comma)
      break;
  }

  if (!startsWithGreater(Tok)) {
    checkForInputIncomplete();
    diagnose(Tok, diag::expected_rangle_generic_arg_list);
    diagnose(LAngleLoc, diag::opening_angle);

    Junk.append(Args.begin(), Args.end());
    skipUntilGreaterInTypeListSyntax(Junk);
    return makeParsedError<ParsedGenericArgumentClauseSyntax>(Junk);
  }

  auto ArgList =
      ParsedSyntaxRecorder::makeGenericArgumentList(Args, *SyntaxContext);
  auto RAngle = consumeStartingGreaterSyntax();
  auto Clause = ParsedSyntaxRecorder::makeGenericArgumentClause(
      LAngle, ArgList, RAngle, *SyntaxContext);
  return makeParsedSuccess(Clause);
}

ParserStatus
Parser::parseGenericArgumentsAST(SmallVectorImpl<TypeRepr *> &ArgsAST,
                                 SourceLoc &LAngleLoc, SourceLoc &RAngleLoc) {
  auto StartLoc = leadingTriviaLoc();
  auto ParsedClauseResult = parseGenericArgumentClauseSyntax();

  if (!ParsedClauseResult.isSuccess()) {
    for (auto &&Node : ParsedClauseResult.getUnknownNodes())
      SyntaxContext->addSyntax(Node);
    if (ParsedClauseResult.isCodeCompletion())
      return makeParserCodeCompletionStatus();
    return makeParserError();
  }

  SyntaxContext->addSyntax(ParsedClauseResult.getResult());
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
Parser::TypeResult Parser::parseTypeIdentifier() {
  if (Tok.isNot(tok::identifier) && Tok.isNot(tok::kw_Self)) {
    // is this the 'Any' type
    if (Tok.is(tok::kw_Any))
      return parseAnyType();

    if (Tok.is(tok::code_complete)) {
      if (CodeCompletion)
        CodeCompletion->completeTypeSimpleBeginning();
      // Eat the code completion token because we handled it.
      SmallVector<ParsedSyntax, 0> CodeComplete{consumeTokenSyntax(tok::code_complete)};
      return makeParsedCodeCompletion<ParsedTypeSyntax>(CodeComplete);
    }

    diagnose(Tok, diag::expected_identifier_for_type);

    // If there is a keyword at the start of a new line, we won't want to
    // skip it as a recovery but rather keep it.
    if (Tok.isKeyword() && !Tok.isAtStartOfLine()) {
      return makeParsedError<ParsedTypeSyntax>({consumeTokenSyntax()});
    }

    return makeParsedErrorEmpty<ParsedTypeSyntax>();
  }

  SmallVector<ParsedSyntax, 0> Junk;

  auto BaseLoc = leadingTriviaLoc();
  ParserStatus Status;
  Optional<ParsedTypeSyntax> Base;
  Optional<ParsedTokenSyntax> Period;
  while (true) {
    Optional<ParsedTokenSyntax> Identifier;
    if (Tok.is(tok::kw_Self)) {
      Identifier = consumeIdentifierSyntax();
    } else {
      // FIXME: specialize diagnostic for 'Type': type cannot start with
      // 'metatype'
      // FIXME: offer a fixit: 'self' -> 'Self'
      Identifier =
          parseIdentifierSyntax(diag::expected_identifier_in_dotted_type);
      if (!Identifier) {
        Status.setIsParseError();
        if (Base)
          Junk.push_back(*Base);
        if (Period)
          Junk.push_back(*Period);
      }
    }

    if (Identifier) {
      Optional<ParsedGenericArgumentClauseSyntax> GenericArgs;
      
      if (startsWithLess(Tok)) {
        SmallVector<TypeRepr *, 4> GenericArgsAST;
        SourceLoc LAngleLoc, RAngleLoc;
        auto GenericArgsResult = parseGenericArgumentClauseSyntax();
        if (!GenericArgsResult.isSuccess()) {
          if (Base)
            Junk.push_back(*Base);
          if (Period)
            Junk.push_back(*Period);
          Junk.push_back(*Identifier);
          auto genericJunks = GenericArgsResult.getUnknownNodes();
          Junk.append(genericJunks.begin(), genericJunks.end());
          return makeParsedResult<ParsedTypeSyntax>(
              Junk, GenericArgsResult.getStatus());
        }
        GenericArgs = GenericArgsResult.getResult();
      }

      if (!Base)
        Base = ParsedSyntaxRecorder::makeSimpleTypeIdentifier(
            *Identifier, GenericArgs, *SyntaxContext);
      else
        Base = ParsedSyntaxRecorder::makeMemberTypeIdentifier(
            *Base, *Period, *Identifier, GenericArgs, *SyntaxContext);
    }

    // Treat 'Foo.<anything>' as an attempt to write a dotted type
    // unless <anything> is 'Type'.
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix))) {
      if (peekToken().is(tok::code_complete)) {
        Status.setHasCodeCompletion();
        break;
      }
      if (!peekToken().isContextualKeyword("Type") &&
          !peekToken().isContextualKeyword("Protocol")) {
        Period = consumeTokenSyntax();
        continue;
      }
    } else if (Tok.is(tok::code_complete)) {
      if (!Tok.isAtStartOfLine())
        Status.setHasCodeCompletion();
      break;
    }
    break;
  }

  if (Status.hasCodeCompletion()) {
    IdentTypeRepr *ITR = nullptr;

    if (Base) {
      SyntaxContext->addSyntax(*Base);
      auto T = SyntaxContext->topNode<TypeSyntax>();
      SyntaxContext->popIf<ParsedTypeSyntax>();
      ITR = dyn_cast<IdentTypeRepr>(Generator.generate(T, BaseLoc));
      Junk.push_back(*Base);
    }

    if (Tok.isNot(tok::code_complete)) {
      // We have a dot.
      auto Dot = consumeTokenSyntax();
      Junk.push_back(Dot);
      if (CodeCompletion)
        CodeCompletion->completeTypeIdentifierWithDot(ITR);
    } else {
      if (CodeCompletion)
        CodeCompletion->completeTypeIdentifierWithoutDot(ITR);
    }
    // Eat the code completion token because we handled it.
    Junk.push_back(consumeTokenSyntax(tok::code_complete));
    return makeParsedCodeCompletion<ParsedTypeSyntax>(Junk);
  }
  
  if (Status.isError())
    return makeParsedError<ParsedTypeSyntax>(Junk);

  return makeParsedSuccess(*Base);
}

Parser::TypeASTResult
Parser::parseTypeSimpleOrCompositionAST(Diag<> MessageID,
                                        bool HandleCodeCompletion) {
  auto Loc = leadingTriviaLoc();

  auto CompositionResult =
      parseTypeSimpleOrComposition(MessageID, HandleCodeCompletion);

  if (!CompositionResult.isSuccess()) {
    auto nodes = CompositionResult.getUnknownNodes();
    if (nodes.size() > 0) {
      if (nodes.size() != 1 || !nodes.front().is<ParsedTypeSyntax>()) {
        auto ParsedUnknown = ParsedSyntaxRecorder::makeUnknownType(
            nodes, *SyntaxContext);
        SyntaxContext->addSyntax(ParsedUnknown);
      } else {
        SyntaxContext->addSyntax(nodes.front());
      }
    }
    TypeRepr *CorrectedAST = nullptr;
    if (SyntaxContext->isTopNode<TypeSyntax>()) {
      auto Unknown = SyntaxContext->topNode<TypeSyntax>();
      CorrectedAST = Generator.generate(Unknown, Loc);
    }
    return makeParserResult(CompositionResult.getStatus(), CorrectedAST);
  }

  SyntaxContext->addSyntax(CompositionResult.getResult());
  auto Composition = SyntaxContext->topNode<TypeSyntax>();
  auto CompositionAST = Generator.generate(Composition, Loc);

  return makeParserResult(CompositionAST);
}

/// parseTypeSimpleOrComposition
///
///   type-composition:
///     'some'? type-simple
///     type-composition '&' type-simple
Parser::TypeResult
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

  auto ApplySome = [this](ParsedTypeSyntax Type, Optional<ParsedTokenSyntax> Some) {
    return Some ? ParsedSyntaxRecorder::makeSomeType(*Some, Type, *SyntaxContext)
                : Type;
  };

  // Parse the first type
  auto FirstTypeResult = parseTypeSimple(MessageID, HandleCodeCompletion);

  // todo [gsoc]: handle Junk properly here
  if (!FirstTypeResult.isSuccess())
    return FirstTypeResult;

  auto FirstType = FirstTypeResult.getResult();

  if (!Tok.isContextualPunctuator("&"))
    return makeParsedSuccess(ApplySome(FirstType, FirstSome));

  SmallVector<ParsedCompositionTypeElementSyntax, 4> Elements;
  
  Optional<ParsedTokenSyntax> Ampersand = consumeTokenSyntax();
  auto FirstElement = ParsedSyntaxRecorder::makeCompositionTypeElement(
      FirstType, *Ampersand, *SyntaxContext);
  Elements.push_back(FirstElement);

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
      auto following = NextTypeResult.getUnknownNodes();
      if (following.empty()) {
        Status |= NextTypeResult.getStatus();
        break;
      }
      SmallVector<ParsedSyntax, 0> nodes;
      nodes.append(Elements.begin(), Elements.end());
      nodes.append(following.begin(), following.end());
      return makeParsedResult<ParsedTypeSyntax>(nodes, NextTypeResult.getStatus());
    }

    auto NextType = ApplySome(NextTypeResult.getResult(), NextSome);
    Ampersand = Tok.isContextualPunctuator("&") 
        ? consumeTokenSyntax() 
        : llvm::Optional<ParsedTokenSyntax>();
    auto NextElement = ParsedSyntaxRecorder::makeCompositionTypeElement(
        NextType, Ampersand, *SyntaxContext);
    Elements.push_back(NextElement);
  } while (Ampersand);

  // todo [gsoc]: handle failure here

  auto ElementList =
      ParsedSyntaxRecorder::makeCompositionTypeElementList(Elements, *SyntaxContext);
  auto Composition =
      ParsedSyntaxRecorder::makeCompositionType(ElementList, *SyntaxContext);
  if (Status.isSuccess()) {
    return makeParsedSuccess(ApplySome(Composition, FirstSome));
  } else {
    return makeParsedResult<ParsedTypeSyntax>({ApplySome(Composition, FirstSome)}, Status);
  }

  return makeParsedSuccess(ApplySome(Composition, FirstSome));
}

Parser::TypeASTResult Parser::parseAnyTypeAST() {
  auto AnyLoc = leadingTriviaLoc();
  auto ParsedAny = parseAnyType().getResult();
  SyntaxContext->addSyntax(ParsedAny);
  auto Any = SyntaxContext->topNode<SimpleTypeIdentifierSyntax>();
  return makeParserResult(Generator.generate(Any, AnyLoc));
}

Parser::TypeResult Parser::parseAnyType() {
  auto Any = consumeTokenSyntax(tok::kw_Any);
  auto Type = ParsedSyntaxRecorder::makeSimpleTypeIdentifier(Any, llvm::None,
                                                             *SyntaxContext);
  return makeParsedSuccess(Type);
}

/// parseOldStyleProtocolComposition
///   type-composition-deprecated:
///     'protocol' '<' '>'
///     'protocol' '<' type-composition-list-deprecated '>'
///
///   type-composition-list-deprecated:
///     type-identifier
///     type-composition-list-deprecated ',' type-identifier
Parser::TypeErrorResult Parser::parseOldStyleProtocolComposition() {
  // Defer all nodes so that we can de-structure the composed types in case we
  // need to emit a diagnostic (below).
  DeferringContextRAII Deferring(*SyntaxContext);

  SmallVector<ParsedSyntax, 0> Junk;

  auto ProtocolLoc = Tok.getLoc();
  auto Protocol = consumeTokenSyntax();
  auto LAngleLoc = Tok.getLoc();
  auto LAngle = consumeStartingLessSyntax();

  Junk.push_back(Protocol);
  Junk.push_back(LAngle);

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
      if (TypeResult.isSuccess()) {
        auto Type = TypeResult.getResult();
        Junk.push_back(Type);
        if (!IsAny)
          Protocols.push_back(Type);
      }
      Comma = consumeTokenSyntaxIf(tok::comma);
      if (Comma)
        Junk.push_back(*Comma);
    } while (Comma);
  }

  // Check for the terminating '>'.
  Optional<SourceLoc> RAngleLoc;
  if (startsWithGreater(Tok)) {
    RAngleLoc = Tok.getLoc();
    auto RAngle = consumeStartingGreaterSyntax();
    Junk.push_back(RAngle);
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
      Junk.push_back(Piece);
  }

  if (Status.isSuccess()) {
    SmallString<32> replacement;
    if (Protocols.empty()) {
      replacement = "Any";
    } else {
      auto extractText = [&](ParsedTypeSyntax Type) -> StringRef {
        auto SourceRange = Type.getRaw().getDeferredRange();
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
  return makeParsedSuccess(Unknown);
}

/// parseTypeTupleBody
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)* '...'?
///   type-tuple-element:
///     identifier? identifier ':' type
///     type
Parser::TypeResult Parser::parseTypeTupleBody() {
  // Force the context to create deferred nodes, as we might need to
  // de-structure the tuple type to create a function type.
  DeferringContextRAII Deferring(*SyntaxContext);
  Parser::StructureMarkerRAII ParsingTypeTuple(*this, Tok);

  if (ParsingTypeTuple.isFailed())
    return makeParsedError<ParsedTypeSyntax>({});

  SmallVector<ParsedSyntax, 0> Junk;
  
  auto LParenLoc = Tok.getLoc();
  auto LParen = consumeTokenSyntax(tok::l_paren);

  Junk.push_back(LParen);

  SmallVector<ParsedTupleTypeElementSyntax, 4> Elements;
  SmallVector<std::tuple<SourceLoc, SourceLoc, SourceLoc>, 4> ElementsLoc;
  Optional<ParsedTokenSyntax> FirstEllipsis;
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

      LocalJunk.push_back(*InOut);
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
      LocalJunk.push_back(*Name);

      // If there is a second name, consume it as well.
      if (Tok.canBeArgumentLabel()) {
        SecondNameLoc = Tok.getLoc();
        SecondName = consumeArgumentLabelSyntax();
        LocalJunk.push_back(*SecondName);
      }

      // Consume the ':'.
      if ((Colon = consumeTokenSyntaxIf(tok::colon))) {
        LocalJunk.push_back(*Colon);
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

    Backtracking.reset();

    // Parse the type annotation.
    auto TypeLoc = Tok.getLoc();
    auto TypeASTResult = parseType(diag::expected_type);
    auto Type = SyntaxContext->popIf<ParsedTypeSyntax>();
    if (TypeASTResult.hasCodeCompletion() || TypeASTResult.isNull()) {
      Junk.append(LocalJunk.begin(), LocalJunk.end());
      if (Type)
        Junk.push_back(*Type);
      skipListUntilDeclRBraceSyntax(Junk, LParenLoc, tok::r_paren, tok::comma);
      return TypeASTResult.hasCodeCompletion()
                 ? makeParserCodeCompletionStatus()
                 : makeParserError();
    }

    if (IsInOutObsoleted) {
      bool IsTypeAlreadyAttributed = false;
      if (auto AttributedType = Type->getAs<ParsedAttributedTypeSyntax>())
        IsTypeAlreadyAttributed = AttributedType->getDeferredSpecifier().hasValue();

      if (IsTypeAlreadyAttributed) {
        // If the parsed type is already attributed, suggest removing `inout`.
        diagnose(Tok, diag::parameter_specifier_repeated)
            .fixItRemove(InOutLoc);
      } else {
        diagnose(InOutLoc, diag::parameter_specifier_as_attr_disallowed, "inout")
            .fixItRemove(InOutLoc)
            .fixItInsert(TypeLoc, "inout ");
      }
    }

    Optional<ParsedTokenSyntax> ElementEllipsis;
    if (Tok.isEllipsis()) {
      Tok.setKind(tok::ellipsis);
      auto ElementEllipsisLoc = Tok.getLoc();
      ElementEllipsis = consumeTokenSyntax();
      if (!FirstEllipsis) {
        FirstEllipsis = ElementEllipsis;
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
      auto Expr = *SyntaxContext->popIf<ParsedExprSyntax>();
      Initializer = ParsedSyntaxRecorder::makeInitializerClause(*Equal, Expr,
                                                                *SyntaxContext);
    }

    Comma = consumeTokenSyntaxIf(tok::comma);

    auto Element = ParsedSyntaxRecorder::makeTupleTypeElement(
        InOut, Name, SecondName, Colon, *Type, ElementEllipsis, Initializer,
        Comma, *SyntaxContext);

    Junk.push_back(Element);

    Elements.push_back(Element);
    ElementsLoc.emplace_back(NameLoc, SecondNameLoc, TypeLoc);

    return makeParserSuccess();
  });

  if (!Status.isSuccess())
    return makeParsedResult<ParsedTupleTypeSyntax>(Junk, Status);

  auto ElementList =
      ParsedSyntaxRecorder::makeTupleTypeElementList(Elements, *SyntaxContext);

  auto TupleType = ParsedSyntaxRecorder::makeTupleType(LParen, ElementList,
                                                       *RParen, *SyntaxContext);

  bool IsFunctionType = Tok.isAny(tok::arrow, tok::kw_throws, tok::kw_rethrows);

  auto GetNameText = [this](Optional<ParsedTokenSyntax> Name) {
    return !Name ? StringRef()
                 : SourceMgr.extractText(
                       Name->getRaw().getDeferredTokenRangeWithoutBackticks(),
                       L->getBufferID());
  };

  if (!IsFunctionType) {
    for (unsigned i = 0; i < Elements.size(); i++) {
      // true tuples have labels
      auto Element = Elements[i];
      SourceLoc NameLoc, SecondNameLoc, TypeLoc;
      std::tie(NameLoc, SecondNameLoc, TypeLoc) = ElementsLoc[i];
      // If there were two names, complain.
      if (NameLoc.isValid() && SecondNameLoc.isValid()) {
        auto Diag = diagnose(NameLoc, diag::tuple_type_multiple_labels);
        auto Name = Element.getDeferredName();
        auto NameText = SourceMgr.extractText(
            Name->getRaw().getDeferredTokenRangeWithoutBackticks(),
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
      auto Element = Elements[i];
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

  return makeParsedSuccess(TupleType);
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
Parser::TypeErrorResult Parser::parseTypeArray(ParsedTypeSyntax Base,
                                               SourceLoc BaseLoc) {
  assert(Tok.isFollowingLSquare());
  Parser::StructureMarkerRAII ParsingArrayBound(*this, Tok);
  SmallVector<ParsedSyntax, 0> Junk{Base};
  auto LSquareLoc = Tok.getLoc();
  auto LSquare = consumeTokenSyntax();
  Junk.push_back(LSquare);

  if (Tok.isNot(tok::r_square)) {
    auto SizeExprAST = parseExprBasic(diag::expected_expr);
    if (SizeExprAST.hasCodeCompletion())
      return makeParsedCodeCompletion<ParsedUnknownTypeSyntax>(Junk);
    if (SizeExprAST.isNull())
      return makeParsedError<ParsedUnknownTypeSyntax>(Junk);
    if (auto ParsedSizeExpr = SyntaxContext->popIf<ParsedExprSyntax>())
      Junk.push_back(*ParsedSizeExpr);
  }

  auto RSquare = parseMatchingTokenSyntax(
      tok::r_square, diag::expected_rbracket_array_type, LSquareLoc);

  if (RSquare) {
    Junk.push_back(*RSquare);
    // If we parsed something valid, diagnose it with a fixit to rewrite it to
    // Swift syntax.
    diagnose(LSquareLoc, diag::new_array_syntax)
        .fixItInsert(BaseLoc, "[")
        .fixItRemove(LSquareLoc);
  }

  auto Unknown = ParsedSyntaxRecorder::makeUnknownType(Junk, *SyntaxContext);
  return makeParsedSuccess(Unknown);
}

/// Parse a collection type.
///   type-simple:
///     '[' type ']'
///     '[' type ':' type ']'
Parser::TypeResult Parser::parseTypeCollection() {
  ParserStatus Status;
  assert(Tok.is(tok::l_square));
  Parser::StructureMarkerRAII parsingCollection(*this, Tok);
  auto LSquareLoc = Tok.getLoc();
  auto LSquare = consumeTokenSyntax(tok::l_square);

  auto ElementTypeASTResult = parseType(diag::expected_element_type);
  auto ElementType = SyntaxContext->popIf<ParsedTypeSyntax>();
  Status |= ElementTypeASTResult;

  Optional<ParsedTokenSyntax> Colon;
  ParserResult<TypeRepr> ValueTypeASTResult;
  Optional<ParsedTypeSyntax> ValueType;

  if (Tok.is(tok::colon)) {
    Colon = consumeTokenSyntax(tok::colon);
    ValueTypeASTResult = parseType(diag::expected_dictionary_value_type);
    ValueType = SyntaxContext->popIf<ParsedTypeSyntax>();
    Status |= ValueTypeASTResult;
  }

  auto Diag = Colon ? diag::expected_rbracket_dictionary_type
                    : diag::expected_rbracket_array_type;
  auto RSquare = parseMatchingTokenSyntax(tok::r_square, Diag, LSquareLoc);
  if (!RSquare)
    Status.setIsParseError();

  if (!Status.isSuccess()) {
    SmallVector<ParsedSyntax, 0> Pieces;
    Pieces.push_back(LSquare);
    if (ElementType)
      Pieces.push_back(*ElementType);
    if (Colon)
      Pieces.push_back(*Colon);
    if (ValueType)
      Pieces.push_back(*ValueType);
    if (RSquare)
      Pieces.push_back(*RSquare);

    return makeParsedResult<ParsedTypeSyntax>(Pieces, Status);
  }

  if (Colon)
    return makeParsedSuccess(ParsedSyntaxRecorder::makeDictionaryType(
        LSquare, *ElementType, *Colon, *ValueType, *RSquare, *SyntaxContext));
  
  return makeParsedSuccess(ParsedSyntaxRecorder::makeArrayType(
      LSquare, *ElementType, *RSquare, *SyntaxContext));
}

Parser::TypeResult Parser::parseMetatypeType(ParsedTypeSyntax Base) {
  auto Period = consumeTokenSyntax(); // tok::period or tok::period_prefix
  auto Keyword = consumeTokenSyntax(tok::identifier); // "Type" or "Protocol"
  auto MetatypeType = ParsedSyntaxRecorder::makeMetatypeType(
      Base, Period, Keyword, *SyntaxContext);
  return makeParsedSuccess(MetatypeType);
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

Parser::TypeResult Parser::parseOptionalType(ParsedTypeSyntax Base) {
  auto Question = consumeOptionalTokenSyntax();
  auto Optional =
      ParsedSyntaxRecorder::makeOptionalType(Base, Question, *SyntaxContext);
  return makeParsedSuccess(Optional);
}

Parser::TypeResult
Parser::parseImplicitlyUnwrappedOptionalType(ParsedTypeSyntax Base) {
  auto Exclamation = consumeImplicitlyUnwrappedOptionalTokenSyntax();
  auto Unwrapped = ParsedSyntaxRecorder::makeImplicitlyUnwrappedOptionalType(
      Base, Exclamation, *SyntaxContext);
  return makeParsedSuccess(Unwrapped);
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
