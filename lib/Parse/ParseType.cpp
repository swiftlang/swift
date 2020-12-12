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

#include "ParseList.h"
#include "swift/Parse/Parser.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/GenericParamList.h"
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
ParserResult<TypeRepr> Parser::parseTypeSimple(Diag<> MessageID) {
  ParserResult<TypeRepr> ty;

  if (Tok.is(tok::kw_inout) ||
      (Tok.is(tok::identifier) && (Tok.getRawText().equals("__shared") ||
                                   Tok.getRawText().equals("__owned")))) {
    // Type specifier should already be parsed before here. This only happens
    // for construct like 'P1 & inout P2'.
    diagnose(Tok.getLoc(), diag::attr_only_on_parameters, Tok.getRawText());
    consumeToken();
  }

  switch (Tok.getKind()) {
  case tok::kw_Self:
  case tok::kw_Any:
  case tok::identifier: {
    ty = parseTypeIdentifier();
    break;
  }
  case tok::l_paren:
    ty = parseTypeTupleBody();
    break;
  case tok::code_complete:
    if (CodeCompletion)
      CodeCompletion->completeTypeSimpleBeginning();
    return makeParserCodeCompletionResult<TypeRepr>(
        new (Context) ErrorTypeRepr(consumeToken(tok::code_complete)));
  case tok::l_square: {
    ty = parseTypeCollection();
    break;
  }
  case tok::kw_protocol:
    if (startsWithLess(peekToken())) {
      ty = parseOldStyleProtocolComposition();
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
      ty = makeParserErrorResult(new (Context) ErrorTypeRepr(Tok.getLoc()));
      consumeToken();
      return ty;
    }
    checkForInputIncomplete();
    return nullptr;
  }

  // '.Type', '.Protocol', '?', '!', and '[]' still leave us with type-simple.
  while (ty.isNonNull()) {
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix))) {
      if (peekToken().isContextualKeyword("Type")) {
        consumeToken();
        SourceLoc metatypeLoc = consumeToken(tok::identifier);
        ty = makeParserResult(ty,
          new (Context) MetatypeTypeRepr(ty.get(), metatypeLoc));
        SyntaxContext->createNodeInPlace(SyntaxKind::MetatypeType);
        continue;
      }
      if (peekToken().isContextualKeyword("Protocol")) {
        consumeToken();
        SourceLoc protocolLoc = consumeToken(tok::identifier);
        ty = makeParserResult(ty,
          new (Context) ProtocolTypeRepr(ty.get(), protocolLoc));
        SyntaxContext->createNodeInPlace(SyntaxKind::MetatypeType);
        continue;
      }
    }

    if (!Tok.isAtStartOfLine()) {
      if (isOptionalToken(Tok)) {
        ty = parseTypeOptional(ty.get());
        continue;
      }
      if (isImplicitlyUnwrappedOptionalToken(Tok)) {
        ty = parseTypeImplicitlyUnwrappedOptional(ty.get());
        continue;
      }
      // Parse legacy array types for migration.
      if (Tok.is(tok::l_square)) {
        ty = parseTypeArray(ty.get());
        continue;
      }
    }
    break;
  }

  return ty;
}

ParserResult<TypeRepr> Parser::parseType() {
  return parseType(diag::expected_type);
}

ParserResult<TypeRepr> Parser::parseSILBoxType(GenericParamList *generics,
                                               const TypeAttributes &attrs) {
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
      
      if (!consumeIf(tok::comma))
        break;
    }
  }
  
  if (!Tok.is(tok::r_brace)) {
    diagnose(Tok, diag::sil_box_expected_r_brace);
    return makeParserError();
  }
  
  auto RBraceLoc = consumeToken(tok::r_brace);

  SourceLoc LAngleLoc, RAngleLoc;
  SmallVector<TypeRepr*, 4> Args;
  if (startsWithLess(Tok)) {
    LAngleLoc = consumeStartingLess();
    for (;;) {
      auto argTy = parseType();
      if (!argTy.getPtrOrNull())
        return makeParserError();
      Args.push_back(argTy.get());
      if (!consumeIf(tok::comma))
        break;
    }
    if (!startsWithGreater(Tok)) {
      diagnose(Tok, diag::sil_box_expected_r_angle);
      return makeParserError();
    }
    
    RAngleLoc = consumeStartingGreater();
  }
  
  auto repr = SILBoxTypeRepr::create(Context, generics,
                                     LBraceLoc, Fields, RBraceLoc,
                                     LAngleLoc, Args, RAngleLoc);
  return makeParserResult(applyAttributeToType(repr, attrs,
                                               ParamDecl::Specifier::Owned,
                                               SourceLoc()));
}


/// parseType
///   type:
///     attribute-list type-composition
///     attribute-list type-function
///
///   type-function:
///     type-composition 'async'? 'throws'? '->' type
///
ParserResult<TypeRepr> Parser::parseType(Diag<> MessageID,
                                         bool IsSILFuncDecl) {
  // Start a context for creating type syntax.
  SyntaxParsingContext TypeParsingContext(SyntaxContext,
                                          SyntaxContextKind::Type);
  // Parse attributes.
  ParamDecl::Specifier specifier;
  SourceLoc specifierLoc;
  TypeAttributes attrs;
  parseTypeAttributeList(specifier, specifierLoc, attrs);

  // Parse generic parameters in SIL mode.
  GenericParamList *generics = nullptr;
  SourceLoc substitutedLoc;
  GenericParamList *patternGenerics = nullptr;
  if (isInSILMode()) {
    generics = maybeParseGenericParams().getPtrOrNull();
    
    if (Tok.is(tok::at_sign) && peekToken().getText() == "substituted") {
      consumeToken(tok::at_sign);
      substitutedLoc = consumeToken(tok::identifier);
      patternGenerics = maybeParseGenericParams().getPtrOrNull();
      if (!patternGenerics) {
        diagnose(Tok.getLoc(), diag::sil_function_subst_expected_generics);
      }
    }
  }
  
  // In SIL mode, parse box types { ... }.
  if (isInSILMode() && Tok.is(tok::l_brace)) {
    if (patternGenerics) {
      diagnose(Tok.getLoc(), diag::sil_function_subst_expected_function);
    }
    return parseSILBoxType(generics, attrs);
  }

  ParserResult<TypeRepr> ty = parseTypeSimpleOrComposition(MessageID);
  if (ty.isNull())
    return ty;
  auto tyR = ty.get();
  auto status = ParserStatus(ty);

  // Parse an async specifier.
  SourceLoc asyncLoc;
  if (shouldParseExperimentalConcurrency() &&
      Tok.isContextualKeyword("async") &&
      peekToken().isAny(tok::arrow, tok::kw_throws)) {
    asyncLoc = consumeToken();
  }

  // Parse a throws specifier.
  // Don't consume 'throws', if the next token is not '->' or 'async', so we
  // can emit a more useful diagnostic when parsing a function decl.
  SourceLoc throwsLoc;
  if (Tok.isAny(tok::kw_throws, tok::kw_rethrows, tok::kw_throw, tok::kw_try) &&
      (peekToken().is(tok::arrow) ||
       (shouldParseExperimentalConcurrency() &&
        peekToken().isContextualKeyword("async")))) {
    if (Tok.isAny(tok::kw_rethrows, tok::kw_throw, tok::kw_try)) {
      // 'rethrows' is only allowed on function declarations for now.
      // 'throw' or 'try' are probably typos for 'throws'.
      Diag<> DiagID = Tok.is(tok::kw_rethrows) ?
        diag::rethrowing_function_type : diag::throw_in_function_type;
      diagnose(Tok.getLoc(), DiagID)
        .fixItReplace(Tok.getLoc(), "throws");
    }
    throwsLoc = consumeToken();

    // 'async' must preceed 'throws'; accept this but complain.
    if (shouldParseExperimentalConcurrency() &&
        Tok.isContextualKeyword("async")) {
      asyncLoc = consumeToken();

      diagnose(asyncLoc, diag::async_after_throws, false)
        .fixItRemove(asyncLoc)
        .fixItInsert(throwsLoc, "async ");
    }
  }

  if (Tok.is(tok::arrow)) {
    // Handle type-function if we have an arrow.
    SourceLoc arrowLoc = consumeToken();

    // Handle async/throws in the wrong place.
    parseAsyncThrows(arrowLoc, asyncLoc, throwsLoc, /*rethrows=*/nullptr);

    ParserResult<TypeRepr> SecondHalf =
        parseType(diag::expected_type_function_result);
    if (SecondHalf.isNull()) {
      status.setIsParseError();
      return status;
    }
    status |= SecondHalf;

    if (SyntaxContext->isEnabled()) {
      ParsedFunctionTypeSyntaxBuilder Builder(*SyntaxContext);
      Builder.useReturnType(std::move(*SyntaxContext->popIf<ParsedTypeSyntax>()));
      Builder.useArrow(SyntaxContext->popToken());
      if (throwsLoc.isValid())
        Builder.useThrowsOrRethrowsKeyword(SyntaxContext->popToken());
      if (asyncLoc.isValid())
        Builder.useAsyncKeyword(SyntaxContext->popToken());

      auto InputNode(std::move(*SyntaxContext->popIf<ParsedTypeSyntax>()));
      if (auto TupleTypeNode = InputNode.getAs<ParsedTupleTypeSyntax>()) {
        // Decompose TupleTypeSyntax and repack into FunctionType.
        auto LeftParen = TupleTypeNode->getDeferredLeftParen();
        auto Arguments = TupleTypeNode->getDeferredElements();
        auto RightParen = TupleTypeNode->getDeferredRightParen();
        Builder
          .useLeftParen(std::move(LeftParen))
          .useArguments(std::move(Arguments))
          .useRightParen(std::move(RightParen));
      } else {
        Builder.addArgumentsMember(ParsedSyntaxRecorder::makeTupleTypeElement(
            std::move(InputNode), /*TrailingComma=*/None, *SyntaxContext));
      }
      SyntaxContext->addSyntax(Builder.build());
    }

    TupleTypeRepr *argsTyR = nullptr;
    if (auto *TTArgs = dyn_cast<TupleTypeRepr>(tyR)) {
      argsTyR = TTArgs;
    } else {
      bool isVoid = false;
      if (const auto Void = dyn_cast<SimpleIdentTypeRepr>(tyR)) {
        if (Void->getNameRef().isSimpleName(Context.Id_Void)) {
          isVoid = true;
        }
      }

      if (isVoid) {
        diagnose(tyR->getStartLoc(), diag::function_type_no_parens)
          .fixItReplace(tyR->getStartLoc(), "()");
        argsTyR = TupleTypeRepr::createEmpty(Context, tyR->getSourceRange());
      } else {
        diagnose(tyR->getStartLoc(), diag::function_type_no_parens)
          .highlight(tyR->getSourceRange())
          .fixItInsert(tyR->getStartLoc(), "(")
          .fixItInsertAfter(tyR->getEndLoc(), ")");
        argsTyR = TupleTypeRepr::create(Context, {tyR},
                                        tyR->getSourceRange());
      }
    }
    
    // Parse substitutions for substituted SIL types.
    MutableArrayRef<TypeRepr *> invocationSubsTypes;
    MutableArrayRef<TypeRepr *> patternSubsTypes;
    if (isInSILMode()) {
      auto parseSubstitutions =
          [&](MutableArrayRef<TypeRepr*> &subs) -> Optional<bool> {
        if (!consumeIf(tok::kw_for)) return None;

        if (!startsWithLess(Tok)) {
          diagnose(Tok, diag::sil_function_subst_expected_l_angle);
          return false;
        }

        consumeStartingLess();

        SmallVector<TypeRepr*, 4> SubsTypesVec;
        for (;;) {
          auto argTy = parseType();
          if (!argTy.getPtrOrNull())
            return false;
          SubsTypesVec.push_back(argTy.get());
          if (!consumeIf(tok::comma))
            break;
        }
        if (!startsWithGreater(Tok)) {
          diagnose(Tok, diag::sil_function_subst_expected_r_angle);
          return false;
        }
        consumeStartingGreater();

        subs = Context.AllocateCopy(SubsTypesVec);
        return true;
      };

      // Parse pattern substitutions.  These must exist if we had pattern
      // generics above.
      if (patternGenerics) {
        auto result = parseSubstitutions(patternSubsTypes);
        if (!result || patternSubsTypes.empty()) {
          diagnose(Tok, diag::sil_function_subst_expected_subs);
          patternGenerics = nullptr;
        } else if (!*result) {
          return makeParserError();
        }
      }

      if (generics) {
        if (auto result = parseSubstitutions(invocationSubsTypes))
          if (!*result) return makeParserError();
      }

      if (Tok.is(tok::kw_for)) {
        diagnose(Tok, diag::sil_function_subs_without_generics);
        return makeParserError();
      }
    }

    tyR = new (Context) FunctionTypeRepr(generics, argsTyR, asyncLoc, throwsLoc,
                                         arrowLoc, SecondHalf.get(),
                                         patternGenerics, patternSubsTypes,
                                         invocationSubsTypes);
  } else if (auto firstGenerics = generics ? generics : patternGenerics) {
    // Only function types may be generic.
    auto brackets = firstGenerics->getSourceRange();
    diagnose(brackets.Start, diag::generic_non_function);

    // Forget any generic parameters we saw in the type.
    class EraseTypeParamWalker : public ASTWalker {
    public:
      bool walkToTypeReprPre(TypeRepr *T) override {
        if (auto ident = dyn_cast<ComponentIdentTypeRepr>(T)) {
          if (auto decl = ident->getBoundDecl()) {
            if (auto genericParam = dyn_cast<GenericTypeParamDecl>(decl))
              ident->overwriteNameRef(genericParam->createNameRef());
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

  return makeParserResult(status, applyAttributeToType(tyR, attrs, specifier,
                                                       specifierLoc));
}

ParserResult<TypeRepr> Parser::parseDeclResultType(Diag<> MessageID) {
  if (Tok.is(tok::code_complete)) {
    if (CodeCompletion)
      CodeCompletion->completeTypeDeclResultBeginning();
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionStatus();
  }

  auto result = parseType(MessageID);

  if (!result.isParseErrorOrHasCompletion()) {
    if (Tok.is(tok::r_square)) {
      auto diag = diagnose(Tok, diag::extra_rbracket);
      diag.fixItInsert(result.get()->getStartLoc(), getTokenText(tok::l_square));
      consumeToken();
      return makeParserErrorResult(new (Context)
                                       ErrorTypeRepr(getTypeErrorLoc()));
    }

    if (Tok.is(tok::colon)) {
      auto colonTok = consumeToken();
      auto secondType = parseType(diag::expected_dictionary_value_type);

      auto diag = diagnose(colonTok, diag::extra_colon);
      diag.fixItInsert(result.get()->getStartLoc(), getTokenText(tok::l_square));
      if (!secondType.isParseErrorOrHasCompletion()) {
        if (Tok.is(tok::r_square)) {
          consumeToken();
        } else {
          diag.fixItInsertAfter(secondType.get()->getEndLoc(), getTokenText(tok::r_square));
        }
      }
      return makeParserErrorResult(new (Context)
                                       ErrorTypeRepr(getTypeErrorLoc()));
    }
  }
  return result;
}

SourceLoc Parser::getTypeErrorLoc() const {
  // Use the same location as a missing close brace, etc.
  return getErrorOrMissingLoc();
}

ParserStatus Parser::parseGenericArguments(SmallVectorImpl<TypeRepr *> &Args,
                                           SourceLoc &LAngleLoc,
                                           SourceLoc &RAngleLoc) {
  SyntaxParsingContext GenericArgumentsContext(
      SyntaxContext, SyntaxKind::GenericArgumentClause);

  // Parse the opening '<'.
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  LAngleLoc = consumeStartingLess();

  {
    SyntaxParsingContext ListContext(SyntaxContext,
        SyntaxKind::GenericArgumentList);

    while (true) {
      SyntaxParsingContext ElementContext(SyntaxContext,
                                          SyntaxKind::GenericArgument);
      ParserResult<TypeRepr> Ty = parseType(diag::expected_type);
      if (Ty.isNull() || Ty.hasCodeCompletion()) {
        // Skip until we hit the '>'.
        RAngleLoc = skipUntilGreaterInTypeList();
        return ParserStatus(Ty);
      }

      Args.push_back(Ty.get());
      // Parse the comma, if the list continues.
      if (!consumeIf(tok::comma))
        break;
    }
  }

  if (!startsWithGreater(Tok)) {
    checkForInputIncomplete();
    diagnose(Tok, diag::expected_rangle_generic_arg_list);
    diagnose(LAngleLoc, diag::opening_angle);

    // Skip until we hit the '>'.
    RAngleLoc = skipUntilGreaterInTypeList();
    return makeParserError();
  } else {
    RAngleLoc = consumeStartingGreater();
  }

  return makeParserSuccess();
}

ParserResult<TypeRepr>
Parser::parseTypeIdentifier(bool isParsingQualifiedDeclBaseType) {
  auto typeLoc = leadingTriviaLoc();
  auto result = parseTypeIdentifierSyntax(isParsingQualifiedDeclBaseType);
  auto status = result.getStatus();
  if (result.isNull()) {
    return status;
  }
  SyntaxContext->addSyntax(std::move(result.get()));
  auto collectionType = SyntaxContext->topNode<TypeSyntax>();
  auto typeRepr = ASTGenerator.generate(collectionType, typeLoc);
  if (!typeRepr) {
    status.setIsParseError();
  }
  return makeParserResult(status, typeRepr);
}

/// parseTypeSimpleOrComposition
///
///   type-composition:
///     'some'? type-simple
///     type-composition '&' type-simple
ParserResult<TypeRepr>
Parser::parseTypeSimpleOrComposition(Diag<> MessageID) {
  SyntaxParsingContext SomeTypeContext(SyntaxContext, SyntaxKind::SomeType);
  // Check for the opaque modifier.
  // This is only semantically allowed in certain contexts, but we parse it
  // generally for diagnostics and recovery.
  SourceLoc opaqueLoc;
  if (Tok.isContextualKeyword("some")) {
    // Treat some as a keyword.
    TokReceiver->registerTokenKindChange(Tok.getLoc(), tok::contextual_keyword);
    opaqueLoc = consumeToken();
  } else {
    // This isn't a some type.
    SomeTypeContext.setTransparent();
  }
  
  auto applyOpaque = [&](TypeRepr *type) -> TypeRepr* {
    if (opaqueLoc.isValid()) {
      type = new (Context) OpaqueReturnTypeRepr(opaqueLoc, type);
    }
    return type;
  };
  
  SyntaxParsingContext CompositionContext(SyntaxContext, SyntaxContextKind::Type);
  // Parse the first type
  ParserResult<TypeRepr> FirstType = parseTypeSimple(MessageID);
  if (FirstType.isNull())
    return FirstType;
  if (!Tok.isContextualPunctuator("&")) {
    return makeParserResult(ParserStatus(FirstType),
                            applyOpaque(FirstType.get()));
  }

  SmallVector<TypeRepr *, 4> Types;
  ParserStatus Status(FirstType);
  SourceLoc FirstTypeLoc = FirstType.get()->getStartLoc();
  SourceLoc FirstAmpersandLoc = Tok.getLoc();

  auto addType = [&](TypeRepr *T) {
    if (!T) return;
    if (auto Comp = dyn_cast<CompositionTypeRepr>(T)) {
      // Accept protocol<P1, P2> & P3; explode it.
      auto TyRs = Comp->getTypes();
      if (!TyRs.empty()) // If empty, is 'Any'; ignore.
        Types.append(TyRs.begin(), TyRs.end());
      return;
    }
    Types.push_back(T);
  };

  addType(FirstType.get());
  SyntaxContext->setCreateSyntax(SyntaxKind::CompositionType);
  assert(Tok.isContextualPunctuator("&"));
  do {
    if (SyntaxContext->isEnabled()) {
      auto Type = SyntaxContext->popIf<ParsedTypeSyntax>();
      consumeToken(); // consume '&'
      if (Type) {
        ParsedCompositionTypeElementSyntaxBuilder Builder(*SyntaxContext);
        auto Ampersand = SyntaxContext->popToken();
        Builder
          .useAmpersand(std::move(Ampersand))
          .useType(std::move(*Type));
        SyntaxContext->addSyntax(Builder.build());
      }
    } else {
      consumeToken(); // consume '&'
    }
    
    // Diagnose invalid `some` after an ampersand.
    if (Tok.isContextualKeyword("some")) {
      auto badLoc = consumeToken();

      diagnose(badLoc, diag::opaque_mid_composition)
          .fixItRemove(badLoc)
          .fixItInsert(FirstTypeLoc, "some ");

      if (opaqueLoc.isInvalid())
        opaqueLoc = badLoc;
    }

    // Parse next type.
    ParserResult<TypeRepr> ty =
      parseTypeSimple(diag::expected_identifier_for_type);
    if (ty.hasCodeCompletion())
      return makeParserCodeCompletionResult<TypeRepr>();
    Status |= ty;
    addType(ty.getPtrOrNull());
  } while (Tok.isContextualPunctuator("&"));

  if (SyntaxContext->isEnabled()) {
    if (auto synType = SyntaxContext->popIf<ParsedTypeSyntax>()) {
      auto LastNode = ParsedSyntaxRecorder::makeCompositionTypeElement(
          std::move(*synType), None, *SyntaxContext);
      SyntaxContext->addSyntax(std::move(LastNode));
    }
  }
  SyntaxContext->collectNodesInPlace(SyntaxKind::CompositionTypeElementList);
  
  return makeParserResult(Status, applyOpaque(CompositionTypeRepr::create(
    Context, Types, FirstTypeLoc, {FirstAmpersandLoc, PreviousLoc})));
}

ParserResult<TypeRepr> Parser::parseAnyType() {
  auto AnyLoc = leadingTriviaLoc();
  auto ParsedAny = parseTypeAnySyntax().get();
  SyntaxContext->addSyntax(std::move(ParsedAny));
  auto Any = SyntaxContext->topNode<SimpleTypeIdentifierSyntax>();
  return makeParserResult(ASTGenerator.generate(Any, AnyLoc));
}

/// parseOldStyleProtocolComposition
///   type-composition-deprecated:
///     'protocol' '<' '>'
///     'protocol' '<' type-composition-list-deprecated '>'
///
///   type-composition-list-deprecated:
///     type-identifier
///     type-composition-list-deprecated ',' type-identifier
ParserResult<TypeRepr> Parser::parseOldStyleProtocolComposition() {
  assert(Tok.is(tok::kw_protocol) && startsWithLess(peekToken()));

  // Start a context for creating type syntax.
  SyntaxParsingContext TypeParsingContext(SyntaxContext,
                                          SyntaxContextKind::Type);

  SourceLoc ProtocolLoc = consumeToken();
  SourceLoc LAngleLoc = consumeStartingLess();

  // Parse the type-composition-list.
  ParserStatus Status;
  SmallVector<TypeRepr *, 4> Protocols;
  bool IsEmpty = startsWithGreater(Tok);
  if (!IsEmpty) {
    do {
      // Parse the type-identifier.
      ParserResult<TypeRepr> Protocol = parseTypeIdentifier();
      Status |= Protocol;
      if (auto *ident =
            dyn_cast_or_null<IdentTypeRepr>(Protocol.getPtrOrNull()))
        Protocols.push_back(ident);
    } while (consumeIf(tok::comma));
  }

  // Check for the terminating '>'.
  SourceLoc RAngleLoc = PreviousLoc;
  if (startsWithGreater(Tok)) {
    RAngleLoc = consumeStartingGreater();
  } else {
    if (Status.isSuccess() && !Status.hasCodeCompletion()) {
      diagnose(Tok, diag::expected_rangle_protocol);
      diagnose(LAngleLoc, diag::opening_angle);
      Status.setIsParseError();
    }
    
    // Skip until we hit the '>'.
    RAngleLoc = skipUntilGreaterInTypeList(/*protocolComposition=*/true);
  }

  auto composition = CompositionTypeRepr::create(
    Context, Protocols, ProtocolLoc, {LAngleLoc, RAngleLoc});

  if (Status.isSuccess() && !Status.hasCodeCompletion()) {
    // Only if we have complete protocol<...> construct, diagnose deprecated.
    SmallString<32> replacement;
    if (Protocols.empty()) {
      replacement = "Any";
    } else {
      auto extractText = [&](TypeRepr *Ty) -> StringRef {
        auto SourceRange = Ty->getSourceRange();
        return SourceMgr.extractText(
          Lexer::getCharSourceRangeFromSourceRange(SourceMgr, SourceRange));
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
    StringRef TrailingContent = L->getTokenAt(RAngleLoc).getRange().str().
      substr(1);
    if (!TrailingContent.empty()) {
      replacement += TrailingContent;
    }

    // Replace 'protocol<T1, T2>' with 'T1 & T2'
    diagnose(ProtocolLoc,
      IsEmpty              ? diag::deprecated_any_composition :
      Protocols.size() > 1 ? diag::deprecated_protocol_composition :
                             diag::deprecated_protocol_composition_single)
      .highlight(composition->getSourceRange())
      .fixItReplace(composition->getSourceRange(), replacement);
  }

  return makeParserResult(Status, composition);
}

/// parseTypeTupleBody
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)* '...'?
///   type-tuple-element:
///     identifier? identifier ':' type
///     type
ParserResult<TypeRepr> Parser::parseTypeTupleBody() {
  auto typeLoc = leadingTriviaLoc();
  auto result = parseTypeTupleBodySyntax();
  auto status = result.getStatus();
  if (result.isNull()) {
    return status;
  }
  SyntaxContext->addSyntax(std::move(result.get()));
  auto type = SyntaxContext->topNode<TypeSyntax>();
  auto typeRepr = ASTGenerator.generate(type, typeLoc);
  if (!typeRepr) {
    status.setIsParseError();
  }
  return makeParserResult(status, typeRepr);
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
ParserResult<TypeRepr> Parser::parseTypeArray(TypeRepr *Base) {
  assert(Tok.isFollowingLSquare());
  Parser::StructureMarkerRAII ParsingArrayBound(*this, Tok);
  SourceLoc lsquareLoc = consumeToken();
  ArrayTypeRepr *ATR = nullptr;
  
  // Handle a postfix [] production, a common typo for a C-like array.

  // If we have something that might be an array size expression, parse it as
  // such, for better error recovery.
  if (Tok.isNot(tok::r_square)) {
    auto sizeEx = parseExprBasic(diag::expected_expr);
    if (sizeEx.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
  }
  
  SourceLoc rsquareLoc;
  if (parseMatchingToken(tok::r_square, rsquareLoc,
                         diag::expected_rbracket_array_type, lsquareLoc))
    return makeParserErrorResult(Base);

  // If we parsed something valid, diagnose it with a fixit to rewrite it to
  // Swift syntax.
  diagnose(lsquareLoc, diag::new_array_syntax)
    .fixItInsert(Base->getStartLoc(), "[")
    .fixItRemove(lsquareLoc);
  
  // Build a normal array slice type for recovery.
  ATR = new (Context) ArrayTypeRepr(Base,
                              SourceRange(Base->getStartLoc(), rsquareLoc));
  return makeParserResult(ATR);
}

ParserResult<TypeRepr> Parser::parseTypeCollection() {
  auto typeLoc = leadingTriviaLoc();
  auto result = parseTypeCollectionSyntax();
  auto status = result.getStatus();
  if (result.isNull()) {
    return status;
  }
  SyntaxContext->addSyntax(std::move(result.get()));
  auto collectionType = SyntaxContext->topNode<TypeSyntax>();
  auto typeRepr = ASTGenerator.generate(collectionType, typeLoc);
  if (!typeRepr) {
    status.setIsParseError();
  }
  return makeParserResult(status, typeRepr);
}

bool Parser::isOptionalToken(const Token &T) const {
  // A postfix '?' by itself is obviously optional.
  if (T.is(tok::question_postfix))
    return true;
  // A postfix or bound infix operator token that begins with '?' can be
  // optional too.
  if (T.is(tok::oper_postfix) || T.is(tok::oper_binary_unspaced)) {
    // We'll munch off the '?', so long as it is left-bound with
    // the type (i.e., parsed as a postfix or unspaced binary operator).
    return T.getText().startswith("?");
  }

  return false;
}

bool Parser::isImplicitlyUnwrappedOptionalToken(const Token &T) const {
  // A postfix '!' by itself, or a '!' in SIL mode, is obviously implicitly
  // unwrapped optional.
  if (T.is(tok::exclaim_postfix) || T.is(tok::sil_exclamation))
    return true;
  // A postfix or bound infix operator token that begins with '!' can be
  // implicitly unwrapped optional too.
  if (T.is(tok::oper_postfix) || T.is(tok::oper_binary_unspaced)) {
    // We'll munch off the '!', so long as it is left-bound with
    // the type (i.e., parsed as a postfix or unspaced binary operator).
    return T.getText().startswith("!");
  }

  return false;
}

SourceLoc Parser::consumeOptionalToken() {
  assert(isOptionalToken(Tok) && "not a '?' token?!");
  return consumeStartingCharacterOfCurrentToken(tok::question_postfix);
}

SourceLoc Parser::consumeImplicitlyUnwrappedOptionalToken() {
  assert(isImplicitlyUnwrappedOptionalToken(Tok) && "not a '!' token?!");
  // If the text of the token is just '!', grab the next token.
  return consumeStartingCharacterOfCurrentToken(tok::exclaim_postfix);
}

/// Parse a single optional suffix, given that we are looking at the
/// question mark.
ParserResult<TypeRepr>
Parser::parseTypeOptional(TypeRepr *base) {
  SourceLoc questionLoc = consumeOptionalToken();
  auto TyR = new (Context) OptionalTypeRepr(base, questionLoc);
  SyntaxContext->createNodeInPlace(SyntaxKind::OptionalType);
  return makeParserResult(TyR);
}

/// Parse a single implicitly unwrapped optional suffix, given that we
/// are looking at the exclamation mark.
ParserResult<TypeRepr>
Parser::parseTypeImplicitlyUnwrappedOptional(TypeRepr *base) {
  SourceLoc exclamationLoc = consumeImplicitlyUnwrappedOptionalToken();
  auto TyR =
      new (Context) ImplicitlyUnwrappedOptionalTypeRepr(base, exclamationLoc);
  SyntaxContext->createNodeInPlace(SyntaxKind::ImplicitlyUnwrappedOptionalType);
  return makeParserResult(TyR);
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

  if (Tok.isContextualKeyword("some"))
    consumeToken();

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

  // Handle type-function if we have an 'async'.
  if (shouldParseExperimentalConcurrency() &&
      Tok.isContextualKeyword("async")) {
    consumeToken();

    // 'async' isn't a valid type without being followed by throws/rethrows
    // or a return.
    if (!Tok.isAny(tok::kw_throws, tok::kw_rethrows, tok::arrow))
      return false;
  }

  // Handle type-function if we have an arrow or 'throws'/'rethrows' modifier.
  if (Tok.isAny(tok::kw_throws, tok::kw_rethrows)) {
    consumeToken();

    // Allow 'async' here even though it is ill-formed, so we can provide
    // a better error.
    if (shouldParseExperimentalConcurrency() &&
        Tok.isContextualKeyword("async"))
      consumeToken();

    // "throws" or "rethrows" isn't a valid type without being followed by
    // a return. We also accept 'async' here so we can provide a better
    // error.
    if (!Tok.is(tok::arrow))
      return false;
  }
  
  if (consumeIf(tok::arrow)) {
    return canParseType();
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

bool Parser::canParseSimpleTypeIdentifier() {
  // Parse an identifier.
  if (!Tok.isAny(tok::identifier, tok::kw_Self, tok::kw_Any))
    return false;
  consumeToken();

  // Parse an optional generic argument list.
  if (startsWithLess(Tok) && !canParseGenericArguments())
    return false;

  return true;
}

bool Parser::canParseTypeIdentifier() {
  while (true) {
    if (!canParseSimpleTypeIdentifier())
      return false;

    // Treat 'Foo.<anything>' as an attempt to write a dotted type
    // unless <anything> is 'Type' or 'Protocol'.
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        !peekToken().isContextualKeyword("Type") &&
        !peekToken().isContextualKeyword("Protocol")) {
      consumeToken();
    } else {
      return true;
    }
  }
}

bool Parser::canParseBaseTypeForQualifiedDeclName() {
  BacktrackingScope backtrack(*this);

  // Parse a simple type identifier.
  if (!canParseSimpleTypeIdentifier())
    return false;

  // Qualified name base types must be followed by a period.
  // If the next token starts with a period, return true.
  return startsWithSymbol(Tok, '.');
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
      Tok.isNotEllipsis() && !isStartOfSwiftDecl()) {
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
                 Tok.isNot(tok::comma) && !isStartOfSwiftDecl()) {
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

//===--------------------------------------------------------------------===//
// MARK: - Type Parsing using libSyntax

ParsedSyntaxResult<ParsedTypeSyntax>
Parser::applyAttributeToTypeSyntax(ParsedSyntaxResult<ParsedTypeSyntax> &&Type,
                                   Optional<ParsedTokenSyntax> Specifier,
                                   Optional<ParsedAttributeListSyntax> Attrs) {
  if (!Attrs && !Specifier) {
    return std::move(Type);
  }

  if (Type.isNull()) {
    SmallVector<ParsedSyntax, 2> junk;
    if (Specifier) {
      junk.emplace_back(std::move(*Specifier));
    }
    if (Attrs) {
      junk.emplace_back(std::move(*Attrs));
    }
    return makeParsedResult(
        ParsedSyntaxRecorder::makeUnknownType(junk, *SyntaxContext),
        Type.getStatus());
  }

  return makeParsedResult(
      ParsedSyntaxRecorder::makeAttributedType(
          std::move(Specifier), std::move(Attrs), Type.get(), *SyntaxContext),
      Type.getStatus());
}


ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeAnySyntax() {
  auto Any = consumeTokenSyntax(tok::kw_Any);
  auto Type = ParsedSyntaxRecorder::makeSimpleTypeIdentifier(
      std::move(Any), llvm::None, *SyntaxContext);
  return makeParsedResult(std::move(Type));
}

/// Parse a collection type.
///   type-simple:
///     '[' type ']'
///     '[' type ':' type ']'
ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeCollectionSyntax() {
  assert(Tok.is(tok::l_square));
  Parser::StructureMarkerRAII parsingCollection(*this, Tok);

  ParserStatus Status;

  // Parse the leading '['.
  SourceLoc lsquareLoc = Tok.getLoc();
  ParsedTokenSyntax lsquare = consumeTokenSyntax(tok::l_square);

  // Parse the element type.
  ParsedSyntaxResult<ParsedTypeSyntax> firstTypeResult =
      parseTypeSyntax(diag::expected_element_type);
  Status |= firstTypeResult.getStatus();
  Optional<ParsedTypeSyntax> firstType = firstTypeResult.getOrNull();
  if (!firstType) {
    // If parsing of the type failed, add an unknown type to the libSyntax
    // tree to keep its layout valid.
    firstType = ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext);
  }

  // If there is a ':', this is a dictionary type.
  Optional<ParsedTokenSyntax> colon;
  Optional<ParsedTypeSyntax> secondType;
  if (Tok.is(tok::colon)) {
    colon = consumeTokenSyntax(tok::colon);

    // Parse the second type.
    ParsedSyntaxResult<ParsedTypeSyntax> secondTypeResult =
        parseTypeSyntax(diag::expected_dictionary_value_type);
    Status |= secondTypeResult.getStatus();
    secondType = secondTypeResult.getOrNull();
    if (!secondType) {
      secondType = ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext);
    }
  }

  // Parse the closing ']'.
  auto rsquare = parseMatchingTokenSyntax(
      tok::r_square,
      colon.hasValue() ? diag::expected_rbracket_dictionary_type
                       : diag::expected_rbracket_array_type,
      lsquareLoc);
  Status |= rsquare.getStatus();

  if (colon) {
    ParsedDictionaryTypeSyntaxBuilder builder(*SyntaxContext);
    builder.useLeftSquareBracket(std::move(lsquare));
    builder.useKeyType(std::move(*firstType));
    builder.useColon(std::move(*colon));
    builder.useValueType(std::move(*secondType));
    if (!rsquare.isNull()) {
      builder.useRightSquareBracket(rsquare.get());
    }
    return makeParsedResult(builder.build(), Status);
  } else {
    ParsedArrayTypeSyntaxBuilder builder(*SyntaxContext);
    builder.useLeftSquareBracket(std::move(lsquare));
    builder.useElementType(std::move(*firstType));
    if (!rsquare.isNull()) {
      builder.useRightSquareBracket(rsquare.get());
    }
    return makeParsedResult(builder.build(), Status);
  }
}

ParsedSyntaxResult<ParsedGenericArgumentClauseSyntax>
Parser::parseTypeGenericArgumentClauseSyntax() {
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  auto lAngleLoc = Tok.getLoc();
  ParsedGenericArgumentClauseSyntaxBuilder builder(*SyntaxContext);
  ParserStatus status;

  // Parse '<'.
  builder.useLeftAngleBracket(consumeStartingLessSyntax());

  bool hasNext = true;
  while (hasNext) {
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
  }

  // Parse '>'.
  if (startsWithGreater(Tok)) {
    builder.useRightAngleBracket(consumeStartingGreaterSyntax());
  } else {
    if (status.isSuccess()) {
      diagnose(Tok, diag::expected_rangle_generic_arg_list);
      diagnose(lAngleLoc, diag::opening_angle);
    }
    checkForInputIncomplete();
    status.setIsParseError();
    if (ignoreUntilGreaterInTypeList(/*ProtocolComposition=*/false,
                                     /*Collect=*/nullptr)) {
      builder.useRightAngleBracket(consumeStartingGreaterSyntax());
    }
  }

  return makeParsedResult(builder.build(), status);
}

/// parseTypeIdentifierSyntax
///
///   type-identifier:
///     identifier generic-args? ('.' identifier generic-args?)*
///
ParsedSyntaxResult<ParsedTypeSyntax>
Parser::parseTypeIdentifierSyntax(bool isParsingQualifiedDeclBaseType) {
  // If parsing a qualified declaration name, return error if base type cannot
  // be parsed.
  if (isParsingQualifiedDeclBaseType &&
      !canParseBaseTypeForQualifiedDeclName()) {
    return makeParserError();
  }

  if (Tok.is(tok::kw_Any)) {
    return parseTypeAnySyntax();
  } else if (Tok.is(tok::code_complete)) {
    if (CodeCompletion) {
      CodeCompletion->completeTypeSimpleBeginning();
    }
    // Eat the code completion token because we handled it.
    auto ccTok = consumeTokenSyntax(tok::code_complete);
    auto ccType = ParsedSyntaxRecorder::makeCodeCompletionType(
        None, None, std::move(ccTok), *SyntaxContext);
    return makeParsedCodeCompletion(std::move(ccType));
  } else if (Tok.isNot(tok::identifier, tok::kw_Self)) {
    diagnose(Tok, diag::expected_identifier_for_type);

    // If there is a keyword at the start of a new line, we won't want to
    // skip it as a recovery but rather keep it.
    if (Tok.isKeyword() && !Tok.isAtStartOfLine()) {
      auto kwTok = consumeTokenSyntax();
      ParsedTypeSyntax type =
          ParsedSyntaxRecorder::makeUnknownType({&kwTok, 1}, *SyntaxContext);
      return makeParsedError(std::move(type));
    }

    return makeParsedError<ParsedTypeSyntax>();
  }

  SmallVector<ParsedSyntax, 0> Junk;

  /// Parse a type component and its generic args into \p Identifier and \p
  /// GenericArgs. Return the parser status after parsint the type component.
  auto parseComponent =
      [&](Optional<ParsedTokenSyntax> &Identifier,
          Optional<ParsedGenericArgumentClauseSyntax> &GenericArgs)
      -> ParserStatus {
    // Parse the type identifier
    if (Tok.is(tok::kw_Self)) {
      Identifier = consumeIdentifierSyntax(/*diagnoseDollarIdentifier=*/false);
    } else {
      // FIXME: specialize diagnostic for 'Type': type cannot start with
      // 'metatype'
      // FIXME: offer a fixit: 'self' -> 'Self'
      Identifier =
          parseIdentifierSyntax(diag::expected_identifier_in_dotted_type,
                                /*diagnoseDollarIdentifier=*/true);
    }

    if (!Identifier) {
      return makeParserError();
    }

    // Parse generic arguments if there are any
    if (startsWithLess(Tok)) {
      auto genericArgsResult = parseTypeGenericArgumentClauseSyntax();
      if (!genericArgsResult.isNull()) {
        GenericArgs = genericArgsResult.get();
      }
      return genericArgsResult.getStatus();
    }

    return makeParserSuccess();
  };

  ParsedSyntaxResult<ParsedTypeSyntax> result;

  // Parse the base identifier.
  {
    Optional<ParsedTokenSyntax> identifier;
    Optional<ParsedGenericArgumentClauseSyntax> genericArgs;
    auto status = parseComponent(identifier, genericArgs);
    // If this isn't an identifier we should have bailed out earlier
    assert(identifier);
    auto typeSyntax = ParsedSyntaxRecorder::makeSimpleTypeIdentifier(
        std::move(*identifier), std::move(genericArgs), *SyntaxContext);
    result = makeParsedResult(std::move(typeSyntax), status);
  }

  // Parse member identifiers.
  while (result.isSuccess() && Tok.isAny(tok::period, tok::period_prefix)) {
    if (peekToken().isContextualKeyword("Type") ||
        peekToken().isContextualKeyword("Protocol")) {
      break;
    }
    // If parsing a qualified declaration name, break before parsing the
    // period before the final declaration name component.
    if (isParsingQualifiedDeclBaseType) {
      // If qualified name base type cannot be parsed from the current
      // point (i.e. the next type identifier is not followed by a '.'),
      // then the next identifier is the final declaration name component.
      BacktrackingScope backtrack(*this);
      consumeStartingCharacterOfCurrentToken(tok::period);
      if (!canParseBaseTypeForQualifiedDeclName()) {
        break;
      }
    }

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
      if (genericArgs) {
        builder.useGenericArgumentClause(std::move(*genericArgs));
      }
      result = makeParsedResult(builder.build(), status);
      continue;
    }

    // If there was no identifer there shouldn't be genericArgs.
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

ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeSyntax(Diag<> MessageID, bool IsSILFuncDecl) {
  SourceLoc TypeLoc = Tok.getLoc();

  // Set up a SyntaxParsingContext that captures the libSyntax nodes generated
  // by the legacy AST parser.
  SyntaxParsingContext TypeParsingContext(SyntaxContext,
                                          SyntaxContextKind::Type);
  TypeParsingContext.setTransparent();
  ParserResult<TypeRepr> Result = parseType(MessageID, IsSILFuncDecl);

  // If parsing succeeded, we have a ParsedTypeSyntax in the TypeParsingContext.
  // Pop it and return it. The caller of this method will add it to its
  // SyntaxParsingContext manually.
  if (auto ParsedType = TypeParsingContext.popIf<ParsedTypeSyntax>()) {
    ASTGenerator.addType(Result.getPtrOrNull(), TypeLoc);
    return makeParsedResult(std::move(*ParsedType), Result.getStatus());
  }
  // Otherwise parsing failed. Return the parser status.
  return Result.getStatus();
}

/// parseTypeTupleBodySyntax
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)* '...'?
///   type-tuple-element:
///     identifier? identifier ':' type
///     type
ParsedSyntaxResult<ParsedTypeSyntax> Parser::parseTypeTupleBodySyntax() {
  // Force the context to create deferred nodes, as we might need to
  // de-structure the tuple type to create a function type.
  DeferringContextRAII Deferring(*SyntaxContext);
  Parser::StructureMarkerRAII ParsingTypeTuple(*this, Tok);

  if (ParsingTypeTuple.isFailed()) {
    return makeParsedError<ParsedTypeSyntax>();
  }

  ParsedTupleTypeSyntaxBuilder builder(*SyntaxContext);

  // Parse '('.
  auto LParenLoc = Tok.getLoc();
  builder.useLeftParen(consumeTokenSyntax(tok::l_paren));

  // Parse the elements.
  SmallVector<ParsedTupleTypeElementSyntax, 4> elements;
  SmallVector<std::tuple<SourceLoc, SourceLoc, SourceLoc>, 4> elementsLoc;
  SourceLoc FirstEllipsisLoc;
  auto Status = parseListSyntax(
      elements, /*AllowEmpty=*/true, /*AllowSepAfterLast=*/false,
      [&] { return Tok.is(tok::r_paren); },
      [&](ParsedTupleTypeElementSyntaxBuilder &ElemBuilder) {
        Optional<BacktrackingScope> backtracking;

        // 'inout' here can be beither obsoleted use of the marker in front of
        // an argument label or a valid type annotation in case there are no
        // argument labels. Until we are convinced of the opposite, we must
        // assume the former. IsInOutObsoleted keeps track of which case we are
        // in.
        SourceLoc inOutLoc;
        Optional<ParsedTokenSyntax> inOut;
        bool hasArgumentLabel = false;
        if (Tok.is(tok::kw_inout)) {
          inOutLoc = Tok.getLoc();
          inOut = consumeTokenSyntax(tok::kw_inout);
        }

        // If the label is "some", this could end up being an opaque type
        // description if there's `some <identifier>` without a following colon,
        // so we may need to be able to backtrack.
        if (Tok.getText().equals("some")) {
          backtracking.emplace(*this);
        }

        // If the tuple element starts with a potential argument label followed
        // by a ':' or another potential argument label, then the identifier is
        // an element tag, and it is followed by a type annotation.
        Optional<ParsedTokenSyntax> name;
        Optional<ParsedTokenSyntax> secondName;
        Optional<ParsedTokenSyntax> colon;
        SourceLoc nameLoc;
        SourceLoc secondNameLoc;
        // Argument labels can be either
        // name ':' or
        // name secondName ':'
        // Check if we are in either of these cases
        if (Tok.canBeArgumentLabel() &&
            (peekToken().is(tok::colon) || peekToken().canBeArgumentLabel())) {
          hasArgumentLabel = true;
          // Consume a name.
          nameLoc = Tok.getLoc();
          name = consumeArgumentLabelSyntax();

          // If there is a second name, consume it as well.
          if (Tok.canBeArgumentLabel()) {
            secondNameLoc = Tok.getLoc();
            secondName = consumeArgumentLabelSyntax();
          }

          // Consume the ':'.
          colon = consumeTokenSyntaxIf(tok::colon);
          if (colon) {
            // If we succeed, then we successfully parsed a label and there's
            // no need to backtrack.
            if (backtracking) {
              backtracking->cancelBacktrack();
            }
          } else {
            // Otherwise, try backtracking and parse 'some' as a type attribute.
            // If that's not possible, this is a syntax error.
            if (!backtracking) {
              diagnose(Tok, diag::expected_parameter_colon);
            }
            nameLoc = SourceLoc();
            secondNameLoc = SourceLoc();
          }
        }

        if (!backtracking || !backtracking->willBacktrack()) {
          if (name) {
            ElemBuilder.useName(std::move(*name));
          }
          if (secondName) {
            ElemBuilder.useSecondName(std::move(*secondName));
          }
          if (colon) {
            ElemBuilder.useColon(std::move(*colon));
          }
        } else if (backtracking && backtracking->willBacktrack()) {
          nameLoc = SourceLoc();
          secondNameLoc = SourceLoc();
          name.reset();
          secondName.reset();
          assert(!colon.hasValue());
        }
        backtracking.reset();

        // Parse the type.
        auto typeLoc = Tok.getLoc();
        auto ty = parseTypeSyntax(diag::expected_type);
        if (ty.isNull()) {
          ty = makeParsedResult(
              ParsedSyntaxRecorder::makeUnknownType({}, *SyntaxContext),
              ty.getStatus());
        }
        assert(!ty.isNull() && "We should always have a type by now");

        // Handle pre-parsed 'inout'.
        if (inOut) {
          if (hasArgumentLabel) {
            ElemBuilder.useInOut(std::move(*inOut));
            bool isTypeAlreadyAttributed = false;
            if (auto AttributedType = ty.getAs<ParsedAttributedTypeSyntax>()) {
              isTypeAlreadyAttributed =
                  AttributedType->getDeferredSpecifier().hasValue();
            }
            if (isTypeAlreadyAttributed) {
              // If the parsed type is already attributed, suggest removing
              // `inout`.
              diagnose(Tok, diag::parameter_specifier_repeated)
                  .fixItRemove(inOutLoc);
            } else {
              diagnose(inOutLoc, diag::parameter_specifier_as_attr_disallowed,
                       "inout")
                  .fixItRemove(inOutLoc)
                  .fixItInsert(typeLoc, "inout ");
            }
          } else {
            // Apply 'inout' to the parsed type.
            ParsedAttributedTypeSyntaxBuilder builder(*SyntaxContext);
            ty = applyAttributeToTypeSyntax(std::move(ty), std::move(inOut),
                                            None);
            typeLoc = inOutLoc;
            inOutLoc = SourceLoc();
            inOut.reset();
          }
        }
        assert(!ty.isNull() && "We should not have replaced the type by null");
        ElemBuilder.useType(ty.get());
        elementsLoc.emplace_back(nameLoc, secondNameLoc, typeLoc);
        if (ty.isError()) {
          return ty.getStatus();
        }

        // Parse '...'.
        if (Tok.isEllipsis()) {
          auto ElementEllipsisLoc = Tok.getLoc();
          Tok.setKind(tok::ellipsis);
          ElemBuilder.useEllipsis(consumeTokenSyntax(tok::ellipsis));
          if (!FirstEllipsisLoc.isValid()) {
            FirstEllipsisLoc = ElementEllipsisLoc;
          } else {
            diagnose(ElementEllipsisLoc, diag::multiple_ellipsis_in_tuple)
                .highlight(FirstEllipsisLoc)
                .fixItRemove(ElementEllipsisLoc);
          }
        }

        // If we find an initializer ('=' expr), this is not valid inside a type
        // tuple. Parse it into the libSyntax tree for better diagnostics. It
        // will be ignored in terms of the AST.
        if (Tok.is(tok::equal)) {
          ParsedInitializerClauseSyntaxBuilder initBuilder(*SyntaxContext);
          auto equalLoc = Tok.getLoc();
          initBuilder.useEqual(consumeTokenSyntax(tok::equal));

          // The context into which the expression will be parsed until
          // expression parsing has been migrated to libSyntax. We don't need to
          // worry about feeding the expression to ASTGen since it is ignored by
          // the AST.
          SyntaxParsingContext tmpCtxt(SyntaxContext);
          tmpCtxt.setTransparent();

          auto init = parseExpr(diag::expected_init_value);
          auto inFlight = diagnose(equalLoc, diag::tuple_type_init);
          if (init.isNonNull()) {
            inFlight.fixItRemove(SourceRange(equalLoc, PreviousLoc));
          }
          if (auto expr = SyntaxContext->popIf<ParsedExprSyntax>()) {
            initBuilder.useValue(std::move(*expr));
          } else {
            initBuilder.useValue(
                ParsedSyntaxRecorder::makeUnknownExpr({}, *SyntaxContext));
          }
          ElemBuilder.useInitializer(initBuilder.build());
        }

        return makeParserSuccess();
      });

  // Parse ')'.
  auto rParen = parseMatchingTokenSyntax(
      tok::r_paren, diag::expected_rparen_tuple_type_list, LParenLoc,
      /*SilenceDiag=*/Status.isError());
  Status |= rParen.getStatus();

  bool isFunctionType =
      Tok.isAny(tok::arrow, tok::kw_throws, tok::kw_rethrows) ||
      (shouldParseExperimentalConcurrency() &&
       Tok.isContextualKeyword("async"));

  auto getTokenText = [this](Optional<ParsedTokenSyntax> Name) -> StringRef {
    return !Name ? StringRef()
                 : SourceMgr.extractText(Name->getRaw().getDeferredTokenRange(),
                                         L->getBufferID());
  };

  if (isFunctionType) {
    for (unsigned i = 0; i < elements.size(); i++) {
      auto &element = elements[i];
      SourceLoc nameLoc, secondNameLoc, typeLoc;
      std::tie(nameLoc, secondNameLoc, typeLoc) = elementsLoc[i];
      if (nameLoc.isValid()) {
        auto nameText = getTokenText(element.getDeferredName());
        if (nameText != "_") {
          // If there was a first name, complain; arguments in function types
          // are always unlabeled.
          auto nameIdentifier = Context.getIdentifier(nameText);
          auto diag = diagnose(nameLoc, diag::function_type_argument_label,
                               nameIdentifier);
          auto secondNameText = getTokenText(element.getDeferredSecondName());
          if (secondNameLoc.isInvalid()) {
            // If we only have a first name (i.e. name ':' type) suggest
            // changing it to '_' name ':' type.
            diag.fixItInsert(nameLoc, "_ ");
          } else if (secondNameText == "_") {
            // If the second name is '_' just remove the first name.
            diag.fixItRemoveChars(nameLoc, typeLoc);
          } else {
            // Otherwiese we have a first and second name. The first name is
            // invalid. Replace it by '_'.
            diag.fixItReplace(SourceRange(nameLoc), "_");
          }
        }
      }
    }
  } else {
    for (unsigned i = 0; i < elements.size(); i++) {
      auto &element = elements[i];
      SourceLoc nameLoc, secondNameLoc, typeLoc;
      std::tie(nameLoc, secondNameLoc, typeLoc) = elementsLoc[i];
      if (nameLoc.isValid() && secondNameLoc.isValid()) {
        // True tuples can't have two labels
        auto diag = diagnose(nameLoc, diag::tuple_type_multiple_labels);
        auto name = element.getDeferredName();
        auto nameText = SourceMgr.extractText(
            name->getRaw().getDeferredTokenRange(), L->getBufferID());
        if (nameText == "_") {
          diag.fixItRemoveChars(nameLoc, typeLoc);
        } else {
          diag.fixItRemove(SourceRange(
              Lexer::getLocForEndOfToken(SourceMgr, nameLoc), secondNameLoc));
        }
      }
    }
  }
  for (auto &elem : elements) {
    builder.addElementsMember(std::move(elem));
  }
  if (!rParen.isNull()) {
    builder.useRightParen(rParen.get());
  }

  return makeParsedResult(builder.build(), Status);
}
