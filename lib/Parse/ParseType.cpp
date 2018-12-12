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
#include "swift/Syntax/SyntaxBuilders.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace swift::syntax;

TypeRepr *Parser::applyAttributeToType(TypeRepr *ty,
                                       const TypeAttributes &attrs,
                                       VarDecl::Specifier specifier,
                                       SourceLoc specifierLoc) {
  // Apply those attributes that do apply.
  if (!attrs.empty())
    ty = new (Context) AttributedTypeRepr(attrs, ty);

  // Apply 'inout' or '__shared' or '__owned'
  if (specifierLoc.isValid()) {
    switch (specifier) {
    case VarDecl::Specifier::Owned:
      ty = new (Context) OwnedTypeRepr(ty, specifierLoc);
      break;
    case VarDecl::Specifier::InOut:
      ty = new (Context) InOutTypeRepr(ty, specifierLoc);
      break;
    case VarDecl::Specifier::Shared:
      ty = new (Context) SharedTypeRepr(ty, specifierLoc);
      break;
    case VarDecl::Specifier::Default:
      break;
    case VarDecl::Specifier::Var:
      llvm_unreachable("cannot have var as specifier");
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
ParserResult<TypeRepr> Parser::parseTypeSimple(Diag<> MessageID,
                                               bool HandleCodeCompletion) {
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
    if (!HandleCodeCompletion)
      break;
    if (CodeCompletion)
      CodeCompletion->completeTypeSimpleBeginning();
    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
    return makeParserCodeCompletionResult<TypeRepr>();
  case tok::l_square: {
    auto Result = parseTypeCollection();
    if (Result.hasSyntax())
      SyntaxContext->addSyntax(Result.getSyntax());
    ty = Result.getASTResult();
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

  auto makeMetatypeTypeSyntax = [&]() {
    if (!SyntaxContext->isEnabled())
      return;
    MetatypeTypeSyntaxBuilder Builder(Context.getSyntaxArena());
    Builder
      .useTypeOrProtocol(SyntaxContext->popToken())
      .usePeriod(SyntaxContext->popToken())
      .useBaseType(SyntaxContext->popIf<TypeSyntax>().getValue());
    SyntaxContext->addSyntax(Builder.build());
  };
  
  // '.Type', '.Protocol', '?', '!', and '[]' still leave us with type-simple.
  while (ty.isNonNull()) {
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix))) {
      if (peekToken().isContextualKeyword("Type")) {
        consumeToken();
        SourceLoc metatypeLoc = consumeToken(tok::identifier);
        ty = makeParserResult(ty,
          new (Context) MetatypeTypeRepr(ty.get(), metatypeLoc));
        makeMetatypeTypeSyntax();
        continue;
      }
      if (peekToken().isContextualKeyword("Protocol")) {
        consumeToken();
        SourceLoc protocolLoc = consumeToken(tok::identifier);
        ty = makeParserResult(ty,
          new (Context) ProtocolTypeRepr(ty.get(), protocolLoc));
        makeMetatypeTypeSyntax();
        continue;
      }
    }

    if (!Tok.isAtStartOfLine()) {
      if (isOptionalToken(Tok)) {
        auto Result = parseTypeOptional(ty.get());
        if (Result.hasSyntax())
          SyntaxContext->addSyntax(Result.getSyntax());
        ty = Result.getASTResult();
        continue;
      }
      if (isImplicitlyUnwrappedOptionalToken(Tok)) {
        auto Result = parseTypeImplicitlyUnwrappedOptional(ty.get());
        if (Result.hasSyntax())
          SyntaxContext->addSyntax(Result.getSyntax());
        ty = Result.getASTResult();
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
  
  auto repr = SILBoxTypeRepr::create(Context, generics,
                                     LBraceLoc, Fields, RBraceLoc,
                                     LAngleLoc, Args, RAngleLoc);
  return makeParserResult(applyAttributeToType(repr, attrs,
                                               VarDecl::Specifier::Owned,
                                               SourceLoc()));
}


/// parseType
///   type:
///     attribute-list type-composition
///     attribute-list type-function
///
///   type-function:
///     type-composition '->' type
///     type-composition 'throws' '->' type
///
ParserResult<TypeRepr> Parser::parseType(Diag<> MessageID,
                                         bool HandleCodeCompletion,
                                         bool IsSILFuncDecl) {
  // Start a context for creating type syntax.
  SyntaxParsingContext TypeParsingContext(SyntaxContext,
                                          SyntaxContextKind::Type);
  // Parse attributes.
  VarDecl::Specifier specifier;
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
    return parseSILBoxType(generics, attrs, GenericsScope);
  }

  ParserResult<TypeRepr> ty =
    parseTypeSimpleOrComposition(MessageID, HandleCodeCompletion);
  if (ty.hasCodeCompletion())
    return makeParserCodeCompletionResult<TypeRepr>();
  if (ty.isNull())
    return nullptr;
  auto tyR = ty.get();

  // Parse a throws specifier.
  // Don't consume 'throws', if the next token is not '->', so we can emit a
  // more useful diagnostic when parsing a function decl.
  SourceLoc throwsLoc;
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
    throwsLoc = consumeToken();
  }

  if (Tok.is(tok::arrow)) {
    // Handle type-function if we have an arrow.
    SourceLoc arrowLoc = consumeToken();
    ParserResult<TypeRepr> SecondHalf =
      parseType(diag::expected_type_function_result);
    if (SecondHalf.hasCodeCompletion())
      return makeParserCodeCompletionResult<TypeRepr>();
    if (SecondHalf.isNull())
      return nullptr;

    if (SyntaxContext->isEnabled()) {
      FunctionTypeSyntaxBuilder Builder(Context.getSyntaxArena());
      Builder.useReturnType(SyntaxContext->popIf<TypeSyntax>().getValue());
      Builder.useArrow(SyntaxContext->popToken());
      if (throwsLoc.isValid())
        Builder.useThrowsOrRethrowsKeyword(SyntaxContext->popToken());

      auto InputNode = SyntaxContext->popIf<TypeSyntax>().getValue();
      if (auto TupleTypeNode = InputNode.getAs<TupleTypeSyntax>()) {
        // Decompose TupleTypeSyntax and repack into FunctionType.
        Builder
          .useLeftParen(TupleTypeNode->getLeftParen())
          .useArguments(TupleTypeNode->getElements())
          .useRightParen(TupleTypeNode->getRightParen());
      } else {
        Builder.addTupleTypeElement(SyntaxFactory::makeTupleTypeElement(
            InputNode, /*TrailingComma=*/None, Context.getSyntaxArena()));
      }
      SyntaxContext->addSyntax(Builder.build());
    }

    TupleTypeRepr *argsTyR = nullptr;
    if (auto *TTArgs = dyn_cast<TupleTypeRepr>(tyR)) {
      argsTyR = TTArgs;
    } else {
      bool isVoid = false;
      if (const auto Void = dyn_cast<SimpleIdentTypeRepr>(tyR)) {
        if (Void->getIdentifier().str() == "Void") {
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

    tyR = new (Context) FunctionTypeRepr(generics, argsTyR, throwsLoc, arrowLoc,
                                         SecondHalf.get());
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

  return makeParserResult(applyAttributeToType(tyR, attrs, specifier,
                                               specifierLoc));
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

/// parseTypeIdentifier
///   
///   type-identifier:
///     identifier generic-args? ('.' identifier generic-args?)*
///
ParserResult<TypeRepr> Parser::parseTypeIdentifier() {
  if (Tok.isNot(tok::identifier) && Tok.isNot(tok::kw_Self)) {
    // is this the 'Any' type
    if (Tok.is(tok::kw_Any)) {
      return parseAnyType();
    } else if (Tok.is(tok::code_complete)) {
      if (CodeCompletion)
        CodeCompletion->completeTypeSimpleBeginning();
      // Eat the code completion token because we handled it.
      consumeToken(tok::code_complete);
      return makeParserCodeCompletionResult<IdentTypeRepr>();
    }

    diagnose(Tok, diag::expected_identifier_for_type);

    // If there is a keyword at the start of a new line, we won't want to
    // skip it as a recovery but rather keep it.
    if (Tok.isKeyword() && !Tok.isAtStartOfLine())
      consumeToken();

    return nullptr;
  }
  SyntaxParsingContext IdentTypeCtxt(SyntaxContext, SyntaxContextKind::Type);

  ParserStatus Status;
  SmallVector<ComponentIdentTypeRepr *, 4> ComponentsR;
  SourceLoc EndLoc;
  while (true) {
    SourceLoc Loc;
    Identifier Name;
    if (Tok.is(tok::kw_Self)) {
      Loc = consumeIdentifier(&Name);
    } else {
      // FIXME: specialize diagnostic for 'Type': type cannot start with
      // 'metatype'
      // FIXME: offer a fixit: 'self' -> 'Self'
      if (parseIdentifier(Name, Loc, diag::expected_identifier_in_dotted_type))
        Status.setIsParseError();
    }

    if (Loc.isValid()) {
      SourceLoc LAngle, RAngle;
      SmallVector<TypeRepr*, 8> GenericArgs;
      if (startsWithLess(Tok)) {
        auto genericArgsStatus = parseGenericArguments(GenericArgs, LAngle, RAngle);
        if (genericArgsStatus.isError())
          return genericArgsStatus;
      }
      EndLoc = Loc;

      ComponentIdentTypeRepr *CompT;
      if (!GenericArgs.empty())
        CompT = GenericIdentTypeRepr::create(Context, Loc, Name, GenericArgs,
                                             SourceRange(LAngle, RAngle));
      else
        CompT = new (Context) SimpleIdentTypeRepr(Loc, Name);
      ComponentsR.push_back(CompT);
    }
    SyntaxContext->createNodeInPlace(ComponentsR.size() == 1
                                         ? SyntaxKind::SimpleTypeIdentifier
                                         : SyntaxKind::MemberTypeIdentifier);

    // Treat 'Foo.<anything>' as an attempt to write a dotted type
    // unless <anything> is 'Type'.
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix))) {
      if (peekToken().is(tok::code_complete)) {
        Status.setHasCodeCompletion();
        break;
      }
      if (!peekToken().isContextualKeyword("Type")
          && !peekToken().isContextualKeyword("Protocol")) {
        consumeToken();
        continue;
      }
    } else if (Tok.is(tok::code_complete)) {
      if (!Tok.isAtStartOfLine())
        Status.setHasCodeCompletion();
      break;
    }
    break;
  }

  IdentTypeRepr *ITR = nullptr;
  if (!ComponentsR.empty()) {
    // Lookup element #0 through our current scope chains in case it is some
    // thing local (this returns null if nothing is found).
    if (auto Entry = lookupInScope(ComponentsR[0]->getIdentifier()))
      if (auto *TD = dyn_cast<TypeDecl>(Entry))
        ComponentsR[0]->setValue(TD, nullptr);

    ITR = IdentTypeRepr::create(Context, ComponentsR);
  }

  if (Status.hasCodeCompletion()) {
    if (Tok.isNot(tok::code_complete)) {
      // We have a dot.
      consumeToken();
      if (CodeCompletion)
        CodeCompletion->completeTypeIdentifierWithDot(ITR);
    } else {
      if (CodeCompletion)
        CodeCompletion->completeTypeIdentifierWithoutDot(ITR);
    }
    // Eat the code completion token because we handled it.
    consumeToken(tok::code_complete);
  }

  return makeParserResult(Status, ITR);
}

/// parseTypeSimpleOrComposition
///
///   type-composition:
///     type-simple
///     type-composition '&' type-simple
ParserResult<TypeRepr>
Parser::parseTypeSimpleOrComposition(Diag<> MessageID,
                                     bool HandleCodeCompletion) {
  SyntaxParsingContext CompositionContext(SyntaxContext, SyntaxContextKind::Type);
  // Parse the first type
  ParserResult<TypeRepr> FirstType = parseTypeSimple(MessageID,
                                                     HandleCodeCompletion);
  if (FirstType.hasCodeCompletion())
    return makeParserCodeCompletionResult<TypeRepr>();
  if (FirstType.isNull() || !Tok.isContextualPunctuator("&"))
    return FirstType;

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
    consumeToken(); // consume '&'

    if (SyntaxContext->isEnabled() && Status.isSuccess()) {
      CompositionTypeElementSyntaxBuilder Builder(Context.getSyntaxArena());
      Builder
        .useAmpersand(SyntaxContext->popToken())
        .useType(SyntaxContext->popIf<TypeSyntax>().getValue());
      SyntaxContext->addSyntax(Builder.build());
    }

    // Parse next type.
    ParserResult<TypeRepr> ty =
      parseTypeSimple(diag::expected_identifier_for_type, HandleCodeCompletion);
    if (ty.hasCodeCompletion())
      return makeParserCodeCompletionResult<TypeRepr>();
    Status |= ty;
    addType(ty.getPtrOrNull());
  } while (Tok.isContextualPunctuator("&"));

  if (SyntaxContext->isEnabled() && Status.isSuccess()) {
    auto LastNode = SyntaxFactory::makeCompositionTypeElement(
        SyntaxContext->popIf<TypeSyntax>().getValue(), None,
        SyntaxContext->getArena());
    SyntaxContext->addSyntax(LastNode);
  }
  SyntaxContext->collectNodesInPlace(SyntaxKind::CompositionTypeElementList);
  
  return makeParserResult(Status, CompositionTypeRepr::create(
    Context, Types, FirstTypeLoc, {FirstAmpersandLoc, PreviousLoc}));
}

ParserResult<CompositionTypeRepr>
Parser::parseAnyType() {
  SyntaxParsingContext IdentTypeCtxt(SyntaxContext,
                                     SyntaxKind::SimpleTypeIdentifier);
  auto Loc = consumeToken(tok::kw_Any);
  auto TyR = CompositionTypeRepr::createEmptyComposition(Context, Loc);
  return makeParserResult(TyR);
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
    if (Status.isSuccess()) {
      diagnose(Tok, diag::expected_rangle_protocol);
      diagnose(LAngleLoc, diag::opening_angle);
      Status.setIsParseError();
    }
    
    // Skip until we hit the '>'.
    RAngleLoc = skipUntilGreaterInTypeList(/*protocolComposition=*/true);
  }

  auto composition = CompositionTypeRepr::create(
    Context, Protocols, ProtocolLoc, {LAngleLoc, RAngleLoc});

  if (Status.isSuccess()) {
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
///     identifier ':' type
///     type
ParserResult<TupleTypeRepr> Parser::parseTypeTupleBody() {
  SyntaxParsingContext TypeContext(SyntaxContext, SyntaxKind::TupleType);
  Parser::StructureMarkerRAII ParsingTypeTuple(*this, Tok);
   
  if (ParsingTypeTuple.isFailed()) {
    return makeParserError();
  }

  SourceLoc RPLoc, LPLoc = consumeToken(tok::l_paren);
  SourceLoc EllipsisLoc;
  unsigned EllipsisIdx;
  SmallVector<TupleTypeReprElement, 8> ElementsR;

  ParserStatus Status = parseList(tok::r_paren, LPLoc, RPLoc,
                                  /*AllowSepAfterLast=*/false,
                                  diag::expected_rparen_tuple_type_list,
                                  SyntaxKind::TupleTypeElementList,
                                  [&] () -> ParserStatus {
    TupleTypeReprElement element;

    // 'inout' here can be a obsoleted use of the marker in an argument list,
    // consume it in backtracking context so we can determine it's really a
    // deprecated use of it.
    llvm::Optional<BacktrackingScope> Backtracking;
    SourceLoc ObsoletedInOutLoc;
    if (Tok.is(tok::kw_inout)) {
      Backtracking.emplace(*this);
      ObsoletedInOutLoc = consumeToken(tok::kw_inout);
    }

    // If the tuple element starts with a potential argument label followed by a
    // ':' or another potential argument label, then the identifier is an
    // element tag, and it is followed by a type annotation.
    if (Tok.canBeArgumentLabel() &&
        (peekToken().is(tok::colon) || peekToken().canBeArgumentLabel())) {
      if (Backtracking)
        // Found obsoleted 'inout' use.
        Backtracking->cancelBacktrack();

      // Consume a name.
      element.NameLoc = consumeArgumentLabel(element.Name);

      // If there is a second name, consume it as well.
      if (Tok.canBeArgumentLabel())
        element.SecondNameLoc = consumeArgumentLabel(element.SecondName);

      // Consume the ':'.
      if (!consumeIf(tok::colon, element.ColonLoc))
        diagnose(Tok, diag::expected_parameter_colon);

    } else if (Backtracking) {
      // If we don't have labels, 'inout' is not a obsoleted use.
      ObsoletedInOutLoc = SourceLoc();
    }
    Backtracking.reset();

    // Parse the type annotation.
    auto type = parseType(diag::expected_type);
    if (type.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
    if (type.isNull())
      return makeParserError();
    element.Type = type.get();

    // Complain obsoleted 'inout' etc. position; (inout name: Ty)
    if (ObsoletedInOutLoc.isValid()) {
      if (isa<SpecifierTypeRepr>(element.Type)) {
        // If the parsed type is already a inout type et al, just remove it.
        diagnose(Tok, diag::parameter_specifier_repeated)
            .fixItRemove(ObsoletedInOutLoc);
      } else {
        diagnose(ObsoletedInOutLoc,
                 diag::parameter_specifier_as_attr_disallowed, "inout")
            .fixItRemove(ObsoletedInOutLoc)
            .fixItInsert(element.Type->getStartLoc(), "inout ");
        // Build inout type. Note that we bury the inout locator within the
        // named locator. This is weird but required by Sema apparently.
        element.Type =
            new (Context) InOutTypeRepr(element.Type, ObsoletedInOutLoc);
      }
    }

    // Parse optional '...'.
    if (Tok.isEllipsis()) {
      auto ElementEllipsisLoc = consumeToken();
      if (EllipsisLoc.isInvalid()) {
        EllipsisLoc = ElementEllipsisLoc;
        EllipsisIdx = ElementsR.size();
      } else {
        diagnose(ElementEllipsisLoc, diag::multiple_ellipsis_in_tuple)
          .highlight(EllipsisLoc)
          .fixItRemove(ElementEllipsisLoc);
      }
    }

    // Parse '= expr' here so we can complain about it directly, rather
    // than dying when we see it.
    if (Tok.is(tok::equal)) {
      SyntaxParsingContext InitContext(SyntaxContext,
                                       SyntaxKind::InitializerClause);
      SourceLoc equalLoc = consumeToken(tok::equal);
      auto init = parseExpr(diag::expected_init_value);
      auto inFlight = diagnose(equalLoc, diag::tuple_type_init);
      if (init.isNonNull())
        inFlight.fixItRemove(SourceRange(equalLoc, init.get()->getEndLoc()));
    }

    // Record the ',' location.
    if (Tok.is(tok::comma))
      element.TrailingCommaLoc = Tok.getLoc();

    ElementsR.push_back(element);
    return makeParserSuccess();
  });

  if (EllipsisLoc.isInvalid())
    EllipsisIdx = ElementsR.size();

  bool isFunctionType = Tok.isAny(tok::arrow, tok::kw_throws,
                                  tok::kw_rethrows);

  // If there were any labels, figure out which labels should go into the type
  // representation.
  for (auto &element : ElementsR) {
    // True tuples have labels.
    if (!isFunctionType) {
      // If there were two names, complain.
      if (element.NameLoc.isValid() && element.SecondNameLoc.isValid()) {
        auto diag = diagnose(element.NameLoc, diag::tuple_type_multiple_labels);
        if (element.Name.empty()) {
          diag.fixItRemoveChars(element.NameLoc,
                                element.Type->getStartLoc());
        } else {
          diag.fixItRemove(
            SourceRange(Lexer::getLocForEndOfToken(SourceMgr, element.NameLoc),
                        element.SecondNameLoc));
        }
      }
      continue;
    }

    // If there was a first name, complain; arguments in function types are
    // always unlabeled.
    if (element.NameLoc.isValid() && !element.Name.empty()) {
      auto diag = diagnose(element.NameLoc, diag::function_type_argument_label,
                           element.Name);
      if (element.SecondNameLoc.isInvalid())
        diag.fixItInsert(element.NameLoc, "_ ");
      else if (element.SecondName.empty())
        diag.fixItRemoveChars(element.NameLoc,
                              element.Type->getStartLoc());
      else
        diag.fixItReplace(SourceRange(element.NameLoc), "_");
    }

    if (element.SecondNameLoc.isValid()) {
      // Form the named parameter type representation.
      element.UnderscoreLoc = element.NameLoc;
      element.Name = element.SecondName;
      element.NameLoc = element.SecondNameLoc;
    }
  }

  return makeParserResult(Status,
                          TupleTypeRepr::create(Context, ElementsR,
                                                SourceRange(LPLoc, RPLoc),
                                                EllipsisLoc, EllipsisIdx));
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
    if (sizeEx.isNull())
      return makeParserErrorResult(Base);
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

SyntaxParserResult<TypeSyntax, TypeRepr> Parser::parseTypeCollection() {
  ParserStatus Status;
  // Parse the leading '['.
  assert(Tok.is(tok::l_square));
  Parser::StructureMarkerRAII parsingCollection(*this, Tok);
  SourceLoc lsquareLoc = consumeToken();

  // Parse the element type.
  ParserResult<TypeRepr> firstTy = parseType(diag::expected_element_type);
  Status |= firstTy;

  // If there is a ':', this is a dictionary type.
  SourceLoc colonLoc;
  ParserResult<TypeRepr> secondTy;
  if (Tok.is(tok::colon)) {
    colonLoc = consumeToken();

    // Parse the second type.
    secondTy = parseType(diag::expected_dictionary_value_type);
    Status |= secondTy;
  }

  // Parse the closing ']'.
  SourceLoc rsquareLoc;
  if (parseMatchingToken(tok::r_square, rsquareLoc,
                         colonLoc.isValid()
                             ? diag::expected_rbracket_dictionary_type
                             : diag::expected_rbracket_array_type,
                         lsquareLoc))
    Status.setIsParseError();

  if (Status.hasCodeCompletion())
    return Status;

  // If we couldn't parse anything for one of the types, propagate the error.
  if (Status.isError())
    return makeParserError();

  TypeRepr *TyR;
  llvm::Optional<TypeSyntax> SyntaxNode;

  SourceRange brackets(lsquareLoc, rsquareLoc);
  if (colonLoc.isValid()) {
    // Form the dictionary type.
    TyR = new (Context)
        DictionaryTypeRepr(firstTy.get(), secondTy.get(), colonLoc, brackets);
    if (SyntaxContext->isEnabled()) {
      DictionaryTypeSyntaxBuilder Builder(Context.getSyntaxArena());
      Builder
        .useRightSquareBracket(SyntaxContext->popToken())
        .useValueType(SyntaxContext->popIf<TypeSyntax>().getValue())
        .useColon(SyntaxContext->popToken())
        .useKeyType(SyntaxContext->popIf<TypeSyntax>().getValue())
        .useLeftSquareBracket(SyntaxContext->popToken());
      SyntaxNode.emplace(Builder.build());
    }
  } else {
    // Form the array type.
    TyR = new (Context) ArrayTypeRepr(firstTy.get(), brackets);
    if (SyntaxContext->isEnabled()) {
      ArrayTypeSyntaxBuilder Builder(Context.getSyntaxArena());
      Builder
        .useRightSquareBracket(SyntaxContext->popToken())
        .useElementType(SyntaxContext->popIf<TypeSyntax>().getValue())
        .useLeftSquareBracket(SyntaxContext->popToken());
      SyntaxNode.emplace(Builder.build());
    }
  }
    
  return makeSyntaxResult(Status, SyntaxNode, TyR);
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
SyntaxParserResult<TypeSyntax, OptionalTypeRepr>
Parser::parseTypeOptional(TypeRepr *base) {
  SourceLoc questionLoc = consumeOptionalToken();
  auto TyR = new (Context) OptionalTypeRepr(base, questionLoc);
  llvm::Optional<TypeSyntax> SyntaxNode;
  if (SyntaxContext->isEnabled()) {
    auto QuestionMark = SyntaxContext->popToken();
    if (auto WrappedType = SyntaxContext->popIf<TypeSyntax>()) {
      OptionalTypeSyntaxBuilder Builder(Context.getSyntaxArena());
      Builder
        .useQuestionMark(QuestionMark)
        .useWrappedType(WrappedType.getValue());
      SyntaxNode.emplace(Builder.build());
    } else {
      // Undo the popping of the question mark
      SyntaxContext->addSyntax(QuestionMark);
    }
  }
  return makeSyntaxResult(SyntaxNode, TyR);
}

/// Parse a single implicitly unwrapped optional suffix, given that we
/// are looking at the exclamation mark.
SyntaxParserResult<TypeSyntax, ImplicitlyUnwrappedOptionalTypeRepr>
Parser::parseTypeImplicitlyUnwrappedOptional(TypeRepr *base) {
  SourceLoc exclamationLoc = consumeImplicitlyUnwrappedOptionalToken();
  auto TyR =
      new (Context) ImplicitlyUnwrappedOptionalTypeRepr(base, exclamationLoc);
  llvm::Optional<TypeSyntax> SyntaxNode;
  if (SyntaxContext->isEnabled()) {
    ImplicitlyUnwrappedOptionalTypeSyntaxBuilder Builder(
        Context.getSyntaxArena());
    Builder
      .useExclamationMark(SyntaxContext->popToken())
      .useWrappedType(SyntaxContext->popIf<TypeSyntax>().getValue());
    SyntaxNode.emplace(Builder.build());
  }
  return makeSyntaxResult(SyntaxNode, TyR);
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
