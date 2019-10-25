//===--- ASTGen.cpp -------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/ASTGen.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/SourceManager.h"

#include "DebuggerContextChange.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/Scope.h"

using namespace swift;
using namespace swift::syntax;

SourceLoc ASTGen::generate(const TokenSyntax &Tok, const SourceLoc Loc) {
  return advanceLocBegin(Loc, Tok);
}

SourceLoc ASTGen::generateIdentifierDeclName(const syntax::TokenSyntax &Tok,
                                             const SourceLoc Loc,
                                             Identifier &Id) {
  StringRef text;
  if (Tok.getText() == "Any")
    // Special handle 'Any' because we don't want to accidantaly declare 'Any'
    // type in any way.
    text = "#Any";
  else
    text = Tok.getIdentifierText();

  Id = Context.getIdentifier(text);
  return advanceLocBegin(Loc, Tok);
}

Decl *ASTGen::generate(const DeclSyntax &D, const SourceLoc Loc) {
  Decl *DeclAST = nullptr;

  if (auto associatedTypeDecl = D.getAs<AssociatedtypeDeclSyntax>()) {
    DeclAST = generate(*associatedTypeDecl, Loc);
  } else if (auto typealiasDecl = D.getAs<TypealiasDeclSyntax>()) {
    DeclAST = generate(*typealiasDecl, Loc);
  } else {
    llvm_unreachable("unsupported decl kind");
  }

  return DeclAST;
}

DeclAttributes
ASTGen::generateDeclAttributes(const Syntax &D, SourceLoc Loc,
                               bool includeComments) {
  // Find the AST attribute-list from the lookup table.
  if (auto firstTok = D.getFirstToken()) {
    auto declLoc = advanceLocBegin(Loc, *firstTok);
    if (hasDeclAttributes(declLoc))
      return takeDeclAttributes(declLoc);
  }
  return DeclAttributes();
}

MutableArrayRef<TypeLoc>
ASTGen::generate(const TypeInheritanceClauseSyntax &clause, SourceLoc Loc,
                 bool allowClassRequirement) {
  SmallVector<TypeLoc, 2> inherited;

  bool hasClass = false;
  for (const auto elem : clause.getInheritedTypeCollection()) {
    const auto &tySyntax = elem.getTypeName();
    if (tySyntax.is<ClassRestrictionTypeSyntax>()) {
      // Accept 'class' only if it's allowed and it's the first one.
      if (!allowClassRequirement || hasClass)
        continue;
      hasClass = true;
    }
    if (auto ty = generate(tySyntax, Loc))
      inherited.emplace_back(ty);
  }

  return Context.AllocateCopy(inherited);
}

TypeDecl *ASTGen::generate(const AssociatedtypeDeclSyntax &D,
                           const SourceLoc Loc) {
  if (!isa<ProtocolDecl>(P.CurDeclContext)) {
    // This is already diagnosed in Parser.
    return nullptr;
  }

  auto idToken = D.getIdentifier();
  if (idToken.isMissing())
    return nullptr;

  auto keywordLoc = advanceLocBegin(Loc, D.getAssociatedtypeKeyword());
  Identifier name;
  SourceLoc nameLoc = generateIdentifierDeclName(idToken, Loc, name);

  DeclAttributes attrs = generateDeclAttributes(D, Loc, true);

  DebuggerContextChange DCC(P, name, DeclKind::AssociatedType);

  ArrayRef<TypeLoc> inherited;
  if (const auto inheritanceClause = D.getInheritanceClause())
    inherited =
        generate(*inheritanceClause, Loc, /*allowClassRequirement=*/true);

  TypeRepr *defaultTy = nullptr;
  if (const auto init = D.getInitializer())
    defaultTy = generate(init->getValue(), Loc);

  TrailingWhereClause *trailingWhere = nullptr;
  if (auto whereClause = D.getGenericWhereClause())
    trailingWhere = generate(*whereClause, Loc);

  auto assocType = new (Context)
      AssociatedTypeDecl(P.CurDeclContext, keywordLoc, name, nameLoc, defaultTy,
                         trailingWhere);
  assocType->getAttrs() = attrs;
  if (!inherited.empty())
    assocType->setInherited(Context.AllocateCopy(inherited));
  addToScope(assocType);
  return assocType;
}

TypeDecl *ASTGen::generate(const TypealiasDeclSyntax &D, const SourceLoc Loc) {
  auto idToken = D.getIdentifier();
  if (idToken.isMissing())
    return nullptr;

  auto keywordLoc = advanceLocBegin(Loc, D.getTypealiasKeyword());
  Identifier name;
  SourceLoc nameLoc = generateIdentifierDeclName(idToken, Loc, name);
  auto attrs = generateDeclAttributes(D, Loc, true);
  SourceLoc equalLoc;

  DebuggerContextChange DCC(P, name, DeclKind::TypeAlias);

  Optional<Scope> GenericScope;
  GenericParamList *genericParams = nullptr;
  GenericScope.emplace(&P, ScopeKind::Generics);
  if (auto clause = D.getGenericParameterClause())
    genericParams = generate(*clause, Loc);

  auto *TAD = new (Context) TypeAliasDecl(keywordLoc, equalLoc, name, nameLoc,
                                          genericParams, P.CurDeclContext);
  P.setLocalDiscriminator(TAD);
  TAD->getAttrs() = attrs;

  TypeRepr *underlyingType = nullptr;
  SourceLoc typeEndLoc;
  if (auto init = D.getInitializer()) {
    Parser::ContextChange CC(P, TAD);
    equalLoc = generate(init->getEqual(), Loc);
    underlyingType = generate(init->getValue(), Loc);
    if (auto lastToken = init->getLastToken())
      typeEndLoc = generate(*lastToken, Loc);
  }
  TAD->setUnderlyingTypeRepr(underlyingType);

  SourceLoc whereLoc;
  if (auto clause = D.getGenericWhereClause()) {
    whereLoc = advanceLocBegin(Loc, clause->getWhereKeyword());
    Parser::ContextChange CC(P, TAD);
    generateFreeStandingGenericWhereClause(*clause, Loc, genericParams);
  }
  P.diagnoseWhereClauseInGenericParamList(genericParams, whereLoc);

  if (equalLoc.isInvalid())
    return nullptr;

  GenericScope.reset();

  addToScope(TAD);
  return DCC.fixupParserResult(TAD).getPtrOrNull();
}

void ASTGen::generateFreeStandingGenericWhereClause(
    const syntax::GenericWhereClauseSyntax &syntax, const SourceLoc Loc,
    GenericParamList *genericParams) {

  SourceLoc whereLoc = generate(syntax.getWhereKeyword(), Loc);

  if (!genericParams) {
    P.diagnose(whereLoc, diag::where_without_generic_params,
               unsigned(Parser::WhereClauseKind::Declaration));
    return;
  }

  // Push the generic parameters back into a local scope so that references
  // will find them.
  Scope S(&P, ScopeKind::Generics);
  for (auto pd : genericParams->getParams())
    addToScope(pd);

  SmallVector<RequirementRepr, 4> requirements;
  requirements.reserve(syntax.getRequirementList().size());
  for (auto elem : syntax.getRequirementList()) {
    if (auto req = generate(elem, Loc))
      requirements.push_back(*req);
  }
  if (requirements.empty())
    return;

  genericParams->addTrailingWhereClause(Context, whereLoc, requirements);
}

TrailingWhereClause *ASTGen::generate(const GenericWhereClauseSyntax &syntax,
                                      const SourceLoc Loc) {
  SourceLoc whereLoc = advanceLocBegin(Loc, syntax.getWhereKeyword());

  SmallVector<RequirementRepr, 4> requirements;
  requirements.reserve(syntax.getRequirementList().size());
  for (auto elem : syntax.getRequirementList()) {
    if (auto req = generate(elem, Loc))
      requirements.push_back(*req);
  }

  if (requirements.empty())
    return nullptr;
  return TrailingWhereClause::create(Context, whereLoc, requirements);
}

Expr *ASTGen::generate(const ExprSyntax &E, const SourceLoc Loc) {
  Expr *result = nullptr;

  auto exprLoc = advanceLocBegin(Loc, E);
  if (hasExpr(exprLoc))
    return takeExpr(exprLoc);

  if (auto identifierExpr = E.getAs<IdentifierExprSyntax>())
    result = generate(*identifierExpr, Loc);
  else if (auto superRefExpr = E.getAs<SuperRefExprSyntax>())
    result = generate(*superRefExpr, Loc);
  else if (auto specializeExpr = E.getAs<SpecializeExprSyntax>())
    result = generate(*specializeExpr, Loc);
  else if (auto editorPlaceHolderExpr = E.getAs<EditorPlaceholderExprSyntax>())
    result = generate(*editorPlaceHolderExpr, Loc);
  else if (auto arrayExpr = E.getAs<ArrayExprSyntax>())
    result = generate(*arrayExpr, Loc);
  else if (auto dictionaryExpr = E.getAs<DictionaryExprSyntax>())
    result = generate(*dictionaryExpr, Loc);
  else if (auto tupleExpr = E.getAs<TupleExprSyntax>())
    result = generate(*tupleExpr, Loc);
  else if (auto callExpr  = E.getAs<FunctionCallExprSyntax>())
    result = generate(*callExpr, Loc);
  else if (auto memberExpr  = E.getAs<MemberAccessExprSyntax>())
    result = generate(*memberExpr, Loc);
  else if (auto integerLiteralExpr = E.getAs<IntegerLiteralExprSyntax>())
    result = generate(*integerLiteralExpr, Loc);
  else if (auto floatLiteralExpr = E.getAs<FloatLiteralExprSyntax>())
    result = generate(*floatLiteralExpr, Loc);
  else if (auto nilLiteral = E.getAs<NilLiteralExprSyntax>())
    result = generate(*nilLiteral, Loc);
  else if (auto boolLiteral = E.getAs<BooleanLiteralExprSyntax>())
    result = generate(*boolLiteral, Loc);
  else if (auto poundFileExpr = E.getAs<PoundFileExprSyntax>())
    result = generate(*poundFileExpr, Loc);
  else if (auto poundLineExpr = E.getAs<PoundLineExprSyntax>())
    result = generate(*poundLineExpr, Loc);
  else if (auto poundColumnExpr = E.getAs<PoundColumnExprSyntax>())
    result = generate(*poundColumnExpr, Loc);
  else if (auto poundFunctionExpr = E.getAs<PoundFunctionExprSyntax>())
    result = generate(*poundFunctionExpr, Loc);
  else if (auto poundDsohandleExpr = E.getAs<PoundDsohandleExprSyntax>())
    result = generate(*poundDsohandleExpr, Loc);
  else if (auto objcKeyPathExpr = E.getAs<ObjcKeyPathExprSyntax>())
    result = generate(*objcKeyPathExpr, Loc);
  else if (auto objectLiteralExpr = E.getAs<ObjectLiteralExprSyntax>())
    result = generate(*objectLiteralExpr, Loc);
  else if (auto completionExpr = E.getAs<CodeCompletionExprSyntax>())
    result = generate(*completionExpr, Loc);
  else if (auto unknownExpr = E.getAs<UnknownExprSyntax>())
    result = generate(*unknownExpr, Loc);
  else {
#ifndef NDEBUG
    E.dump();
    llvm_unreachable("unsupported expression");
#endif
  }

  return result;
}

std::pair<DeclName, DeclNameLoc> ASTGen::generateUnqualifiedDeclName(
    const TokenSyntax &idTok, const Optional<DeclNameArgumentsSyntax> &args,
    const SourceLoc Loc) {
  SourceLoc baseNameLoc = advanceLocBegin(Loc, idTok);

  DeclBaseName baseName;
  if (idTok.getTokenKind() == tok::kw_init)
    baseName = DeclBaseName::createConstructor();
  else if (idTok.getTokenKind() == tok::kw_deinit)
    baseName = DeclBaseName::createDestructor();
  else if (idTok.getTokenKind() == tok::kw_subscript)
    baseName = DeclBaseName::createSubscript();
  else
    baseName = Context.getIdentifier(idTok.getIdentifierText());

  if (!args)
    return {DeclName(baseName), DeclNameLoc(baseNameLoc)};

  // FIXME: Remove this block and use 'Loc'.
  // This is needed for the case 'idTok' and 'args' are not in the same tree.
  // i.e. Call from parseUnqualifiedDeclName().
  SourceLoc argsLeadingLoc = Loc;
  if (!args->getParent()) {
    argsLeadingLoc = Loc.getAdvancedLoc(idTok.getTextLength());
  } else {
    assert(idTok.getData().getParent() == args->getData().getParent() &&
           idTok.getIndexInParent() + 1 == args->getIndexInParent() &&
           "'idTok' must be immediately followed by 'args'");
  }

  SmallVector<Identifier, 2> argumentLabels;
  SmallVector<SourceLoc, 2> argumentLabelLocs;
  for (auto arg : args->getArguments()) {
    Identifier label;
    if (!arg.getName().isMissing() &&
        arg.getName().getTokenKind() != tok::kw__) {
      label = Context.getIdentifier(arg.getName().getIdentifierText());
    }
    argumentLabels.push_back(label);
    argumentLabelLocs.push_back(advanceLocBegin(argsLeadingLoc,
                                                *arg.getFirstToken()));
  }
  SourceLoc lParenLoc = advanceLocBegin(argsLeadingLoc, args->getLeftParen());
  SourceLoc rParenLoc = advanceLocBegin(argsLeadingLoc, args->getRightParen());

  DeclName name(Context, baseName, argumentLabels);
  DeclNameLoc nameLoc;
  if (argumentLabelLocs.empty())
    nameLoc = DeclNameLoc(baseNameLoc);
  else
    nameLoc = DeclNameLoc(Context, baseNameLoc, lParenLoc, argumentLabelLocs,
                          rParenLoc);
  return {name, nameLoc};
}

Expr *ASTGen::generate(const IdentifierExprSyntax &E, const SourceLoc Loc) {
  auto idTok = E.getIdentifier();
  DeclName name;
  DeclNameLoc nameLoc;
  std::tie(name, nameLoc) = generateUnqualifiedDeclName(
      E.getIdentifier(), E.getDeclNameArguments(), Loc);

  ValueDecl *D = nullptr;
  if (!P.InPoundIfEnvironment) {
    D = lookupInScope(name);
    // FIXME: We want this to work: "var x = { x() }", but for now it's better
    // to disallow it than to crash.
    if (D) {
      for (auto activeVar : P.DisabledVars) {
        if (activeVar != D)
          continue;
        P.diagnose(nameLoc.getBaseNameLoc(), P.DisabledVarReason);
        return new (Context) ErrorExpr(nameLoc.getSourceRange());
      }
    } else {
      for (auto activeVar : P.DisabledVars) {
        if (activeVar->getFullName() != name)
          continue;
        P.diagnose(nameLoc.getBaseNameLoc(), P.DisabledVarReason);
        return new (Context) ErrorExpr(nameLoc.getSourceRange());
      }
    }
  }

  if (!D) {
    return new (Context)
        UnresolvedDeclRefExpr(name, DeclRefKind::Ordinary, nameLoc);
  }

  if (auto TD = dyn_cast<TypeDecl>(D)) {
    // When parsing default argument expressions for generic functions,
    // we haven't built a FuncDecl or re-parented the GenericTypeParamDecls
    // to the FuncDecl yet. Other than that, we should only ever find
    // global or local declarations here.
    assert(!TD->getDeclContext()->isTypeContext() ||
           isa<GenericTypeParamDecl>(TD));
    return TypeExpr::createForDecl(nameLoc.getBaseNameLoc(), TD,
                                   /*DeclContext=*/nullptr,
                                   /*inplicit=*/false);
  }

  return new (Context) DeclRefExpr(D, nameLoc, /*implicit=*/false);
}

static VarDecl *getImplicitSelfDeclForSuperContext(Parser &P,
                                                   DeclContext *DC,
                                                   SourceLoc Loc) {
  auto *methodContext = DC->getInnermostMethodContext();
  if (!methodContext) {
    P.diagnose(Loc, diag::super_not_in_class_method);
    return nullptr;
  }

  // Do an actual lookup for 'self' in case it shows up in a capture list.
  auto *methodSelf = methodContext->getImplicitSelfDecl();
  auto *lookupSelf = P.lookupInScope(P.Context.Id_self);
  if (lookupSelf && lookupSelf != methodSelf) {
    // FIXME: This is the wrong diagnostic for if someone manually declares a
    // variable named 'self' using backticks.
    P.diagnose(Loc, diag::super_in_closure_with_capture);
    P.diagnose(lookupSelf->getLoc(), diag::super_in_closure_with_capture_here);
    return nullptr;
  }

  return methodSelf;
}

Expr *ASTGen::generate(const SuperRefExprSyntax &E, const SourceLoc Loc) {
  auto superLoc = advanceLocBegin(Loc, E.getSuperKeyword());
  VarDecl *selfDecl =
      getImplicitSelfDeclForSuperContext(P, P.CurDeclContext, superLoc);
  if (!selfDecl)
    return new (Context) ErrorExpr(superLoc);

  return new (Context) SuperRefExpr(selfDecl, superLoc, /*Implicit=*/false);
}

Expr *ASTGen::generate(const EditorPlaceholderExprSyntax &E, const SourceLoc Loc) {
  assert(!E.getIdentifier().isMissing());

  auto text = E.getIdentifier().getText();
  auto tokLoc = advanceLocBegin(Loc, E.getIdentifier());
  return P.parseExprEditorPlaceholder(tokLoc, text);
}

Expr *ASTGen::generate(const SpecializeExprSyntax &E, const SourceLoc Loc) {
  auto base = generate(E.getExpression(), Loc);

  SourceLoc lAngleLoc, rAngleLoc;
  SmallVector<TypeRepr *, 4> argTyRs;
  generate(E.getGenericArgumentClause(), Loc, lAngleLoc, rAngleLoc, argTyRs);
  if (argTyRs.empty())
    return base;

  SmallVector<TypeLoc, 4> args;
  args.assign(argTyRs.begin(), argTyRs.end());
  return UnresolvedSpecializeExpr::create(Context, base, lAngleLoc, args,
                                          rAngleLoc);
}

/// validateCollectionElement - Check if a given collection element is valid.
///
/// At the moment, this checks whether a given collection element is a subscript
/// expression and whether we're subscripting into an array. If we are, then it
/// we emit a diagnostic in case it was not something that the user was
/// expecting.
///
/// For example: `let array [ [0, 1] [42] ]`
void ASTGen::validateCollectionElement(Expr *elementExpr) {
  if (!elementExpr)
    return;

  if (!isa<SubscriptExpr>(elementExpr))
    return;

  auto subscriptExpr = cast<SubscriptExpr>(elementExpr);
  if (!isa<ArrayExpr>(subscriptExpr->getBase()))
    return;

  auto arrayExpr = cast<ArrayExpr>(subscriptExpr->getBase());

  auto startLocOfSubscript = subscriptExpr->getIndex()->getStartLoc();
  auto endLocOfArray = arrayExpr->getEndLoc();

  auto locForEndOfTokenArray =
      Lexer::getLocForEndOfToken(Context.SourceMgr, endLocOfArray);

  if (locForEndOfTokenArray != startLocOfSubscript) {
    auto subscriptLoc = subscriptExpr->getLoc();
    P.diagnose(subscriptLoc, diag::subscript_array_element)
        .highlight(subscriptExpr->getSourceRange());
    P.diagnose(subscriptLoc, diag::subscript_array_element_fix_it_add_comma)
        .fixItInsertAfter(endLocOfArray, ",");
    P.diagnose(subscriptLoc, diag::subscript_array_element_fix_it_remove_space)
        .fixItRemoveChars(locForEndOfTokenArray, startLocOfSubscript);
  }
}

Expr *ASTGen::generate(const ArrayExprSyntax &E, const SourceLoc Loc) {
  SmallVector<Expr *, 8> elements;
  SmallVector<SourceLoc, 8> commaLocs;
  elements.reserve(E.getElements().size());
  for (auto elemSyntax : E.getElements()) {
    if (auto elemAST = generate(elemSyntax.getExpression(), Loc)) {
      validateCollectionElement(elemAST);
      elements.push_back(elemAST);
    }
    if (auto comma = elemSyntax.getTrailingComma())
      commaLocs.push_back(advanceLocBegin(Loc, *comma));
  }

  // Don't bother to create expression if any expressions aren't parsed.
  if (elements.empty() && !E.getElements().empty())
    return nullptr;

  auto LSquareLoc = advanceLocBegin(Loc, E);
  auto RSquareLoc = advanceLocEnd(Loc, E);
  return ArrayExpr::create(Context, LSquareLoc, elements, commaLocs,
                           RSquareLoc);
}

Expr *ASTGen::generate(const DictionaryExprSyntax &E, const SourceLoc Loc) {
  SmallVector<Expr *, 8> elements;
  SmallVector<SourceLoc, 8> commaLocs;
  if (auto contents = E.getContent().getAs<DictionaryElementListSyntax>()) {
    elements.reserve(contents->size());
    for (auto elemSyntax : *contents) {
      if (auto key = generate(elemSyntax.getKeyExpression(), Loc)) {
        auto val = generate(elemSyntax.getValueExpression(), Loc);
        if (!val)
          val = new (Context) ErrorExpr(advanceLocEnd(Loc, elemSyntax));
        auto elemAST = TupleExpr::createImplicit(Context, {key, val}, {});
        elements.push_back(elemAST);
      }
      if (auto comma = elemSyntax.getTrailingComma())
        commaLocs.push_back(advanceLocBegin(Loc, *comma));
    }
    // Don't bother to create expression if any expressions aren't parsed.
    if (elements.empty() && !contents->empty())
      return nullptr;
  }

  auto LSquareLoc = advanceLocBegin(Loc, E);
  auto RSquareLoc = advanceLocEnd(Loc, E);
  return DictionaryExpr::create(Context, LSquareLoc, elements, commaLocs,
                                RSquareLoc);
}

Expr *ASTGen::generate(const TupleExprSyntax &E, const SourceLoc Loc) {
  SmallVector<Expr *, 2> exprs;
  SmallVector<Identifier, 2> exprLabels;
  SmallVector<SourceLoc, 2> exprLabelLocs;
  generateExprTupleElementList(E.getElementList(), Loc,
  /*isForCallArguments=*/false, exprs, exprLabels,
                               exprLabelLocs);

  SourceLoc leftLoc = advanceLocBegin(Loc, E.getLeftParen());
  SourceLoc rightLoc = advanceLocEnd(Loc, E);

  // A tuple with a single, unlabeled element is just parentheses.
  if (exprs.size() == 1 && exprLabels.empty()) {
    return new (Context) ParenExpr(leftLoc, exprs[0], rightLoc,
                                   /*hasTrailingClosure=*/false);
  }

  return TupleExpr::create(Context, leftLoc, exprs, exprLabels, exprLabelLocs,
                           rightLoc, /*HasTrailingClosure=*/false,
                           /*Implicit=*/false);
}

void ASTGen::generateExprTupleElementList(const TupleExprElementListSyntax &elements,
                                          const SourceLoc Loc, bool isForCallArguments,
                                          SmallVectorImpl<Expr *> &exprs,
                                  SmallVectorImpl<Identifier> &exprLabels,
                                  SmallVectorImpl<SourceLoc> &exprLabelLocs) {
  auto isFirst = true;
  for (auto elem : elements) {
    auto *subExpr = generate(elem.getExpression(), Loc);
    if (!subExpr)
      continue;

    // Handle call arguments specially because it may need argument labels.
    if (P.CodeCompletion && isForCallArguments && !elem.getLabel())
      if (auto CCExpr = elem.getExpression().getAs<CodeCompletionExprSyntax>())
        if (!CCExpr->getBase() && !CCExpr->getPeriodOrParen())
          P.CodeCompletion->completeCallArg(cast<CodeCompletionExpr>(subExpr),
                                              isFirst);
    isFirst = false;

    Identifier fieldName;
    SourceLoc fieldNameLoc;
    if (auto label = elem.getLabel()) {
      fieldNameLoc = advanceLocBegin(Loc, *label);
      if (label->getTokenKind() == tok::identifier)
        fieldName = Context.getIdentifier(label->getIdentifierText());
    }

    // Don't populate label vectors unless we see at least one label.
    if (!exprLabels.empty()) {
      exprLabels.push_back(fieldName);
      exprLabelLocs.push_back(fieldNameLoc);
    } else if (fieldNameLoc.isValid()) {
      exprLabels.resize(exprs.size());
      exprLabelLocs.resize(exprs.size());
      exprLabels.push_back(fieldName);
      exprLabelLocs.push_back(fieldNameLoc);
    }
    exprs.push_back(subExpr);
  }
  assert((exprLabels.size() == 0 || exprs.size() == exprLabels.size()) &&
         exprLabels.size() == exprLabelLocs.size());
}

Expr *ASTGen::generate(const FunctionCallExprSyntax &E, const SourceLoc Loc) {
  auto callee = E.getCalledExpression();

  SourceLoc LParenLoc, RParenLoc;
  SmallVector<Expr *, 2> args;
  SmallVector<Identifier, 2> argLabels;
  SmallVector<SourceLoc, 2> argLabelLocs;
  generateExprTupleElementList(E.getArgumentList(), Loc,
                               /*isForCallArguments=*/true, args, argLabels,
                               argLabelLocs);
  Expr *trailingClosure = nullptr;
  if (auto CE = E.getTrailingClosure())
    trailingClosure = generate(*CE, Loc);
  if (auto LParen = E.getLeftParen()) {
    LParenLoc = advanceLocBegin(Loc, *LParen);
    if (auto RParen = E.getRightParen())
      RParenLoc = advanceLocBegin(Loc, *RParen);
    else
      RParenLoc = advanceLocEnd(Loc, E.getArgumentList());
  }

  if (auto memberAccess = callee.getAs<MemberAccessExprSyntax>()) {
    if (!memberAccess->getBase()) {
      // This is UnresolvedMemberExpr with call arguments.
      if (memberAccess->getName().isMissing())
        return nullptr;

      SourceLoc dotLoc = advanceLocBegin(Loc, memberAccess->getDot());
      DeclName name;
      DeclNameLoc nameLoc;
      std::tie(name, nameLoc) = generateUnqualifiedDeclName(
          memberAccess->getName(), memberAccess->getDeclNameArguments(), Loc);

      return UnresolvedMemberExpr::create(
          Context, dotLoc, nameLoc, name, LParenLoc, args, argLabels,
          argLabelLocs, RParenLoc, trailingClosure,
          /*implicit=*/false);
    }
  }
  llvm_unreachable("call expression not implemented");
  return nullptr;
}

Expr *ASTGen::generate(const MemberAccessExprSyntax &E, const SourceLoc Loc) {
  if (!E.getBase()) {
    // This is an UnresolvedMemberExpr.
    if (E.getName().isMissing())
      return nullptr;

    DeclName name;
    DeclNameLoc nameLoc;
    std::tie(name, nameLoc) =
        generateUnqualifiedDeclName(E.getName(), E.getDeclNameArguments(), Loc);
    SourceLoc dotLoc = advanceLocBegin(Loc, E.getDot());

    return UnresolvedMemberExpr::create(Context, dotLoc, nameLoc, name,
                                        /*implicit=*/false);
  }
  llvm_unreachable("member access expression not implemented");
  return nullptr;
}

Expr *ASTGen::generate(const IntegerLiteralExprSyntax &Expr,
                       const SourceLoc Loc) {
  auto Digits = Expr.getDigits();
  auto Text = copyAndStripUnderscores(Digits.getText());
  auto DigitsLoc = advanceLocBegin(Loc, Digits);
  return new (Context) IntegerLiteralExpr(Text, DigitsLoc);
}

Expr *ASTGen::generate(const FloatLiteralExprSyntax &Expr,
                       const SourceLoc Loc) {
  auto Digits = Expr.getFloatingDigits();
  auto Text = copyAndStripUnderscores(Digits.getText());
  auto DigitsLoc = advanceLocBegin(Loc, Digits);
  return new (Context) FloatLiteralExpr(Text, DigitsLoc);
}

Expr *ASTGen::generate(const NilLiteralExprSyntax &Expr, const SourceLoc Loc) {
  auto Nil = Expr.getNilKeyword();
  auto NilLoc = advanceLocBegin(Loc, Nil);
  return new (Context) NilLiteralExpr(NilLoc);
}

Expr *ASTGen::generate(const BooleanLiteralExprSyntax &Expr,
                       const SourceLoc Loc) {
  auto Boolean = Expr.getBooleanLiteral();
  auto Value = Boolean.getTokenKind() == tok::kw_true;
  auto BooleanLoc = advanceLocBegin(Loc, Boolean);
  return new (Context) BooleanLiteralExpr(Value, BooleanLoc);
}

Expr *ASTGen::generate(const PoundFileExprSyntax &Expr, const SourceLoc Loc) {
  return generateMagicIdentifierLiteralExpression(Expr.getPoundFile(), Loc);
}

Expr *ASTGen::generate(const PoundLineExprSyntax &Expr, const SourceLoc Loc) {
  return generateMagicIdentifierLiteralExpression(Expr.getPoundLine(), Loc);
}

Expr *ASTGen::generate(const PoundColumnExprSyntax &Expr, const SourceLoc Loc) {
  return generateMagicIdentifierLiteralExpression(Expr.getPoundColumn(), Loc);
}

Expr *ASTGen::generate(const PoundFunctionExprSyntax &Expr,
                       const SourceLoc Loc) {
  return generateMagicIdentifierLiteralExpression(Expr.getPoundFunction(), Loc);
}

Expr *ASTGen::generate(const PoundDsohandleExprSyntax &Expr,
                       const SourceLoc Loc) {
  return generateMagicIdentifierLiteralExpression(Expr.getPoundDsohandle(),
                                                  Loc);
}

Expr *ASTGen::generate(const ObjcKeyPathExprSyntax &E, const SourceLoc Loc) {
  SmallVector<KeyPathExpr::Component, 4> components;
  if (E.getLeftParen().isMissing())
    return nullptr;

  for (auto piece : E.getName()) {
    DeclName name;
    DeclNameLoc nameLoc;
    std::tie(name, nameLoc) =
        generateUnqualifiedDeclName(piece.getName(),
                                    piece.getDeclNameArguments(), Loc);
    auto component = KeyPathExpr::Component::forUnresolvedProperty(name,
                                                      nameLoc.getBaseNameLoc());
    components.push_back(component);
  }
  auto keywordLoc = advanceLocBegin(Loc, E.getKeyPath());
  auto LParenLoc = advanceLocBegin(Loc, E.getLeftParen());
  auto RParenLoc = advanceLocEnd(Loc, E);

  if (components.empty())
    return new (Context) ErrorExpr(SourceRange(keywordLoc, RParenLoc));

  return new (Context) KeyPathExpr(
      Context, keywordLoc, LParenLoc, components, RParenLoc);
}

Expr *ASTGen::generate(const ObjectLiteralExprSyntax &E, const SourceLoc Loc) {
  ObjectLiteralExpr::LiteralKind kind;
  switch (E.getIdentifier().getTokenKind()) {
#define POUND_OBJECT_LITERAL(Name, Desc, Proto)                                \
  case tok::pound_##Name:                                                      \
    kind = ObjectLiteralExpr::Name;                                            \
    break;
#include "swift/Syntax/TokenKinds.def"
  default:
    llvm_unreachable("unknown token kind for object literal expression");
  }

  SmallVector<Expr *, 2> args;
  SmallVector<Identifier, 2> argLabels;
  SmallVector<SourceLoc, 2> argLabelLocs;
  generateExprTupleElementList(E.getArguments(), Loc, true, args, argLabels,
                               argLabelLocs);

  ClosureExpr *trailingClosure = nullptr;
  if (auto CE = E.getTrailingClosure())
    trailingClosure = dyn_cast_or_null<ClosureExpr>(generate(*CE, Loc));

  if (E.getLeftParen().isMissing() || E.getRightParen().isMissing())
    return nullptr;

  SourceLoc poundLoc = advanceLocBegin(Loc, E.getIdentifier());
  SourceLoc LParenLoc = advanceLocBegin(Loc, E.getLeftParen());
  SourceLoc RParenLoc = advanceLocBegin(Loc, E.getRightParen());

  return ObjectLiteralExpr::create(Context, poundLoc, kind, LParenLoc, args,
                                   argLabels, argLabelLocs, RParenLoc,
                                   trailingClosure, /*implicit=*/false);
}

Expr *ASTGen::generate(const CodeCompletionExprSyntax &E, const SourceLoc Loc) {
  if (!E.getBase()) {
    if (auto punctuator = E.getPeriodOrParen()) {
      // '.' <cc-token>
      if (punctuator->getTokenKind() == tok::period ||
          punctuator->getTokenKind() == tok::period_prefix) {
        auto ccLoc = advanceLocBegin(Loc, E.getCodeCompletionToken());
        auto dotLoc = advanceLocBegin(Loc, *punctuator);
        
        auto CCE = new (Context) CodeCompletionExpr(ccLoc);
        if (P.CodeCompletion)
          P.CodeCompletion->completeUnresolvedMember(CCE, dotLoc);
        return CCE;
      }
    } else {
      llvm_unreachable("'(' <cc-token> is not suppported");
    }
  } else {
    if (auto objcKeyPathExpr = E.getBase()->getAs<ObjcKeyPathExprSyntax>()) {
      // #keyPath(<cc-token>
      // #keyPath(some <cc-token>
      // #keyPath(some.<cc-token>
      auto expr = generate(*objcKeyPathExpr, Loc);
      if (P.CodeCompletion) {
        SourceLoc dotLoc;
        if (!expr || isa<ErrorExpr>(expr)) {
          P.CodeCompletion->completeExprKeyPath(nullptr, SourceLoc());
        } else {
          auto namePieces = objcKeyPathExpr->getName();
          if (!namePieces.empty())
            if (auto dot = namePieces[namePieces.getNumChildren() - 1].getDot())
              dotLoc = advanceLocBegin(Loc, *dot);
          P.CodeCompletion->completeExprKeyPath(cast<KeyPathExpr>(expr), dotLoc);
        }
      }
      return expr;
    }
    // TODO: implement
  }
  llvm_unreachable("code completion expression not implemented");
  return nullptr;
}

Expr *ASTGen::generate(const UnknownExprSyntax &Expr, const SourceLoc Loc) {
  if (Expr.getNumChildren() == 1 && Expr.getChild(0)->isToken()) {
    Syntax Token = *Expr.getChild(0);
    tok Kind = Token.getRaw()->getTokenKind();
    switch (Kind) {
    case tok::kw___FILE__:
    case tok::kw___LINE__:
    case tok::kw___COLUMN__:
    case tok::kw___FUNCTION__:
    case tok::kw___DSO_HANDLE__: {
      auto MagicKind = getMagicIdentifierLiteralKind(Kind);
      auto KindLoc = advanceLocBegin(Loc, Token);
      return new (Context) MagicIdentifierLiteralExpr(MagicKind, KindLoc);
    }
    default:
      return nullptr;
    }
  }
  return nullptr;
}

TypeRepr *ASTGen::generate(const TypeSyntax &Type, const SourceLoc Loc,
                           bool IsSILFuncDecl) {
  TypeRepr *TypeAST = nullptr;

  if (auto SimpleIdentifier = Type.getAs<SimpleTypeIdentifierSyntax>())
    TypeAST = generate(*SimpleIdentifier, Loc);
  else if (auto MemberIdentifier = Type.getAs<MemberTypeIdentifierSyntax>())
    TypeAST = generate(*MemberIdentifier, Loc);
  else if (auto Composition = Type.getAs<CompositionTypeSyntax>())
    TypeAST = generate(*Composition, Loc);
  else if (auto Function = Type.getAs<FunctionTypeSyntax>())
    TypeAST = generate(*Function, Loc);
  else if (auto Metatype = Type.getAs<MetatypeTypeSyntax>())
    TypeAST = generate(*Metatype, Loc);
  else if (auto Array = Type.getAs<ArrayTypeSyntax>())
    TypeAST = generate(*Array, Loc);
  else if (auto Dictionary = Type.getAs<DictionaryTypeSyntax>())
    TypeAST = generate(*Dictionary, Loc);
  else if (auto Tuple = Type.getAs<TupleTypeSyntax>())
    TypeAST = generate(*Tuple, Loc);
  else if (auto Some = Type.getAs<SomeTypeSyntax>())
    TypeAST = generate(*Some, Loc);
  else if (auto Optional = Type.getAs<OptionalTypeSyntax>())
    TypeAST = generate(*Optional, Loc);
  else if (auto Unwrapped = Type.getAs<ImplicitlyUnwrappedOptionalTypeSyntax>())
    TypeAST = generate(*Unwrapped, Loc);
  else if (auto Attributed = Type.getAs<AttributedTypeSyntax>())
    TypeAST = generate(*Attributed, Loc);
  else if (auto ClassRestriction = Type.getAs<ClassRestrictionTypeSyntax>())
    TypeAST = generate(*ClassRestriction, Loc);
  else if (auto SILBoxType = Type.getAs<SILBoxTypeSyntax>())
    TypeAST = generate(*SILBoxType, Loc, IsSILFuncDecl);
  else if (auto SILFunctionType = Type.getAs<SILFunctionTypeSyntax>())
    TypeAST = generate(*SILFunctionType, Loc, IsSILFuncDecl);
  else if (auto CompletionTy = Type.getAs<CodeCompletionTypeSyntax>())
    TypeAST = generate(*CompletionTy, Loc);
  else if (auto Unknown = Type.getAs<UnknownTypeSyntax>())
    TypeAST = generate(*Unknown, Loc);

  return cacheType(Type, TypeAST);
}

TypeRepr *ASTGen::generate(const FunctionTypeSyntax &Type,
                           const SourceLoc Loc) {
  TupleTypeRepr *ArgumentTypes = nullptr;

  SourceLoc VoidLoc;
  if (Type.getLeftParen().isMissing() && Type.getArguments().size() == 1) {
    if (auto ident = Type.getArguments()[0]
                         .getType()
                         .getAs<SimpleTypeIdentifierSyntax>()) {
      if (!ident->getGenericArgumentClause().hasValue() &&
          ident->getName().getText() == "Void")
        VoidLoc = advanceLocBegin(Loc, ident->getName());
    }
  }

  if (VoidLoc.isValid())
    ArgumentTypes = TupleTypeRepr::createEmpty(Context, VoidLoc);
  else {
    ArgumentTypes = generateTuple(Type.getLeftParen(), Type.getArguments(),
                                  Type.getRightParen(), Loc,
                                  /*IsFunction=*/true);
  }
  if (!ArgumentTypes)
    return nullptr;

  auto ThrowsLoc = Type.getThrowsOrRethrowsKeyword()
                       ? generate(*Type.getThrowsOrRethrowsKeyword(), Loc)
                       : SourceLoc();

  auto ArrowLoc = generate(Type.getArrow(), Loc);
  auto ReturnType = generate(Type.getReturnType(), Loc);
  if (!ReturnType)
    return nullptr;

  return new (Context)
      FunctionTypeRepr(nullptr, ArgumentTypes, ThrowsLoc, ArrowLoc, ReturnType);
}

TupleTypeRepr *ASTGen::generateTuple(const TokenSyntax &LParen,
                                     const TupleTypeElementListSyntax &Elements,
                                     const TokenSyntax &RParen,
                                     const SourceLoc Loc, bool IsFunction) {
  auto LPLoc = advanceLocBegin(Loc, LParen);
  auto RPLoc = advanceLocEnd(Loc, RParen);

  SmallVector<TupleTypeReprElement, 4> TupleElements;

  SourceLoc EllipsisLoc;
  unsigned EllipsisIdx;

  for (unsigned i = 0; i < Elements.size(); i++) {
    auto Element = Elements[i];
    TupleTypeReprElement ElementAST;
    ElementAST.Type = generate(Element.getType(), Loc);
    if (!ElementAST.Type)
      continue;

    if (auto Name = Element.getName()) {
      ElementAST.NameLoc = generate(*Name, Loc);
      ElementAST.Name = Name->getText() == "_"
                            ? Identifier()
                            : Context.getIdentifier(Name->getIdentifierText());
    }
    if (auto Colon = Element.getColon())
      ElementAST.ColonLoc = generate(*Colon, Loc);
    if (auto SecondName = Element.getSecondName()) {
      ElementAST.SecondNameLoc = generate(*SecondName, Loc);
      ElementAST.SecondName =
          SecondName->getText() == "_"
              ? Identifier()
              : Context.getIdentifier(SecondName->getIdentifierText());
      if (IsFunction) {
        // Form the named parameter type representation.
        ElementAST.UnderscoreLoc = ElementAST.NameLoc;
        ElementAST.Name = ElementAST.SecondName;
        ElementAST.NameLoc = ElementAST.SecondNameLoc;
      }
    }

    if (auto InOut = Element.getInOut()) {
      // don't apply multiple inout specifiers to a type: that's invalid and was
      // already reported in the parser, handle gracefully
      if (!isa<InOutTypeRepr>(ElementAST.Type)) {
        auto InOutLoc = generate(*InOut, Loc);
        ElementAST.Type =
            new (Context) InOutTypeRepr(ElementAST.Type, InOutLoc);
      }
    }
    if (auto Comma = Element.getTrailingComma())
      ElementAST.TrailingCommaLoc = generate(*Comma, Loc);

    if (auto Ellipsis = Element.getEllipsis()) {
      if (EllipsisLoc.isInvalid()) {
        EllipsisLoc = generate(*Ellipsis, Loc);
        EllipsisIdx = i;
      }
    }
    TupleElements.push_back(ElementAST);
  }
  if (EllipsisLoc.isInvalid())
    EllipsisIdx = TupleElements.size();

  return TupleTypeRepr::create(Context, TupleElements, {LPLoc, RPLoc},
                               EllipsisLoc, EllipsisIdx);
}

TypeAttributes ASTGen::generateTypeAttributes(const AttributeListSyntax &syntax,
                                              const SourceLoc Loc) {
  TypeAttributes attrs;

  for (auto elem : syntax) {
    auto attrSyntax = elem.castTo<AttributeSyntax>();
    if (attrSyntax.getAttributeName().isMissing())
      continue;

    auto attrName = attrSyntax.getAttributeName().getText();

    auto atLoc = advanceLocBegin(Loc, attrSyntax.getAtSignToken());
    if (attrs.AtLoc.isInvalid())
      attrs.AtLoc = atLoc;

    auto attr = TypeAttributes::getAttrKindFromString(attrName);
    if (attr == TAK_Count)
      continue;

    if (attrs.has(attr)) {
      P.diagnose(atLoc, diag::duplicate_attribute, /*isModifier=*/false);
      continue;
    }

    auto arg = attrSyntax.getArgument();

    if (attr == TAK_sil_weak || attr == TAK_sil_unowned) {
      if (attrs.hasOwnership()) {
        P.diagnose(atLoc, diag::duplicate_attribute, /*isModifier*/false);
      }
    } else if (attr == TAK_convention) {
      // @convention(block)
      // @convention(witness_method: ProtocolName)
      if (!arg)
        continue;

      if (auto conventionNameTok = arg->getAs<TokenSyntax>()) {
        assert(conventionNameTok->getTokenKind() == tok::identifier);
        auto convention =
            Context.getIdentifier(conventionNameTok->getIdentifierText());
        attrs.convention = convention.str();
      } else if (auto witness =
                     arg->getAs<NamedAttributeStringArgumentSyntax>()) {
        assert(witness->getNameTok().getIdentifierText() == "witness_method");
        if (witness->getStringOrDeclname().isMissing())
          continue;
        auto protocolName =
            witness->getStringOrDeclname().castTo<DeclNameSyntax>();
        auto protocol = Context.getIdentifier(
            protocolName.getDeclBaseName().getIdentifierText());
        attrs.convention = "witness_method";
        attrs.conventionWitnessMethodProtocol = protocol.str();
      } else {
        continue;
      }
    } else if (attr == TAK_opened) {
      // @opened("01234567-89ab-cdef-0123-111111111111")
      if (!arg)
        continue;

      assert(arg->castTo<TokenSyntax>().getTokenKind() ==
             tok::string_literal);
      auto tokText = arg->castTo<TokenSyntax>().getText();
      auto literalText = tokText.slice(1, tokText.size() - 1);
      attrs.OpenedID = UUID::fromString(literalText.str().c_str());
    } else if (attr == TAK__opaqueReturnTypeOf) {
      // @_opaqueReturnTypeOf("$sMangledName", 0)
      if (!arg)
        continue;

      auto opaqueArg =
          arg->castTo<OpaqueReturnTypeOfAttributeArgumentsSyntax>();

      auto manglingTok = opaqueArg.getMangledName();
      auto indexTok = opaqueArg.getIndex();
      if (manglingTok.isMissing() || indexTok.isMissing())
        continue;

      auto tokText = manglingTok.getText();
      auto mangling =
          Context.getIdentifier(tokText.slice(1, tokText.size() - 1));
      unsigned index;
      if (indexTok.getText().getAsInteger(10, index))
        continue;
      attrs.setOpaqueReturnTypeOf(mangling.str(), index);
      // SWIFT_ENABLE_TENSORFLOW
    } else if (attr == TAK_differentiable) {
      if (arg) {
        auto argSyntax = arg->getAs<TokenSyntax>();
        attrs.linear = argSyntax->getTokenKind() == tok::identifier &&
                       argSyntax->getIdentifierText() == "linear";
      } else {
        attrs.linear = false;
      }
    }

    attrs.setAttr(attr, atLoc);
  }

  return attrs;
}

TypeRepr *ASTGen::generate(const AttributedTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto TypeAST = generate(Type.getBaseType(), Loc);
  if (!TypeAST)
    return nullptr;

  if (auto Attributes = Type.getAttributes()) {
    TypeAttributes attrs = generateTypeAttributes(*Attributes, Loc);
    if (!attrs.empty())
      TypeAST = new (Context) AttributedTypeRepr(attrs, TypeAST);
  }

  if (auto Specifier = Type.getSpecifier()) {
    auto SpecifierLoc = generate(*Specifier, Loc);
    auto SpecifierText = Specifier->getText();

    // don't apply multiple specifiers to a type: that's invalid and was already
    // reported in the parser, handle gracefully
    if (!isa<SpecifierTypeRepr>(TypeAST)) {
      if (SpecifierText == "inout")
        TypeAST = new (Context) InOutTypeRepr(TypeAST, SpecifierLoc);
      else if (SpecifierText == "__owned")
        TypeAST = new (Context) OwnedTypeRepr(TypeAST, SpecifierLoc);
      else if (SpecifierText == "__shared")
        TypeAST = new (Context) SharedTypeRepr(TypeAST, SpecifierLoc);
    }
  }

  return TypeAST;
}

TypeRepr *ASTGen::generate(const TupleTypeSyntax &Type, const SourceLoc Loc) {
  return generateTuple(Type.getLeftParen(), Type.getElements(),
                       Type.getRightParen(), Loc);
}

TypeRepr *ASTGen::generate(const SomeTypeSyntax &Type, const SourceLoc Loc) {
  auto Some = Type.getSomeSpecifier();
  auto SomeLoc = generate(Some, Loc);
  auto BaseType = generate(Type.getBaseType(), Loc);
  return new (Context) OpaqueReturnTypeRepr(SomeLoc, BaseType);
}

TypeRepr *ASTGen::generate(const CompositionTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto Elements = Type.getElements();
  auto FirstElem = Elements[0];
  auto LastElem = Elements[Elements.size() - 1];

  SmallVector<TypeRepr *, 4> ElemTypes;
  for (unsigned i = 0; i < Elements.size(); i++) {
    auto ElemType = Elements[i].getType();

    TypeRepr *ElemTypeR = nullptr;
    if (auto Some = ElemType.getAs<SomeTypeSyntax>()) {
      // the invalid `some` after an ampersand was already diagnosed by the
      // parser, handle it gracefully
      ElemTypeR = generate(Some->getBaseType(), Loc);
    } else {
      ElemTypeR = generate(ElemType, Loc);
    }

    if (ElemTypeR) {
      if (auto Comp = dyn_cast<CompositionTypeRepr>(ElemTypeR)) {
        // Accept protocol<P1, P2> & P3; explode it.
        auto TyRs = Comp->getTypes();
        ElemTypes.append(TyRs.begin(), TyRs.end());
      } else {
        ElemTypes.push_back(ElemTypeR);
      }
    }
  }

  auto FirstTypeLoc = advanceLocBegin(Loc, FirstElem);
  auto FirstAmpersandLoc = advanceLocBegin(Loc, *FirstElem.getAmpersand());
  auto LastTypeLoc = advanceLocBegin(Loc, *LastElem.getLastToken());
  return CompositionTypeRepr::create(Context, ElemTypes, FirstTypeLoc,
                                     {FirstAmpersandLoc, LastTypeLoc});
}

void ASTGen::gatherTypeIdentifierComponents(
    const TypeSyntax &Component, const SourceLoc Loc,
    SmallVectorImpl<ComponentIdentTypeRepr *> &Components) {
  if (auto SimpleIdentifier = Component.getAs<SimpleTypeIdentifierSyntax>()) {
    auto ComponentType = generateIdentifier(*SimpleIdentifier, Loc);
    Components.push_back(ComponentType);
    return;
  }

  if (auto MemberIdentifier = Component.getAs<MemberTypeIdentifierSyntax>()) {
    auto ComponentType = generateIdentifier(*MemberIdentifier, Loc);
    Components.push_back(ComponentType);
    gatherTypeIdentifierComponents(MemberIdentifier->getBaseType(), Loc,
                                   Components);
    return;
  }

  llvm_unreachable("unexpected type identifier component");
}

template <typename T>
TypeRepr *ASTGen::generateSimpleOrMemberIdentifier(const T &Type,
                                                   const SourceLoc Loc) {
  SmallVector<ComponentIdentTypeRepr *, 4> Components;
  gatherTypeIdentifierComponents(Type, Loc, Components);
  std::reverse(Components.begin(), Components.end());

  auto IdentType = IdentTypeRepr::create(Context, Components);
  auto FirstComponent = IdentType->getComponentRange().front();
  // Lookup element #0 through our current scope chains in case it is some
  // thing local (this returns null if nothing is found).
  if (auto Entry = lookupInScope(FirstComponent->getIdentifier())) {
    if (auto *TD = dyn_cast<TypeDecl>(Entry))
      FirstComponent->setValue(TD, nullptr);
  }

  return IdentType;
}

template <typename T>
ComponentIdentTypeRepr *ASTGen::generateIdentifier(const T &Type,
                                                   const SourceLoc Loc) {
  auto IdentifierLoc = advanceLocBegin(Loc, Type.getName());
  auto Identifier = Context.getIdentifier(Type.getName().getIdentifierText());
  if (auto Clause = Type.getGenericArgumentClause()) {
    SourceLoc lAngleLoc, rAngleLoc;
    SmallVector<TypeRepr *, 4> args;
    generate(*Clause, Loc, lAngleLoc, rAngleLoc, args);
    if (!args.empty())
      return GenericIdentTypeRepr::create(Context, IdentifierLoc, Identifier,
                                          args, {lAngleLoc, rAngleLoc});
  }
  return new (Context) SimpleIdentTypeRepr(IdentifierLoc, Identifier);
}

TypeRepr *ASTGen::generate(const SimpleTypeIdentifierSyntax &Type,
                           const SourceLoc Loc) {
  if (Type.getName().getTokenKind() == tok::kw_Any) {
    auto AnyLoc = advanceLocBegin(Loc, Type.getName());
    return CompositionTypeRepr::createEmptyComposition(Context, AnyLoc);
  }

  return generateSimpleOrMemberIdentifier(Type, Loc);
}

TypeRepr *ASTGen::generate(const MemberTypeIdentifierSyntax &Type,
                           SourceLoc Loc) {
  return generateSimpleOrMemberIdentifier(Type, Loc);
}

TypeRepr *ASTGen::generate(const DictionaryTypeSyntax &Type,
                           const SourceLoc Loc) {
  TypeRepr *ValueType = generate(Type.getValueType(), Loc);
  TypeRepr *KeyType = generate(Type.getKeyType(), Loc);
  if (!ValueType || !KeyType)
    return nullptr;
  auto ColonLoc = advanceLocBegin(Loc, Type.getColon());

  SourceLoc LBracketLoc, RBracketLoc;
  LBracketLoc = advanceLocBegin(Loc, Type);
  RBracketLoc = advanceLocEnd(Loc, Type);
  SourceRange Range{LBracketLoc, RBracketLoc};
  return new (Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

TypeRepr *ASTGen::generate(const ArrayTypeSyntax &Type, SourceLoc Loc) {
  TypeRepr *ElementType = generate(Type.getElementType(), Loc);
  if (!ElementType)
    return nullptr;
  SourceLoc LBracketLoc, RBracketLoc;
  LBracketLoc = advanceLocBegin(Loc, Type);
  RBracketLoc = advanceLocEnd(Loc, Type);
  return new (Context) ArrayTypeRepr(ElementType, {LBracketLoc, RBracketLoc});
}

TypeRepr *ASTGen::generate(const MetatypeTypeSyntax &Type,
                           const SourceLoc Loc) {
  TypeRepr *BaseType = generate(Type.getBaseType(), Loc);
  auto TypeOrProtocol = Type.getTypeOrProtocol();
  auto TypeOrProtocolLoc = advanceLocBegin(Loc, TypeOrProtocol);
  if (TypeOrProtocol.getText() == "Type")
    return new (Context) MetatypeTypeRepr(BaseType, TypeOrProtocolLoc);
  return new (Context) ProtocolTypeRepr(BaseType, TypeOrProtocolLoc);
}

TypeRepr *ASTGen::generate(const OptionalTypeSyntax &Type,
                           const SourceLoc Loc) {
  TypeRepr *WrappedType = generate(Type.getWrappedType(), Loc);
  auto QuestionLoc = advanceLocBegin(Loc, Type.getQuestionMark());
  return new (Context) OptionalTypeRepr(WrappedType, QuestionLoc);
}

TypeRepr *ASTGen::generate(const ImplicitlyUnwrappedOptionalTypeSyntax &Type,
                           const SourceLoc Loc) {
  TypeRepr *WrappedType = generate(Type.getWrappedType(), Loc);
  auto ExclamationLoc = advanceLocBegin(Loc, Type.getExclamationMark());
  return new (Context)
      ImplicitlyUnwrappedOptionalTypeRepr(WrappedType, ExclamationLoc);
}

TypeRepr *
ASTGen::generate(const ClassRestrictionTypeSyntax &Type, const SourceLoc Loc) {
  auto classLoc = advanceLocBegin(Loc, Type);
  return new (Context)
      SimpleIdentTypeRepr(classLoc, Context.getIdentifier("AnyObject"));
}

TypeRepr *ASTGen::generate(const SILBoxTypeSyntax &Type, const SourceLoc Loc,
                           bool IsSILFuncDecl) {
  if (Type.getRightBrace().isMissing())
    return nullptr;

  // If this is part of a sil function decl, generic parameters are visible in
  // the function body; otherwise, they are visible when parsing the type.
  Optional<Scope> GenericsScope;
  if (!IsSILFuncDecl)
    GenericsScope.emplace(&P, ScopeKind::Generics);

  GenericParamList *generics = nullptr;
  if (auto genericParamsSyntax = Type.getGenericParameterClauses())
    generics = generate(*genericParamsSyntax, Loc);

  SmallVector<SILBoxTypeRepr::Field, 4> Fields;
  for (auto field : Type.getFields()) {
    auto specifier = field.getSpecifier();
    if (specifier.isMissing())
      return nullptr;
    bool Mutable;
    if (specifier.getTokenKind() == tok::kw_let)
      Mutable = false;
    else if (specifier.getTokenKind() == tok::kw_var)
      Mutable = true;
    else
      return nullptr;
    SourceLoc VarOrLetLoc = advanceLocBegin(Loc, specifier);;
    auto fieldTy = generate(field.getType(), Loc);
    if (!fieldTy)
      return nullptr;

    Fields.emplace_back(VarOrLetLoc, Mutable, fieldTy);
  }
  GenericsScope.reset();

  auto LBraceLoc = advanceLocBegin(Loc, Type.getLeftBrace());
  auto RBraceLoc = advanceLocBegin(Loc, Type.getRightBrace());

  SourceLoc LAngleLoc, RAngleLoc;
  SmallVector<TypeRepr *, 4> Args;
  if (auto genericArgs = Type.getGenericArgumentClause()) {
    if (genericArgs->getRightAngleBracket().isMissing())
      return nullptr;
    generate(*genericArgs, Loc, LAngleLoc, RAngleLoc, Args);
  }

  auto SILType = SILBoxTypeRepr::create(Context, generics, LBraceLoc, Fields,
                                        RBraceLoc, LAngleLoc, Args, RAngleLoc);
  return SILType;
}

TypeRepr *ASTGen::generate(const SILFunctionTypeSyntax &Type,
                           const SourceLoc Loc, bool IsSILFuncDecl) {
  // If this is part of a sil function decl, generic parameters are visible in
  // the function body; otherwise, they are visible when parsing the type.
  Optional<Scope> GenericsScope;
  if (!IsSILFuncDecl)
    GenericsScope.emplace(&P, ScopeKind::Generics);

  GenericParamList *generics = nullptr;
  if (auto genericParamsSyntax = Type.getGenericParameterClauses())
    generics = generate(*genericParamsSyntax, Loc);

  auto tyR = cast<FunctionTypeRepr>(generate(Type.getFunction(), Loc));
  return new (Context)
      FunctionTypeRepr(generics, tyR->getArgsTypeRepr(), tyR->getThrowsLoc(),
                       tyR->getArrowLoc(), tyR->getResultTypeRepr());
}

TypeRepr *ASTGen::generate(const CodeCompletionTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto base = Type.getBase();
  if (!base) {
    if (P.CodeCompletion)
      P.CodeCompletion->completeTypeSimpleBeginning();
    return nullptr;
  }

  if (P.CodeCompletion) {
    if (auto *parsedTyR = generate(*base, Loc)) {
      P.CodeCompletion->setParsedTypeLoc(parsedTyR);
      if (Type.getPeriod())
        P.CodeCompletion->completeTypeIdentifierWithDot();
      else
        P.CodeCompletion->completeTypeIdentifierWithoutDot();
    }
  }

  // Return nullptr to typecheck this TypeRepr independently in code completion.
  return nullptr;
}

TypeRepr *ASTGen::generate(const UnknownTypeSyntax &Type, const SourceLoc Loc) {
  auto ChildrenCount = Type.getNumChildren();

  // Recover from old-style protocol composition:
  //   `protocol` `<` protocols `>`
  if (ChildrenCount >= 2) {
    auto keyword = Type.getChild(0)->getAs<TokenSyntax>();

    if (keyword && keyword->getText() == "protocol") {
      auto keywordLoc = advanceLocBegin(Loc, *keyword);
      auto LAngle = Type.getChild(1);
      auto RAngle = Type.getChild(ChildrenCount - 1);

      auto LAngleLoc = advanceLocBegin(Loc, *LAngle);
      auto RAngleLoc = advanceLocBegin(Loc, *RAngle);

      SmallVector<TypeRepr *, 4> protocols;
      for (unsigned i = 2; i < Type.getNumChildren(); i++) {
        if (auto elem = Type.getChild(i)->getAs<TypeSyntax>())
          if (auto proto = generate(*elem, Loc))
            protocols.push_back(proto);
      }

      return CompositionTypeRepr::create(Context, protocols, keywordLoc,
                                         {LAngleLoc, RAngleLoc});
    }
  }

  // Create ErrorTypeRepr for keywords.
  if (ChildrenCount == 1) {
    auto Keyword = Type.getChild(0)->getAs<TokenSyntax>();
    if (Keyword && isTokenKeyword(Keyword->getTokenKind())) {
      auto ErrorLoc = generate(*Keyword, Loc);
      return new (Context) ErrorTypeRepr(ErrorLoc);
    }
  }

  // generate child 'TypeSyntax' anyway to trigger the side effects e.g.
  // code-completion.
  for (size_t i = 0; i != ChildrenCount; ++i) {
    auto elem = *Type.getChild(i);
    if (auto ty = elem.getAs<TypeSyntax>())
      (void)generate(*ty, Loc);
  }

  // let's hope the main `generate` method can find this node in the type map
  return nullptr;
}

void
ASTGen::generate(const GenericArgumentClauseSyntax &clause, const SourceLoc Loc,
                 SourceLoc &lAngleLoc, SourceLoc &rAngleLoc,
                 SmallVectorImpl<TypeRepr *> &args) {
  lAngleLoc = advanceLocBegin(Loc, clause);
  rAngleLoc = advanceLocEnd(Loc, clause);

  assert(args.empty());
  for (auto Arg : clause.getArguments()) {
    auto tyR = generate(Arg.getArgumentType(), Loc);
    if (!tyR)
      tyR = new (Context) ErrorTypeRepr(advanceLocBegin(Loc, Arg));
    args.push_back(tyR);
  }
}

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig, ASTContext &Context) {
  char *start = static_cast<char *>(Context.Allocate(Orig.size(), 1));
  char *p = start;

  if (p) {
    for (char c : Orig) {
      if (c != '_') {
        *p++ = c;
      }
    }
  }

  return StringRef(start, p - start);
}

GenericParamList *
ASTGen::generate(const GenericParameterClauseListSyntax &clauses,
                 const SourceLoc Loc) {
  GenericParamList *curr = nullptr;

  // The first one is the outmost generic parameter list.
  for (const auto &clause : clauses) {
    auto params = generate(clause, Loc);
    if (!params)
      continue;
    params->setOuterParameters(curr);
    curr = params;
  }

  return curr;
}

GenericParamList *ASTGen::generate(const GenericParameterClauseSyntax &clause,
                                   const SourceLoc Loc) {
  SmallVector<GenericTypeParamDecl *, 4> params;
  params.reserve(clause.getGenericParameterList().getNumChildren());

  for (auto elem : clause.getGenericParameterList()) {
    auto nameTok = elem.getName();
    if (nameTok.isMissing())
      break;

    DeclAttributes attrs = generateDeclAttributes(elem, Loc, false);
    Identifier name = Context.getIdentifier(elem.getName().getIdentifierText());
    SourceLoc nameLoc = advanceLocBegin(Loc, elem.getName());

    // We always create generic type parameters with an invalid depth.
    // Semantic analysis fills in the depth when it processes the generic
    // parameter list.
    auto param = new (Context)
        GenericTypeParamDecl(P.CurDeclContext, name, nameLoc,
                             GenericTypeParamDecl::InvalidDepth, params.size());

    if (auto inherited = elem.getInheritedType()) {
      if (auto ty = generate(*inherited, Loc)) {
        SmallVector<TypeLoc, 1> constraints = {ty};
        param->setInherited(Context.AllocateCopy(constraints));
      }
    }

    // Attach attributes.
    param->getAttrs() = attrs;

    // Add this parameter to the scope.
    addToScope(param);

    params.push_back(param);
  }
  if (params.empty())
    return nullptr;

  SourceLoc whereLoc;
  SmallVector<RequirementRepr, 4> requirements;
  if (auto whereClause = clause.getObsoletedWhereClause()) {
    requirements.reserve(whereClause->getRequirementList().size());
    for (auto elem : whereClause->getRequirementList()) {
      if (auto req = generate(elem, Loc))
        requirements.push_back(*req);
    }
    // There's an invariant that valid 'where' loc means that there's at
    // at least one valid requirement.
    if (!requirements.empty())
      whereLoc = advanceLocBegin(Loc, whereClause->getWhereKeyword());
  }

  auto lAngleLoc = advanceLocBegin(Loc, clause);
  auto rAngleLoc = advanceLocEnd(Loc, clause);
  return GenericParamList::create(Context, lAngleLoc, params, whereLoc,
                                  requirements, rAngleLoc);
}

Optional<RequirementRepr>
ASTGen::generate(const syntax::GenericRequirementSyntax &req,
                 const SourceLoc Loc) {
  if (auto sameTypeReq = req.getBody().getAs<SameTypeRequirementSyntax>()) {
    auto firstType = generate(sameTypeReq->getLeftTypeIdentifier(), Loc);
    auto secondType = generate(sameTypeReq->getRightTypeIdentifier(), Loc);
    if (!firstType || !secondType)
      return None;
    return RequirementRepr::getSameType(
        firstType, advanceLocBegin(Loc, sameTypeReq->getEqualityToken()),
        secondType);
  } else if (auto conformanceReq =
                 req.getBody().getAs<ConformanceRequirementSyntax>()) {
    auto firstType = generate(conformanceReq->getLeftTypeIdentifier(), Loc);
    auto secondType = generate(conformanceReq->getRightTypeIdentifier(), Loc);
    if (!firstType || !secondType)
      return None;
    return RequirementRepr::getTypeConstraint(
        firstType, advanceLocBegin(Loc, conformanceReq->getColon()),
        secondType);
  } else if (auto layoutReq = req.getBody().getAs<LayoutRequirementSyntax>()) {
    auto firstType = generate(layoutReq->getLeftTypeIdentifier(), Loc);
    auto layout = generate(layoutReq->getLayoutConstraint(), Loc);
    if (!firstType || layout.isNull())
      return None;
    auto colonLoc = advanceLocBegin(Loc, layoutReq->getColon());
    auto layoutLoc = advanceLocBegin(Loc, layoutReq->getLayoutConstraint());
    return RequirementRepr::getLayoutConstraint(
        firstType, colonLoc, LayoutConstraintLoc(layout, layoutLoc));
  } else {
    llvm_unreachable("invalid syntax kind for requirement body");
  }
}

static LayoutConstraintKind getLayoutConstraintKind(Identifier &id,
                                                    ASTContext &Ctx) {
  if (id == Ctx.Id_TrivialLayout)
    return LayoutConstraintKind::TrivialOfExactSize;
  if (id == Ctx.Id_TrivialAtMostLayout)
    return LayoutConstraintKind::TrivialOfAtMostSize;
  if (id == Ctx.Id_RefCountedObjectLayout)
    return LayoutConstraintKind::RefCountedObject;
  if (id == Ctx.Id_NativeRefCountedObjectLayout)
    return LayoutConstraintKind::NativeRefCountedObject;
  if (id == Ctx.Id_ClassLayout)
    return LayoutConstraintKind::Class;
  if (id == Ctx.Id_NativeClassLayout)
    return LayoutConstraintKind::NativeClass;
  return LayoutConstraintKind::UnknownLayout;
}

LayoutConstraint ASTGen::generate(const LayoutConstraintSyntax &constraint,
                                  const SourceLoc Loc) {
  auto name = Context.getIdentifier(constraint.getName().getIdentifierText());
  auto constraintKind = getLayoutConstraintKind(name, Context);
  assert(constraintKind != LayoutConstraintKind::UnknownLayout);

  // Non-trivial constraint kinds don't have size/alignment.
  // TODO: Diagnose if it's supplied?
  if (!LayoutConstraintInfo::isTrivial(constraintKind))
    return LayoutConstraint::getLayoutConstraint(constraintKind, Context);

  // '_Trivial' without explicit size/alignment.
  if (!constraint.getSize())
    return LayoutConstraint::getLayoutConstraint(LayoutConstraintKind::Trivial,
                                                 Context);

  int size = 0;
  if (auto sizeSyntax = constraint.getSize())
    sizeSyntax->getText().getAsInteger(10, size);
  assert(size >= 0);

  int alignment = 0;
  if (auto alignmentSyntax = constraint.getAlignment())
    alignmentSyntax->getText().getAsInteger(10, alignment);
  assert(alignment >= 0);

  return LayoutConstraint::getLayoutConstraint(constraintKind, size, alignment,
                                               Context);
}

SourceLoc ASTGen::advanceLocBegin(const SourceLoc &Loc, const Syntax &Node) {
  return Loc.getAdvancedLoc(Node.getAbsolutePosition().getOffset());
}

SourceLoc ASTGen::advanceLocEnd(const SourceLoc &Loc, const Syntax &Node) {
  if (!Node.isMissing()) {
    // NOTE: We cannot use 'getLastToken()' because it doesn't take string
    // literal expressions into account.
    if (Node.isToken() || Node.is<StringLiteralExprSyntax>())
      return advanceLocBegin(Loc, Node);
    for (size_t I = Node.getNumChildren(); I != 0; --I)
      if (auto Child = Node.getChild(I - 1))
        return advanceLocEnd(Loc, *Child);
  }
  if (auto Prev = Node.getPreviousNode())
    return advanceLocEnd(Loc, *Prev);
  assert(false && "No tokens in tree?");
  return Loc;
}

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig) {
  return copyAndStripUnderscores(Orig, Context);
}

Expr *
ASTGen::generateMagicIdentifierLiteralExpression(const TokenSyntax &PoundToken,
                                                 const SourceLoc Loc) {
  auto Kind = getMagicIdentifierLiteralKind(PoundToken.getTokenKind());
  auto KindLoc = advanceLocBegin(Loc, PoundToken);
  return new (Context) MagicIdentifierLiteralExpr(Kind, KindLoc);
}

MagicIdentifierLiteralExpr::Kind
ASTGen::getMagicIdentifierLiteralKind(tok Kind) {
  switch (Kind) {
  case tok::kw___COLUMN__:
  case tok::pound_column:
    return MagicIdentifierLiteralExpr::Kind::Column;
  case tok::kw___FILE__:
  case tok::pound_file:
    return MagicIdentifierLiteralExpr::Kind::File;
  case tok::kw___FUNCTION__:
  case tok::pound_function:
    return MagicIdentifierLiteralExpr::Kind::Function;
  case tok::kw___LINE__:
  case tok::pound_line:
    return MagicIdentifierLiteralExpr::Kind::Line;
  case tok::kw___DSO_HANDLE__:
  case tok::pound_dsohandle:
    return MagicIdentifierLiteralExpr::Kind::DSOHandle;
  default:
    llvm_unreachable("not a magic literal");
  }
}

ValueDecl *ASTGen::lookupInScope(DeclName Name) {
  return P.lookupInScope(Name);
}

void ASTGen::addToScope(ValueDecl *D, bool diagnoseRedefinitions) {
  P.addToScope(D, diagnoseRedefinitions);
}

TypeRepr *ASTGen::cacheType(TypeSyntax Type, TypeRepr *TypeAST) {
  TypeCache[Type.getId()] = TypeAST;
  return TypeAST;
}

TypeRepr *ASTGen::lookupType(TypeSyntax Type) {
  auto Found = TypeCache.find(Type.getId());
  return Found != TypeCache.end() ? Found->second : nullptr;
}

void ASTGen::addExpr(Expr *E, const SourceLoc Loc) {
  assert(!hasExpr(Loc));
  Exprs[Loc] = E;
}

bool ASTGen::hasExpr(const SourceLoc Loc) const {
  return Exprs.find(Loc) != Exprs.end();
}

Expr *ASTGen::takeExpr(const SourceLoc Loc) {
  auto I = Exprs.find(Loc);
  assert(I != Exprs.end());
  auto expr = I->second;
  Exprs.erase(I);
  return expr;
}

void ASTGen::addDeclAttributes(DeclAttributes attrs, SourceLoc Loc) {
  assert(!hasDeclAttributes(Loc));
  ParsedDeclAttrs[Loc] = attrs;
}

bool ASTGen::hasDeclAttributes(SourceLoc Loc) const {
  return ParsedDeclAttrs.find(Loc) != ParsedDeclAttrs.end();
}

DeclAttributes ASTGen::takeDeclAttributes(SourceLoc Loc) {
  auto I = ParsedDeclAttrs.find(Loc);
  assert(I != ParsedDeclAttrs.end());
  auto attrs = I->second;
  ParsedDeclAttrs.erase(I);
  return attrs;
}
