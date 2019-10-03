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
ASTGen::generateDeclAttributes(const DeclSyntax &D,
                               const Optional<AttributeListSyntax> &attrs,
                               const Optional<ModifierListSyntax> &modifiers,
                               SourceLoc Loc, bool includeComments) {
  SourceLoc attrsLoc;
  if (attrs) {
    attrsLoc = advanceLocBegin(Loc, *attrs->getFirstToken());
  } else if (modifiers) {
    attrsLoc = advanceLocBegin(Loc, *modifiers->getFirstToken());
  } else {
    // We might have comment attributes.
    attrsLoc = advanceLocBegin(Loc, *D.getFirstToken());
  }
  if (hasDeclAttributes(attrsLoc))
    return getDeclAttributes(attrsLoc);
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

  DeclAttributes attrs =
      generateDeclAttributes(D, D.getAttributes(), D.getModifiers(), Loc, true);

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
  auto attrs =
      generateDeclAttributes(D, D.getAttributes(), D.getModifiers(), Loc, true);
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
  auto LPLoc = generate(LParen, Loc);
  auto RPLoc = generate(RParen, Loc);

  SmallVector<TupleTypeReprElement, 4> TupleElements;

  SourceLoc EllipsisLoc;
  unsigned EllipsisIdx = Elements.size();

  for (unsigned i = 0; i < Elements.getNumChildren(); i++) {
    auto Element = Elements.getChild(i)->castTo<TupleTypeElementSyntax>();
    TupleTypeReprElement ElementAST;
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
    ElementAST.Type = generate(Element.getType(), Loc);
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
      EllipsisLoc = generate(*Ellipsis, Loc);
      if (EllipsisIdx == Elements.size())
        EllipsisIdx = i;
    }
    TupleElements.push_back(ElementAST);
  }

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
    auto Args = Clause->getArguments();
    if (!Args.empty()) {
      auto LAngleLoc = advanceLocBegin(Loc, Clause->getLeftAngleBracket());
      auto RAngleLoc = advanceLocBegin(Loc, Clause->getRightAngleBracket());
      SourceRange Range{LAngleLoc, RAngleLoc};
      auto ArgsAST = generate(Args, Loc);
      return GenericIdentTypeRepr::create(Context, IdentifierLoc, Identifier,
                                          ArgsAST, Range);
    }
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
  if (Type.getLeftSquareBracket().isPresent())
    LBracketLoc = advanceLocBegin(Loc, Type.getLeftSquareBracket());
  else
    LBracketLoc = advanceLocBegin(Loc, *Type.getFirstToken());
  if (Type.getRightSquareBracket().isPresent())
    RBracketLoc = advanceLocBegin(Loc, Type.getRightSquareBracket());
  else
    RBracketLoc = advanceLocBegin(Loc, *Type.getLastToken());
  SourceRange Range{LBracketLoc, RBracketLoc};
  return new (Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

TypeRepr *ASTGen::generate(const ArrayTypeSyntax &Type, SourceLoc Loc) {
  TypeRepr *ElementType = generate(Type.getElementType(), Loc);
  if (!ElementType)
    return nullptr;
  SourceLoc LBracketLoc, RBracketLoc;
  if (Type.getLeftSquareBracket().isPresent())
    LBracketLoc = advanceLocBegin(Loc, Type.getLeftSquareBracket());
  else
    LBracketLoc = advanceLocBegin(Loc, *Type.getFirstToken());
  if (Type.getRightSquareBracket().isPresent())
    RBracketLoc = advanceLocBegin(Loc, Type.getRightSquareBracket());
  else
    RBracketLoc = advanceLocBegin(Loc, *Type.getLastToken());
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
  SmallVector<TypeRepr*, 4> Args;
  if (auto genericArgs = Type.getGenericArgumentClause()) {
    if (genericArgs->getRightAngleBracket().isMissing())
      return nullptr;
    LAngleLoc = advanceLocBegin(Loc, genericArgs->getLeftAngleBracket());
    RAngleLoc = advanceLocBegin(Loc, genericArgs->getRightAngleBracket());
    Args = generate(genericArgs->getArguments(), Loc);
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

  // Create empty TupleTypeRepr for types starting with `(`.
  if (ChildrenCount >= 1) {
    auto LParen = Type.getChild(0)->getAs<TokenSyntax>();
    if (LParen && LParen->getTokenKind() == tok::l_paren) {
      // generate child 'TypeSyntax' anyway to trigger the side effects e.g.
      // code-completion.
      for (size_t i = 1; i != ChildrenCount; ++i) {
        auto elem = *Type.getChild(i);
        if (auto ty = elem.getAs<TypeSyntax>())
          (void)generate(*ty, Loc);
      }
      auto LParenLoc = advanceLocBegin(Loc, *LParen);
      auto EndLoc =
          advanceLocBegin(Loc, *Type.getChild(Type.getNumChildren() - 1));
      return TupleTypeRepr::createEmpty(Context, {LParenLoc, EndLoc});
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

SmallVector<TypeRepr *, 4>
ASTGen::generate(const GenericArgumentListSyntax &Args, const SourceLoc Loc) {
  SmallVector<TypeRepr *, 4> Types;
  for (auto Arg : Args) {
    auto tyR = generate(Arg, Loc);
    if (!tyR)
      tyR = new (Context) ErrorTypeRepr(advanceLocBegin(Loc, Arg));
    Types.push_back(tyR);
  }

  return Types;
}

TypeRepr *ASTGen::generate(const GenericArgumentSyntax &Arg,
                           const SourceLoc Loc) {
  return generate(Arg.getArgumentType(), Loc);
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

    DeclAttributes attrs;
    if (auto attrsSyntax = elem.getAttributes()) {
      if (auto firstTok = attrsSyntax->getFirstToken()) {
        auto attrsLoc = advanceLocBegin(Loc, *firstTok);
        if (hasDeclAttributes(attrsLoc))
          attrs = getDeclAttributes(attrsLoc);
      }
    }
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

  auto lAngleLoc = advanceLocBegin(Loc, clause.getLeftAngleBracket());
  auto rAngleLoc = advanceLocBegin(Loc, clause.getRightAngleBracket());
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

void ASTGen::addDeclAttributes(DeclAttributes attrs, SourceLoc Loc) {
  ParsedDeclAttrs.insert({Loc, attrs});
}

bool ASTGen::hasDeclAttributes(SourceLoc Loc) const {
  return ParsedDeclAttrs.find(Loc) != ParsedDeclAttrs.end();
}

DeclAttributes ASTGen::getDeclAttributes(SourceLoc Loc) const {
  return ParsedDeclAttrs.find(Loc)->second;
}
