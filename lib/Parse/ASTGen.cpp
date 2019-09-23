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

#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Parser.h"

using namespace swift;
using namespace swift::syntax;

SourceLoc ASTGen::generate(const TokenSyntax &Tok, const SourceLoc Loc) {
  return advanceLocBegin(Loc, Tok);
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

TypeRepr *ASTGen::generate(const TypeSyntax &Type, const SourceLoc Loc) {
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
  else if (auto Unknown = Type.getAs<UnknownTypeSyntax>())
    TypeAST = generate(*Unknown, Loc);

  // todo [gsoc]: handle InheritedTypeSyntax & ClassRestrictionTypeSyntax?

  if (!TypeAST && hasType(advanceLocBegin(Loc, Type)))
    TypeAST = getType(advanceLocBegin(Loc, Type));

  return cacheType(Type, TypeAST);
}

TypeRepr *ASTGen::generate(const FunctionTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto ArgumentTypes = generateTuple(Type.getLeftParen(), Type.getArguments(),
                                     Type.getRightParen(), Loc,
                                     /*IsFunction=*/true);

  auto ThrowsLoc = Type.getThrowsOrRethrowsKeyword()
                       ? generate(*Type.getThrowsOrRethrowsKeyword(), Loc)
                       : SourceLoc();

  auto ArrowLoc = generate(Type.getArrow(), Loc);
  auto ReturnType = generate(Type.getReturnType(), Loc);

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

TypeRepr *ASTGen::generate(const AttributedTypeSyntax &Type,
                           const SourceLoc Loc) {
  // todo [gsoc]: improve this after refactoring attribute parsing

  auto TypeAST = generate(Type.getBaseType(), Loc);

  if (auto Attributes = Type.getAttributes()) {
    TypeAttributes TypeAttrs;

    for (auto Attribute : *Attributes) {
      auto Attr = Attribute.castTo<AttributeSyntax>();
      auto AttrNameStr = Attr.getAttributeName().getText();

      auto AtLoc = advanceLocBegin(Loc, Attr.getAtSignToken());
      auto AttrKind = TypeAttributes::getAttrKindFromString(AttrNameStr);

      TypeAttrs.setAttr(AttrKind, AtLoc);

      if (AttrKind == TAK_convention) {
        auto Argument = Attr.getArgument()->castTo<TokenSyntax>();
        auto Convention = Context.getIdentifier(Argument.getIdentifierText());
        TypeAttrs.convention = Convention.str();
      }

      if (AttrKind == TAK_opened) {
        auto AttrText = Attr.getArgument()->castTo<TokenSyntax>().getText();
        auto LiteralText = AttrText.slice(1, AttrText.size() - 1);
        TypeAttrs.OpenedID = UUID::fromString(LiteralText.str().c_str());
      }

      if (TypeAttrs.AtLoc.isInvalid())
        TypeAttrs.AtLoc = AtLoc;
    }

    if (!TypeAttrs.empty())
      TypeAST = new (Context) AttributedTypeRepr(TypeAttrs, TypeAST);
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
  if (Type.getName().getText() == "class") {
    auto classLoc = advanceLocBegin(Loc, Type.getName());
    return new (Context)
        SimpleIdentTypeRepr(classLoc, Context.getIdentifier("AnyObject"));
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
  auto LBraceLoc = advanceLocBegin(Loc, Type.getLeftSquareBracket());
  auto ColonLoc = advanceLocBegin(Loc, Type.getColon());
  auto RBraceLoc = advanceLocBegin(Loc, Type.getRightSquareBracket());
  SourceRange Range{LBraceLoc, RBraceLoc};
  return new (Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

TypeRepr *ASTGen::generate(const ArrayTypeSyntax &Type, SourceLoc Loc) {
  TypeRepr *ElementType = generate(Type.getElementType(), Loc);
  SourceLoc LBraceLoc, RBraceLoc;
  if (Type.getLeftSquareBracket().isPresent())
    LBraceLoc = advanceLocBegin(Loc, Type.getLeftSquareBracket());
  else
    LBraceLoc = advanceLocBegin(Loc, Type.getElementType());
  if (Type.getLeftSquareBracket().isPresent())
    RBraceLoc = advanceLocBegin(Loc, Type.getRightSquareBracket());
  else
    RBraceLoc = advanceLocBegin(Loc, *Type.getLastToken());
  return new (Context) ArrayTypeRepr(ElementType, {LBraceLoc, RBraceLoc});
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

TypeRepr *ASTGen::generate(const UnknownTypeSyntax &Type, const SourceLoc Loc) {
  auto ChildrenCount = Type.getNumChildren();

  // Recover from old-style protocol composition:
  //   `protocol` `<` protocols `>`
  if (ChildrenCount >= 2) {
    auto Protocol = Type.getChild(0)->getAs<TokenSyntax>();

    if (Protocol && Protocol->getText() == "protocol") {
      auto LAngle = Type.getChild(1);

      SmallVector<TypeSyntax, 4> Protocols;
      for (unsigned i = 2; i < Type.getNumChildren(); i++)
        if (auto PType = Type.getChild(i)->getAs<TypeSyntax>())
          Protocols.push_back(*PType);

      auto RAngle = Type.getChild(ChildrenCount - 1);

      auto ProtocolLoc = advanceLocBegin(Loc, *Protocol);
      auto LAngleLoc = advanceLocBegin(Loc, *LAngle);
      auto RAngleLoc = advanceLocBegin(Loc, *RAngle);

      SmallVector<TypeRepr *, 4> ProtocolTypes;
      for (auto &&P : llvm::reverse(Protocols))
        ProtocolTypes.push_back(generate(P, Loc));
      std::reverse(std::begin(ProtocolTypes), std::end(ProtocolTypes));

      return CompositionTypeRepr::create(Context, ProtocolTypes, ProtocolLoc,
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
      auto LParenLoc = advanceLocBegin(Loc, *LParen);
      auto EndLoc =
          advanceLocBegin(Loc, *Type.getChild(Type.getNumChildren() - 1));
      return TupleTypeRepr::createEmpty(Context, {LParenLoc, EndLoc});
    }
  }

  // let's hope the main `generate` method can find this node in the type map
  return nullptr;
}

SmallVector<TypeRepr *, 4>
ASTGen::generate(const GenericArgumentListSyntax &Args, const SourceLoc Loc) {
  SmallVector<TypeRepr *, 4> Types;
  Types.resize(Args.size());

  for (int i = Args.size() - 1; i >= 0; i--) {
    auto Arg = Args.getChild(i).getValue().castTo<GenericArgumentSyntax>();
    auto Type = generate(Arg, Loc);
    Types[i] = Type;
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
      auto attrsLoc = advanceLocBegin(Loc, *attrsSyntax->getFirstToken());
      attrs = getDeclAttributes(attrsLoc);
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
        SmallVector<TypeLoc, 1> constraints = {generate(*inherited, Loc)};
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

void ASTGen::addType(TypeRepr *Type, const SourceLoc Loc) {
  Types.insert({Loc, Type});
}

bool ASTGen::hasType(const SourceLoc Loc) const {
  return Types.find(Loc) != Types.end();
}

TypeRepr *ASTGen::getType(const SourceLoc Loc) const {
  return Types.find(Loc)->second;
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
