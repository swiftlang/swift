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

#include "swift/Basic/SourceManager.h"
#include "swift/Parse/ASTGen.h"

using namespace swift;
using namespace swift::syntax;

SourceLoc ASTGen::generate(TokenSyntax Tok, SourceLoc &Loc) {
  return advanceLocBegin(Loc, Tok);
}

Expr *ASTGen::generate(IntegerLiteralExprSyntax &Expr, SourceLoc &Loc) {
  auto Digits = Expr.getDigits();
  auto Text = copyAndStripUnderscores(Digits.getText());
  auto DigitsLoc = advanceLocBegin(Loc, Digits);
  return new (Context) IntegerLiteralExpr(Text, DigitsLoc);
}

Expr *ASTGen::generate(FloatLiteralExprSyntax &Expr, SourceLoc &Loc) {
  auto Digits = Expr.getFloatingDigits();
  auto Text = copyAndStripUnderscores(Digits.getText());
  auto DigitsLoc = advanceLocBegin(Loc, Digits);
  return new (Context) FloatLiteralExpr(Text, DigitsLoc);
}

Expr *ASTGen::generate(NilLiteralExprSyntax &Expr, SourceLoc &Loc) {
  auto Nil = Expr.getNilKeyword();
  auto NilLoc = advanceLocBegin(Loc, Nil);
  return new (Context) NilLiteralExpr(NilLoc);
}

Expr *ASTGen::generate(BooleanLiteralExprSyntax &Expr, SourceLoc &Loc) {
  auto Boolean = Expr.getBooleanLiteral();
  auto Value = Boolean.getTokenKind() == tok::kw_true;
  auto BooleanLoc = advanceLocBegin(Loc, Boolean);
  return new (Context) BooleanLiteralExpr(Value, BooleanLoc);
}

Expr *ASTGen::generate(PoundFileExprSyntax &Expr, SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpression(Expr.getPoundFile(), Loc);
}

Expr *ASTGen::generate(PoundLineExprSyntax &Expr, SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpression(Expr.getPoundLine(), Loc);
}

Expr *ASTGen::generate(PoundColumnExprSyntax &Expr, SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpression(Expr.getPoundColumn(), Loc);
}

Expr *ASTGen::generate(PoundFunctionExprSyntax &Expr, SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpression(Expr.getPoundFunction(), Loc);
}

Expr *ASTGen::generate(PoundDsohandleExprSyntax &Expr, SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpression(Expr.getPoundDsohandle(), Loc);
}

Expr *ASTGen::generate(UnknownExprSyntax &Expr, SourceLoc &Loc) {
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

TypeRepr *ASTGen::generate(TypeSyntax Type, SourceLoc &Loc) {
  TypeRepr *TypeAST = lookupType(Type);

  if (TypeAST)
    return TypeAST;

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

TypeRepr *ASTGen::generate(FunctionTypeSyntax Type, SourceLoc &Loc) {
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

TupleTypeRepr *ASTGen::generateTuple(TokenSyntax LParen,
                                     TupleTypeElementListSyntax Elements,
                                     TokenSyntax RParen, SourceLoc &Loc,
                                     bool IsFunction) {
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
                            : Context.getIdentifier(Name->getText());
    }
    if (auto Colon = Element.getColon())
      ElementAST.ColonLoc = generate(*Colon, Loc);
    if (auto SecondName = Element.getSecondName()) {
      ElementAST.SecondNameLoc = generate(*SecondName, Loc);
      ElementAST.SecondName =
          SecondName->getText() == "_"
              ? Identifier()
              : Context.getIdentifier(SecondName->getText());
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

TypeRepr *ASTGen::generate(AttributedTypeSyntax Type, SourceLoc &Loc) {
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
        auto Begin = advanceLocBegin(Loc, Argument);
        auto End = advanceLocEnd(Loc, Argument);
        CharSourceRange Range{Context.SourceMgr, Begin, End};
        TypeAttrs.convention = Range.str();
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

TypeRepr *ASTGen::generate(TupleTypeSyntax Type, SourceLoc &Loc) {
  return generateTuple(Type.getLeftParen(), Type.getElements(),
                       Type.getRightParen(), Loc);
}

TypeRepr *ASTGen::generate(SomeTypeSyntax Type, SourceLoc &Loc) {
  auto Some = Type.getSomeSpecifier();
  auto SomeLoc = generate(Some, Loc);
  auto BaseType = generate(Type.getBaseType(), Loc);
  return new (Context) OpaqueReturnTypeRepr(SomeLoc, BaseType);
}

TypeRepr *ASTGen::generate(CompositionTypeSyntax Type, SourceLoc &Loc) {
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
    TypeSyntax Component, SourceLoc &Loc,
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
TypeRepr *ASTGen::generateSimpleOrMemberIdentifier(T Type, SourceLoc &Loc) {
  SmallVector<ComponentIdentTypeRepr *, 4> Components;
  gatherTypeIdentifierComponents(Type, Loc, Components);
  std::reverse(Components.begin(), Components.end());

  auto IdentType = IdentTypeRepr::create(Context, Components);
  auto FirstComponent = IdentType->getComponentRange().front();
  // Lookup element #0 through our current scope chains in case it is some
  // thing local (this returns null if nothing is found).
  if (auto Entry = lookupInScope(FirstComponent->getIdentifier()))
    if (auto *TD = dyn_cast<TypeDecl>(Entry))
      FirstComponent->setValue(TD, nullptr);

  return IdentType;
}

template <typename T>
ComponentIdentTypeRepr *ASTGen::generateIdentifier(T Type, SourceLoc &Loc) {
  auto IdentifierLoc = advanceLocBegin(Loc, Type.getName());
  auto Identifier = Context.getIdentifier(Type.getName().getText());
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

TypeRepr *ASTGen::generate(SimpleTypeIdentifierSyntax Type, SourceLoc &Loc) {
  if (Type.getName().getTokenKind() == tok::kw_Any) {
    auto AnyLoc = advanceLocBegin(Loc, Type.getName());
    return CompositionTypeRepr::createEmptyComposition(Context, AnyLoc);
  }

  return generateSimpleOrMemberIdentifier(Type, Loc);
}

TypeRepr *ASTGen::generate(MemberTypeIdentifierSyntax Type, SourceLoc &Loc) {
  return generateSimpleOrMemberIdentifier(Type, Loc);
}

TypeRepr *ASTGen::generate(DictionaryTypeSyntax Type, SourceLoc &Loc) {
  TypeRepr *ValueType = generate(Type.getValueType(), Loc);
  TypeRepr *KeyType = generate(Type.getKeyType(), Loc);
  auto LBraceLoc = advanceLocBegin(Loc, Type.getLeftSquareBracket());
  auto ColonLoc = advanceLocBegin(Loc, Type.getColon());
  auto RBraceLoc = advanceLocBegin(Loc, Type.getRightSquareBracket());
  SourceRange Range{LBraceLoc, RBraceLoc};
  return new (Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

TypeRepr *ASTGen::generate(ArrayTypeSyntax Type, SourceLoc &Loc) {
  TypeRepr *ElementType = generate(Type.getElementType(), Loc);
  auto LBraceLoc = advanceLocBegin(Loc, Type.getLeftSquareBracket());
  auto RBraceLoc = advanceLocBegin(Loc, Type.getRightSquareBracket());
  SourceRange Range{LBraceLoc, RBraceLoc};
  return new (Context) ArrayTypeRepr(ElementType, Range);
}

TypeRepr *ASTGen::generate(MetatypeTypeSyntax Type, SourceLoc &Loc) {
  TypeRepr *BaseType = generate(Type.getBaseType(), Loc);
  auto TypeOrProtocol = Type.getTypeOrProtocol();
  auto TypeOrProtocolLoc = advanceLocBegin(Loc, TypeOrProtocol);
  if (TypeOrProtocol.getText() == "Type")
    return new (Context) MetatypeTypeRepr(BaseType, TypeOrProtocolLoc);
  return new (Context) ProtocolTypeRepr(BaseType, TypeOrProtocolLoc);
}

TypeRepr *ASTGen::generate(OptionalTypeSyntax Type, SourceLoc &Loc) {
  TypeRepr *WrappedType = generate(Type.getWrappedType(), Loc);
  auto QuestionLoc = advanceLocBegin(Loc, Type.getQuestionMark());
  return new (Context) OptionalTypeRepr(WrappedType, QuestionLoc);
}

TypeRepr *ASTGen::generate(ImplicitlyUnwrappedOptionalTypeSyntax Type,
                           SourceLoc &Loc) {
  TypeRepr *WrappedType = generate(Type.getWrappedType(), Loc);
  auto ExclamationLoc = advanceLocBegin(Loc, Type.getExclamationMark());
  return new (Context)
      ImplicitlyUnwrappedOptionalTypeRepr(WrappedType, ExclamationLoc);
}

TypeRepr *ASTGen::generate(UnknownTypeSyntax Type, SourceLoc &Loc) {
  auto ChildrenCount = Type.getNumChildren();

  // Recover from C-style array type:
  //   type '[' ']'
  //   type '[' expr ']'
  if (ChildrenCount == 3 || ChildrenCount == 4) {
    auto Element = Type.getChild(0)->getAs<TypeSyntax>();
    auto LSquare = Type.getChild(1)->getAs<TokenSyntax>();
    auto Last = Type.getChild(ChildrenCount - 1);

    if (Element && LSquare && LSquare->getTokenKind() == tok::l_square) {
      auto ElementType = generate(*Element, Loc);
      auto Begin = advanceLocBegin(Loc, *Element);
      auto End = advanceLocBegin(Loc, *Last);
      return new (Context) ArrayTypeRepr(ElementType, {Begin, End});
    }
  }

  // Recover from extra `[`:
  //   type `[`
  if (ChildrenCount == 2) {
    auto Element = Type.getChild(0)->getAs<TypeSyntax>();
    auto LSquare = Type.getChild(1)->getAs<TokenSyntax>();

    if (Element && LSquare && LSquare->getTokenKind() == tok::l_square) {
      return generate(*Element, Loc);
    }
  }

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
      auto EndLoc = advanceLocBegin(Loc, *Type.getChild(Type.getNumChildren() - 1));
      return TupleTypeRepr::createEmpty(Context, {LParenLoc, EndLoc});
    }
  }

  // let's hope the main `generate` method can find this node in the type map
  return nullptr;
}

SmallVector<TypeRepr *, 4> ASTGen::generate(GenericArgumentListSyntax Args,
                                            SourceLoc &Loc) {
  SmallVector<TypeRepr *, 4> Types;
  Types.resize(Args.size());

  for (int i = Args.size() - 1; i >= 0; i--) {
    auto Arg = Args.getChild(i).getValue().castTo<GenericArgumentSyntax>();
    auto Type = generate(Arg, Loc);
    Types[i] = Type;
  }

  return Types;
}

TypeRepr *ASTGen::generate(GenericArgumentSyntax Arg, SourceLoc &Loc) {
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

SourceLoc ASTGen::advanceLocBegin(const SourceLoc &Loc, const Syntax &Node) {
  return Loc.getAdvancedLoc(Node.getAbsolutePosition().getOffset());
}

SourceLoc ASTGen::advanceLocEnd(const SourceLoc &Loc, const TokenSyntax &Token) {
  return advanceLocAfter(Loc, Token.withTrailingTrivia({}));
}

SourceLoc ASTGen::advanceLocAfter(const SourceLoc &Loc, const Syntax &Node) {
  return Loc.getAdvancedLoc(
      Node.getAbsoluteEndPositionAfterTrailingTrivia().getOffset());
}

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig) {
  return copyAndStripUnderscores(Orig, Context);
}

Expr *ASTGen::generateMagicIdentifierLiteralExpression(TokenSyntax PoundToken,
                                                       SourceLoc &Loc) {
  auto Kind = getMagicIdentifierLiteralKind(PoundToken.getTokenKind());
  auto KindLoc = advanceLocBegin(Loc, PoundToken);
  return new (Context) MagicIdentifierLiteralExpr(Kind, KindLoc);
}

MagicIdentifierLiteralExpr::Kind ASTGen::getMagicIdentifierLiteralKind(tok Kind) {
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
  return Context.LangOpts.EnableASTScopeLookup
             ? nullptr
             : (*ParserState)->getScopeInfo().lookupValueName(Name);
}

TypeRepr *ASTGen::cacheType(TypeSyntax Type, TypeRepr *TypeAST) {
  TypeCache[Type.getId()] = TypeAST;
  return TypeAST;
}

TypeRepr *ASTGen::lookupType(TypeSyntax Type) {
  auto Found = TypeCache.find(Type.getId());
  return Found != TypeCache.end() ? Found->second : nullptr;
}

TypeRepr *ASTGen::addType(TypeRepr *Type, const SourceLoc &Loc) {
  return Types.insert({Loc, Type}).first->second;
}

bool ASTGen::hasType(const SourceLoc &Loc) const {
  return Types.find(Loc) != Types.end();
}

TypeRepr *ASTGen::getType(const SourceLoc &Loc) const {
  return Types.find(Loc)->second;
}
