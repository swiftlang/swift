#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "swift/Syntax/SyntaxBuilders.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using llvm::None;
using llvm::SmallString;

using namespace swift;
using namespace swift::syntax;

#pragma mark - declaration-modifier

DeclModifierSyntax getCannedDeclModifier(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  auto Private = Factory.makeIdentifier("private", "", "");
  auto LParen = Factory.makeLeftParenToken("", "");
  auto Set = Factory.makeIdentifier("set", "", "");
  auto RParen = Factory.makeRightParenToken("", "");
  auto DeclModifierDetail = Factory.makeDeclModifierDetail(
      /*GarbageNodes=*/None, LParen, /*GarbageNodes=*/None, Set,
      /*GarbageNodes=*/None, RParen);
  return Factory.makeDeclModifier(/*GarbageNodes=*/None, Private,
                                  /*GarbageNodes=*/None, DeclModifierDetail);
}

TEST(DeclSyntaxTests, DeclModifierMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SyntaxFactory Factory(Arena);
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankDeclModifier().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<24> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedDeclModifier(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "private(set)");
  }
}

TEST(DeclSyntaxTests, DeclModifierGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Private = Factory.makeIdentifier("private", "", "");
  auto LParen = Factory.makeLeftParenToken("", "");
  auto Set = Factory.makeIdentifier("set", "", "");
  auto RParen = Factory.makeRightParenToken("", "");
  auto DeclModifierDetail = Factory.makeDeclModifierDetail(
      /*GarbageNodes=*/None, LParen, /*GarbageNodes=*/None, Set,
      /*GarbageNodes=*/None, RParen);
  auto Mod =
      Factory.makeDeclModifier(/*GarbageNodes=*/None, Private,
                               /*GarbageNodes=*/None, DeclModifierDetail);

  ASSERT_EQ(Private.getRaw(), Mod.getName().getRaw());
  ASSERT_EQ(LParen.getRaw(), Mod.getDetail()->getLeftParen().getRaw());
  ASSERT_EQ(Set.getRaw(), Mod.getDetail()->getDetail().getRaw());
  ASSERT_EQ(RParen.getRaw(), Mod.getDetail()->getRightParen().getRaw());
}

TEST(DeclSyntaxTests, DeclModifierWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Private = Factory.makeIdentifier("private", "", "");
  auto LParen = Factory.makeLeftParenToken("", "");
  auto Set = Factory.makeIdentifier("set", "", "");
  auto RParen = Factory.makeRightParenToken("", "");

  SmallString<24> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  Factory.makeBlankDeclModifier()
      .withName(Private)
      .withDetail(Factory.makeDeclModifierDetail(/*GarbageNodes=*/None, LParen,
                                                 /*GarbageNodes=*/None, Set,
                                                 /*GarbageNodes=*/None, RParen))
      .print(OS);
  ASSERT_EQ(OS.str().str(), "private(set)");
}

#pragma mark - typealias-decl

TEST(DeclSyntaxTests, TypealiasMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankTypealiasDecl().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Typealias = Factory.makeTypealiasKeyword("", " ");
    auto Subsequence = Factory.makeIdentifier("MyCollection", "", "");
    auto ElementName = Factory.makeIdentifier("Element", "", "");
    auto ElementParam = Factory.makeGenericParameter(
        /*GarbageNodes=*/None, /*Attributes=*/None, /*GarbageNodes=*/None,
        /*Name=*/ElementName, /*GarbageNodes=*/None, /*Colon=*/None,
        /*GarbageNodes=*/None, /*InheritedType=*/None, /*GarbageNodes=*/None,
        /*TrailingComma=*/None);
    auto LeftAngle = Factory.makeLeftAngleToken("", "");
    auto RightAngle = Factory.makeRightAngleToken("", " ");
    auto GenericParams = GenericParameterClauseSyntaxBuilder(Arena)
                             .useLeftAngleBracket(LeftAngle)
                             .useRightAngleBracket(RightAngle)
                             .addGenericParameter(ElementParam)
                             .build();
    auto Assignment = Factory.makeEqualToken("", " ");
    auto ElementType = Factory.makeTypeIdentifier("Element", "", "");
    auto ElementArg = Factory.makeGenericArgument(
        /*GarbageNodes=*/None, /*ArgumentType=*/ElementType,
        /*GarbageNodes=*/None, /*TrailingComma=*/None);

    auto GenericArgs =
        GenericArgumentClauseSyntaxBuilder(Arena)
            .useLeftAngleBracket(LeftAngle)
            .useRightAngleBracket(Factory.makeRightAngleToken("", ""))
            .addArgument(ElementArg)
            .build();

    auto Array = Factory.makeIdentifier("Array", "", "");
    auto Array_Int = Factory.makeSimpleTypeIdentifier(
        /*GarbageNodes=*/None, /*Name=*/Array, /*GarbageNodes=*/None,
        /*GenericArgumentClause=*/GenericArgs);
    auto TypeInit = Factory.makeTypeInitializerClause(
        /*GarbageNodes=*/None, /*Equal=*/Assignment, /*GarbageNodes=*/None,
        /*Value=*/Array_Int);
    Factory
        .makeTypealiasDecl(
            /*GarbageNodes=*/None, /*Attributes=*/None, /*GarbageNodes=*/None,
            /*Modifiers=*/None, /*GarbageNodes=*/None,
            /*TypealiasKeyword=*/Typealias, /*GarbageNodes=*/None,
            /*Identifier=*/Subsequence, /*GarbageNodes=*/None,
            /*GenericParameterClause=*/GenericParams, /*GarbageNodes=*/None,
            /*Initializer=*/TypeInit, /*GarbageNodes=*/None,
            /*GenericWhereClause=*/None)
        .print(OS);
    ASSERT_EQ(OS.str().str(),
              "typealias MyCollection<Element> = Array<Element>");
  }
}

TEST(DeclSyntaxTests, TypealiasWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Typealias = Factory.makeTypealiasKeyword("", " ");
  auto MyCollection = Factory.makeIdentifier("MyCollection", "", "");
  auto ElementName = Factory.makeIdentifier("Element", "", "");
  auto ElementParam = Factory.makeGenericParameter(
      /*GarbageNodes=*/None, None, /*GarbageNodes=*/None, ElementName,
      /*GarbageNodes=*/None, None, /*GarbageNodes=*/None, None,
      /*GarbageNodes=*/None, None);
  auto LeftAngle = Factory.makeLeftAngleToken("", "");
  auto RightAngle = Factory.makeRightAngleToken("", " ");
  auto GenericParams = GenericParameterClauseSyntaxBuilder(Arena)
                           .useLeftAngleBracket(LeftAngle)
                           .useRightAngleBracket(RightAngle)
                           .addGenericParameter(ElementParam)
                           .build();
  auto Equal = Factory.makeEqualToken("", " ");

  auto ElementType = Factory.makeTypeIdentifier("Element", "", "");
  auto ElementArg = Factory.makeGenericArgument(
      /*GarbageNodes=*/None, ElementType, /*GarbageNodes=*/None, None);
  auto GenericArgs =
      GenericArgumentClauseSyntaxBuilder(Arena)
          .useLeftAngleBracket(LeftAngle)
          .useRightAngleBracket(Factory.makeRightAngleToken("", ""))
          .addArgument(ElementArg)
          .build();

  auto Array = Factory.makeIdentifier("Array", "", "");
  auto Array_Int = Factory.makeSimpleTypeIdentifier(
      /*GarbageNodes=*/None, Array, /*GarbageNodes=*/None, GenericArgs);
  auto Type_Init = Factory.makeTypeInitializerClause(
      /*GarbageNodes=*/None, Equal, /*GarbageNodes=*/None, Array_Int);
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankTypealiasDecl()
        .withTypealiasKeyword(Typealias)
        .withIdentifier(MyCollection)
        .withGenericParameterClause(GenericParams)
        .withInitializer(Type_Init)
        .print(OS);
    ASSERT_EQ(OS.str().str(),
              "typealias MyCollection<Element> = Array<Element>");
  }
}

TEST(DeclSyntaxTests, TypealiasBuilderAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  SmallString<64> Scratch;
  llvm::raw_svector_ostream OS(Scratch);

  auto Typealias = Factory.makeTypealiasKeyword("", " ");
  auto MyCollection = Factory.makeIdentifier("MyCollection", "", "");
  auto ElementName = Factory.makeIdentifier("Element", "", "");
  auto ElementParam = Factory.makeGenericParameter(ElementName, None);
  auto LeftAngle = Factory.makeLeftAngleToken("", "");
  auto RightAngle = Factory.makeRightAngleToken("", " ");
  auto GenericParams = GenericParameterClauseSyntaxBuilder(Arena)
                           .useLeftAngleBracket(LeftAngle)
                           .useRightAngleBracket(RightAngle)
                           .addGenericParameter(ElementParam)
                           .build();
  auto Equal = Factory.makeEqualToken("", " ");

  auto ElementType = Factory.makeTypeIdentifier("Element", "", "");
  auto ElementArg = Factory.makeGenericArgument(
      /*GarbageNodes=*/None, ElementType, /*GarbageNodes=*/None, None);

  auto GenericArgs =
      GenericArgumentClauseSyntaxBuilder(Arena)
          .useLeftAngleBracket(LeftAngle)
          .useRightAngleBracket(Factory.makeRightAngleToken("", ""))
          .addArgument(ElementArg)
          .build();

  auto Array = Factory.makeIdentifier("Array", "", "");
  auto Array_Int = Factory.makeSimpleTypeIdentifier(
      /*GarbageNodes=*/None, Array, /*GarbageNodes=*/None, GenericArgs);
  auto Type_Init = Factory.makeTypeInitializerClause(
      /*GarbageNodes=*/None, Equal, /*GarbageNodes=*/None, Array_Int);
  TypealiasDeclSyntaxBuilder(Arena)
      .useTypealiasKeyword(Typealias)
      .useIdentifier(MyCollection)
      .useGenericParameterClause(GenericParams)
      .useInitializer(Type_Init)
      .build()
      .print(OS);
  ASSERT_EQ(OS.str().str(),
            "typealias MyCollection<Element> = Array<Element>");
}

#pragma mark - parameter

FunctionParameterSyntax getCannedFunctionParameter(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  auto ExternalName = Factory.makeIdentifier("with", "", " ");
  auto LocalName = Factory.makeIdentifier("radius", "", "");
  auto Colon = Factory.makeColonToken("", " ");
  auto Int = Factory.makeTypeIdentifier("Int", "", " ");
  auto NoEllipsis = TokenSyntax::missingToken(tok::identifier, "...", Arena);
  auto Equal = Factory.makeEqualToken("", " ");

  auto Sign = Factory.makePrefixOperator("-", "", "");
  auto OneDigits = Factory.makeIntegerLiteral("1", "", "");
  auto OneLiteral =
      Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, OneDigits);
  auto One = Factory.makePrefixOperatorExpr(/*GarbageNodes=*/None, Sign,
                                            /*GarbageNodes=*/None, OneLiteral);
  auto DefaultArg = Factory.makeInitializerClause(/*GarbageNodes=*/None, Equal,
                                                  /*GarbageNodes=*/None, One);
  auto Comma = Factory.makeCommaToken("", " ");

  return Factory.makeFunctionParameter(
      /*GarbageNodes=*/None, None, /*GarbageNodes=*/None, ExternalName,
      /*GarbageNodes=*/None, LocalName, /*GarbageNodes=*/None, Colon,
      /*GarbageNodes=*/None, Int, /*GarbageNodes=*/None, NoEllipsis,
      /*GarbageNodes=*/None, DefaultArg, /*GarbageNodes=*/None, Comma);
}

TEST(DeclSyntaxTests, FunctionParameterMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "with radius: Int = -1, ");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankFunctionParameter().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

TEST(DeclSyntaxTests, FunctionParameterGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto ExternalName = Factory.makeIdentifier("with", "", " ");
  auto LocalName = Factory.makeIdentifier("radius", "", "");
  auto Colon = Factory.makeColonToken("", " ");
  auto Int = Factory.makeTypeIdentifier("Int", "", " ");
  auto NoEllipsis = TokenSyntax::missingToken(tok::identifier, "...", Arena);
  auto Equal = Factory.makeEqualToken("", " ");

  auto Sign = Factory.makePrefixOperator("-", "", "");
  auto OneDigits = Factory.makeIntegerLiteral("1", "", "");
  auto OneLiteral =
      Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, OneDigits);
  auto One = Factory.makePrefixOperatorExpr(/*GarbageNodes=*/None, Sign,
                                            /*GarbageNodes=*/None, OneLiteral);
  auto DefaultArg = Factory.makeInitializerClause(/*GarbageNodes=*/None, Equal,
                                                  /*GarbageNodes=*/None, One);
  auto Comma = Factory.makeCommaToken("", "");

  auto Param = Factory.makeFunctionParameter(
      /*GarbageNodes=*/None, None, /*GarbageNodes=*/None, ExternalName,
      /*GarbageNodes=*/None, LocalName, /*GarbageNodes=*/None, Colon,
      /*GarbageNodes=*/None, Int, /*GarbageNodes=*/None, NoEllipsis,
      /*GarbageNodes=*/None, DefaultArg, /*GarbageNodes=*/None, Comma);

  ASSERT_EQ(ExternalName.getRaw(), Param.getFirstName()->getRaw());
  ASSERT_EQ(LocalName.getRaw(), Param.getSecondName()->getRaw());
  ASSERT_EQ(Colon.getRaw(), Param.getColon()->getRaw());

  auto GottenType = Param.getType();
  auto GottenType2 = Param.getType();
  ASSERT_TRUE(GottenType->hasSameIdentityAs(*GottenType2));

  ASSERT_EQ(DefaultArg.getRaw(), Param.getDefaultArgument()->getRaw());

  auto GottenDefaultValue = Param.getDefaultArgument()->getValue();
  auto GottenDefaultValue2 = Param.getDefaultArgument()->getValue();
  ASSERT_TRUE(GottenDefaultValue.hasSameIdentityAs(GottenDefaultValue2));

  ASSERT_EQ(Comma.getRaw(), Param.getTrailingComma()->getRaw());

  // Test that llvm::None is returned for non-token missing children:
  auto Decimated = Param
    .withType(llvm::None)
    .withDefaultArgument(llvm::None);

  ASSERT_FALSE(Decimated.getType().hasValue());
  ASSERT_FALSE(Decimated.getDefaultArgument().hasValue());
}

TEST(DeclSyntaxTests, FunctionParameterWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto ExternalName = Factory.makeIdentifier("for", "", " ");
  auto LocalName = Factory.makeIdentifier("integer", "", "");
  auto Colon = Factory.makeColonToken(" ", " ");
  auto Int = Factory.makeTypeIdentifier("Int", "", " ");
  auto Equal = Factory.makeEqualToken("", " ");

  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "", Arena);
  auto OneDigits = Factory.makeIntegerLiteral("1", "", "");
  auto One = Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, OneDigits);
  auto DefaultArg = Factory.makeInitializerClause(/*GarbageNodes=*/None, Equal,
                                                  /*GarbageNodes=*/None, One);
  auto Comma = Factory.makeCommaToken("", "");

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter(Arena)
      .withFirstName(ExternalName)
      .withSecondName(LocalName)
      .withColon(Colon)
      .withType(Int)
      .withDefaultArgument(DefaultArg)
      .withTrailingComma(Comma)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "for integer : Int = 1,");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter(Arena)
      .withType(llvm::None)
      .withDefaultArgument(llvm::None)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "with radius: , ");
  }
}

TEST(DeclSyntaxTests, FunctionParameterWithEllipsis) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto ExternalName = Factory.makeIdentifier("for", "", " ");
  auto LocalName = Factory.makeIdentifier("integer", "", "");
  auto Colon = Factory.makeColonToken(" ", " ");
  auto Int = Factory.makeTypeIdentifier("Int", "", "");
  auto Ellipsis = Factory.makeEllipsisToken("", " ");
  auto Comma = Factory.makeCommaToken("", "");

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter(Arena)
        .withFirstName(ExternalName)
        .withSecondName(LocalName)
        .withColon(Colon)
        .withType(Int)
        .withEllipsis(Ellipsis)
        .withDefaultArgument(llvm::None)
        .withTrailingComma(Comma)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "for integer : Int... ,");
  }
}

#pragma mark - parameter-list

TEST(DeclSyntaxTests, FunctionParameterListMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankFunctionParameterList().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Param = getCannedFunctionParameter(Arena);
    std::vector<FunctionParameterSyntax> Params { Param, Param, Param };
    Factory.makeFunctionParameterList(Params).print(OS);
    ASSERT_EQ(OS.str().str(),
      "with radius: Int = -1, with radius: Int = -1, with radius: Int = -1, ");
  }
}

#pragma mark - function-signature

FunctionSignatureSyntax getCannedFunctionSignature(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  auto LParen = Factory.makeLeftParenToken("", "");
  auto Param = getCannedFunctionParameter(Arena);
  auto List = Factory.makeBlankFunctionParameterList()
                  .appending(Param)
                  .appending(Param)
                  .appending(Param)
                  .castTo<FunctionParameterListSyntax>();
  auto RParen = Factory.makeRightParenToken("", " ");
  auto Parameter = Factory.makeParameterClause(/*GarbageNodes=*/None, LParen,
                                               /*GarbageNodes=*/None, List,
                                               /*GarbageNodes=*/None, RParen);
  auto Throws = Factory.makeThrowsKeyword("", " ");
  auto Arrow = Factory.makeArrowToken("", " ");
  auto Int = Factory.makeTypeIdentifier("Int", "", " ");
  auto Return = Factory.makeReturnClause(/*GarbageNodes=*/None, Arrow,
                                         /*GarbageNodes=*/None, Int);

  return Factory.makeFunctionSignature(
      /*GarbageNodes=*/None, Parameter, /*GarbageNodes=*/None, None,
      /*GarbageNodes=*/None, Throws, /*GarbageNodes=*/None, Return);
}

TEST(DeclSyntaxTests, FunctionSignatureMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankFunctionSignature().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionSignature(Arena).print(OS);
    ASSERT_EQ(OS.str().str(),
      "(with radius: Int = -1, "
      "with radius: Int = -1, "
      "with radius: Int = -1, ) throws -> Int ");
  }
}

TEST(DeclSyntaxTests, FunctionSignatureGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto LParen = Factory.makeLeftParenToken("", "");
  auto Param = getCannedFunctionParameter(Arena);
  auto List = Factory.makeBlankFunctionParameterList()
                  .appending(Param)
                  .appending(Param)
                  .appending(Param)
                  .castTo<FunctionParameterListSyntax>();
  auto RParen = Factory.makeRightParenToken("", " ");
  auto Throws = Factory.makeThrowsKeyword("", " ");
  auto Arrow = Factory.makeArrowToken("", " ");

  auto Int = Factory.makeTypeIdentifier("Int", "", "");

  auto ParamClause = Factory.makeParameterClause(/*GarbageNodes=*/None, LParen,
                                                 /*GarbageNodes=*/None, List,
                                                 /*GarbageNodes=*/None, RParen);
  auto ReturnClause = Factory.makeReturnClause(/*GarbageNodes=*/None, Arrow,
                                               /*GarbageNodes=*/None, Int);
  auto Sig = Factory.makeFunctionSignature(
      /*GarbageNodes=*/None, ParamClause, /*GarbageNodes=*/None, None,
      /*GarbageNodes=*/None, Throws, /*GarbageNodes=*/None, ReturnClause);

  ASSERT_EQ(LParen.getRaw(), Sig.getInput().getLeftParen().getRaw());

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenList1 = Sig.getInput().getParameterList();
    auto GottenList2 = Sig.getInput().getParameterList();
    ASSERT_TRUE(GottenList1.hasSameIdentityAs(GottenList2));
    GottenList1.print(OS);
    ASSERT_EQ(OS.str().str(),
              "with radius: Int = -1, "
              "with radius: Int = -1, "
              "with radius: Int = -1, ");
  }

  ASSERT_EQ(RParen.getRaw(), Sig.getInput().getRightParen().getRaw());
  ASSERT_EQ(Throws.getRaw(), Sig.getThrowsOrRethrowsKeyword()->getRaw());
  ASSERT_EQ(Sig.getThrowsOrRethrowsKeyword()->getTokenKind(), tok::kw_throws);
  ASSERT_EQ(Arrow.getRaw(), Sig.getOutput()->getArrow().getRaw());

  {
    SmallString<3> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenReturnType1 = Sig.getOutput()->getReturnType();
    auto GottenReturnType2 = Sig.getOutput()->getReturnType();
    ASSERT_TRUE(GottenReturnType1.hasSameIdentityAs(GottenReturnType2));
    GottenReturnType1.print(OS);
    ASSERT_EQ(OS.str().str(), "Int");
  }
}

TEST(DeclSyntaxTests, FunctionSignatureWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto LParen = Factory.makeLeftParenToken("", "");
  auto Param = getCannedFunctionParameter(Arena);
  auto List = Factory.makeBlankFunctionParameterList()
                  .appending(Param)
                  .appending(Param)
                  .appending(Param)
                  .castTo<FunctionParameterListSyntax>();
  auto RParen = Factory.makeRightParenToken("", " ");
  auto Throws = Factory.makeThrowsKeyword("", " ");
  auto Arrow = Factory.makeArrowToken("", " ");
  auto Int = Factory.makeTypeIdentifier("Int", "", "");

  auto Parameter = Factory.makeParameterClause(/*GarbageNodes=*/None, LParen,
                                               /*GarbageNodes=*/None, List,
                                               /*GarbageNodes=*/None, RParen);
  auto Return = Factory.makeReturnClause(/*GarbageNodes=*/None, Arrow,
                                         /*GarbageNodes=*/None, Int);
  SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  Factory.makeBlankFunctionSignature()
      .withInput(Parameter)
      .withThrowsOrRethrowsKeyword(Throws)
      .withOutput(Return)
      .print(OS);
  ASSERT_EQ(OS.str().str(),
            "(with radius: Int = -1, "
            "with radius: Int = -1, "
            "with radius: Int = -1, ) throws -> Int");
}

#pragma mark - function-declaration

ModifierListSyntax getCannedModifiers(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  auto PublicID = Factory.makePublicKeyword("", " ");
  auto NoLParen = TokenSyntax::missingToken(tok::l_paren, "(", Arena);
  auto NoArgument = TokenSyntax::missingToken(tok::identifier, "", Arena);
  auto NoRParen = TokenSyntax::missingToken(tok::r_paren, ")", Arena);
  auto PublicDeclModifierDetail = Factory.makeDeclModifierDetail(
      /*GarbageNodes=*/None, NoLParen, /*GarbageNodes=*/None, NoArgument,
      /*GarbageNodes=*/None, NoRParen);
  auto Public = Factory.makeDeclModifier(
      /*GarbageNodes=*/None, PublicID,
      /*GarbageNodes=*/None, PublicDeclModifierDetail);

  auto StaticKW = Factory.makeStaticKeyword("", " ");
  auto StaticDeclModifierDetail = Factory.makeDeclModifierDetail(
      /*GarbageNodes=*/None, NoLParen, /*GarbageNodes=*/None, NoArgument,
      /*GarbageNodes=*/None, NoRParen);
  auto Static =
      Factory.makeDeclModifier(/*GarbageNodes=*/None, StaticKW,
                               /*GarbageNodes=*/None, StaticDeclModifierDetail);

  return Factory.makeBlankModifierList().appending(Public).appending(Static);
}

GenericParameterClauseSyntax getCannedGenericParams(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  GenericParameterClauseSyntaxBuilder GB(Arena);

  auto LAngle = Factory.makeLeftAngleToken("", "");
  auto RAngle = Factory.makeRightAngleToken("", "");
  auto TType = Factory.makeIdentifier("T", "", "");
  auto UType = Factory.makeIdentifier("U", "", "");

  auto Comma = Factory.makeCommaToken("", " ");
  auto T = Factory.makeGenericParameter(TType, Comma);
  auto U = Factory.makeGenericParameter(UType, None);

  GB.addGenericParameter(T);
  GB.addGenericParameter(U);
  GB.useLeftAngleBracket(LAngle);
  GB.useRightAngleBracket(RAngle);

  return GB.build();
}

CodeBlockSyntax getCannedBody(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "-", Arena);
  auto OneDigits = Factory.makeIntegerLiteral("1", "", "");
  auto One = Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, OneDigits);
  auto ReturnKW = Factory.makeReturnKeyword("\n  ", "");
  auto Return = Factory.makeReturnStmt(/*GarbageNodes=*/None, ReturnKW,
                                       /*GarbageNodes=*/None, One);
  auto ReturnItem = Factory.makeCodeBlockItem(/*GarbageNodes=*/None, Return,
                                              /*GarbageNodes=*/None, None,
                                              /*GarbageNodes=*/None, None);

  auto Stmts = Factory.makeCodeBlockItemList({ReturnItem});

  auto LBrace = Factory.makeLeftBraceToken("", "");
  auto RBrace = Factory.makeRightBraceToken("\n", "");

  return Factory.makeCodeBlock(/*GarbageNodes=*/None, LBrace,
                               /*GarbageNodes=*/None, Stmts,
                               /*GarbageNodes=*/None, RBrace);
}

GenericWhereClauseSyntax getCannedWhereClause(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  auto WhereKW = Factory.makeWhereKeyword("", " ");
  auto T = Factory.makeTypeIdentifier("T", "", " ");
  auto EqualEqual = Factory.makeEqualityOperator("", " ");
  auto Int = Factory.makeTypeIdentifier("Int", "", " ");
  auto SameType = Factory.makeSameTypeRequirement(
      /*GarbageNodes=*/None, T, /*GarbageNodes=*/None, EqualEqual,
      /*GarbageNodes=*/None, Int);
  auto Req = Factory.makeGenericRequirement(/*GarbageNodes=*/None, SameType,
                                            /*GarbageNodes=*/None, None);

  auto Requirements = Factory.makeBlankGenericRequirementList().appending(Req);

  return Factory.makeBlankGenericWhereClause()
      .withWhereKeyword(WhereKW)
      .withRequirementList(Requirements);
}

FunctionDeclSyntax getCannedFunctionDecl(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  auto NoAttributes = Factory.makeBlankAttributeList();
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto FuncKW = Factory.makeFuncKeyword("", " ");
  auto Modifiers = getCannedModifiers(Arena);
  auto GenericParams = getCannedGenericParams(Arena);
  auto GenericWhere = getCannedWhereClause(Arena);
  auto Signature = getCannedFunctionSignature(Arena);
  auto Body = getCannedBody(Arena);

  return Factory.makeFunctionDecl(
      /*GarbageNodes=*/None, NoAttributes, /*GarbageNodes=*/None, Modifiers,
      /*GarbageNodes=*/None, FuncKW, /*GarbageNodes=*/None, Foo,
      /*GarbageNodes=*/None, GenericParams, /*GarbageNodes=*/None, Signature,
      /*GarbageNodes=*/None, GenericWhere, /*GarbageNodes=*/None, Body);
}

TEST(DeclSyntaxTests, FunctionDeclMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankFunctionDecl().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionDecl(Arena).print(OS);
    ASSERT_EQ(OS.str().str(),
              "public static func foo<T, U>"
              "(with radius: Int = -1, "
              "with radius: Int = -1, "
              "with radius: Int = -1, ) "
              "throws -> Int "
              "where T == Int {\n"
              "  return1\n"
              "}");
  }
}

TEST(DeclSyntaxTests, FunctionDeclGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankFunctionDecl().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {

  }
}

TEST(DeclSyntaxTests, FunctionDeclWithAPIs) {

}

TEST(DeclSyntaxTests, FunctionDeclBuilderAPIs) {

}

#pragma mark - parameterized protocol-decl

TEST(DeclSyntaxTests, ProtocolMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankProtocolDecl().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Protocol = Factory.makeProtocolKeyword("", " ");
    auto MyCollection = Factory.makeIdentifier("MyCollection", "", "");
    auto ElementName = Factory.makeIdentifier("Element", "", "");
    auto ElementParam = Factory.makePrimaryAssociatedType(
        /*GarbageNodes=*/None, ElementName, /*GarbageNodes=*/None, None);
    auto LeftAngle = Factory.makeLeftAngleToken("", "");
    auto RightAngle = Factory.makeRightAngleToken("", " ");
    auto PrimaryAssocs = PrimaryAssociatedTypeClauseSyntaxBuilder(Arena)
                             .useLeftAngleBracket(LeftAngle)
                             .useRightAngleBracket(RightAngle)
                             .addPrimaryAssociatedType(ElementParam)
                             .build();

    auto LeftBrace = Factory.makeLeftBraceToken("", "");
    auto RightBrace = Factory.makeRightBraceToken("", "");
    auto Members = MemberDeclBlockSyntaxBuilder(Arena)
                      .useLeftBrace(LeftBrace)
                      .useRightBrace(RightBrace)
                      .build();
    Factory
        .makeProtocolDecl(/*GarbageNodes=*/None, None, /*GarbageNodes=*/None,
                          None, /*GarbageNodes=*/None, Protocol,
                          /*GarbageNodes=*/None, MyCollection,
                          /*GarbageNodes=*/None, PrimaryAssocs,
                          /*GarbageNodes=*/None, None, /*GarbageNodes=*/None,
                          None, /*GarbageNodes=*/None, Members)
        .print(OS);
    ASSERT_EQ(OS.str().str(),
              "protocol MyCollection<Element> {}");
  }
}
