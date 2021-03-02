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

DeclModifierSyntax getCannedDeclModifier() {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Private = SyntaxFactory::makeIdentifier("private", "", "", Arena);
  auto LParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto Set = SyntaxFactory::makeIdentifier("set", "", "", Arena);
  auto RParen = SyntaxFactory::makeRightParenToken("", "", Arena);
  return SyntaxFactory::makeDeclModifier(Private, LParen, Set, RParen, Arena);
}

TEST(DeclSyntaxTests, DeclModifierMakeAPIs) {
  {
    RC<SyntaxArena> Arena = SyntaxArena::make();
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankDeclModifier(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<24> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedDeclModifier().print(OS);
    ASSERT_EQ(OS.str().str(), "private(set)");
  }
}

TEST(DeclSyntaxTests, DeclModifierGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Private = SyntaxFactory::makeIdentifier("private", "", "", Arena);
  auto LParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto Set = SyntaxFactory::makeIdentifier("set", "", "", Arena);
  auto RParen = SyntaxFactory::makeRightParenToken("", "", Arena);
  auto Mod =
      SyntaxFactory::makeDeclModifier(Private, LParen, Set, RParen, Arena);

  ASSERT_EQ(Private.getRaw(), Mod.getName().getRaw());
  ASSERT_EQ(LParen.getRaw(), Mod.getDetailLeftParen()->getRaw());
  ASSERT_EQ(Set.getRaw(), Mod.getDetail()->getRaw());
  ASSERT_EQ(RParen.getRaw(), Mod.getDetailRightParen()->getRaw());
}

TEST(DeclSyntaxTests, DeclModifierWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Private = SyntaxFactory::makeIdentifier("private", "", "", Arena);
  auto LParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto Set = SyntaxFactory::makeIdentifier("set", "", "", Arena);
  auto RParen = SyntaxFactory::makeRightParenToken("", "", Arena);

  SmallString<24> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  SyntaxFactory::makeBlankDeclModifier(Arena)
      .withName(Private)
      .withDetailLeftParen(LParen)
      .withDetail(Set)
      .withDetailRightParen(RParen)
      .print(OS);
  ASSERT_EQ(OS.str().str(), "private(set)");
}

#pragma mark - typealias-decl

TEST(DeclSyntaxTests, TypealiasMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTypealiasDecl(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Typealias = SyntaxFactory::makeTypealiasKeyword("", " ", Arena);
    auto Subsequence =
        SyntaxFactory::makeIdentifier("MyCollection", "", "", Arena);
    auto ElementName = SyntaxFactory::makeIdentifier("Element", "", "", Arena);
    auto ElementParam = SyntaxFactory::makeGenericParameter(
        None, ElementName, None, None, None, Arena);
    auto LeftAngle = SyntaxFactory::makeLeftAngleToken("", "", Arena);
    auto RightAngle = SyntaxFactory::makeRightAngleToken("", " ", Arena);
    auto GenericParams = GenericParameterClauseSyntaxBuilder(Arena)
                             .useLeftAngleBracket(LeftAngle)
                             .useRightAngleBracket(RightAngle)
                             .addGenericParameter(ElementParam)
                             .build();
    auto Assignment = SyntaxFactory::makeEqualToken("", " ", Arena);
    auto ElementType =
        SyntaxFactory::makeTypeIdentifier("Element", "", "", Arena);
    auto ElementArg =
        SyntaxFactory::makeGenericArgument(ElementType, None, Arena);

    auto GenericArgs =
        GenericArgumentClauseSyntaxBuilder(Arena)
            .useLeftAngleBracket(LeftAngle)
            .useRightAngleBracket(
                SyntaxFactory::makeRightAngleToken("", "", Arena))
            .addArgument(ElementArg)
            .build();

    auto Array = SyntaxFactory::makeIdentifier("Array", "", "", Arena);
    auto Array_Int =
        SyntaxFactory::makeSimpleTypeIdentifier(Array, GenericArgs, Arena);
    auto TypeInit =
        SyntaxFactory::makeTypeInitializerClause(Assignment, Array_Int, Arena);
    SyntaxFactory::makeTypealiasDecl(None, None, Typealias, Subsequence,
                                     GenericParams, TypeInit, None, Arena)
        .print(OS);
    ASSERT_EQ(OS.str().str(),
              "typealias MyCollection<Element> = Array<Element>");
  }
}

TEST(DeclSyntaxTests, TypealiasWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Typealias = SyntaxFactory::makeTypealiasKeyword("", " ", Arena);
  auto MyCollection =
      SyntaxFactory::makeIdentifier("MyCollection", "", "", Arena);
  auto ElementName = SyntaxFactory::makeIdentifier("Element", "", "", Arena);
  auto ElementParam = SyntaxFactory::makeGenericParameter(
      None, ElementName, None, None, None, Arena);
  auto LeftAngle = SyntaxFactory::makeLeftAngleToken("", "", Arena);
  auto RightAngle = SyntaxFactory::makeRightAngleToken("", " ", Arena);
  auto GenericParams = GenericParameterClauseSyntaxBuilder(Arena)
                           .useLeftAngleBracket(LeftAngle)
                           .useRightAngleBracket(RightAngle)
                           .addGenericParameter(ElementParam)
                           .build();
  auto Equal = SyntaxFactory::makeEqualToken("", " ", Arena);

  auto ElementType =
      SyntaxFactory::makeTypeIdentifier("Element", "", "", Arena);
  auto ElementArg =
      SyntaxFactory::makeGenericArgument(ElementType, None, Arena);
  auto GenericArgs = GenericArgumentClauseSyntaxBuilder(Arena)
                         .useLeftAngleBracket(LeftAngle)
                         .useRightAngleBracket(
                             SyntaxFactory::makeRightAngleToken("", "", Arena))
                         .addArgument(ElementArg)
                         .build();

  auto Array = SyntaxFactory::makeIdentifier("Array", "", "", Arena);
  auto Array_Int =
      SyntaxFactory::makeSimpleTypeIdentifier(Array, GenericArgs, Arena);
  auto Type_Init =
      SyntaxFactory::makeTypeInitializerClause(Equal, Array_Int, Arena);
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTypealiasDecl(Arena)
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
  SmallString<64> Scratch;
  llvm::raw_svector_ostream OS(Scratch);

  auto Typealias = SyntaxFactory::makeTypealiasKeyword("", " ", Arena);
  auto MyCollection =
      SyntaxFactory::makeIdentifier("MyCollection", "", "", Arena);
  auto ElementName = SyntaxFactory::makeIdentifier("Element", "", "", Arena);
  auto ElementParam =
      SyntaxFactory::makeGenericParameter(ElementName, None, Arena);
  auto LeftAngle = SyntaxFactory::makeLeftAngleToken("", "", Arena);
  auto RightAngle = SyntaxFactory::makeRightAngleToken("", " ", Arena);
  auto GenericParams = GenericParameterClauseSyntaxBuilder(Arena)
                           .useLeftAngleBracket(LeftAngle)
                           .useRightAngleBracket(RightAngle)
                           .addGenericParameter(ElementParam)
                           .build();
  auto Equal = SyntaxFactory::makeEqualToken("", " ", Arena);

  auto ElementType =
      SyntaxFactory::makeTypeIdentifier("Element", "", "", Arena);
  auto ElementArg =
      SyntaxFactory::makeGenericArgument(ElementType, None, Arena);

  auto GenericArgs = GenericArgumentClauseSyntaxBuilder(Arena)
                         .useLeftAngleBracket(LeftAngle)
                         .useRightAngleBracket(
                             SyntaxFactory::makeRightAngleToken("", "", Arena))
                         .addArgument(ElementArg)
                         .build();

  auto Array = SyntaxFactory::makeIdentifier("Array", "", "", Arena);
  auto Array_Int =
      SyntaxFactory::makeSimpleTypeIdentifier(Array, GenericArgs, Arena);
  auto Type_Init =
      SyntaxFactory::makeTypeInitializerClause(Equal, Array_Int, Arena);
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

FunctionParameterSyntax getCannedFunctionParameter() {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto ExternalName = SyntaxFactory::makeIdentifier("with", "", " ", Arena);
  auto LocalName = SyntaxFactory::makeIdentifier("radius", "", "", Arena);
  auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", " ", Arena);
  auto NoEllipsis = TokenSyntax::missingToken(tok::identifier, "...", Arena);
  auto Equal = SyntaxFactory::makeEqualToken("", " ", Arena);

  auto Sign = SyntaxFactory::makePrefixOperator("-", "", "", Arena);
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", "", "", Arena);
  auto One = SyntaxFactory::makePrefixOperatorExpr(
      Sign, SyntaxFactory::makeIntegerLiteralExpr(OneDigits, Arena), Arena);
  auto DefaultArg = SyntaxFactory::makeInitializerClause(Equal, One, Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);

  return SyntaxFactory::makeFunctionParameter(None, ExternalName, LocalName,
                                              Colon, Int, NoEllipsis,
                                              DefaultArg, Comma, Arena);
}

TEST(DeclSyntaxTests, FunctionParameterMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter().print(OS);
    ASSERT_EQ(OS.str().str(), "with radius: Int = -1, ");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionParameter(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

TEST(DeclSyntaxTests, FunctionParameterGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto ExternalName = SyntaxFactory::makeIdentifier("with", "", " ", Arena);
  auto LocalName = SyntaxFactory::makeIdentifier("radius", "", "", Arena);
  auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", " ", Arena);
  auto NoEllipsis = TokenSyntax::missingToken(tok::identifier, "...", Arena);
  auto Equal = SyntaxFactory::makeEqualToken("", " ", Arena);

  auto Sign = SyntaxFactory::makePrefixOperator("-", "", "", Arena);
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", "", "", Arena);
  auto One = SyntaxFactory::makePrefixOperatorExpr(
      Sign, SyntaxFactory::makeIntegerLiteralExpr(OneDigits, Arena), Arena);
  auto DefaultArg = SyntaxFactory::makeInitializerClause(Equal, One, Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", "", Arena);

  auto Param = SyntaxFactory::makeFunctionParameter(
      None, ExternalName, LocalName, Colon, Int, NoEllipsis, DefaultArg, Comma,
      Arena);

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
  auto ExternalName = SyntaxFactory::makeIdentifier("for", "", " ", Arena);
  auto LocalName = SyntaxFactory::makeIdentifier("integer", "", "", Arena);
  auto Colon = SyntaxFactory::makeColonToken(" ", " ", Arena);
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", " ", Arena);
  auto Equal = SyntaxFactory::makeEqualToken("", " ", Arena);

  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "", Arena);
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", "", "", Arena);
  auto One = SyntaxFactory::makeIntegerLiteralExpr(OneDigits, Arena);
  auto DefaultArg = SyntaxFactory::makeInitializerClause(Equal, One, Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", "", Arena);

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter()
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
    getCannedFunctionParameter()
      .withType(llvm::None)
      .withDefaultArgument(llvm::None)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "with radius: , ");
  }
}

TEST(DeclSyntaxTests, FunctionParameterWithEllipsis) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto ExternalName = SyntaxFactory::makeIdentifier("for", "", " ", Arena);
  auto LocalName = SyntaxFactory::makeIdentifier("integer", "", "", Arena);
  auto Colon = SyntaxFactory::makeColonToken(" ", " ", Arena);
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);
  auto Ellipsis = SyntaxFactory::makeEllipsisToken("", " ", Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", "", Arena);

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter()
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
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionParameterList(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Param = getCannedFunctionParameter();
    std::vector<FunctionParameterSyntax> Params { Param, Param, Param };
    SyntaxFactory::makeFunctionParameterList(Params, Arena).print(OS);
    ASSERT_EQ(OS.str().str(),
      "with radius: Int = -1, with radius: Int = -1, with radius: Int = -1, ");
  }
}

#pragma mark - function-signature

FunctionSignatureSyntax getCannedFunctionSignature() {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto LParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto Param = getCannedFunctionParameter();
  auto List = SyntaxFactory::makeBlankFunctionParameterList(Arena)
                  .appending(Param)
                  .appending(Param)
                  .appending(Param)
                  .castTo<FunctionParameterListSyntax>();
  auto RParen = SyntaxFactory::makeRightParenToken("", " ", Arena);
  auto Parameter =
      SyntaxFactory::makeParameterClause(LParen, List, RParen, Arena);
  auto Throws = SyntaxFactory::makeThrowsKeyword("", " ", Arena);
  auto Arrow = SyntaxFactory::makeArrowToken("", " ", Arena);
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", " ", Arena);
  auto Return = SyntaxFactory::makeReturnClause(Arrow, Int, Arena);

  return SyntaxFactory::makeFunctionSignature(Parameter, None, Throws, Return,
                                              Arena);
}

TEST(DeclSyntaxTests, FunctionSignatureMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionSignature(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionSignature().print(OS);
    ASSERT_EQ(OS.str().str(),
      "(with radius: Int = -1, "
      "with radius: Int = -1, "
      "with radius: Int = -1, ) throws -> Int ");
  }
}

TEST(DeclSyntaxTests, FunctionSignatureGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto LParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto Param = getCannedFunctionParameter();
  auto List = SyntaxFactory::makeBlankFunctionParameterList(Arena)
                  .appending(Param)
                  .appending(Param)
                  .appending(Param)
                  .castTo<FunctionParameterListSyntax>();
  auto RParen = SyntaxFactory::makeRightParenToken("", " ", Arena);
  auto Throws = SyntaxFactory::makeThrowsKeyword("", " ", Arena);
  auto Arrow = SyntaxFactory::makeArrowToken("", " ", Arena);

  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);

  auto Sig = SyntaxFactory::makeFunctionSignature(
      SyntaxFactory::makeParameterClause(LParen, List, RParen, Arena), None,
      Throws, SyntaxFactory::makeReturnClause(Arrow, Int, Arena), Arena);

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
  auto LParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto Param = getCannedFunctionParameter();
  auto List = SyntaxFactory::makeBlankFunctionParameterList(Arena)
                  .appending(Param)
                  .appending(Param)
                  .appending(Param)
                  .castTo<FunctionParameterListSyntax>();
  auto RParen = SyntaxFactory::makeRightParenToken("", " ", Arena);
  auto Throws = SyntaxFactory::makeThrowsKeyword("", " ", Arena);
  auto Arrow = SyntaxFactory::makeArrowToken("", " ", Arena);
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);

  auto Parameter =
      SyntaxFactory::makeParameterClause(LParen, List, RParen, Arena);
  auto Return = SyntaxFactory::makeReturnClause(Arrow, Int, Arena);
  SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  SyntaxFactory::makeBlankFunctionSignature(Arena)
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

ModifierListSyntax getCannedModifiers() {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto PublicID = SyntaxFactory::makePublicKeyword("", " ", Arena);
  auto NoLParen = TokenSyntax::missingToken(tok::l_paren, "(", Arena);
  auto NoArgument = TokenSyntax::missingToken(tok::identifier, "", Arena);
  auto NoRParen = TokenSyntax::missingToken(tok::r_paren, ")", Arena);
  auto Public = SyntaxFactory::makeDeclModifier(PublicID, NoLParen, NoArgument,
                                                NoRParen, Arena);

  auto StaticKW = SyntaxFactory::makeStaticKeyword("", " ", Arena);
  auto Static = SyntaxFactory::makeDeclModifier(StaticKW, NoLParen, NoArgument,
                                                NoRParen, Arena);

  return SyntaxFactory::makeBlankModifierList(Arena)
      .appending(Public)
      .appending(Static);
}

GenericParameterClauseSyntax getCannedGenericParams() {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  GenericParameterClauseSyntaxBuilder GB(Arena);

  auto LAngle = SyntaxFactory::makeLeftAngleToken("", "", Arena);
  auto RAngle = SyntaxFactory::makeRightAngleToken("", "", Arena);
  auto TType = SyntaxFactory::makeIdentifier("T", "", "", Arena);
  auto UType = SyntaxFactory::makeIdentifier("U", "", "", Arena);

  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
  auto T = SyntaxFactory::makeGenericParameter(TType, Comma, Arena);
  auto U = SyntaxFactory::makeGenericParameter(UType, None, Arena);

  GB.addGenericParameter(T);
  GB.addGenericParameter(U);
  GB.useLeftAngleBracket(LAngle);
  GB.useRightAngleBracket(RAngle);

  return GB.build();
}

CodeBlockSyntax getCannedBody() {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "-", Arena);
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", "", "", Arena);
  auto One = SyntaxFactory::makeIntegerLiteralExpr(OneDigits, Arena);
  auto ReturnKW = SyntaxFactory::makeReturnKeyword("\n  ", "", Arena);
  auto Return = SyntaxFactory::makeReturnStmt(ReturnKW, One, Arena);
  auto ReturnItem = SyntaxFactory::makeCodeBlockItem(Return, None, None, Arena);

  auto Stmts = SyntaxFactory::makeCodeBlockItemList({ReturnItem}, Arena);

  auto LBrace = SyntaxFactory::makeLeftBraceToken("", "", Arena);
  auto RBrace = SyntaxFactory::makeRightBraceToken("\n", "", Arena);

  return SyntaxFactory::makeCodeBlock(LBrace, Stmts, RBrace, Arena);
}

GenericWhereClauseSyntax getCannedWhereClause() {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto WhereKW = SyntaxFactory::makeWhereKeyword("", " ", Arena);
  auto T = SyntaxFactory::makeTypeIdentifier("T", "", " ", Arena);
  auto EqualEqual = SyntaxFactory::makeEqualityOperator("", " ", Arena);
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", " ", Arena);
  auto SameType =
      SyntaxFactory::makeSameTypeRequirement(T, EqualEqual, Int, Arena);
  auto Req = SyntaxFactory::makeGenericRequirement(SameType, None, Arena);

  auto Requirements =
      SyntaxFactory::makeBlankGenericRequirementList(Arena).appending(Req);

  return SyntaxFactory::makeBlankGenericWhereClause(Arena)
      .withWhereKeyword(WhereKW)
      .withRequirementList(Requirements);
}

FunctionDeclSyntax getCannedFunctionDecl() {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto NoAttributes = SyntaxFactory::makeBlankAttributeList(Arena);
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto FuncKW = SyntaxFactory::makeFuncKeyword("", " ", Arena);
  auto Modifiers = getCannedModifiers();
  auto GenericParams = getCannedGenericParams();
  auto GenericWhere = getCannedWhereClause();
  auto Signature = getCannedFunctionSignature();
  auto Body = getCannedBody();

  return SyntaxFactory::makeFunctionDecl(NoAttributes, Modifiers, FuncKW, Foo,
                                         GenericParams, Signature, GenericWhere,
                                         Body, Arena);
}

TEST(DeclSyntaxTests, FunctionDeclMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionDecl(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionDecl().print(OS);
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
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionDecl(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {

  }
}

TEST(DeclSyntaxTests, FunctionDeclWithAPIs) {

}

TEST(DeclSyntaxTests, FunctionDeclBuilderAPIs) {

}
