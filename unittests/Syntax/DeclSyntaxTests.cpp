#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/DeclSyntax.h"
#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/StmtSyntax.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using llvm::None;
using llvm::SmallString;

using namespace swift;
using namespace swift::syntax;

#pragma mark - declaration-modifier

DeclModifierSyntax getCannedDeclModifier() {
  auto Private = SyntaxFactory::makeIdentifier("private", {}, {});
  auto LParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto Set = SyntaxFactory::makeIdentifier("set", {}, {});
  auto RParen = SyntaxFactory::makeRightParenToken({}, {});
  return SyntaxFactory::makeDeclModifier(Private, LParen, Set, RParen);
}

TEST(DeclSyntaxTests, DeclModifierMakeAPIs) {
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankDeclModifier().print(OS);
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
  auto Private = SyntaxFactory::makeIdentifier("private", {}, {});
  auto LParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto Set = SyntaxFactory::makeIdentifier("set", {}, {});
  auto RParen = SyntaxFactory::makeRightParenToken({}, {});
  auto Mod = SyntaxFactory::makeDeclModifier(Private, LParen, Set, RParen);

  ASSERT_EQ(Private, Mod.getName());
  ASSERT_EQ(LParen, Mod.getLeftParenToken());
  ASSERT_EQ(Set, Mod.getArgument());
  ASSERT_EQ(RParen, Mod.getRightParenToken());
}

TEST(DeclSyntaxTests, DeclModifierWithAPIs) {
  auto Private = SyntaxFactory::makeIdentifier("private", {}, {});
  auto LParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto Set = SyntaxFactory::makeIdentifier("set", {}, {});
  auto RParen = SyntaxFactory::makeRightParenToken({}, {});

  SmallString<24> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  SyntaxFactory::makeBlankDeclModifier()
    .withName(Private)
    .withLeftParenToken(LParen)
    .withArgument(Set)
    .withRightParenToken(RParen)
    .print(OS);
  ASSERT_EQ(OS.str().str(), "private(set)");
}

#pragma mark - typealias-decl

TEST(DeclSyntaxTests, TypealiasMakeAPIs) {
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTypealiasDecl().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Typealias =
      SyntaxFactory::makeTypealiasKeyword({}, { Trivia::spaces(1) });
    auto Subsequence = SyntaxFactory::makeIdentifier("MyCollection", {}, {});
    auto ElementParam =
      SyntaxFactory::makeGenericParameter("Element", {}, {});
    auto LeftAngle = SyntaxFactory::makeLeftAngleToken({}, {});
    auto RightAngle = SyntaxFactory::makeRightAngleToken({}, Trivia::spaces(1));
    auto GenericParams = GenericParameterClauseBuilder()
      .useLeftAngleBracket(LeftAngle)
      .useRightAngleBracket(RightAngle)
      .addParameter(None, ElementParam)
      .build();
    auto Assignment = SyntaxFactory::makeEqualToken({}, { Trivia::spaces(1) });

    auto ElementArg = SyntaxFactory::makeTypeIdentifier("Element", {}, {});

    auto GenericArgs = GenericArgumentClauseBuilder()
      .useLeftAngleBracket(LeftAngle)
      .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
      .addGenericArgument(None, ElementArg)
      .build();

    auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
    auto Array_Int = SyntaxFactory::makeTypeIdentifier(Array, GenericArgs);

    SyntaxFactory::makeTypealiasDecl(Typealias, Subsequence, GenericParams,
                                       Assignment, Array_Int)
      .print(OS);
    ASSERT_EQ(OS.str().str(),
              "typealias MyCollection<Element> = Array<Element>");
  }
}

TEST(DeclSyntaxTests, TypealiasWithAPIs) {
  auto Typealias =
    SyntaxFactory::makeTypealiasKeyword({}, { Trivia::spaces(1) });
  auto MyCollection = SyntaxFactory::makeIdentifier("MyCollection", {}, {});
  auto ElementParam =
    SyntaxFactory::makeGenericParameter("Element", {}, {});
  auto LeftAngle = SyntaxFactory::makeLeftAngleToken({}, {});
  auto RightAngle =
    SyntaxFactory::makeRightAngleToken({}, { Trivia::spaces(1) });
  auto GenericParams = GenericParameterClauseBuilder()
    .useLeftAngleBracket(LeftAngle)
    .useRightAngleBracket(RightAngle)
    .addParameter(None, ElementParam)
    .build();
  auto Equal = SyntaxFactory::makeEqualToken({}, { Trivia::spaces(1) });

  auto ElementArg = SyntaxFactory::makeTypeIdentifier("Element", {}, {});

  auto GenericArgs = GenericArgumentClauseBuilder()
    .useLeftAngleBracket(LeftAngle)
    .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
    .addGenericArgument(None, ElementArg)
    .build();

  auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
  auto Array_Int = SyntaxFactory::makeTypeIdentifier(Array, GenericArgs);

  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTypealiasDecl()
      .withTypeAliasKeyword(Typealias)
      .withIdentifier(MyCollection)
      .withGenericParameterClause(GenericParams)
      .withEqualToken(Equal)
      .withTypeSyntax(Array_Int)
      .print(OS);
    ASSERT_EQ(OS.str().str(),
              "typealias MyCollection<Element> = Array<Element>");
  }
}

TEST(DeclSyntaxTests, TypealiasBuilderAPIs) {
  SmallString<64> Scratch;
  llvm::raw_svector_ostream OS(Scratch);

  auto Typealias =
    SyntaxFactory::makeTypealiasKeyword({}, { Trivia::spaces(1) });
  auto MyCollection = SyntaxFactory::makeIdentifier("MyCollection", {}, {});
  auto ElementParam =
    SyntaxFactory::makeGenericParameter("Element", {}, {});
  auto LeftAngle = SyntaxFactory::makeLeftAngleToken({}, {});
  auto RightAngle =
    SyntaxFactory::makeRightAngleToken({}, { Trivia::spaces(1) });
  auto GenericParams = GenericParameterClauseBuilder()
    .useLeftAngleBracket(LeftAngle)
    .useRightAngleBracket(RightAngle)
    .addParameter(None, ElementParam)
    .build();
  auto Equal =
    SyntaxFactory::makeEqualToken({}, { Trivia::spaces(1) });

  auto ElementArg = SyntaxFactory::makeTypeIdentifier("Element", {}, {});

  auto GenericArgs = GenericArgumentClauseBuilder()
    .useLeftAngleBracket(LeftAngle)
    .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
    .addGenericArgument(None, ElementArg)
    .build();

  auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
  auto Array_Int = SyntaxFactory::makeTypeIdentifier(Array, GenericArgs);

  TypeAliasDeclSyntaxBuilder()
    .useTypeAliasKeyword(Typealias)
    .useIdentifier(MyCollection)
    .useGenericParameterClause(GenericParams)
    .useEqualToken(Equal)
    .useType(Array_Int)
    .build()
    .print(OS);
  ASSERT_EQ(OS.str().str(),
            "typealias MyCollection<Element> = Array<Element>");
}

#pragma mark - parameter

FunctionParameterSyntax getCannedFunctionParameter() {
  auto ExternalName = SyntaxFactory::makeIdentifier("with", {},
                                                    Trivia::spaces(1));
  auto LocalName = SyntaxFactory::makeIdentifier("radius", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {},
                                               Trivia::spaces(1));
  auto NoEllipsis = TokenSyntax::missingToken(tok::identifier, "...");
  auto Equal = SyntaxFactory::makeEqualToken({}, Trivia::spaces(1));

  auto Sign = SyntaxFactory::makePrefixOperator("-", {});
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(Sign, OneDigits);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));

  return SyntaxFactory::makeFunctionParameter(ExternalName, LocalName, Colon,
                                              Int, NoEllipsis, Equal, One,
                                              Comma);
}

TEST(DeclSyntaxTests, FunctionParameterMakeAPIs) {
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter().print(OS);
    ASSERT_EQ(OS.str().str(), "with radius: Int = -1, ");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionParameter().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

TEST(DeclSyntaxTests, FunctionParameterGetAPIs) {
  auto ExternalName = SyntaxFactory::makeIdentifier("with", {},
                                                    Trivia::spaces(1));
  auto LocalName = SyntaxFactory::makeIdentifier("radius", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {},
                                                  Trivia::spaces(1));
  auto NoEllipsis = TokenSyntax::missingToken(tok::identifier, "...");
  auto Equal = SyntaxFactory::makeEqualToken({}, Trivia::spaces(1));

  auto Sign = SyntaxFactory::makePrefixOperator("-", {});
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(Sign, OneDigits);
  auto Comma = SyntaxFactory::makeCommaToken({}, {});

  auto Param = SyntaxFactory::makeFunctionParameter(ExternalName, LocalName,
                                                    Colon, Int, NoEllipsis,
                                                    Equal, One, Comma);

  ASSERT_EQ(ExternalName, Param.getExternalName());
  ASSERT_EQ(LocalName, Param.getLocalName());
  ASSERT_EQ(Colon, Param.getColonToken());

  auto GottenType = Param.getTypeSyntax().getValue();
  auto GottenType2 = Param.getTypeSyntax().getValue();
  ASSERT_TRUE(GottenType.hasSameIdentityAs(GottenType2));

  ASSERT_EQ(Equal, Param.getEqualToken());

  auto GottenDefaultValue = Param.getDefaultValue().getValue();
  auto GottenDefaultValue2 = Param.getDefaultValue().getValue();
  ASSERT_TRUE(GottenDefaultValue.hasSameIdentityAs(GottenDefaultValue2));

  ASSERT_EQ(Comma, Param.getTrailingComma());

  // Test that llvm::None is returned for non-token missing children:
  auto Decimated = Param
    .withTypeSyntax(llvm::None)
    .withDefaultValue(llvm::None);

  ASSERT_FALSE(Decimated.getTypeSyntax().hasValue());
  ASSERT_FALSE(Decimated.getDefaultValue().hasValue());
}

TEST(DeclSyntaxTests, FunctionParameterWithAPIs) {
  auto ExternalName = SyntaxFactory::makeIdentifier("for", {},
                                                    Trivia::spaces(1));
  auto LocalName = SyntaxFactory::makeIdentifier("integer", {}, {});
  auto Colon = SyntaxFactory::makeColonToken(Trivia::spaces(1),
                                             Trivia::spaces(1));
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {},
                                                  Trivia::spaces(1));
  auto Equal = SyntaxFactory::makeEqualToken({}, Trivia::spaces(1));

  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "");
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(NoSign, OneDigits);
  auto Comma = SyntaxFactory::makeCommaToken({}, {});

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter()
      .withExternalName(ExternalName)
      .withLocalName(LocalName)
      .withColonToken(Colon)
      .withTypeSyntax(Int)
      .withEqualToken(Equal)
      .withDefaultValue(One)
      .withTrailingComma(Comma)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "for integer : Int = 1,");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter()
      .withTypeSyntax(llvm::None)
      .withDefaultValue(llvm::None)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "with radius: = , ");
  }
}

#pragma mark - parameter-list

TEST(DeclSyntaxTests, FunctionParameterListMakeAPIs) {
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionParameterList().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Param = getCannedFunctionParameter();
    std::vector<FunctionParameterSyntax> Params { Param, Param, Param };
    SyntaxFactory::makeFunctionParameterList(Params).print(OS);
    ASSERT_EQ(OS.str().str(),
      "with radius: Int = -1, with radius: Int = -1, with radius: Int = -1, ");
  }
}

#pragma mark - function-signature

FunctionSignatureSyntax getCannedFunctionSignature() {
  auto LParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto Param = getCannedFunctionParameter();
  auto List = SyntaxFactory::makeBlankFunctionParameterList()
    .appending(Param)
    .appending(Param)
    .appending(Param)
    .castTo<FunctionParameterListSyntax>();
  auto RParen = SyntaxFactory::makeRightParenToken({}, Trivia::spaces(1));
  auto Throws = SyntaxFactory::makeThrowsKeyword({}, Trivia::spaces(1));
  auto Arrow = SyntaxFactory::makeArrow({}, Trivia::spaces(1));
  auto NoAttributes = SyntaxFactory::makeBlankTypeAttributes();
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, Trivia::spaces(1));

  return SyntaxFactory::makeFunctionSignature(LParen, List, RParen, Throws,
                                              Arrow, NoAttributes, Int);
}

TEST(DeclSyntaxTests, FunctionSignatureMakeAPIs) {
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionSignature().print(OS);
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
  auto LParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto Param = getCannedFunctionParameter();
  auto List = SyntaxFactory::makeBlankFunctionParameterList()
    .appending(Param)
    .appending(Param)
    .appending(Param)
    .castTo<FunctionParameterListSyntax>();
  auto RParen = SyntaxFactory::makeRightParenToken({}, Trivia::spaces(1));
  auto Throws = SyntaxFactory::makeThrowsKeyword({}, Trivia::spaces(1));
  auto Arrow = SyntaxFactory::makeArrow({}, Trivia::spaces(1));
  auto NoAttributes = SyntaxFactory::makeBlankTypeAttributes();
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});

  auto Sig = SyntaxFactory::makeFunctionSignature(LParen, List, RParen, Throws,
                                                  Arrow, NoAttributes, Int);

  ASSERT_EQ(LParen, Sig.getLeftParenToken());

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenList1 = Sig.getParameterList();
    auto GottenList2 = Sig.getParameterList();
    ASSERT_TRUE(GottenList1.hasSameIdentityAs(GottenList2));
    GottenList1.print(OS);
    ASSERT_EQ(OS.str().str(),
              "with radius: Int = -1, "
              "with radius: Int = -1, "
              "with radius: Int = -1, ");
  }

  ASSERT_EQ(RParen, Sig.getRightParenToken());
  ASSERT_EQ(Throws, Sig.getThrowsToken());
  ASSERT_TRUE(Sig.getRethrowsToken()->isMissing());
  ASSERT_EQ(Arrow, Sig.getArrowToken());

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenAttrs1 = Sig.getReturnTypeAttributes();
    auto GottenAttrs2 = Sig.getReturnTypeAttributes();
    ASSERT_TRUE(GottenAttrs1.hasSameIdentityAs(GottenAttrs2));
    ASSERT_EQ(OS.str().str(), "");
  }

  {
    SmallString<3> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenReturnType1 = Sig.getReturnTypeSyntax();
    auto GottenReturnType2 = Sig.getReturnTypeSyntax();
    ASSERT_TRUE(GottenReturnType1.hasSameIdentityAs(GottenReturnType2));
    GottenReturnType1.print(OS);
    ASSERT_EQ(OS.str().str(), "Int");
  }
}

TEST(DeclSyntaxTests, FunctionSignatureWithAPIs) {
  auto LParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto Param = getCannedFunctionParameter();
  auto List = SyntaxFactory::makeBlankFunctionParameterList()
    .appending(Param)
    .appending(Param)
    .appending(Param)
    .castTo<FunctionParameterListSyntax>();
  auto RParen = SyntaxFactory::makeRightParenToken({}, Trivia::spaces(1));
  auto Throws = SyntaxFactory::makeThrowsKeyword({}, Trivia::spaces(1));
  auto Arrow = SyntaxFactory::makeArrow({}, Trivia::spaces(1));
  auto NoAttributes = SyntaxFactory::makeBlankTypeAttributes();
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});

  SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  SyntaxFactory::makeBlankFunctionSignature()
    .withLeftParenToken(LParen)
    .withParameterList(List)
    .withRightParenToken(RParen)
    .withThrowsToken(Throws)
    .withReturnTypeAttributes(NoAttributes)
    .withArrowToken(Arrow)
    .withReturnTypeSyntax(Int)
    .print(OS);
  ASSERT_EQ(OS.str().str(),
            "(with radius: Int = -1, "
            "with radius: Int = -1, "
            "with radius: Int = -1, ) throws -> Int");
}

#pragma mark - function-declaration

DeclModifierListSyntax getCannedModifiers() {
  auto PublicID = SyntaxFactory::makePublicKeyword({}, Trivia::spaces(1));
  auto NoLParen = TokenSyntax::missingToken(tok::l_paren, "(");
  auto NoArgument = TokenSyntax::missingToken(tok::identifier, "");
  auto NoRParen = TokenSyntax::missingToken(tok::r_paren, ")");
  auto Public = SyntaxFactory::makeDeclModifier(PublicID, NoLParen, NoArgument,
                                                NoRParen);

  auto StaticKW = SyntaxFactory::makeStaticKeyword({}, Trivia::spaces(1));
  auto Static = SyntaxFactory::makeDeclModifier(StaticKW, NoLParen, NoArgument,
                                                NoRParen);

  return SyntaxFactory::makeBlankDeclModifierList()
    .appending(Public)
    .appending(Static)
    .castTo<DeclModifierListSyntax>();
}

GenericParameterClauseSyntax getCannedGenericParams() {
  GenericParameterClauseBuilder GB;

  auto LAngle = SyntaxFactory::makeLeftAngleToken({}, {});
  auto RAngle = SyntaxFactory::makeRightAngleToken({}, {});

  auto T = SyntaxFactory::makeGenericParameter("T", {}, {});
  auto U = SyntaxFactory::makeGenericParameter("U", {}, {});

  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));

  GB.addParameter(llvm::None, T);
  GB.addParameter(Comma, U);
  GB.useLeftAngleBracket(LAngle);
  GB.useRightAngleBracket(RAngle);

  return GB.build();
}

CodeBlockStmtSyntax getCannedBody() {
  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "-");
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(NoSign, OneDigits);
  auto ReturnKW =
    SyntaxFactory::makeReturnKeyword(Trivia::newlines(1) + Trivia::spaces(2),
                                     {});
  auto Return = SyntaxFactory::makeReturnStmt(ReturnKW, One);

  auto Stmts = SyntaxFactory::makeBlankStmtList()
    .appending(Return)
    .castTo<StmtListSyntax>();

  auto LBrace = SyntaxFactory::makeLeftBraceToken({}, {});
  auto RBrace = SyntaxFactory::makeRightBraceToken(Trivia::newlines(1), {});

  return SyntaxFactory::makeCodeBlock(LBrace, Stmts, RBrace);
}

GenericWhereClauseSyntax getCannedWhereClause() {
  auto WhereKW = SyntaxFactory::makeWhereKeyword({}, Trivia::spaces(1));
  auto T = SyntaxFactory::makeTypeIdentifier("T", {}, Trivia::spaces(1));
  auto EqualEqual = SyntaxFactory::makeEqualityOperator({}, Trivia::spaces(1));
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, Trivia::spaces(1));
  auto SameType = SyntaxFactory::makeSameTypeRequirement(T, EqualEqual, Int);

  auto Requirements = SyntaxFactory::makeBlankGenericRequirementList()
    .appending(SameType.castTo<GenericRequirementSyntax>())
    .castTo<GenericRequirementListSyntax>();

  return SyntaxFactory::makeBlankGenericWhereClause()
    .withWhereKeyword(WhereKW)
    .withRequirementList(Requirements);
}

FunctionDeclSyntax getCannedFunctionDecl() {
  auto NoAttributes = SyntaxFactory::makeBlankTypeAttributes();
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto FuncKW = SyntaxFactory::makeFuncKeyword({}, Trivia::spaces(1));
  auto Modifiers = getCannedModifiers();
  auto GenericParams = getCannedGenericParams();
  auto GenericWhere = getCannedWhereClause();
  auto Signature = getCannedFunctionSignature();
  auto Body = getCannedBody();

  return SyntaxFactory::makeFunctionDecl(NoAttributes, Modifiers, FuncKW, Foo,
                                         GenericParams, Signature, GenericWhere,
                                         Body);
}

TEST(DeclSyntaxTests, FunctionDeclMakeAPIs) {
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionDecl().print(OS);
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
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionDecl().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {

  }
}

TEST(DeclSyntaxTests, FunctionDeclWithAPIs) {

}

TEST(DeclSyntaxTests, FunctionDeclBuilderAPIs) {

}
