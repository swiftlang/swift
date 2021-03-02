#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/SyntaxBuilders.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - integer-literal-expression

TEST(ExprSyntaxTests, IntegerLiteralExprMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    auto LiteralToken = SyntaxFactory::makeIntegerLiteral("100", "", "", Arena);
    auto Sign = SyntaxFactory::makePrefixOperator("-", "", "", Arena);
    auto Literal = SyntaxFactory::makePrefixOperatorExpr(
        Sign, SyntaxFactory::makeIntegerLiteralExpr(LiteralToken, Arena),
        Arena);

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "-100");
    ASSERT_EQ(Literal.getKind(), SyntaxKind::PrefixOperatorExpr);
  }
  {
    auto LiteralToken =
        SyntaxFactory::makeIntegerLiteral("1_000", "", "", Arena);
    auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "", Arena);
    auto Literal = SyntaxFactory::makeIntegerLiteralExpr(LiteralToken, Arena);

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "1_000");
  }
  {
    auto Literal =
        SyntaxFactory::makeBlankPrefixOperatorExpr(Arena)
            .withOperatorToken(
                TokenSyntax::missingToken(tok::oper_prefix, "", Arena))
            .withPostfixExpression(SyntaxFactory::makeIntegerLiteralExpr(
                SyntaxFactory::makeIntegerLiteral("0", "", "    ", Arena),
                Arena));

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "0    ");
  }
  {
    auto LiteralToken =
        SyntaxFactory::makeIntegerLiteral("1_000_000_000_000", "", "", Arena);
    auto PlusSign = SyntaxFactory::makePrefixOperator("+", "", "", Arena);
    auto OneThousand = SyntaxFactory::makePrefixOperatorExpr(
        PlusSign, SyntaxFactory::makeIntegerLiteralExpr(LiteralToken, Arena),
        Arena);

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    OneThousand.print(OS);
    ASSERT_EQ(OS.str().str(), "+1_000_000_000_000");
  }
}

#pragma mark - symbolic-reference

TEST(ExprSyntaxTests, SymbolicReferenceExprGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    auto Array = SyntaxFactory::makeIdentifier("Array", "", "", Arena);
    auto Int = SyntaxFactory::makeIdentifier("Int", "", "", Arena);
    auto IntType = SyntaxFactory::makeSimpleTypeIdentifier(Int, None, Arena);
    auto GenericArg = SyntaxFactory::makeGenericArgument(IntType, None, Arena);
    GenericArgumentClauseSyntaxBuilder ArgBuilder(Arena);
    ArgBuilder
        .useLeftAngleBracket(SyntaxFactory::makeLeftAngleToken("", "", Arena))
        .useRightAngleBracket(SyntaxFactory::makeRightAngleToken("", "", Arena))
        .addArgument(GenericArg);

    auto GenericArgs = ArgBuilder.build();

    auto Ref =
        SyntaxFactory::makeSymbolicReferenceExpr(Array, GenericArgs, Arena);

    ASSERT_EQ(Ref.getIdentifier().getRaw(), Array.getRaw());

    auto GottenArgs = Ref.getGenericArgumentClause().getValue();
    auto GottenArgs2 = Ref.getGenericArgumentClause().getValue();
    ASSERT_TRUE(GottenArgs.hasSameIdentityAs(GottenArgs2));

    {
      llvm::SmallString<48> Scratch;
      llvm::raw_svector_ostream OS(Scratch);
      GottenArgs.print(OS);
      ASSERT_EQ(OS.str().str(), "<Int>");
    }
  }
}

TEST(ExprSyntaxTests, SymbolicReferenceExprMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Array = SyntaxFactory::makeIdentifier("Array", "", "", Arena);
  auto Int = SyntaxFactory::makeIdentifier("Int", "", "", Arena);
  auto IntType = SyntaxFactory::makeSimpleTypeIdentifier(Int, None, Arena);
  auto GenericArg = SyntaxFactory::makeGenericArgument(IntType, None, Arena);
  GenericArgumentClauseSyntaxBuilder ArgBuilder(Arena);
  ArgBuilder
      .useLeftAngleBracket(SyntaxFactory::makeLeftAngleToken("", "", Arena))
      .useRightAngleBracket(SyntaxFactory::makeRightAngleToken("", "", Arena))
      .addArgument(GenericArg);
  auto GenericArgs = ArgBuilder.build();

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankSymbolicReferenceExpr(Arena).print(OS);
    EXPECT_EQ(OS.str().str(), "");
  }

  {
    auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto BlankArgs = SyntaxFactory::makeBlankGenericArgumentClause(Arena);

    SyntaxFactory::makeSymbolicReferenceExpr(Foo, BlankArgs, Arena).print(OS);
    EXPECT_EQ(OS.str().str(), "foo");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeSymbolicReferenceExpr(Array, GenericArgs, Arena)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "Array<Int>");
  }
}

TEST(ExprSyntaxTests, SymbolicReferenceExprWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Array = SyntaxFactory::makeIdentifier("Array", "", "", Arena);
  auto Int = SyntaxFactory::makeIdentifier("Int", "", "", Arena);
  auto IntType = SyntaxFactory::makeSimpleTypeIdentifier(Int, None, Arena);
  auto GenericArg = SyntaxFactory::makeGenericArgument(IntType, None, Arena);
  GenericArgumentClauseSyntaxBuilder ArgBuilder(Arena);
  ArgBuilder
      .useLeftAngleBracket(SyntaxFactory::makeLeftAngleToken("", "", Arena))
      .useRightAngleBracket(SyntaxFactory::makeRightAngleToken("", "", Arena))
      .addArgument(GenericArg);
  auto GenericArgs = ArgBuilder.build();

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankSymbolicReferenceExpr(Arena)
        .withIdentifier(Array)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "Array");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankSymbolicReferenceExpr(Arena)
        .withGenericArgumentClause(GenericArgs)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "<Int>");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankSymbolicReferenceExpr(Arena)
        .withIdentifier(Array)
        .withGenericArgumentClause(GenericArgs)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "Array<Int>");
  }
}

#pragma mark - function-call-argument

TEST(ExprSyntaxTests, TupleExprElementGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto X = SyntaxFactory::makeIdentifier("x", "", "", Arena);
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
  auto SymbolicRef =
      SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);

  {
    auto Arg = SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef, Comma,
                                                   Arena);

    ASSERT_EQ(X.getRaw(), Arg.getLabel()->getRaw());
    ASSERT_EQ(Colon.getRaw(), Arg.getColon()->getRaw());

    auto GottenExpr = Arg.getExpression();
    auto GottenExpr2 = Arg.getExpression();
    ASSERT_TRUE(GottenExpr.hasSameIdentityAs(GottenExpr2));
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    GottenExpr.print(OS);
    ASSERT_EQ("foo", OS.str().str());

    ASSERT_EQ(Comma.getRaw(), Arg.getTrailingComma()->getRaw());
  }
}

TEST(ExprSyntaxTests, TupleExprElementMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto X = SyntaxFactory::makeIdentifier("x", "", "", Arena);
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
  auto SymbolicRef =
      SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTupleExprElement(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTupleExprElement(Arena)
        .withExpression(SymbolicRef)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "foo");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef, Comma, Arena)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, ");
  }
}

TEST(ExprSyntaxTests, TupleExprElementWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto X = SyntaxFactory::makeIdentifier("x", "", "", Arena);
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
  auto SymbolicRef =
      SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTupleExprElement(Arena)
        .withLabel(X)
        .withColon(Colon)
        .withExpression(SymbolicRef)
        .withTrailingComma(Comma)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, ");
  }
}

#pragma mark - function-call-argument-list

namespace {
TupleExprElementListSyntax getFullArgumentList(const RC<SyntaxArena> &Arena) {
  auto X = SyntaxFactory::makeIdentifier("x", "", "", Arena);
  auto Y = SyntaxFactory::makeIdentifier("y", "", "", Arena);
  auto Z = SyntaxFactory::makeIdentifier("z", "", "", Arena);
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
  auto SymbolicRef =
      SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);

  auto Arg =
      SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef, Comma, Arena);

  return SyntaxFactory::makeBlankTupleExprElementList(Arena)
      .appending(Arg)
      .appending(Arg.withLabel(Y))
      .appending(Arg.withLabel(Z).withTrailingComma(NoComma))
      .castTo<TupleExprElementListSyntax>();
}

TupleExprElementListSyntax
getLabellessArgumentList(const RC<SyntaxArena> &Arena) {
  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "", Arena);
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", "", "", Arena);
  auto TwoDigits = SyntaxFactory::makeIntegerLiteral("2", "", "", Arena);
  auto ThreeDigits = SyntaxFactory::makeIntegerLiteral("3", "", "", Arena);
  auto One = SyntaxFactory::makeIntegerLiteralExpr(OneDigits, Arena);
  auto NoLabel = TokenSyntax::missingToken(tok::identifier, "", Arena);
  auto NoColon = TokenSyntax::missingToken(tok::colon, ":", Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
  auto Two = SyntaxFactory::makeIntegerLiteralExpr(TwoDigits, Arena);
  auto Three = SyntaxFactory::makeIntegerLiteralExpr(ThreeDigits, Arena);

  auto OneArg =
      SyntaxFactory::makeTupleExprElement(NoLabel, NoColon, One, Comma, Arena);
  auto TwoArg =
      SyntaxFactory::makeTupleExprElement(NoLabel, NoColon, Two, Comma, Arena);
  auto ThreeArg = SyntaxFactory::makeTupleExprElement(NoLabel, NoColon, Three,
                                                      NoComma, Arena);

  return SyntaxFactory::makeBlankTupleExprElementList(Arena)
      .appending(OneArg)
      .appending(TwoArg)
      .appending(ThreeArg)
      .castTo<TupleExprElementListSyntax>();
}
} // end anonymous namespace

TEST(ExprSyntaxTests, TupleExprElementListGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto X = SyntaxFactory::makeIdentifier("x", "", "", Arena);
  auto Y = SyntaxFactory::makeIdentifier("y", "", "", Arena);
  auto Z = SyntaxFactory::makeIdentifier("z", "", "", Arena);
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
  auto SymbolicRef =
      SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);

  auto Arg =
      SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef, Comma, Arena);

  auto ArgList = SyntaxFactory::makeBlankTupleExprElementList(Arena)
                     .appending(Arg)
                     .appending(Arg.withLabel(Y))
                     .appending(Arg.withLabel(Z).withTrailingComma(NoComma))
                     .castTo<TupleExprElementListSyntax>();

  ASSERT_EQ(ArgList.size(), size_t(3));

  {
    llvm::SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenArg1 = ArgList[0];
    auto GottenArg1_2 = ArgList[0];
    ASSERT_TRUE(GottenArg1.hasSameIdentityAs(GottenArg1_2));
    GottenArg1.print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, ");
  }

  {
    llvm::SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenArg2 = ArgList[1];
    auto GottenArg2_2 = ArgList[1];
    ASSERT_TRUE(GottenArg2.hasSameIdentityAs(GottenArg2_2));
    GottenArg2.print(OS);
    ASSERT_EQ(OS.str().str(), "y: foo, ");
  }

  {
    llvm::SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenArg3 = ArgList[2];
    auto GottenArg3_2 = ArgList[2];
    ASSERT_TRUE(GottenArg3.hasSameIdentityAs(GottenArg3_2));
    GottenArg3.print(OS);
    ASSERT_EQ(OS.str().str(), "z: foo");
  }
}

TEST(ExprSyntaxTests, TupleExprElementListMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTupleExprElementList(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    auto X = SyntaxFactory::makeIdentifier("x", "", "", Arena);
    auto Y = SyntaxFactory::makeIdentifier("y", "", "", Arena);
    auto Z = SyntaxFactory::makeIdentifier("z", "", "", Arena);
    auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
    auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
    auto SymbolicRef =
        SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);
    auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
    auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);

    auto Arg = SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef, Comma,
                                                   Arena);

    std::vector<TupleExprElementSyntax> Args {
      Arg, Arg.withLabel(Y), Arg.withLabel(Z).withTrailingComma(NoComma)
    };

    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto ArgList = SyntaxFactory::makeTupleExprElementList(Args, Arena);
    ArgList.print(OS);
    ASSERT_EQ(ArgList.size(), size_t(3));
    ASSERT_EQ(OS.str().str(), "x: foo, y: foo, z: foo");
  }
}

TEST(ExprSyntaxTests, TupleExprElementListWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto ArgList = getFullArgumentList(Arena);
  llvm::SmallString<64> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  ASSERT_EQ(ArgList.size(), size_t(3));
  ArgList.print(OS);
  ASSERT_EQ(ArgList.size(), size_t(3));
  ASSERT_EQ(OS.str().str(), "x: foo, y: foo, z: foo");
}

#pragma mark - function-call-expression

TEST(ExprSyntaxTests, FunctionCallExprGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto SymbolicRef =
      SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);
  auto LeftParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto ArgList = getFullArgumentList(Arena);
  auto RightParen = SyntaxFactory::makeRightParenToken("", "", Arena);

  auto Call = SyntaxFactory::makeFunctionCallExpr(
      SymbolicRef, LeftParen, ArgList, RightParen, None, None, Arena);

  {
    auto GottenExpression1 = Call.getCalledExpression();
    auto GottenExpression2 = Call.getCalledExpression();
    ASSERT_TRUE(GottenExpression1.hasSameIdentityAs(GottenExpression2));
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    GottenExpression1.print(OS);
    ASSERT_EQ(OS.str().str(), "foo");
  }

  ASSERT_EQ(LeftParen.getRaw(), Call.getLeftParen()->getRaw());
  ASSERT_EQ(RightParen.getRaw(), Call.getRightParen()->getRaw());

  {
    auto GottenArgs1 = Call.getArgumentList();
    auto GottenArgs2 = Call.getArgumentList();
    ASSERT_TRUE(GottenArgs1.hasSameIdentityAs(GottenArgs2));
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    GottenArgs1.print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, y: foo, z: foo");
  }
}

TEST(ExprSyntaxTests, FunctionCallExprMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto SymbolicRef =
      SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);
  auto LeftParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto ArgList = getFullArgumentList(Arena);
  auto RightParen = SyntaxFactory::makeRightParenToken("", "", Arena);

  {
    auto Call = SyntaxFactory::makeFunctionCallExpr(
        SymbolicRef, LeftParen, ArgList, RightParen, None, None, Arena);
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Call.print(OS);
    ASSERT_EQ(OS.str().str(), "foo(x: foo, y: foo, z: foo)");
  }

  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionCallExpr(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

TEST(ExprSyntaxTests, FunctionCallExprWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto SymbolicRef =
      SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);
  auto LeftParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto ArgList = getFullArgumentList(Arena);
  auto RightParen = SyntaxFactory::makeRightParenToken("", "", Arena);

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionCallExpr(Arena)
        .withCalledExpression(SymbolicRef)
        .withLeftParen(LeftParen)
        .withRightParen(RightParen)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "foo()");
  }
  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionCallExpr(Arena)
        .withCalledExpression(SymbolicRef)
        .withLeftParen(LeftParen)
        .withArgumentList(getLabellessArgumentList(Arena))
        .withRightParen(RightParen)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "foo(1, 2, 3)");
  }
}

TEST(ExprSyntaxTests, FunctionCallExprBuilderAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  FunctionCallExprSyntaxBuilder CallBuilder(Arena);

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  auto LeftParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto RightParen = SyntaxFactory::makeRightParenToken("", "", Arena);

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.useLeftParen(LeftParen);
    CallBuilder.useRightParen(RightParen);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "()");
  }

  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "", Arena);
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", "", "", Arena);
  auto TwoDigits = SyntaxFactory::makeIntegerLiteral("2", "", "", Arena);
  auto ThreeDigits = SyntaxFactory::makeIntegerLiteral("3", "", "", Arena);
  auto One = SyntaxFactory::makeIntegerLiteralExpr(OneDigits, Arena);
  auto NoLabel = TokenSyntax::missingToken(tok::identifier, "", Arena);
  auto NoColon = TokenSyntax::missingToken(tok::colon, ":", Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto SymbolicRef =
      SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.useCalledExpression(SymbolicRef);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "foo()");
  }

  auto OneArg =
      SyntaxFactory::makeTupleExprElement(NoLabel, NoColon, One, Comma, Arena);
  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.addArgument(OneArg);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "foo(1, )");
  }

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.addArgument(OneArg.withTrailingComma(NoComma));
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "foo(1, 1)");
  }
}
