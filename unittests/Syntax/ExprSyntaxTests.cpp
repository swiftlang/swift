#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/SyntaxBuilders.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - integer-literal-expression

TEST(ExprSyntaxTests, IntegerLiteralExprMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    auto LiteralToken = Factory.makeIntegerLiteral("100", "", "");
    auto Sign = Factory.makePrefixOperator("-", "", "");
    auto Literal = Factory.makePrefixOperatorExpr(
        Sign, Factory.makeIntegerLiteralExpr(LiteralToken));

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "-100");
    ASSERT_EQ(Literal.getKind(), SyntaxKind::PrefixOperatorExpr);
  }
  {
    auto LiteralToken = Factory.makeIntegerLiteral("1_000", "", "");
    auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "", Arena);
    auto Literal = Factory.makeIntegerLiteralExpr(LiteralToken);

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "1_000");
  }
  {
    auto Literal = Factory.makeBlankPrefixOperatorExpr()
                       .withOperatorToken(TokenSyntax::missingToken(
                           tok::oper_prefix, "", Arena))
                       .withPostfixExpression(Factory.makeIntegerLiteralExpr(
                           Factory.makeIntegerLiteral("0", "", "    ")));

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "0    ");
  }
  {
    auto LiteralToken = Factory.makeIntegerLiteral("1_000_000_000_000", "", "");
    auto PlusSign = Factory.makePrefixOperator("+", "", "");
    auto OneThousand = Factory.makePrefixOperatorExpr(
        PlusSign, Factory.makeIntegerLiteralExpr(LiteralToken));

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    OneThousand.print(OS);
    ASSERT_EQ(OS.str().str(), "+1_000_000_000_000");
  }
}

#pragma mark - symbolic-reference

TEST(ExprSyntaxTests, SymbolicReferenceExprGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    auto Array = Factory.makeIdentifier("Array", "", "");
    auto Int = Factory.makeIdentifier("Int", "", "");
    auto IntType = Factory.makeSimpleTypeIdentifier(Int, None);
    auto GenericArg = Factory.makeGenericArgument(IntType, None);
    GenericArgumentClauseSyntaxBuilder ArgBuilder(Arena);
    ArgBuilder.useLeftAngleBracket(Factory.makeLeftAngleToken("", ""))
        .useRightAngleBracket(Factory.makeRightAngleToken("", ""))
        .addArgument(GenericArg);

    auto GenericArgs = ArgBuilder.build();

    auto Ref = Factory.makeSymbolicReferenceExpr(Array, GenericArgs);

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
  SyntaxFactory Factory(Arena);
  auto Array = Factory.makeIdentifier("Array", "", "");
  auto Int = Factory.makeIdentifier("Int", "", "");
  auto IntType = Factory.makeSimpleTypeIdentifier(Int, None);
  auto GenericArg = Factory.makeGenericArgument(IntType, None);
  GenericArgumentClauseSyntaxBuilder ArgBuilder(Arena);
  ArgBuilder.useLeftAngleBracket(Factory.makeLeftAngleToken("", ""))
      .useRightAngleBracket(Factory.makeRightAngleToken("", ""))
      .addArgument(GenericArg);
  auto GenericArgs = ArgBuilder.build();

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankSymbolicReferenceExpr().print(OS);
    EXPECT_EQ(OS.str().str(), "");
  }

  {
    auto Foo = Factory.makeIdentifier("foo", "", "");
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto BlankArgs = Factory.makeBlankGenericArgumentClause();

    Factory.makeSymbolicReferenceExpr(Foo, BlankArgs).print(OS);
    EXPECT_EQ(OS.str().str(), "foo");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeSymbolicReferenceExpr(Array, GenericArgs).print(OS);
    ASSERT_EQ(OS.str().str(), "Array<Int>");
  }
}

TEST(ExprSyntaxTests, SymbolicReferenceExprWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Array = Factory.makeIdentifier("Array", "", "");
  auto Int = Factory.makeIdentifier("Int", "", "");
  auto IntType = Factory.makeSimpleTypeIdentifier(Int, None);
  auto GenericArg = Factory.makeGenericArgument(IntType, None);
  GenericArgumentClauseSyntaxBuilder ArgBuilder(Arena);
  ArgBuilder.useLeftAngleBracket(Factory.makeLeftAngleToken("", ""))
      .useRightAngleBracket(Factory.makeRightAngleToken("", ""))
      .addArgument(GenericArg);
  auto GenericArgs = ArgBuilder.build();

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankSymbolicReferenceExpr().withIdentifier(Array).print(OS);
    ASSERT_EQ(OS.str().str(), "Array");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankSymbolicReferenceExpr()
        .withGenericArgumentClause(GenericArgs)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "<Int>");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankSymbolicReferenceExpr()
        .withIdentifier(Array)
        .withGenericArgumentClause(GenericArgs)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "Array<Int>");
  }
}

#pragma mark - function-call-argument

TEST(ExprSyntaxTests, TupleExprElementGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto X = Factory.makeIdentifier("x", "", "");
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto Colon = Factory.makeColonToken("", " ");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = Factory.makeCommaToken("", " ");

  {
    auto Arg = Factory.makeTupleExprElement(X, Colon, SymbolicRef, Comma);

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
  SyntaxFactory Factory(Arena);
  auto X = Factory.makeIdentifier("x", "", "");
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto Colon = Factory.makeColonToken("", " ");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = Factory.makeCommaToken("", " ");

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankTupleExprElement().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankTupleExprElement().withExpression(SymbolicRef).print(OS);
    ASSERT_EQ(OS.str().str(), "foo");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeTupleExprElement(X, Colon, SymbolicRef, Comma).print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, ");
  }
}

TEST(ExprSyntaxTests, TupleExprElementWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto X = Factory.makeIdentifier("x", "", "");
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto Colon = Factory.makeColonToken("", " ");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = Factory.makeCommaToken("", " ");

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankTupleExprElement()
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
  SyntaxFactory Factory(Arena);
  auto X = Factory.makeIdentifier("x", "", "");
  auto Y = Factory.makeIdentifier("y", "", "");
  auto Z = Factory.makeIdentifier("z", "", "");
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto Colon = Factory.makeColonToken("", " ");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = Factory.makeCommaToken("", " ");
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);

  auto Arg = Factory.makeTupleExprElement(X, Colon, SymbolicRef, Comma);

  return Factory.makeBlankTupleExprElementList()
      .appending(Arg)
      .appending(Arg.withLabel(Y))
      .appending(Arg.withLabel(Z).withTrailingComma(NoComma))
      .castTo<TupleExprElementListSyntax>();
}

TupleExprElementListSyntax
getLabellessArgumentList(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "", Arena);
  auto OneDigits = Factory.makeIntegerLiteral("1", "", "");
  auto TwoDigits = Factory.makeIntegerLiteral("2", "", "");
  auto ThreeDigits = Factory.makeIntegerLiteral("3", "", "");
  auto One = Factory.makeIntegerLiteralExpr(OneDigits);
  auto NoLabel = TokenSyntax::missingToken(tok::identifier, "", Arena);
  auto NoColon = TokenSyntax::missingToken(tok::colon, ":", Arena);
  auto Comma = Factory.makeCommaToken("", " ");
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
  auto Two = Factory.makeIntegerLiteralExpr(TwoDigits);
  auto Three = Factory.makeIntegerLiteralExpr(ThreeDigits);

  auto OneArg = Factory.makeTupleExprElement(NoLabel, NoColon, One, Comma);
  auto TwoArg = Factory.makeTupleExprElement(NoLabel, NoColon, Two, Comma);
  auto ThreeArg =
      Factory.makeTupleExprElement(NoLabel, NoColon, Three, NoComma);

  return Factory.makeBlankTupleExprElementList()
      .appending(OneArg)
      .appending(TwoArg)
      .appending(ThreeArg)
      .castTo<TupleExprElementListSyntax>();
}
} // end anonymous namespace

TEST(ExprSyntaxTests, TupleExprElementListGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto X = Factory.makeIdentifier("x", "", "");
  auto Y = Factory.makeIdentifier("y", "", "");
  auto Z = Factory.makeIdentifier("z", "", "");
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto Colon = Factory.makeColonToken("", " ");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = Factory.makeCommaToken("", " ");
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);

  auto Arg = Factory.makeTupleExprElement(X, Colon, SymbolicRef, Comma);

  auto ArgList = Factory.makeBlankTupleExprElementList()
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
  SyntaxFactory Factory(Arena);
  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankTupleExprElementList().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    auto X = Factory.makeIdentifier("x", "", "");
    auto Y = Factory.makeIdentifier("y", "", "");
    auto Z = Factory.makeIdentifier("z", "", "");
    auto Foo = Factory.makeIdentifier("foo", "", "");
    auto Colon = Factory.makeColonToken("", " ");
    auto SymbolicRef = Factory.makeSymbolicReferenceExpr(Foo, llvm::None);
    auto Comma = Factory.makeCommaToken("", " ");
    auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);

    auto Arg = Factory.makeTupleExprElement(X, Colon, SymbolicRef, Comma);

    std::vector<TupleExprElementSyntax> Args {
      Arg, Arg.withLabel(Y), Arg.withLabel(Z).withTrailingComma(NoComma)
    };

    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto ArgList = Factory.makeTupleExprElementList(Args);
    ArgList.print(OS);
    ASSERT_EQ(ArgList.size(), size_t(3));
    ASSERT_EQ(OS.str().str(), "x: foo, y: foo, z: foo");
  }
}

TEST(ExprSyntaxTests, TupleExprElementListWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
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
  SyntaxFactory Factory(Arena);
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(Foo, llvm::None);
  auto LeftParen = Factory.makeLeftParenToken("", "");
  auto ArgList = getFullArgumentList(Arena);
  auto RightParen = Factory.makeRightParenToken("", "");

  auto Call = Factory.makeFunctionCallExpr(SymbolicRef, LeftParen, ArgList,
                                           RightParen, None, None);

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
  SyntaxFactory Factory(Arena);
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(Foo, llvm::None);
  auto LeftParen = Factory.makeLeftParenToken("", "");
  auto ArgList = getFullArgumentList(Arena);
  auto RightParen = Factory.makeRightParenToken("", "");

  {
    auto Call = Factory.makeFunctionCallExpr(SymbolicRef, LeftParen, ArgList,
                                             RightParen, None, None);
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Call.print(OS);
    ASSERT_EQ(OS.str().str(), "foo(x: foo, y: foo, z: foo)");
  }

  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankFunctionCallExpr().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

TEST(ExprSyntaxTests, FunctionCallExprWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(Foo, llvm::None);
  auto LeftParen = Factory.makeLeftParenToken("", "");
  auto ArgList = getFullArgumentList(Arena);
  auto RightParen = Factory.makeRightParenToken("", "");

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankFunctionCallExpr()
        .withCalledExpression(SymbolicRef)
        .withLeftParen(LeftParen)
        .withRightParen(RightParen)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "foo()");
  }
  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankFunctionCallExpr()
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
  SyntaxFactory Factory(Arena);
  FunctionCallExprSyntaxBuilder CallBuilder(Arena);

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  auto LeftParen = Factory.makeLeftParenToken("", "");
  auto RightParen = Factory.makeRightParenToken("", "");

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.useLeftParen(LeftParen);
    CallBuilder.useRightParen(RightParen);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "()");
  }

  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "", Arena);
  auto OneDigits = Factory.makeIntegerLiteral("1", "", "");
  auto TwoDigits = Factory.makeIntegerLiteral("2", "", "");
  auto ThreeDigits = Factory.makeIntegerLiteral("3", "", "");
  auto One = Factory.makeIntegerLiteralExpr(OneDigits);
  auto NoLabel = TokenSyntax::missingToken(tok::identifier, "", Arena);
  auto NoColon = TokenSyntax::missingToken(tok::colon, ":", Arena);
  auto Comma = Factory.makeCommaToken("", " ");
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(Foo, llvm::None);

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.useCalledExpression(SymbolicRef);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "foo()");
  }

  auto OneArg = Factory.makeTupleExprElement(NoLabel, NoColon, One, Comma);
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
