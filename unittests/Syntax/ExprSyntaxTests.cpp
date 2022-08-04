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
    auto IntegerLiteral =
        Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, LiteralToken);
    auto Literal = Factory.makePrefixOperatorExpr(
        /*GarbageNodes=*/None, Sign, /*GarbageNodes=*/None, IntegerLiteral);

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "-100");
    ASSERT_EQ(Literal.getKind(), SyntaxKind::PrefixOperatorExpr);
  }
  {
    auto LiteralToken = Factory.makeIntegerLiteral("1_000", "", "");
    auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "", Arena);
    auto Literal =
        Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, LiteralToken);

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "1_000");
  }
  {
    auto IntLiteral = Factory.makeIntegerLiteral("0", "", "    ");
    auto IntLiteralExpr =
        Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, IntLiteral);
    auto Literal = Factory.makeBlankPrefixOperatorExpr()
                       .withOperatorToken(TokenSyntax::missingToken(
                           tok::oper_prefix, "", Arena))
                       .withPostfixExpression(IntLiteralExpr);

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "0    ");
  }
  {
    auto LiteralToken = Factory.makeIntegerLiteral("1_000_000_000_000", "", "");
    auto PlusSign = Factory.makePrefixOperator("+", "", "");
    auto IntLiteralExpr =
        Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, LiteralToken);
    auto OneThousand = Factory.makePrefixOperatorExpr(
        /*GarbageNodes=*/None, PlusSign, /*GarbageNodes=*/None, IntLiteralExpr);

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
    auto IntType = Factory.makeSimpleTypeIdentifier(
        /*GarbageNodes=*/None, Int, /*GarbageNodes=*/None, None);
    auto GenericArg = Factory.makeGenericArgument(
        /*GarbageNodes=*/None, IntType, /*GarbageNodes=*/None, None);
    GenericArgumentClauseSyntaxBuilder ArgBuilder(Arena);
    ArgBuilder.useLeftAngleBracket(Factory.makeLeftAngleToken("", ""))
        .useRightAngleBracket(Factory.makeRightAngleToken("", ""))
        .addArgument(GenericArg);

    auto GenericArgs = ArgBuilder.build();

    auto Ref = Factory.makeSymbolicReferenceExpr(
        /*GarbageNodes=*/None, Array, /*GarbageNodes=*/None, GenericArgs);

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
  auto IntType = Factory.makeSimpleTypeIdentifier(/*GarbageNodes=*/None, Int,
                                                  /*GarbageNodes=*/None, None);
  auto GenericArg = Factory.makeGenericArgument(/*GarbageNodes=*/None, IntType,
                                                /*GarbageNodes=*/None, None);
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

    Factory
        .makeSymbolicReferenceExpr(/*GarbageNodes=*/None, Foo,
                                   /*GarbageNodes=*/None, BlankArgs)
        .print(OS);
    EXPECT_EQ(OS.str().str(), "foo");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory
        .makeSymbolicReferenceExpr(/*GarbageNodes=*/None, Array,
                                   /*GarbageNodes=*/None, GenericArgs)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "Array<Int>");
  }
}

TEST(ExprSyntaxTests, SymbolicReferenceExprWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Array = Factory.makeIdentifier("Array", "", "");
  auto Int = Factory.makeIdentifier("Int", "", "");
  auto IntType = Factory.makeSimpleTypeIdentifier(/*GarbageNodes=*/None, Int,
                                                  /*GarbageNodes=*/None, None);
  auto GenericArg = Factory.makeGenericArgument(/*GarbageNodes=*/None, IntType,
                                                /*GarbageNodes=*/None, None);
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
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(
      /*GarbageNodes=*/None, Foo, /*GarbageNodes=*/None, None);
  auto Comma = Factory.makeCommaToken("", " ");

  {
    auto Arg = Factory.makeTupleExprElement(
        /*GarbageNodes=*/None, X, /*GarbageNodes=*/None, Colon,
        /*GarbageNodes=*/None, SymbolicRef, /*GarbageNodes=*/None, Comma);

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
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(
      /*GarbageNodes=*/None, Foo, /*GarbageNodes=*/None, None);
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
    Factory
        .makeTupleExprElement(/*GarbageNodes=*/None, X, /*GarbageNodes=*/None,
                              Colon, /*GarbageNodes=*/None, SymbolicRef,
                              /*GarbageNodes=*/None, Comma)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, ");
  }
}

TEST(ExprSyntaxTests, TupleExprElementWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto X = Factory.makeIdentifier("x", "", "");
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto Colon = Factory.makeColonToken("", " ");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(
      /*GarbageNodes=*/None, Foo, /*GarbageNodes=*/None, None);
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
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(
      /*GarbageNodes=*/None, Foo, /*GarbageNodes=*/None, None);
  auto Comma = Factory.makeCommaToken("", " ");
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);

  auto Arg = Factory.makeTupleExprElement(
      /*GarbageNodes=*/None, X, /*GarbageNodes=*/None, Colon,
      /*GarbageNodes=*/None, SymbolicRef, /*GarbageNodes=*/None, Comma);

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
  auto One = Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, OneDigits);
  auto NoLabel = TokenSyntax::missingToken(tok::identifier, "", Arena);
  auto NoColon = TokenSyntax::missingToken(tok::colon, ":", Arena);
  auto Comma = Factory.makeCommaToken("", " ");
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
  auto Two = Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, TwoDigits);
  auto Three =
      Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, ThreeDigits);

  auto OneArg = Factory.makeTupleExprElement(
      /*GarbageNodes=*/None, NoLabel, /*GarbageNodes=*/None, NoColon,
      /*GarbageNodes=*/None, One, /*GarbageNodes=*/None, Comma);
  auto TwoArg = Factory.makeTupleExprElement(
      /*GarbageNodes=*/None, NoLabel, /*GarbageNodes=*/None, NoColon,
      /*GarbageNodes=*/None, Two, /*GarbageNodes=*/None, Comma);
  auto ThreeArg = Factory.makeTupleExprElement(
      /*GarbageNodes=*/None, NoLabel, /*GarbageNodes=*/None, NoColon,
      /*GarbageNodes=*/None, Three, /*GarbageNodes=*/None, NoComma);

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
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(
      /*GarbageNodes=*/None, Foo, /*GarbageNodes=*/None, None);
  auto Comma = Factory.makeCommaToken("", " ");
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);

  auto Arg = Factory.makeTupleExprElement(
      /*GarbageNodes=*/None, X, /*GarbageNodes=*/None, Colon,
      /*GarbageNodes=*/None, SymbolicRef, /*GarbageNodes=*/None, Comma);

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
    auto SymbolicRef = Factory.makeSymbolicReferenceExpr(
        /*GarbageNodes=*/None, Foo, /*GarbageNodes=*/None, None);
    auto Comma = Factory.makeCommaToken("", " ");
    auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);

    auto Arg = Factory.makeTupleExprElement(
        /*GarbageNodes=*/None, X, /*GarbageNodes=*/None, Colon,
        /*GarbageNodes=*/None, SymbolicRef, /*GarbageNodes=*/None, Comma);

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
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(
      /*GarbageNodes=*/None, Foo, /*GarbageNodes=*/None, None);
  auto LeftParen = Factory.makeLeftParenToken("", "");
  auto ArgList = getFullArgumentList(Arena);
  auto RightParen = Factory.makeRightParenToken("", "");

  auto Call = Factory.makeFunctionCallExpr(
      /*GarbageNodes=*/None, SymbolicRef, /*GarbageNodes=*/None, LeftParen,
      /*GarbageNodes=*/None, ArgList,
      /*GarbageNodes=*/None, RightParen, /*GarbageNodes=*/None, None,
      /*GarbageNodes=*/None, None);

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
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(
      /*GarbageNodes=*/None, Foo, /*GarbageNodes=*/None, None);
  auto LeftParen = Factory.makeLeftParenToken("", "");
  auto ArgList = getFullArgumentList(Arena);
  auto RightParen = Factory.makeRightParenToken("", "");

  {
    auto Call = Factory.makeFunctionCallExpr(
        /*GarbageNodes=*/None, SymbolicRef, /*GarbageNodes=*/None, LeftParen,
        /*GarbageNodes=*/None, ArgList,
        /*GarbageNodes=*/None, RightParen, /*GarbageNodes=*/None, None,
        /*GarbageNodes=*/None, None);
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
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(
      /*GarbageNodes=*/None, Foo, /*GarbageNodes=*/None, None);
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
  auto One = Factory.makeIntegerLiteralExpr(/*GarbageNodes=*/None, OneDigits);
  auto NoLabel = TokenSyntax::missingToken(tok::identifier, "", Arena);
  auto NoColon = TokenSyntax::missingToken(tok::colon, ":", Arena);
  auto Comma = Factory.makeCommaToken("", " ");
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
  auto Foo = Factory.makeIdentifier("foo", "", "");
  auto SymbolicRef = Factory.makeSymbolicReferenceExpr(
      /*GarbageNodes=*/None, Foo, /*GarbageNodes=*/None, None);

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.useCalledExpression(SymbolicRef);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "foo()");
  }

  auto OneArg = Factory.makeTupleExprElement(
      /*GarbageNodes=*/None, NoLabel, /*GarbageNodes=*/None, NoColon,
      /*GarbageNodes=*/None, One, /*GarbageNodes=*/None, Comma);
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
