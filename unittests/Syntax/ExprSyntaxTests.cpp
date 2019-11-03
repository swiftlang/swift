#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/SyntaxBuilders.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - integer-literal-expression

TEST(ExprSyntaxTests, IntegerLiteralExprMakeAPIs) {
  {
    auto LiteralToken = SyntaxFactory::makeIntegerLiteral("100", {}, {});
    auto Sign = SyntaxFactory::makePrefixOperator("-", {}, {});
    auto Literal = SyntaxFactory::makePrefixOperatorExpr(Sign,
      SyntaxFactory::makeIntegerLiteralExpr(LiteralToken));

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "-100");
    ASSERT_EQ(Literal.getKind(), SyntaxKind::PrefixOperatorExpr);
  }
  {
    auto LiteralToken = SyntaxFactory::makeIntegerLiteral("1_000", {}, {});
    auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "");
    auto Literal = SyntaxFactory::makeIntegerLiteralExpr(LiteralToken);

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "1_000");
  }
  {
    auto Literal = SyntaxFactory::makeBlankPrefixOperatorExpr()
    .withOperatorToken(TokenSyntax::missingToken(tok::oper_prefix, ""))
    .withPostfixExpression(SyntaxFactory::makeIntegerLiteralExpr(
      SyntaxFactory::makeIntegerLiteral("0", {}, { Trivia::spaces(4) })));

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Literal.print(OS);
    ASSERT_EQ(OS.str().str(), "0    ");
  }
  {
    auto LiteralToken =
      SyntaxFactory::makeIntegerLiteral("1_000_000_000_000", {}, {});
    auto PlusSign = SyntaxFactory::makePrefixOperator("+", {}, {});
    auto OneThousand = SyntaxFactory::makePrefixOperatorExpr(PlusSign,
      SyntaxFactory::makeIntegerLiteralExpr(LiteralToken));

    llvm::SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    OneThousand.print(OS);
    ASSERT_EQ(OS.str().str(), "+1_000_000_000_000");
  }
}

#pragma mark - symbolic-reference

TEST(ExprSyntaxTests, SymbolicReferenceExprGetAPIs) {
  {
    auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
    auto Int = SyntaxFactory::makeIdentifier("Int", {}, {});
    auto IntType = SyntaxFactory::makeSimpleTypeIdentifier(Int, None);
    auto GenericArg = SyntaxFactory::makeGenericArgument(IntType, None);
    GenericArgumentClauseSyntaxBuilder ArgBuilder;
    ArgBuilder
      .useLeftAngleBracket(SyntaxFactory::makeLeftAngleToken({}, {}))
      .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
      .addArgument(GenericArg);

    auto GenericArgs = ArgBuilder.build();

    auto Ref = SyntaxFactory::makeSymbolicReferenceExpr(Array, GenericArgs);

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
  auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
  auto Int = SyntaxFactory::makeIdentifier("Int", {}, {});
  auto IntType = SyntaxFactory::makeSimpleTypeIdentifier(Int, None);
  auto GenericArg = SyntaxFactory::makeGenericArgument(IntType, None);
  GenericArgumentClauseSyntaxBuilder ArgBuilder;
  ArgBuilder
    .useLeftAngleBracket(SyntaxFactory::makeLeftAngleToken({}, {}))
    .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
    .addArgument(GenericArg);
  auto GenericArgs = ArgBuilder.build();

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankSymbolicReferenceExpr().print(OS);
    EXPECT_EQ(OS.str().str(), "");
  }

  {
    auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto BlankArgs = SyntaxFactory::makeBlankGenericArgumentClause();

    SyntaxFactory::makeSymbolicReferenceExpr(Foo, BlankArgs).print(OS);
    EXPECT_EQ(OS.str().str(), "foo");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeSymbolicReferenceExpr(Array, GenericArgs).print(OS);
    ASSERT_EQ(OS.str().str(), "Array<Int>");
  }
}

TEST(ExprSyntaxTests, SymbolicReferenceExprWithAPIs) {
  auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
  auto Int = SyntaxFactory::makeIdentifier("Int", {}, {});
  auto IntType = SyntaxFactory::makeSimpleTypeIdentifier(Int, None);
  auto GenericArg = SyntaxFactory::makeGenericArgument(IntType, None);
  GenericArgumentClauseSyntaxBuilder ArgBuilder;
  ArgBuilder
    .useLeftAngleBracket(SyntaxFactory::makeLeftAngleToken({}, {}))
    .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
    .addArgument(GenericArg);
  auto GenericArgs = ArgBuilder.build();

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankSymbolicReferenceExpr()
      .withIdentifier(Array)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "Array");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankSymbolicReferenceExpr()
      .withGenericArgumentClause(GenericArgs)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "<Int>");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankSymbolicReferenceExpr()
      .withIdentifier(Array)
      .withGenericArgumentClause(GenericArgs)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "Array<Int>");
  }
}

#pragma mark - function-call-argument

TEST(ExprSyntaxTests, TupleExprElementGetAPIs) {
  auto X = SyntaxFactory::makeIdentifier("x", {}, {});
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));

  {
    auto Arg = SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef,
                                                       Comma);

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
  auto X = SyntaxFactory::makeIdentifier("x", {}, {});
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTupleExprElement().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTupleExprElement()
      .withExpression(SymbolicRef).print(OS);
    ASSERT_EQ(OS.str().str(), "foo");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef, Comma)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, ");
  }
}

TEST(ExprSyntaxTests, TupleExprElementWithAPIs) {
  auto X = SyntaxFactory::makeIdentifier("x", {}, {});
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTupleExprElement()
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
TupleExprElementListSyntax getFullArgumentList() {
  auto X = SyntaxFactory::makeIdentifier("x", {}, {});
  auto Y = SyntaxFactory::makeIdentifier("y", {}, {});
  auto Z = SyntaxFactory::makeIdentifier("z", {}, {});
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",");

  auto Arg = SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef,
                                                     Comma);

  return SyntaxFactory::makeBlankTupleExprElementList()
    .appending(Arg)
    .appending(Arg.withLabel(Y))
    .appending(Arg.withLabel(Z).withTrailingComma(NoComma))
    .castTo<TupleExprElementListSyntax>();
}

TupleExprElementListSyntax getLabellessArgumentList() {
  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "");
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", {}, {});
  auto TwoDigits = SyntaxFactory::makeIntegerLiteral("2", {}, {});
  auto ThreeDigits = SyntaxFactory::makeIntegerLiteral("3", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(OneDigits);
  auto NoLabel = TokenSyntax::missingToken(tok::identifier, "");
  auto NoColon = TokenSyntax::missingToken(tok::colon, ":");
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",");
  auto Two = SyntaxFactory::makeIntegerLiteralExpr(TwoDigits);
  auto Three = SyntaxFactory::makeIntegerLiteralExpr(ThreeDigits);

  auto OneArg = SyntaxFactory::makeTupleExprElement(NoLabel, NoColon, One,
                                                        Comma);
  auto TwoArg = SyntaxFactory::makeTupleExprElement(NoLabel, NoColon, Two,
                                                        Comma);
  auto ThreeArg = SyntaxFactory::makeTupleExprElement(NoLabel, NoColon,
                                                          Three, NoComma);

  return SyntaxFactory::makeBlankTupleExprElementList()
    .appending(OneArg)
    .appending(TwoArg)
    .appending(ThreeArg)
    .castTo<TupleExprElementListSyntax>();
}
} // end anonymous namespace

TEST(ExprSyntaxTests, TupleExprElementListGetAPIs) {
  auto X = SyntaxFactory::makeIdentifier("x", {}, {});
  auto Y = SyntaxFactory::makeIdentifier("y", {}, {});
  auto Z = SyntaxFactory::makeIdentifier("z", {}, {});
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",");

  auto Arg = SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef,
                                                     Comma);

  auto ArgList = SyntaxFactory::makeBlankTupleExprElementList()
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
  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTupleExprElementList().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    auto X = SyntaxFactory::makeIdentifier("x", {}, {});
    auto Y = SyntaxFactory::makeIdentifier("y", {}, {});
    auto Z = SyntaxFactory::makeIdentifier("z", {}, {});
    auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
    auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
    auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo,
                                                                llvm::None);
    auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));
    auto NoComma = TokenSyntax::missingToken(tok::comma, ",");

    auto Arg = SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef,
                                                       Comma);

    std::vector<TupleExprElementSyntax> Args {
      Arg, Arg.withLabel(Y), Arg.withLabel(Z).withTrailingComma(NoComma)
    };

    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto ArgList = SyntaxFactory::makeTupleExprElementList(Args);
    ArgList.print(OS);
    ASSERT_EQ(ArgList.size(), size_t(3));
    ASSERT_EQ(OS.str().str(), "x: foo, y: foo, z: foo");
  }
}

TEST(ExprSyntaxTests, TupleExprElementListWithAPIs) {
  auto ArgList = getFullArgumentList();
  llvm::SmallString<64> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  ASSERT_EQ(ArgList.size(), size_t(3));
  ArgList.print(OS);
  ASSERT_EQ(ArgList.size(), size_t(3));
  ASSERT_EQ(OS.str().str(), "x: foo, y: foo, z: foo");
}

#pragma mark - function-call-expression

TEST(ExprSyntaxTests, FunctionCallExprGetAPIs) {
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto ArgList = getFullArgumentList();
  auto RightParen = SyntaxFactory::makeRightParenToken({}, {});

  auto Call = SyntaxFactory::makeFunctionCallExpr(SymbolicRef, LeftParen,
                                                  ArgList, RightParen, None);

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
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto ArgList = getFullArgumentList();
  auto RightParen = SyntaxFactory::makeRightParenToken({}, {});

  {
    auto Call = SyntaxFactory::makeFunctionCallExpr(SymbolicRef, LeftParen,
                                                    ArgList, RightParen, None);
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Call.print(OS);
    ASSERT_EQ(OS.str().str(), "foo(x: foo, y: foo, z: foo)");
  }

  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionCallExpr().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

TEST(ExprSyntaxTests, FunctionCallExprWithAPIs) {
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto ArgList = getFullArgumentList();
  auto RightParen = SyntaxFactory::makeRightParenToken({}, {});

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionCallExpr()
      .withCalledExpression(SymbolicRef)
      .withLeftParen(LeftParen)
      .withRightParen(RightParen)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "foo()");
  }
  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionCallExpr()
      .withCalledExpression(SymbolicRef)
      .withLeftParen(LeftParen)
      .withArgumentList(getLabellessArgumentList())
      .withRightParen(RightParen)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "foo(1, 2, 3)");
  }
}

TEST(ExprSyntaxTests, FunctionCallExprBuilderAPIs) {
  FunctionCallExprSyntaxBuilder CallBuilder;

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto RightParen = SyntaxFactory::makeRightParenToken({}, {});

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.useLeftParen(LeftParen);
    CallBuilder.useRightParen(RightParen);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "()");
  }

  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "");
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", {}, {});
  auto TwoDigits = SyntaxFactory::makeIntegerLiteral("2", {}, {});
  auto ThreeDigits = SyntaxFactory::makeIntegerLiteral("3", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(OneDigits);
  auto NoLabel = TokenSyntax::missingToken(tok::identifier, "");
  auto NoColon = TokenSyntax::missingToken(tok::colon, ":");
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",");
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.useCalledExpression(SymbolicRef);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "foo()");
  }

  auto OneArg = SyntaxFactory::makeTupleExprElement(NoLabel, NoColon, One,
                                                        Comma);
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
