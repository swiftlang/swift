#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - integer-literal-expression

// TODO

#pragma mark - symbolic-reference

TEST(ExprSyntaxTests, SymbolicReferenceExprGetAPIs) {
  {
    auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
    auto IntType = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    GenericArgumentClauseBuilder ArgBuilder;
    ArgBuilder
      .useLeftAngleBracket(SyntaxFactory::makeLeftAngleToken({}, {}))
      .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
      .addGenericArgument(llvm::None, IntType);

    auto GenericArgs = ArgBuilder.build();

    auto Ref = SyntaxFactory::makeSymbolicReferenceExpr(Array, GenericArgs);

    ASSERT_EQ(Ref.getIdentifier(), Array);

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
  auto IntType = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
  GenericArgumentClauseBuilder ArgBuilder;
  ArgBuilder
    .useLeftAngleBracket(SyntaxFactory::makeLeftAngleToken({}, {}))
    .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
    .addGenericArgument(llvm::None, IntType);
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
  auto IntType = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
  GenericArgumentClauseBuilder ArgBuilder;
  ArgBuilder
    .useLeftAngleBracket(SyntaxFactory::makeLeftAngleToken({}, {}))
    .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
    .addGenericArgument(llvm::None, IntType);
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

TEST(ExprSyntaxTests, FunctionCallArgumentGetAPIs) {
  auto X = SyntaxFactory::makeIdentifier("x", {}, {});
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));

  {
    auto Arg = SyntaxFactory::makeFunctionCallArgument(X, Colon, SymbolicRef,
                                                       Comma);

    ASSERT_EQ(X, Arg.getLabel());
    ASSERT_EQ(Colon, Arg.getColonToken());

    auto GottenExpr = Arg.getExpression().getValue();
    auto GottenExpr2 = Arg.getExpression().getValue();
    ASSERT_TRUE(GottenExpr.hasSameIdentityAs(GottenExpr2));
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    GottenExpr.print(OS);
    ASSERT_EQ("foo", OS.str().str());

    ASSERT_EQ(Comma, Arg.getTrailingComma());
  }
}

TEST(ExprSyntaxTests, FunctionCallArgumentMakeAPIs) {
  auto X = SyntaxFactory::makeIdentifier("x", {}, {});
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionCallArgument().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionCallArgument()
      .withExpression(SymbolicRef).print(OS);
    ASSERT_EQ(OS.str().str(), "foo");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeFunctionCallArgument(X, Colon, SymbolicRef, Comma)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, ");
  }
}

TEST(ExprSyntaxTests, FunctionCallArgumentWithAPIs) {
  auto X = SyntaxFactory::makeIdentifier("x", {}, {});
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionCallArgument()
      .withLabel(X)
      .withColonToken(Colon)
      .withExpression(SymbolicRef)
      .withTrailingComma(Comma)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, ");
  }
}

#pragma mark - function-call-argument-list

namespace {
FunctionCallArgumentListSyntax getFullArgumentList() {
  auto X = SyntaxFactory::makeIdentifier("x", {}, {});
  auto Y = SyntaxFactory::makeIdentifier("y", {}, {});
  auto Z = SyntaxFactory::makeIdentifier("z", {}, {});
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",");

  auto Arg = SyntaxFactory::makeFunctionCallArgument(X, Colon, SymbolicRef,
                                                     Comma);

  return SyntaxFactory::makeBlankFunctionCallArgumentList()
    .withAdditionalArgument(Arg)
    .withAdditionalArgument(Arg.withLabel(Y))
    .withAdditionalArgument(Arg.withLabel(Z).withTrailingComma(NoComma));
}

FunctionCallArgumentListSyntax getLabellessArgumentList() {
  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "");
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto TwoDigits = SyntaxFactory::makeIntegerLiteralToken("2", {}, {});
  auto ThreeDigits = SyntaxFactory::makeIntegerLiteralToken("3", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(NoSign, OneDigits);
  auto NoLabel = TokenSyntax::missingToken(tok::identifier, "");
  auto NoColon = TokenSyntax::missingToken(tok::colon, ":");
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",");
  auto Two = SyntaxFactory::makeIntegerLiteralExpr(NoSign, TwoDigits);
  auto Three = SyntaxFactory::makeIntegerLiteralExpr(NoSign, ThreeDigits);

  auto OneArg = SyntaxFactory::makeFunctionCallArgument(NoLabel, NoColon, One,
                                                        Comma);
  auto TwoArg = SyntaxFactory::makeFunctionCallArgument(NoLabel, NoColon, Two,
                                                        Comma);
  auto ThreeArg = SyntaxFactory::makeFunctionCallArgument(NoLabel, NoColon,
                                                          Three, NoComma);

  return SyntaxFactory::makeBlankFunctionCallArgumentList()
    .withAdditionalArgument(OneArg)
    .withAdditionalArgument(TwoArg)
    .withAdditionalArgument(ThreeArg);
}
}

TEST(ExprSyntaxTests, FunctionCallArgumentListGetAPIs) {
  auto X = SyntaxFactory::makeIdentifier("x", {}, {});
  auto Y = SyntaxFactory::makeIdentifier("y", {}, {});
  auto Z = SyntaxFactory::makeIdentifier("z", {}, {});
  auto Foo = SyntaxFactory::makeIdentifier("foo", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto SymbolicRef = SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",");

  auto Arg = SyntaxFactory::makeFunctionCallArgument(X, Colon, SymbolicRef,
                                                     Comma);

  auto ArgList = SyntaxFactory::makeBlankFunctionCallArgumentList()
    .withAdditionalArgument(Arg)
    .withAdditionalArgument(Arg.withLabel(Y))
    .withAdditionalArgument(Arg.withLabel(Z).withTrailingComma(NoComma));

  ASSERT_EQ(ArgList.getNumArguments(), size_t(3));

  {
    llvm::SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenArg1 = ArgList.getArgument(0);
    auto GottenArg1_2 = ArgList.getArgument(0);
    ASSERT_TRUE(GottenArg1.hasSameIdentityAs(GottenArg1_2));
    GottenArg1.print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, ");
  }

  {
    llvm::SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenArg2 = ArgList.getArgument(1);
    auto GottenArg2_2 = ArgList.getArgument(1);
    ASSERT_TRUE(GottenArg2.hasSameIdentityAs(GottenArg2_2));
    GottenArg2.print(OS);
    ASSERT_EQ(OS.str().str(), "y: foo, ");
  }

  {
    llvm::SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto GottenArg3 = ArgList.getArgument(2);
    auto GottenArg3_2 = ArgList.getArgument(2);
    ASSERT_TRUE(GottenArg3.hasSameIdentityAs(GottenArg3_2));
    GottenArg3.print(OS);
    ASSERT_EQ(OS.str().str(), "z: foo");
  }
}

TEST(ExprSyntaxTests, FunctionCallArgumentListMakeAPIs) {
  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionCallArgumentList().print(OS);
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

    auto Arg = SyntaxFactory::makeFunctionCallArgument(X, Colon, SymbolicRef,
                                                       Comma);

    std::vector<FunctionCallArgumentSyntax> Args {
      Arg, Arg.withLabel(Y), Arg.withLabel(Z).withTrailingComma(NoComma)
    };

    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto ArgList = SyntaxFactory::makeFunctionCallArgumentList(Args);
    ArgList.print(OS);
    ASSERT_EQ(ArgList.getNumArguments(), size_t(3));
    ASSERT_EQ(OS.str().str(), "x: foo, y: foo, z: foo");
  }
}

TEST(ExprSyntaxTests, FunctionCallArgumentListWithAPIs) {
  auto ArgList = getFullArgumentList();
  llvm::SmallString<64> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  ASSERT_EQ(ArgList.getNumArguments(), size_t(3));
  ArgList.print(OS);
  ASSERT_EQ(ArgList.getNumArguments(), size_t(3));
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
                                                  ArgList, RightParen);

  {
    auto GottenExpression1 = Call.getCalledExpression();
    auto GottenExpression2 = Call.getCalledExpression();
    ASSERT_TRUE(GottenExpression1.hasSameIdentityAs(GottenExpression2));
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    GottenExpression1.print(OS);
    ASSERT_EQ(OS.str().str(), "foo");
  }

  ASSERT_EQ(LeftParen, Call.getLeftParen());
  ASSERT_EQ(RightParen, Call.getRightParen());

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
                                                    ArgList, RightParen);
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
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto TwoDigits = SyntaxFactory::makeIntegerLiteralToken("2", {}, {});
  auto ThreeDigits = SyntaxFactory::makeIntegerLiteralToken("3", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(NoSign, OneDigits);
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

  auto OneArg = SyntaxFactory::makeFunctionCallArgument(NoLabel, NoColon, One,
                                                        Comma);
  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.appendArgument(OneArg);
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "foo(1, )");
  }

  {
    llvm::SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    CallBuilder.appendArgument(OneArg.withTrailingComma(NoComma));
    CallBuilder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "foo(1, 1)");
  }
}
