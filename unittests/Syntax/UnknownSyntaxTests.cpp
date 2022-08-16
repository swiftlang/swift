#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/SyntaxBuilders.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using llvm::None;
using llvm::SmallString;

using namespace swift;
using namespace swift::syntax;


SymbolicReferenceExprSyntax getCannedSymbolicRef(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  // First, make a symbolic reference to an 'Array<Int>'
  auto Array = Factory.makeIdentifier("Array", {}, {});
  auto Int = Factory.makeIdentifier("Int", {}, {});
  auto IntType = Factory.makeSimpleTypeIdentifier(/*UnexpectedNodes=*/None, Int,
                                                  /*UnexpectedNodes=*/None, None);
  auto IntArg = Factory.makeGenericArgument(/*UnexpectedNodes=*/None, IntType,
                                            /*UnexpectedNodes=*/None, None);
  GenericArgumentClauseSyntaxBuilder ArgBuilder(Arena);
  ArgBuilder.useLeftAngleBracket(Factory.makeLeftAngleToken({}, {}))
      .useRightAngleBracket(Factory.makeRightAngleToken({}, {}))
      .addArgument(IntArg);

  return Factory.makeSymbolicReferenceExpr(
      /*UnexpectedNodes=*/None, Array, /*UnexpectedNodes=*/None, ArgBuilder.build());
}

FunctionCallExprSyntax getCannedFunctionCall(const RC<SyntaxArena> &Arena) {
  SyntaxFactory Factory(Arena);
  auto LParen = Factory.makeLeftParenToken({}, {});
  auto RParen = Factory.makeRightParenToken({}, {});

  auto Label = Factory.makeIdentifier("elements", {}, {});
  auto Colon = Factory.makeColonToken({}, " ");
  auto OneDigits = Factory.makeIntegerLiteral("1", {}, {});
  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "", Arena);
  auto OneLiteral =
      Factory.makeIntegerLiteralExpr(/*UnexpectedNodes=*/None, OneDigits);
  auto One = Factory.makePrefixOperatorExpr(/*UnexpectedNodes=*/None, NoSign,
                                            /*UnexpectedNodes=*/None, OneLiteral);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);

  auto Arg = Factory.makeTupleExprElement(
      /*UnexpectedNodes=*/None, Label, /*UnexpectedNodes=*/None, Colon,
      /*UnexpectedNodes=*/None, One, /*UnexpectedNodes=*/None, NoComma);
  auto Args = Factory.makeTupleExprElementList({Arg});

  return Factory.makeFunctionCallExpr(
      /*UnexpectedNodes=*/None, getCannedSymbolicRef(Arena), /*UnexpectedNodes=*/None,
      LParen, /*UnexpectedNodes=*/None, Args,
      /*UnexpectedNodes=*/None, RParen, /*UnexpectedNodes=*/None, None,
      /*UnexpectedNodes=*/None, None);
}

TEST(UnknownSyntaxTests, UnknownSyntaxMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    auto SymbolicRef = getCannedSymbolicRef(Arena);

    // Print the known symbolic reference. It should print as "Array<Int>".
    SmallString<48> KnownScratch;
    llvm::raw_svector_ostream KnownOS(KnownScratch);
    SymbolicRef.print(KnownOS);
    ASSERT_EQ(KnownOS.str().str(), "Array<Int>");

    // Wrap that symbolic reference as an UnknownSyntax. This has the same
    // RawSyntax layout but with the Unknown Kind.
    auto Unknown = makeRoot<UnknownSyntax>(SymbolicRef.getRaw());

    // Print the unknown syntax. It should also print as "Array<Int>".
    SmallString<48> UnknownScratch;
    llvm::raw_svector_ostream UnknownOS(UnknownScratch);
    Unknown.print(UnknownOS);

    ASSERT_EQ(KnownOS.str().str(), UnknownOS.str().str());
  }
}

TEST(UnknownSyntaxTests, UnknownSyntaxGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Call = getCannedFunctionCall(Arena);

  // Function call child 0 -> layout child 0 -> called expression
  {
    // Pull the called expression from the function call, which should print
    // as "Array<Int>"
    SmallString<48> KnownScratch;
    llvm::raw_svector_ostream KnownOS(KnownScratch);
    auto GottenExpr = Call.getCalledExpression();
    GottenExpr.print(KnownOS);

    // Wrap that call as an UnknownExprSyntax. This has the same
    // RawSyntax layout but with the UnknownExpr Kind.;
    auto Unknown = makeRoot<UnknownExprSyntax>(Call.getRaw());

    ASSERT_EQ(Unknown.getNumChildren(), size_t(12));

    // Get the second child from the unknown call, which is the argument list.
    // This should print the same as the known one: "elements: 1"
    SmallString<48> UnknownScratch;
    llvm::raw_svector_ostream UnknownOS(UnknownScratch);
    auto ExprGottenFromUnknown = Unknown.getChild(1)
      ->castTo<SymbolicReferenceExprSyntax>();
    ExprGottenFromUnknown.print(UnknownOS);

    ASSERT_EQ(KnownOS.str().str(), UnknownOS.str().str());

    auto ExprGottenFromUnknown2 = Unknown.getChild(1);
    ASSERT_TRUE(ExprGottenFromUnknown
                .hasSameIdentityAs(*ExprGottenFromUnknown2));
  }

  // Function call child 1 -> layout child 2 -> function call argument list
  {
    // Pull the argument list from the function call, which should print
    // as "elements: 1"
    SmallString<48> KnownScratch;
    llvm::raw_svector_ostream KnownOS(KnownScratch);
    auto GottenArgs = Call.getArgumentList();
    GottenArgs.print(KnownOS);

    // Wrap that symbolic reference as an UnknownSyntax. This has the same
    // RawSyntax layout but with the Unknown Kind.
    auto Unknown = makeRoot<UnknownSyntax>(Call.getRaw());

    ASSERT_EQ(Unknown.getNumChildren(), size_t(12));

    // Get the third child from the unknown call, which is the argument list.
    // This should print the same as the known one: "elements: 1"
    SmallString<48> UnknownScratch;
    llvm::raw_svector_ostream UnknownOS(UnknownScratch);
    auto ArgsGottenFromUnknown = Unknown.getChild(5)
      ->castTo<TupleExprElementListSyntax>();
    ArgsGottenFromUnknown.print(UnknownOS);

    ASSERT_EQ(KnownOS.str().str(), UnknownOS.str().str());

    auto ArgsGottenFromUnknown2 = Unknown.getChild(5);
    ASSERT_TRUE(ArgsGottenFromUnknown
                  .hasSameIdentityAs(*ArgsGottenFromUnknown2));
  }
}

TEST(UnknownSyntaxTests, EmbedUnknownExpr) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto SymbolicRef = getCannedSymbolicRef(Arena);
  auto LParen = Factory.makeLeftParenToken({}, {});
  auto RParen = Factory.makeRightParenToken({}, {});
  auto EmptyArgs = Factory.makeBlankTupleExprElementList();

  SmallString<48> KnownScratch;
  llvm::raw_svector_ostream KnownOS(KnownScratch);
  auto CallWithKnownExpr = Factory.makeFunctionCallExpr(
      /*UnexpectedNodes=*/None, SymbolicRef, /*UnexpectedNodes=*/None, LParen,
      /*UnexpectedNodes=*/None, EmptyArgs, /*UnexpectedNodes=*/None, RParen,
      /*UnexpectedNodes=*/None, None, /*UnexpectedNodes=*/None, None);
  CallWithKnownExpr.print(KnownOS);

  // Let's make a function call expression where the called expression is
  // actually unknown. It should print the same and have the same structure
  // as one with a known called expression.
  auto UnknownSymbolicRef = makeRoot<UnknownExprSyntax>(SymbolicRef.getRaw())
                              .castTo<ExprSyntax>();

  SmallString<48> UnknownScratch;
  llvm::raw_svector_ostream UnknownOS(UnknownScratch);

  auto CallWithUnknownExpr = CallWithKnownExpr
    .withCalledExpression(UnknownSymbolicRef);

  CallWithUnknownExpr.print(UnknownOS);

  ASSERT_EQ(KnownOS.str().str(), UnknownOS.str().str());
}

