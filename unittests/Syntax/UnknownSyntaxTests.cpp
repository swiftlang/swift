#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/GenericSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/UnknownSyntax.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using llvm::None;
using llvm::SmallString;

using namespace swift;
using namespace swift::syntax;

SymbolicReferenceExprSyntax getCannedSymbolicRef() {
  // First, make a symbolic reference to an 'Array<Int>'
  auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
  auto IntType = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
  GenericArgumentClauseBuilder ArgBuilder;
  ArgBuilder
    .useLeftAngleBracket(SyntaxFactory::makeLeftAngleToken({}, {}))
    .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
    .addGenericArgument(llvm::None, IntType);

  return SyntaxFactory::makeSymbolicReferenceExpr(Array, ArgBuilder.build());
}

FunctionCallExprSyntax getCannedFunctionCall() {
  auto LParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto RParen = SyntaxFactory::makeRightParenToken({}, {});

  auto Label = SyntaxFactory::makeIdentifier("elements", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "");
  auto One = SyntaxFactory::makeIntegerLiteralExpr(NoSign, OneDigits);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",");

  auto Arg = SyntaxFactory::makeFunctionCallArgument(Label, Colon, One,
                                                     NoComma);
  auto Args = SyntaxFactory::makeFunctionCallArgumentList({ Arg });

  return SyntaxFactory::makeFunctionCallExpr(getCannedSymbolicRef(), LParen,
                                             Args, RParen);
}

TEST(UnknownSyntaxTests, UnknownSyntaxMakeAPIs) {
  {
    auto SymbolicRef = getCannedSymbolicRef();

    // Print the known symbolic reference. It should print as "Array<Int>".
    SmallString<48> KnownScratch;
    llvm::raw_svector_ostream KnownOS(KnownScratch);
    SymbolicRef.print(KnownOS);
    ASSERT_EQ(KnownOS.str().str(), "Array<Int>");

    // Wrap that symbolic reference as an UnknownSyntax. This has the same
    // RawSyntax layout but with the Unknown Kind.
    auto UnknownSymbolicRefData = UnknownSyntaxData::make(SymbolicRef.getRaw());

    auto Unknown = UnknownSyntax {
      UnknownSymbolicRefData,
      UnknownSymbolicRefData.get()
    };

    // Print the unknown syntax. It should also print as "Array<Int>".
    SmallString<48> UnknownScratch;
    llvm::raw_svector_ostream UnknownOS(UnknownScratch);
    Unknown.print(UnknownOS);

    ASSERT_EQ(KnownOS.str().str(), UnknownOS.str().str());
  }
}

TEST(UnknownSyntaxTests, UnknownSyntaxGetAPIs) {
  auto Call = getCannedFunctionCall();

  // Function call child 0 -> layout child 0 -> called expression
  {
    // Pull the called expression from the function call, which should print
    // as "Array<Int>"
    SmallString<48> KnownScratch;
    llvm::raw_svector_ostream KnownOS(KnownScratch);
    auto GottenExpr = Call.getCalledExpression();
    GottenExpr.print(KnownOS);

    // Wrap that call as an UnknownExprSyntax. This has the same
    // RawSyntax layout but with the UnknownExpr Kind.
    auto UnknownCallData = UnknownExprSyntaxData::make(Call.getRaw());

    auto Unknown = UnknownExprSyntax {
      UnknownCallData,
      UnknownCallData.get()
    };

    ASSERT_EQ(Unknown.getNumChildren(), size_t(2));

    // Get the second child from the unknown call, which is the argument list.
    // This should print the same as the known one: "elements: 1"
    SmallString<48> UnknownScratch;
    llvm::raw_svector_ostream UnknownOS(UnknownScratch);
    auto ExprGottenFromUnknown = Unknown.getChild(0)
      .castTo<SymbolicReferenceExprSyntax>();
    ExprGottenFromUnknown.print(UnknownOS);

    ASSERT_EQ(KnownOS.str().str(), UnknownOS.str().str());

    auto ExprGottenFromUnknown2 = Unknown.getChild(0);
    ASSERT_TRUE(ExprGottenFromUnknown
                .hasSameIdentityAs(ExprGottenFromUnknown2));
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
    auto UnknownCallData = UnknownSyntaxData::make(Call.getRaw());

    auto Unknown = UnknownSyntax {
      UnknownCallData,
      UnknownCallData.get()
    };

    ASSERT_EQ(Unknown.getNumChildren(), size_t(2));

    // Get the second child from the unknown call, which is the argument list.
    // This should print the same as the known one: "elements: 1"
    SmallString<48> UnknownScratch;
    llvm::raw_svector_ostream UnknownOS(UnknownScratch);
    auto ArgsGottenFromUnknown = Unknown.getChild(1)
      .castTo<FunctionCallArgumentListSyntax>();
    ArgsGottenFromUnknown.print(UnknownOS);

    ASSERT_EQ(KnownOS.str().str(), UnknownOS.str().str());

    auto ArgsGottenFromUnknown2 = Unknown.getChild(1);
    ASSERT_TRUE(ArgsGottenFromUnknown
                  .hasSameIdentityAs(ArgsGottenFromUnknown2));
  }
}

TEST(UnknownSyntaxTests, EmbedUnknownExpr) {
  auto SymbolicRef = getCannedSymbolicRef();
  auto LParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto RParen = SyntaxFactory::makeRightParenToken({}, {});
  auto EmptyArgs = SyntaxFactory::makeBlankFunctionCallArgumentList();

  SmallString<48> KnownScratch;
  llvm::raw_svector_ostream KnownOS(KnownScratch);
  auto CallWithKnownExpr = SyntaxFactory::makeFunctionCallExpr(SymbolicRef,
                                                               LParen,
                                                               EmptyArgs,
                                                               RParen);
  CallWithKnownExpr.print(KnownOS);

  // Let's make a function call expression where the called expression is
  // actually unknown. It should print the same and have the same structure
  // as one with a known called expression.
  auto UnknownSymbolicRefData =
    UnknownExprSyntaxData::make(SymbolicRef.getRaw());

  auto UnknownSymbolicRef = UnknownExprSyntax {
    UnknownSymbolicRefData,
    UnknownSymbolicRefData.get()
  }.castTo<ExprSyntax>();

  SmallString<48> UnknownScratch;
  llvm::raw_svector_ostream UnknownOS(UnknownScratch);

  auto CallWithUnknownExpr = CallWithKnownExpr
    .withCalledExpression(UnknownSymbolicRef);

  CallWithUnknownExpr.print(UnknownOS);

  ASSERT_EQ(KnownOS.str().str(), UnknownOS.str().str());
}

TEST(UnknownSyntaxTests, EmbedUnknownDecl) {
  // TODO
}

TEST(UnknownSyntaxTests, EmbedUnknownStmt) {
  // TODO
}

