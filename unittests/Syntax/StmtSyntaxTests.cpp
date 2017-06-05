#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/StmtSyntax.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - fallthrough-statement

TEST(StmtSyntaxTests, FallthroughStmtGetAPIs) {
  llvm::SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);

  auto FallthroughKW = SyntaxFactory::makeFallthroughKeyword({}, {});

  auto Fallthrough = SyntaxFactory::makeBlankFallthroughStmt()
    .withFallthroughKeyword(FallthroughKW);

  /// This should be directly shared through reference-counting.
  ASSERT_EQ(FallthroughKW, Fallthrough.getFallthroughKeyword());
}

TEST(StmtSyntaxTests, FallthroughStmtWithAPIs) {
  llvm::SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);

  auto FallthroughKW = SyntaxFactory::makeFallthroughKeyword({}, {});

  SyntaxFactory::makeBlankFallthroughStmt()
    .withFallthroughKeyword(FallthroughKW)
    .print(OS);

  ASSERT_EQ(OS.str().str(), "fallthrough");
}

TEST(StmtSyntaxTests, FallthroughStmtMakeAPIs) {
  auto FallthroughKW = SyntaxFactory::makeFallthroughKeyword({}, {});

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    SyntaxFactory::makeFallthroughStmt(FallthroughKW).print(OS);
    ASSERT_EQ(OS.str().str(), "fallthrough");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    FallthroughKW = FallthroughKW->withLeadingTrivia(Trivia::spaces(2));

    SyntaxFactory::makeFallthroughStmt(FallthroughKW).print(OS);
    ASSERT_EQ(OS.str().str(), "  fallthrough");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    FallthroughKW = FallthroughKW->withTrailingTrivia(Trivia::spaces(2));

    SyntaxFactory::makeFallthroughStmt(FallthroughKW).print(OS);
    ASSERT_EQ(OS.str().str(), "  fallthrough  ");
  }

  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    SyntaxFactory::makeBlankFallthroughStmt().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

#pragma mark - break-statement

TEST(StmtSyntaxTests, BreakStmtGetAPIs) {
  auto BreakKW = SyntaxFactory::makeBreakKeyword({}, Trivia::spaces(1));
  auto Label = SyntaxFactory::makeIdentifier("sometimesYouNeedTo", {}, {});
  auto Break = SyntaxFactory::makeBreakStmt(BreakKW, Label);

  /// These should be directly shared through reference-counting.
  ASSERT_EQ(BreakKW, Break.getBreakKeyword());
  ASSERT_EQ(Label, Break.getLabel());
}

TEST(StmtSyntaxTests, BreakStmtWithAPIs) {
  auto BreakKW = SyntaxFactory::makeBreakKeyword({}, {});
  auto Label = SyntaxFactory::makeIdentifier("theRules", {}, {});

  auto Break = SyntaxFactory::makeBlankBreakStmtSyntax();

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Break.print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Break.withBreakKeyword(BreakKW)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "break");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Break.withLabel(Label).print(OS);
    ASSERT_EQ(OS.str().str(), "theRules");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Break.withBreakKeyword(BreakKW->withTrailingTrivia(Trivia::spaces(1)))
      .withLabel(Label)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "break theRules"); // sometimes
  }
}

TEST(StmtSyntaxTests, BreakStmtMakeAPIs) {
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto BreakKW = SyntaxFactory::makeBreakKeyword({}, Trivia::spaces(1));
    auto Label = SyntaxFactory::makeIdentifier("theBuild", {}, {});
    auto Break = SyntaxFactory::makeBreakStmt(BreakKW, Label);
    Break.print(OS);
    ASSERT_EQ(OS.str().str(), "break theBuild"); // don't you dare
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankBreakStmtSyntax().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

#pragma mark - continue-statement

TEST(StmtSyntaxTests, ContinueStmtGetAPIs) {
  auto ContinueKW = SyntaxFactory::makeContinueKeyword({}, Trivia::spaces(1));
  auto Label = SyntaxFactory::makeIdentifier("always", {}, {});
  auto Continue = SyntaxFactory::makeContinueStmt(ContinueKW, Label);

  /// These should be directly shared through reference-counting.
  ASSERT_EQ(ContinueKW, Continue.getContinueKeyword());
  ASSERT_EQ(Label, Continue.getLabel());
}

TEST(StmtSyntaxTests, ContinueStmtWithAPIs) {
  auto ContinueKW = SyntaxFactory::makeContinueKeyword({}, {});
  auto Label = SyntaxFactory::makeIdentifier("toCare", {}, {});
  auto Continue = SyntaxFactory::makeBlankContinueStmtSyntax();

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Continue.print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Continue.withContinueKeyword(ContinueKW)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "continue");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Continue.withLabel(Label).print(OS);
    ASSERT_EQ(OS.str().str(), "toCare");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Continue
      .withContinueKeyword(ContinueKW->withTrailingTrivia(Trivia::spaces(1)))
      .withLabel(Label)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "continue toCare"); // for each other
  }
}

TEST(StmtSyntaxTests, ContinueStmtMakeAPIs) {
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto ContinueKW = SyntaxFactory::makeContinueKeyword({}, Trivia::spaces(1));
    auto Label = SyntaxFactory::makeIdentifier("toLead", {}, {});
    auto Continue = SyntaxFactory::makeContinueStmt(ContinueKW, Label);
    Continue.print(OS);
    ASSERT_EQ(OS.str().str(), "continue toLead"); // by example
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankContinueStmtSyntax().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

#pragma mark - return-statement

TEST(StmtSyntaxTests, ReturnStmtMakeAPIs) {
  auto ReturnKW = SyntaxFactory::makeReturnKeyword({}, Trivia::spaces(1));
  auto Minus = SyntaxFactory::makePrefixOperator("-", {});
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto MinusOne = SyntaxFactory::makeIntegerLiteralExpr(Minus, OneDigits);

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankReturnStmt().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeReturnStmt(ReturnKW, MinusOne).print(OS);
    ASSERT_EQ(OS.str().str(), "return -1");
  }
}

TEST(StmtSyntaxTests, ReturnStmtGetAPIs) {
  auto ReturnKW = SyntaxFactory::makeReturnKeyword({}, Trivia::spaces(1));
  auto Minus = SyntaxFactory::makePrefixOperator("-", {});
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto MinusOne = SyntaxFactory::makeIntegerLiteralExpr(Minus, OneDigits);
  auto Return = SyntaxFactory::makeReturnStmt(ReturnKW, MinusOne);

  ASSERT_EQ(ReturnKW, Return.getReturnKeyword());
  auto GottenExpression = Return.getExpression().getValue();
  auto GottenExpression2 = Return.getExpression().getValue();
  ASSERT_TRUE(GottenExpression.hasSameIdentityAs(GottenExpression2));
}

TEST(StmtSyntaxTests, ReturnStmtWithAPIs) {
  auto ReturnKW = SyntaxFactory::makeReturnKeyword({}, Trivia::spaces(1));
  auto Minus = SyntaxFactory::makePrefixOperator("-", {});
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto MinusOne = SyntaxFactory::makeIntegerLiteralExpr(Minus, OneDigits);

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankReturnStmt().withReturnKeyword(ReturnKW).print(OS);
    ASSERT_EQ(OS.str().str(), "return ");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankReturnStmt().withExpression(MinusOne).print(OS);
    ASSERT_EQ(OS.str().str(), "-1");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankReturnStmt()
      .withReturnKeyword(ReturnKW)
      .withExpression(MinusOne).print(OS);
    ASSERT_EQ(OS.str().str(), "return -1");
  }
}

