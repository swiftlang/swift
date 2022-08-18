#include "swift/Syntax/SyntaxFactory.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - fallthrough-statement

TEST(StmtSyntaxTests, FallthroughStmtGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  llvm::SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);

  auto FallthroughKW = Factory.makeFallthroughKeyword("", "");

  auto Fallthrough =
      Factory.makeBlankFallthroughStmt().withFallthroughKeyword(FallthroughKW);

  /// This should be directly shared through reference-counting.
  ASSERT_EQ(FallthroughKW.getRaw(), Fallthrough.getFallthroughKeyword()
                                               .getRaw());
}

TEST(StmtSyntaxTests, FallthroughStmtWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  llvm::SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);

  auto FallthroughKW = Factory.makeFallthroughKeyword("", "");

  Factory.makeBlankFallthroughStmt()
      .withFallthroughKeyword(FallthroughKW)
      .print(OS);

  ASSERT_EQ(OS.str().str(), "fallthrough");
}

TEST(StmtSyntaxTests, FallthroughStmtMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto FallthroughKW = Factory.makeFallthroughKeyword("", "");

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    Factory.makeFallthroughStmt(/*UnexpectedNodes=*/None, FallthroughKW).print(OS);
    ASSERT_EQ(OS.str().str(), "fallthrough");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto NewFallthroughKW = FallthroughKW.withLeadingTrivia("  ");

    Factory.makeFallthroughStmt(/*UnexpectedNodes=*/None, NewFallthroughKW)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "  fallthrough");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto NewFallthroughKW =
        FallthroughKW.withLeadingTrivia("  ").withTrailingTrivia("  ");

    Factory.makeFallthroughStmt(/*UnexpectedNodes=*/None, NewFallthroughKW)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "  fallthrough  ");
  }

  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    Factory.makeBlankFallthroughStmt().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

#pragma mark - break-statement

TEST(StmtSyntaxTests, BreakStmtGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto BreakKW = Factory.makeBreakKeyword("", " ");
  auto Label = Factory.makeIdentifier("sometimesYouNeedTo", "", "");
  auto Break = Factory.makeBreakStmt(/*UnexpectedNodes=*/None, BreakKW,
                                     /*UnexpectedNodes=*/None, Label);

  /// These should be directly shared through reference-counting.
  ASSERT_EQ(BreakKW.getRaw(), Break.getBreakKeyword().getRaw());
  ASSERT_EQ(Label.getRaw(), Break.getLabel()->getRaw());
}

TEST(StmtSyntaxTests, BreakStmtWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto BreakKW = Factory.makeBreakKeyword("", "");
  auto Label = Factory.makeIdentifier("theRules", "", "");

  auto Break = Factory.makeBlankBreakStmt();

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
    Break.withBreakKeyword(BreakKW.withTrailingTrivia(" "))
        .withLabel(Label)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "break theRules"); // sometimes
  }
}

TEST(StmtSyntaxTests, BreakStmtMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto BreakKW = Factory.makeBreakKeyword("", " ");
    auto Label = Factory.makeIdentifier("theBuild", "", "");
    auto Break = Factory.makeBreakStmt(/*UnexpectedNodes=*/None, BreakKW,
                                       /*UnexpectedNodes=*/None, Label);
    Break.print(OS);
    ASSERT_EQ(OS.str().str(), "break theBuild"); // don't you dare
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankBreakStmt().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

#pragma mark - continue-statement

TEST(StmtSyntaxTests, ContinueStmtGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto ContinueKW = Factory.makeContinueKeyword("", " ");
  auto Label = Factory.makeIdentifier("always", "", "");
  auto Continue = Factory.makeContinueStmt(/*UnexpectedNodes=*/None, ContinueKW,
                                           /*UnexpectedNodes=*/None, Label);

  /// These should be directly shared through reference-counting.
  ASSERT_EQ(ContinueKW.getRaw(), Continue.getContinueKeyword().getRaw());
  ASSERT_EQ(Label.getRaw(), Continue.getLabel()->getRaw());
}

TEST(StmtSyntaxTests, ContinueStmtWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto ContinueKW = Factory.makeContinueKeyword("", "");
  auto Label = Factory.makeIdentifier("toCare", "", "");
  auto Continue = Factory.makeBlankContinueStmt();

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
    Continue.withContinueKeyword(ContinueKW.withTrailingTrivia(" "))
        .withLabel(Label)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "continue toCare"); // for each other
  }
}

TEST(StmtSyntaxTests, ContinueStmtMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto ContinueKW = Factory.makeContinueKeyword("", " ");
    auto Label = Factory.makeIdentifier("toLead", "", "");
    auto Continue = Factory.makeContinueStmt(/*UnexpectedNodes=*/None, ContinueKW,
                                             /*UnexpectedNodes=*/None, Label);
    Continue.print(OS);
    ASSERT_EQ(OS.str().str(), "continue toLead"); // by example
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankContinueStmt().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

#pragma mark - return-statement

TEST(StmtSyntaxTests, ReturnStmtMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto ReturnKW = Factory.makeReturnKeyword("", " ");
  auto Minus = Factory.makePrefixOperator("-", "", "");
  auto OneDigits = Factory.makeIntegerLiteral("1", "", "");
  auto OneLiteral =
      Factory.makeIntegerLiteralExpr(/*UnexpectedNodes=*/None, OneDigits);
  auto MinusOne = Factory.makePrefixOperatorExpr(
      /*UnexpectedNodes=*/None, Minus, /*UnexpectedNodes=*/None, OneLiteral);

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankReturnStmt().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory
        .makeReturnStmt(/*UnexpectedNodes=*/None, ReturnKW, /*UnexpectedNodes=*/None,
                        MinusOne)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "return -1");
  }
}

TEST(StmtSyntaxTests, ReturnStmtGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto ReturnKW = Factory.makeReturnKeyword("", " ");
  auto Minus = Factory.makePrefixOperator("-", "", "");
  auto OneDigits = Factory.makeIntegerLiteral("1", "", "");
  auto OneLiteral =
      Factory.makeIntegerLiteralExpr(/*UnexpectedNodes=*/None, OneDigits);
  auto MinusOne = Factory.makePrefixOperatorExpr(
      /*UnexpectedNodes=*/None, Minus, /*UnexpectedNodes=*/None, OneLiteral);
  auto Return = Factory.makeReturnStmt(/*UnexpectedNodes=*/None, ReturnKW,
                                       /*UnexpectedNodes=*/None, MinusOne);

  ASSERT_EQ(ReturnKW.getRaw(), Return.getReturnKeyword().getRaw());
  auto GottenExpression = Return.getExpression().getValue();
  auto GottenExpression2 = Return.getExpression().getValue();
  ASSERT_TRUE(GottenExpression.hasSameIdentityAs(GottenExpression2));
}

TEST(StmtSyntaxTests, ReturnStmtWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto ReturnKW = Factory.makeReturnKeyword("", " ");
  auto Minus = Factory.makePrefixOperator("-", "", "");
  auto OneDigits = Factory.makeIntegerLiteral("1", "", "");
  auto OneLiteral =
      Factory.makeIntegerLiteralExpr(/*UnexpectedNodes=*/None, OneDigits);
  auto MinusOne = Factory.makePrefixOperatorExpr(
      /*UnexpectedNodes=*/None, Minus, /*UnexpectedNodes=*/None, OneLiteral);

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankReturnStmt().withReturnKeyword(ReturnKW).print(OS);
    ASSERT_EQ(OS.str().str(), "return ");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankReturnStmt().withExpression(MinusOne).print(OS);
    ASSERT_EQ(OS.str().str(), "-1");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankReturnStmt()
        .withReturnKeyword(ReturnKW)
        .withExpression(MinusOne)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "return -1");
  }
}

