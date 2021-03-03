#include "swift/Syntax/SyntaxFactory.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - fallthrough-statement

TEST(StmtSyntaxTests, FallthroughStmtGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  llvm::SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);

  auto FallthroughKW = SyntaxFactory::makeFallthroughKeyword("", "", Arena);

  auto Fallthrough =
      SyntaxFactory::makeBlankFallthroughStmt(Arena).withFallthroughKeyword(
          FallthroughKW);

  /// This should be directly shared through reference-counting.
  ASSERT_EQ(FallthroughKW.getRaw(), Fallthrough.getFallthroughKeyword()
                                               .getRaw());
}

TEST(StmtSyntaxTests, FallthroughStmtWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  llvm::SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);

  auto FallthroughKW = SyntaxFactory::makeFallthroughKeyword("", "", Arena);

  SyntaxFactory::makeBlankFallthroughStmt(Arena)
      .withFallthroughKeyword(FallthroughKW)
      .print(OS);

  ASSERT_EQ(OS.str().str(), "fallthrough");
}

TEST(StmtSyntaxTests, FallthroughStmtMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto FallthroughKW = SyntaxFactory::makeFallthroughKeyword("", "", Arena);

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    SyntaxFactory::makeFallthroughStmt(FallthroughKW, Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "fallthrough");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto NewFallthroughKW = FallthroughKW.withLeadingTrivia("  ");

    SyntaxFactory::makeFallthroughStmt(NewFallthroughKW, Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "  fallthrough");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto NewFallthroughKW =
        FallthroughKW.withLeadingTrivia("  ").withTrailingTrivia("  ");

    SyntaxFactory::makeFallthroughStmt(NewFallthroughKW, Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "  fallthrough  ");
  }

  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    SyntaxFactory::makeBlankFallthroughStmt(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

#pragma mark - break-statement

TEST(StmtSyntaxTests, BreakStmtGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto BreakKW = SyntaxFactory::makeBreakKeyword("", " ", Arena);
  auto Label =
      SyntaxFactory::makeIdentifier("sometimesYouNeedTo", "", "", Arena);
  auto Break = SyntaxFactory::makeBreakStmt(BreakKW, Label, Arena);

  /// These should be directly shared through reference-counting.
  ASSERT_EQ(BreakKW.getRaw(), Break.getBreakKeyword().getRaw());
  ASSERT_EQ(Label.getRaw(), Break.getLabel()->getRaw());
}

TEST(StmtSyntaxTests, BreakStmtWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto BreakKW = SyntaxFactory::makeBreakKeyword("", "", Arena);
  auto Label = SyntaxFactory::makeIdentifier("theRules", "", "", Arena);

  auto Break = SyntaxFactory::makeBlankBreakStmt(Arena);

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
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto BreakKW = SyntaxFactory::makeBreakKeyword("", " ", Arena);
    auto Label = SyntaxFactory::makeIdentifier("theBuild", "", "", Arena);
    auto Break = SyntaxFactory::makeBreakStmt(BreakKW, Label, Arena);
    Break.print(OS);
    ASSERT_EQ(OS.str().str(), "break theBuild"); // don't you dare
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankBreakStmt(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

#pragma mark - continue-statement

TEST(StmtSyntaxTests, ContinueStmtGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto ContinueKW = SyntaxFactory::makeContinueKeyword("", " ", Arena);
  auto Label = SyntaxFactory::makeIdentifier("always", "", "", Arena);
  auto Continue = SyntaxFactory::makeContinueStmt(ContinueKW, Label, Arena);

  /// These should be directly shared through reference-counting.
  ASSERT_EQ(ContinueKW.getRaw(), Continue.getContinueKeyword().getRaw());
  ASSERT_EQ(Label.getRaw(), Continue.getLabel()->getRaw());
}

TEST(StmtSyntaxTests, ContinueStmtWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto ContinueKW = SyntaxFactory::makeContinueKeyword("", "", Arena);
  auto Label = SyntaxFactory::makeIdentifier("toCare", "", "", Arena);
  auto Continue = SyntaxFactory::makeBlankContinueStmt(Arena);

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
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto ContinueKW = SyntaxFactory::makeContinueKeyword("", " ", Arena);
    auto Label = SyntaxFactory::makeIdentifier("toLead", "", "", Arena);
    auto Continue = SyntaxFactory::makeContinueStmt(ContinueKW, Label, Arena);
    Continue.print(OS);
    ASSERT_EQ(OS.str().str(), "continue toLead"); // by example
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankContinueStmt(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

#pragma mark - return-statement

TEST(StmtSyntaxTests, ReturnStmtMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto ReturnKW = SyntaxFactory::makeReturnKeyword("", " ", Arena);
  auto Minus = SyntaxFactory::makePrefixOperator("-", "", "", Arena);
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", "", "", Arena);
  auto MinusOne = SyntaxFactory::makePrefixOperatorExpr(
      Minus, SyntaxFactory::makeIntegerLiteralExpr(OneDigits, Arena), Arena);

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankReturnStmt(Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeReturnStmt(ReturnKW, MinusOne, Arena).print(OS);
    ASSERT_EQ(OS.str().str(), "return -1");
  }
}

TEST(StmtSyntaxTests, ReturnStmtGetAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto ReturnKW = SyntaxFactory::makeReturnKeyword("", " ", Arena);
  auto Minus = SyntaxFactory::makePrefixOperator("-", "", "", Arena);
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", "", "", Arena);
  auto MinusOne = SyntaxFactory::makePrefixOperatorExpr(
      Minus, SyntaxFactory::makeIntegerLiteralExpr(OneDigits, Arena), Arena);
  auto Return = SyntaxFactory::makeReturnStmt(ReturnKW, MinusOne, Arena);

  ASSERT_EQ(ReturnKW.getRaw(), Return.getReturnKeyword().getRaw());
  auto GottenExpression = Return.getExpression().getValue();
  auto GottenExpression2 = Return.getExpression().getValue();
  ASSERT_TRUE(GottenExpression.hasSameIdentityAs(GottenExpression2));
}

TEST(StmtSyntaxTests, ReturnStmtWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto ReturnKW = SyntaxFactory::makeReturnKeyword("", " ", Arena);
  auto Minus = SyntaxFactory::makePrefixOperator("-", "", "", Arena);
  auto OneDigits = SyntaxFactory::makeIntegerLiteral("1", "", "", Arena);
  auto MinusOne = SyntaxFactory::makePrefixOperatorExpr(
      Minus, SyntaxFactory::makeIntegerLiteralExpr(OneDigits, Arena), Arena);

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankReturnStmt(Arena).withReturnKeyword(ReturnKW).print(
        OS);
    ASSERT_EQ(OS.str().str(), "return ");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankReturnStmt(Arena).withExpression(MinusOne).print(
        OS);
    ASSERT_EQ(OS.str().str(), "-1");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankReturnStmt(Arena)
        .withReturnKeyword(ReturnKW)
        .withExpression(MinusOne)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "return -1");
  }
}

