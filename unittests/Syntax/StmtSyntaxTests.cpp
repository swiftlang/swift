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
  auto Label = SyntaxFactory::makeIdentifier("forCoffee", {}, {});
  auto Break = SyntaxFactory::makeBreakStmt(BreakKW, Label);

  ASSERT_EQ(BreakKW, Break.getBreakKeyword());
  ASSERT_EQ(Label, Break.getLabel());
}

TEST(StmtSyntaxTests, BreakStmtWithAPIs) {
  auto BreakKW = SyntaxFactory::makeBreakKeyword({}, {});
  auto Label = SyntaxFactory::makeIdentifier("forCoffee", {}, {});

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
    ASSERT_EQ(OS.str().str(), "forCoffee");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Break.withBreakKeyword(BreakKW->withTrailingTrivia(Trivia::spaces(1)))
      .withLabel(Label)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "break forCoffee");
  }
}

TEST(StmtSyntaxTests, BreakStmtMakeAPIs) {
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto BreakKW = SyntaxFactory::makeBreakKeyword({}, Trivia::spaces(1));
    auto Label = SyntaxFactory::makeIdentifier("forCoffee", {}, {});
    auto Break = SyntaxFactory::makeBreakStmt(BreakKW, Label);
    Break.print(OS);
    ASSERT_EQ(OS.str().str(), "break forCoffee");
  }
  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankBreakStmtSyntax().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}
