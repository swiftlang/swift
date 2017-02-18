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
