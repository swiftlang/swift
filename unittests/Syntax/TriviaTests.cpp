#include "swift/Syntax/Trivia.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;

TEST(TriviaTests, Empty) {
  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Trivia::spaces(0).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Trivia::tabs(0).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Trivia::newlines(0).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
#ifndef NDEBUG
  ASSERT_DEATH({
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Trivia::lineComment("").print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }, "");
  ASSERT_DEATH({
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Trivia::blockComment("").print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }, "");
  ASSERT_DEATH({
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Trivia::docLineComment("").print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }, "");
  ASSERT_DEATH({
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Trivia::docBlockComment("").print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }, "");
#endif
  {
    llvm::SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Trivia().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

TEST(TriviaTests, EmptyEquivalence) {
  ASSERT_EQ(Trivia(), Trivia::spaces(0));
  ASSERT_TRUE(Trivia().empty());
  ASSERT_TRUE((Trivia() + Trivia()).empty());
  ASSERT_EQ(Trivia(), Trivia::tabs(0));
  ASSERT_EQ(Trivia(), Trivia::newlines(0));
  ASSERT_EQ(Trivia() + Trivia(), Trivia());
}

TEST(TriviaTests, Backtick) {
  llvm::SmallString<1> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  Trivia::backtick().print(OS);
  ASSERT_EQ(OS.str().str(), "`");
}

TEST(TriviaTests, PrintingSpaces) {
  llvm::SmallString<4> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  Trivia::spaces(4).print(OS);
  ASSERT_EQ(OS.str().str(), "    ");
}

TEST(TriviaTests, PrintingTabs) {
  llvm::SmallString<4> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  Trivia::tabs(4).print(OS);
  ASSERT_EQ(OS.str().str(), "\t\t\t\t");
}

TEST(TriviaTests, PrintingNewlines) {
  llvm::SmallString<4> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  Trivia::newlines(4).print(OS);
  ASSERT_EQ(OS.str().str(), "\n\n\n\n");
}

TEST(TriviaTests, PrintingLineComments) {
  llvm::SmallString<256> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  auto Lines = Trivia::lineComment("// Line 1") +
               Trivia::newlines(1) +
               Trivia::lineComment("// Line 2");
  Lines.print(OS);
  ASSERT_EQ(OS.str().str(), "// Line 1\n// Line 2");
}

TEST(TriviaTests, PrintingBlockComments) {
  llvm::SmallString<256> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  Trivia::blockComment("/* Block Line 1\n\n  Block Line 2 */").print(OS);
  ASSERT_EQ(OS.str().str(), "/* Block Line 1\n\n  Block Line 2 */");
}

TEST(TriviaTests, PrintingDocLineComments) {
  llvm::SmallString<256> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  auto Lines = Trivia::lineComment("/// Line 1") +
  Trivia::newlines(1) +
  Trivia::lineComment("/// Line 2");
  Lines.print(OS);
  ASSERT_EQ(OS.str().str(), "/// Line 1\n/// Line 2");
}

TEST(TriviaTests, PrintingDocBlockComments) {
  llvm::SmallString<256> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  Trivia::blockComment("/** Block Line 1\n\n  Block Line 2 */").print(OS);
  ASSERT_EQ(OS.str().str(), "/** Block Line 1\n\n  Block Line 2 */");
}

TEST(TriviaTests, PrintingCombinations) {
  {
    llvm::SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    (Trivia() + Trivia()).print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }

  {
    llvm::SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    (Trivia::newlines(2) + Trivia::spaces(2)).print(OS);
    ASSERT_EQ(OS.str().str(), "\n\n  ");
  }

  {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto CCCCombo = Trivia::spaces(1) +
      Trivia::tabs(1) +
      Trivia::newlines(1) +
      Trivia::backtick();
    CCCCombo.print(OS);
    ASSERT_EQ(OS.str().str(), " \t\n`");
  }

  {
    // Combos with comments
  }
}

TEST(TriviaTests, Contains) {
  ASSERT_FALSE(Trivia().contains(TriviaKind::Backtick));
  ASSERT_FALSE(Trivia().contains(TriviaKind::BlockComment));
  ASSERT_FALSE(Trivia().contains(TriviaKind::DocBlockComment));
  ASSERT_FALSE(Trivia().contains(TriviaKind::DocLineComment));
  ASSERT_FALSE(Trivia().contains(TriviaKind::Formfeed));
  ASSERT_FALSE(Trivia().contains(TriviaKind::LineComment));
  ASSERT_FALSE(Trivia().contains(TriviaKind::Newline));
  ASSERT_FALSE(Trivia().contains(TriviaKind::Space));

  ASSERT_TRUE(Trivia::backtick().contains(TriviaKind::Backtick));
  ASSERT_TRUE(Trivia::blockComment("/**/").contains(TriviaKind::BlockComment));
  ASSERT_TRUE(Trivia::docBlockComment("/***/")
              .contains(TriviaKind::DocBlockComment));
  ASSERT_TRUE(Trivia::docLineComment("///")
              .contains(TriviaKind::DocLineComment));
  ASSERT_TRUE(Trivia::lineComment("//").contains(TriviaKind::LineComment));
  ASSERT_TRUE(Trivia::newlines(1).contains(TriviaKind::Newline));
  ASSERT_TRUE(Trivia::spaces(1).contains(TriviaKind::Space));

  auto Combo = Trivia::spaces(1) + Trivia::backtick() + Trivia::newlines(3)
    + Trivia::spaces(1);

  ASSERT_TRUE(Combo.contains(TriviaKind::Space));
  ASSERT_TRUE(Combo.contains(TriviaKind::Newline));
  ASSERT_TRUE(Combo.contains(TriviaKind::Backtick));
  ASSERT_FALSE(Combo.contains(TriviaKind::Tab));
  ASSERT_FALSE(Combo.contains(TriviaKind::LineComment));
  ASSERT_FALSE(Combo.contains(TriviaKind::Formfeed));
}

TEST(TriviaTests, Iteration) {

  llvm::SmallString<6> WholeScratch;
  llvm::raw_svector_ostream WholeOS(WholeScratch);
  auto Triv = Trivia::spaces(2) + Trivia::newlines(2) + Trivia::spaces(2);
  Triv.print(WholeOS);

  llvm::SmallString<6> PiecesScratch;
  llvm::raw_svector_ostream PiecesOS(PiecesScratch);
  for (const auto &Piece : Triv) {
    Piece.print(PiecesOS);
  }

  ASSERT_EQ(WholeOS.str().str(), PiecesOS.str().str());
}

TEST(TriviaTests, push_back) {
  llvm::SmallString<3> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  Trivia Triv;
  Triv.push_back(TriviaPiece::backtick());
  Triv.push_back(TriviaPiece::backtick());
  Triv.push_back(TriviaPiece::backtick());
  Triv.print(OS);
  ASSERT_EQ(OS.str().str(), "```");
}

TEST(TriviaTests, push_front) {
  llvm::SmallString<3> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  Trivia Triv;
  Triv.push_back(TriviaPiece::backtick());
  Triv.push_front(TriviaPiece::spaces(1));
  Triv.push_back(TriviaPiece::spaces(1));
  Triv.push_front(TriviaPiece::backtick());
  Triv.print(OS);
  ASSERT_EQ(OS.str().str(), "` ` ");
}

TEST(TriviaTests, front) {
#ifndef NDEBUG
  ASSERT_DEATH({
    Trivia().front();
  }, "");
#endif

  ASSERT_EQ(Trivia::spaces(1).front(), TriviaPiece::spaces(1));

  ASSERT_EQ((Trivia::spaces(1) + Trivia::newlines(1)).front(),
            TriviaPiece::spaces(1));
}

TEST(TriviaTests, back) {
#ifndef NDEBUG
  ASSERT_DEATH({
    Trivia().back();
  }, "");
#endif
  ASSERT_EQ(Trivia::spaces(1).back(), TriviaPiece::spaces(1));
  ASSERT_EQ((Trivia::spaces(1) + Trivia::newlines(1)).back(),
            TriviaPiece::newlines(1));
}

TEST(TriviaTests, size) {
  ASSERT_EQ(Trivia().size(), 0);
  ASSERT_EQ(Trivia::spaces(1).size(), 1);

  // Trivia doesn't currently coalesce on its own.
  ASSERT_EQ((Trivia::spaces(1) + Trivia::spaces(1)).size(), 2);
}
