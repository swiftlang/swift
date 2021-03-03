#include "swift/Syntax/SyntaxFactory.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using llvm::None;
using llvm::SmallString;

using namespace swift;
using namespace swift::syntax;

TupleExprElementSyntax getCannedArgument(const RC<SyntaxArena> &Arena) {
  auto X = SyntaxFactory::makeIdentifier("x", "", "", Arena);
  auto Foo = SyntaxFactory::makeIdentifier("foo", "", "", Arena);
  auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
  auto SymbolicRef =
      SyntaxFactory::makeSymbolicReferenceExpr(Foo, llvm::None, Arena);
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);

  return SyntaxFactory::makeTupleExprElement(X, Colon, SymbolicRef, Comma,
                                             Arena);
}

TEST(SyntaxCollectionTests, empty) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Empty = SyntaxFactory::makeBlankTupleExprElementList(Arena);
  ASSERT_TRUE(Empty.empty());
  ASSERT_FALSE(Empty.appending(getCannedArgument(Arena)).empty());
}

TEST(SyntaxCollectionTests, size) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Empty = SyntaxFactory::makeBlankTupleExprElementList(Arena);
  ASSERT_EQ(Empty.size(), size_t(0));
  ASSERT_EQ(Empty.appending(getCannedArgument(Arena)).size(), size_t(1));
}

TEST(SyntaxCollectionTests, subscript) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Empty = SyntaxFactory::makeBlankTupleExprElementList(Arena);
#ifndef NDEBUG
  ASSERT_DEATH({ Empty[0]; }, "");
#endif

  SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  auto Arg = getCannedArgument(Arena);
  Arg.print(OS);

  auto List = Empty.appending(Arg);

  SmallString<48> GottenScratch;
  llvm::raw_svector_ostream GottenOS(Scratch);
  List[0].print(GottenOS);

  ASSERT_EQ(OS.str().str(), GottenOS.str().str());

  auto GottenArg1 = List[0];
  auto GottenArg2 = List[0];
  ASSERT_TRUE(GottenArg1.hasSameIdentityAs(GottenArg2));
}

TEST(SyntaxCollectionTests, appending) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Arg = getCannedArgument(Arena);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
  auto List = SyntaxFactory::makeBlankTupleExprElementList(Arena)
                  .appending(Arg)
                  .appending(Arg)
                  .appending(Arg.withTrailingComma(NoComma));

  ASSERT_EQ(List.size(), size_t(3));

  SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  List.print(OS);

  ASSERT_EQ(OS.str().str(), "x: foo, x: foo, x: foo");

  auto GottenArg0_1 = List[0];
  auto GottenArg0_2 = List[0];
  ASSERT_TRUE(GottenArg0_1.hasSameIdentityAs(GottenArg0_2));

  auto GottenArg1_1 = List[1];
  auto GottenArg1_2 = List[1];
  ASSERT_TRUE(GottenArg1_1.hasSameIdentityAs(GottenArg1_2));

  auto GottenArg2_1 = List[2];
  auto GottenArg2_2 = List[2];
  ASSERT_TRUE(GottenArg2_1.hasSameIdentityAs(GottenArg2_2));
}

TEST(SyntaxCollectionTests, removingLast) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  ASSERT_DEATH(
      { SyntaxFactory::makeBlankTupleExprElementList(Arena).removingLast(); },
      "");
  auto Arg = getCannedArgument(Arena);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
  auto List = SyntaxFactory::makeBlankTupleExprElementList(Arena)
                  .appending(Arg)
                  .appending(Arg)
                  .appending(Arg.withTrailingComma(NoComma))
                  .removingLast();
  SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  List.print(OS);
  ASSERT_EQ(OS.str().str(), "x: foo, x: foo, ");
}

TEST(SyntaxCollectionTests, prepending) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Arg = getCannedArgument(Arena);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
  auto List = SyntaxFactory::makeBlankTupleExprElementList(Arena)
                  .prepending(Arg.withTrailingComma(NoComma))
                  .prepending(Arg.withLabel(
                      SyntaxFactory::makeIdentifier("schwifty", "", "", Arena)))
                  .prepending(Arg);

  ASSERT_EQ(List.size(), size_t(3));

  SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  List.print(OS);

  ASSERT_EQ(OS.str().str(), "x: foo, schwifty: foo, x: foo");

  auto GottenArg0_1 = List[0];
  auto GottenArg0_2 = List[0];
  ASSERT_TRUE(GottenArg0_1.hasSameIdentityAs(GottenArg0_2));

  auto GottenArg1_1 = List[1];
  auto GottenArg1_2 = List[1];
  ASSERT_TRUE(GottenArg1_1.hasSameIdentityAs(GottenArg1_2));

  auto GottenArg2_1 = List[2];
  auto GottenArg2_2 = List[2];
  ASSERT_TRUE(GottenArg2_1.hasSameIdentityAs(GottenArg2_2));
}

TEST(SyntaxCollectionTests, removingFirst) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  ASSERT_DEATH(
      { SyntaxFactory::makeBlankTupleExprElementList(Arena).removingFirst(); },
      "");
  auto Arg = getCannedArgument(Arena);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
  auto List = SyntaxFactory::makeBlankTupleExprElementList(Arena)
                  .appending(Arg.withLabel(
                      SyntaxFactory::makeIdentifier("schwifty", "", "", Arena)))
                  .appending(Arg)
                  .appending(Arg.withTrailingComma(NoComma))
                  .removingFirst();
  SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  List.print(OS);
  ASSERT_EQ(OS.str().str(), "x: foo, x: foo");
}

TEST(SyntaxCollectionTests, inserting) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Arg = getCannedArgument(Arena);
  auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
#ifndef NDEBUG
  ASSERT_DEATH(
      {
        SyntaxFactory::makeBlankTupleExprElementList(Arena).inserting(1, Arg);
      },
      "");
#endif

  {
    SmallString<48> InsertedScratch;
    llvm::raw_svector_ostream InsertedOS(InsertedScratch);
    SyntaxFactory::makeBlankTupleExprElementList(Arena)
        .inserting(0, Arg)
        .inserting(0, Arg)
        .inserting(0, Arg)
        .print(InsertedOS);

    SmallString<48> PrependedScratch;
    llvm::raw_svector_ostream PrependedOS(PrependedScratch);
    SyntaxFactory::makeBlankTupleExprElementList(Arena)
        .prepending(Arg)
        .prepending(Arg)
        .prepending(Arg)
        .print(PrependedOS);
    ASSERT_EQ(InsertedOS.str().str(), PrependedOS.str().str());
  }

  {
    SmallString<48> InsertedScratch;
    llvm::raw_svector_ostream InsertedOS(InsertedScratch);
    SyntaxFactory::makeBlankTupleExprElementList(Arena)
        .inserting(0, Arg)
        .inserting(1, Arg)
        .inserting(2, Arg)
        .print(InsertedOS);

    SmallString<48> AppendedScratch;
    llvm::raw_svector_ostream AppendedOS(AppendedScratch);
    SyntaxFactory::makeBlankTupleExprElementList(Arena)
        .appending(Arg)
        .appending(Arg)
        .appending(Arg)
        .print(AppendedOS);
    ASSERT_EQ(InsertedOS.str().str(), AppendedOS.str().str());
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTupleExprElementList(Arena)
        .appending(Arg)
        .appending(Arg)
        .inserting(1, Arg.withLabel(SyntaxFactory::makeIdentifier(
                          "schwifty", "", "", Arena)))
        .print(OS);
    ASSERT_EQ(OS.str().str(), "x: foo, schwifty: foo, x: foo, ");
  }
}

TEST(SyntaxCollectionTests, cleared) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Arg = getCannedArgument(Arena);
  SmallString<1> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  auto List = SyntaxFactory::makeBlankTupleExprElementList(Arena)
                  .appending(Arg)
                  .appending(Arg)
                  .appending(Arg)
                  .cleared();

  List.print(OS);
  ASSERT_EQ(OS.str().str(), "");
  ASSERT_TRUE(List.empty());
  ASSERT_EQ(List.size(), size_t(0));
}

TEST(SyntaxCollectionTests, Iteration) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Arg = getCannedArgument(Arena);
  auto List = SyntaxFactory::makeBlankTupleExprElementList(Arena)
                  .appending(Arg)
                  .appending(Arg)
                  .appending(Arg);

  auto Element0 = List[0];
  auto Element1 = List[1];
  auto Element2 = List[2];

  SmallString<48> IteratedScratch;
  llvm::raw_svector_ostream IteratedOS(IteratedScratch);
  for (auto Element : List) {
    Element.print(IteratedOS);
  }
  ASSERT_EQ(IteratedOS.str().str(), "x: foo, x: foo, x: foo, ");

  auto IteratedElement0 = List[0];
  auto IteratedElement1 = List[1];
  auto IteratedElement2 = List[2];

  ASSERT_TRUE(Element0.hasSameIdentityAs(IteratedElement0));
  ASSERT_TRUE(Element1.hasSameIdentityAs(IteratedElement1));
  ASSERT_TRUE(Element2.hasSameIdentityAs(IteratedElement2));

  SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  List.print(OS);
  ASSERT_EQ(OS.str().str(), IteratedOS.str().str());
}

TEST(SyntaxCollectionTests, Removing) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Arg = getCannedArgument(Arena);
  auto List = SyntaxFactory::makeBlankTupleExprElementList(Arena)
                  .appending(Arg)
                  .appending(Arg.withLabel(
                      SyntaxFactory::makeIdentifier("first", "", "", Arena)))
                  .appending(Arg)
                  .removing(1);

  ASSERT_EQ(List.size(), static_cast<size_t>(2));

  SmallString<48> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  List.print(OS);
  ASSERT_EQ(OS.str().str(), "x: foo, x: foo, ");
}

