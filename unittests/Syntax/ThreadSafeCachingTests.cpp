#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/StmtSyntax.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

#include <thread>

using namespace swift;
using namespace swift::syntax;

static void getExpressionFrom(ReturnStmtSyntax Return,
                              uintptr_t *DataPointer) {
  auto Expression = Return.getExpression().getValue();
  auto Data = Expression.getDataPointer();
  *DataPointer = reinterpret_cast<uintptr_t>(Data);
}

// Tests that, when multiple threads ask for a child node of the same syntax
// node:
// - Only one thread inserts the realized child into the parent
// - Both threads get the exact same child (by identity)
TEST(ThreadSafeCachingTests, ReturnGetExpression) {
  auto ReturnKW = SyntaxFactory::makeReturnKeyword({}, Trivia::spaces(1));
  auto Minus = SyntaxFactory::makePrefixOpereator("-", {});
  auto One = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto MinusOne = SyntaxFactory::makeIntegerLiteralExpr(Minus, One);

  for (unsigned i = 0; i < 10000; ++i) {
    llvm::SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Return = SyntaxFactory::makeReturnStmt(ReturnKW, MinusOne);

    uintptr_t FirstDataPointer;
    uintptr_t SecondDataPointer;

    std::thread first(getExpressionFrom, Return, &FirstDataPointer);
    std::thread second(getExpressionFrom, Return, &SecondDataPointer);
    first.join();
    second.join();

    auto DataPointer = reinterpret_cast<uintptr_t>(
      Return.getExpression().getValue().getDataPointer());

    ASSERT_EQ(FirstDataPointer, SecondDataPointer);
    ASSERT_EQ(FirstDataPointer, DataPointer);

    if (FirstDataPointer != SecondDataPointer ||
        FirstDataPointer != DataPointer) {
      break;
    }
  }
}

