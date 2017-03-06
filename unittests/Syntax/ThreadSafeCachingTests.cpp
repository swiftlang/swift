#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/StmtSyntax.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

#include <future>
#include <thread>
#include <queue>

using namespace swift;
using namespace swift::syntax;

static uintptr_t getExpressionFrom(ReturnStmtSyntax Return) {
  auto Expression = Return.getExpression().getValue();
  return reinterpret_cast<uintptr_t>(Expression.getDataPointer());
}

class Pool {
  static constexpr size_t NumThreads = 2;
  using FuncTy = std::function<uintptr_t(ReturnStmtSyntax)>;
  std::vector<std::thread> Workers;
  std::queue<std::function<void()>> Tasks;
  std::mutex QueueLock;
  std::condition_variable Condition;
  bool Stop;

public:
  Pool() : Stop(false) {
    for(size_t i = 0; i < NumThreads; ++i)
      Workers.emplace_back([this] {
        while (true) {
          std::function<void()> Task;
          {
            std::unique_lock<std::mutex> L(QueueLock);

            Condition.wait(L, [this]{
              return Stop || !Tasks.empty();
            });

            if(Stop && Tasks.empty()) {
              return;
            }

            Task = std::move(Tasks.front());
            Tasks.pop();
          }

          Task();
        }
      });
  }

  std::future<uintptr_t> run(FuncTy Func, ReturnStmtSyntax Return) {
    auto Task = std::make_shared<std::packaged_task<uintptr_t()>>(
      std::bind(Func, Return));

    auto Future = Task->get_future();
    {
      std::unique_lock<std::mutex> L(QueueLock);
      Tasks.emplace([Task](){ (*Task)(); });
    }
    Condition.notify_one();
    return Future;
  }

  ~Pool() {
    {
      std::lock_guard<std::mutex> L(QueueLock);
      Stop = true;
    }
    Condition.notify_all();
    for(auto &Worker : Workers) {
      Worker.join();
    }
  }
};

// Tests that, when multiple threads ask for a child node of the same syntax
// node:
// - Only one thread inserts the realized child into the parent
// - Both threads get the exact same child (by identity)
TEST(ThreadSafeCachingTests, ReturnGetExpression) {
  auto ReturnKW = SyntaxFactory::makeReturnKeyword({}, Trivia::spaces(1));
  auto Minus = SyntaxFactory::makePrefixOpereator("-", {});
  auto One = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto MinusOne = SyntaxFactory::makeIntegerLiteralExpr(Minus, One);

  Pool P;

  for (unsigned i = 0; i < 10000; ++i) {
    auto Return = SyntaxFactory::makeReturnStmt(ReturnKW, MinusOne);

    auto Future1 = P.run(getExpressionFrom, Return);
    auto Future2 = P.run(getExpressionFrom, Return);

    auto FirstDataPointer = Future1.get();
    auto SecondDataPointer = Future2.get();

    auto DataPointer = reinterpret_cast<uintptr_t>(
      Return.getExpression().getValue().getDataPointer());

    ASSERT_EQ(FirstDataPointer, SecondDataPointer);
    ASSERT_EQ(FirstDataPointer, DataPointer);
  }
}

