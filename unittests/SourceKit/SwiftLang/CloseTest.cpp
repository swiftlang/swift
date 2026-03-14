//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "NullEditorConsumer.h"
#include "SourceKit/Core/Context.h"
#include "SourceKit/Core/LangSupport.h"
#include "SourceKit/Core/NotificationCenter.h"
#include "SourceKit/Support/Concurrency.h"
#include "SourceKit/SwiftLang/Factory.h"
#include "swift/Basic/LLVMInitialize.h"
#include "gtest/gtest.h"

#include <chrono>
#include <condition_variable>
#include <mutex>
#include <thread>

using namespace SourceKit;
using namespace llvm;

static StringRef getRuntimeLibPath() {
  return sys::path::parent_path(SWIFTLIB_DIR);
}

static SmallString<128> getSwiftExecutablePath() {
  SmallString<128> path = sys::path::parent_path(getRuntimeLibPath());
  sys::path::append(path, "bin", "swift-frontend");
  return path;
}

namespace {
class CompileTrackingConsumer final : public trace::TraceConsumer {
  std::mutex Mtx;
  std::condition_variable CV;
  bool HasStarted = false;

public:
  CompileTrackingConsumer() {}
  CompileTrackingConsumer(const CompileTrackingConsumer &) = delete;

  void operationStarted(uint64_t OpId, trace::OperationKind OpKind,
                        const trace::SwiftInvocation &Inv,
                        const trace::StringPairs &OpArgs) override {
    std::unique_lock<std::mutex> lk(Mtx);
    HasStarted = true;
    CV.notify_all();
  }

  void waitForBuildToStart() {
    std::unique_lock<std::mutex> lk(Mtx);
    auto secondsToWait = std::chrono::seconds(20);
    auto when = std::chrono::system_clock::now() + secondsToWait;
    CV.wait_until(lk, when, [&]() { return HasStarted; });
    HasStarted = false;
  }

  void operationFinished(uint64_t OpId, trace::OperationKind OpKind,
                         ArrayRef<DiagnosticEntryInfo> Diagnostics) override {}

  swift::OptionSet<trace::OperationKind> desiredOperations() override {
    return trace::OperationKind::PerformSema;
  }
};

class CloseTest : public ::testing::Test {
  std::shared_ptr<SourceKit::Context> Ctx;
  std::shared_ptr<CompileTrackingConsumer> CompileTracker;

  NullEditorConsumer Consumer;

public:
  CloseTest() {
    INITIALIZE_LLVM();
    Ctx = std::make_shared<SourceKit::Context>(
        getSwiftExecutablePath(), getRuntimeLibPath(),
        SourceKit::createSwiftLangSupport,
        [](SourceKit::Context &Ctx) { return nullptr; },
        /*dispatchOnMain=*/false);
  }

  CompileTrackingConsumer &getCompileTracker() const { return *CompileTracker; }
  LangSupport &getLang() { return Ctx->getSwiftLangSupport(); }

  void SetUp() override {
    CompileTracker = std::make_shared<CompileTrackingConsumer>();
    trace::registerConsumer(CompileTracker.get());
  }

  void TearDown() override {
    trace::unregisterConsumer(CompileTracker.get());
    CompileTracker = nullptr;
  }

  void open(const char *DocName, StringRef Text, ArrayRef<const char *> CArgs) {
    auto Args = makeArgs(DocName, CArgs);
    auto Buf = MemoryBuffer::getMemBufferCopy(Text, DocName);
    getLang().editorOpen(DocName, Buf.get(), Consumer, Args, std::nullopt);
  }

  void close(const char *DocName, bool CancelBuilds, bool RemoveCache) {
    getLang().editorClose(DocName, CancelBuilds, RemoveCache);
  }

  void getDiagnosticsAsync(
      const char *DocName, ArrayRef<const char *> CArgs,
      std::function<void(const RequestResult<DiagnosticsResult> &)> callback) {
    auto Args = makeArgs(DocName, CArgs);
    getLang().getDiagnostics(DocName, Args, /*VFSOpts*/ std::nullopt,
                             /*CancelToken*/ {}, callback);
  }

private:
  std::vector<const char *> makeArgs(const char *DocName,
                                     ArrayRef<const char *> CArgs) {
    std::vector<const char *> Args = CArgs;
    Args.push_back(DocName);
    return Args;
  }
};

} // end anonymous namespace

static const char *getComplexSourceText() {
  // best of luck, type-checker
  return
    "struct A: ExpressibleByIntegerLiteral { init(integerLiteral value: Int) {} }\n"
    "struct B: ExpressibleByIntegerLiteral { init(integerLiteral value: Int) {} }\n"
    "struct C: ExpressibleByIntegerLiteral { init(integerLiteral value: Int) {} }\n"

    "func + (lhs: A, rhs: B) -> A { fatalError() }\n"
    "func + (lhs: B, rhs: C) -> A { fatalError() }\n"
    "func + (lhs: C, rhs: A) -> A { fatalError() }\n"

    "func + (lhs: B, rhs: A) -> B { fatalError() }\n"
    "func + (lhs: C, rhs: B) -> B { fatalError() }\n"
    "func + (lhs: A, rhs: C) -> B { fatalError() }\n"

    "func + (lhs: C, rhs: B) -> C { fatalError() }\n"
    "func + (lhs: B, rhs: C) -> C { fatalError() }\n"
    "func + (lhs: A, rhs: A) -> C { fatalError() }\n"

    "let x: C = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8\n";
}

TEST_F(CloseTest, Cancel) {
  const char *DocName = "test.swift";
  auto *Contents = getComplexSourceText();
  const char *Args[] = {"-parse-as-library"};

  // Test twice with RemoveCache = false to test both the prior state of
  // the ASTProducer being cached and not cached.
  for (auto RemoveCache : {true, false, false}) {
    open(DocName, Contents, Args);

    Semaphore BuildResultSema(0);

    getDiagnosticsAsync(DocName, Args,
                        [&](const RequestResult<DiagnosticsResult> &Result) {
      EXPECT_TRUE(Result.isCancelled());
      BuildResultSema.signal();
    });

    getCompileTracker().waitForBuildToStart();

    close(DocName, /*CancelBuilds*/ true, RemoveCache);

    bool Expired = BuildResultSema.wait(30 * 1000);
    if (Expired)
      llvm::report_fatal_error("Did not receive a response for the request");
  }
}
