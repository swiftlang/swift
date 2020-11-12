//===--- TaskFuture.cpp - Unit tests for the task futures API -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/Basic/STLExtras.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {
template <class T> struct FutureContext;

template <class T>
using InvokeFunctionRef =
  llvm::function_ref<void(AsyncTask *task,
                          ExecutorRef executor,
                          FutureContext<T> *context)>;

using BodyFunctionRef =
  llvm::function_ref<void(AsyncTask *task)>;

template <class Storage> struct PODFutureContext : AsyncContext {
  alignas(Storage) char resultStorage[sizeof(Storage)];
  void *errorStorage;
};

template <class Storage> struct FutureContext : PODFutureContext<Storage> {
  InvokeFunctionRef<Storage> storedInvokeFn;

  Storage& getStorage() {
    return *reinterpret_cast<Storage *>(&this->resultStorage[0]);
  }
};

// Disable template argument deduction.
template <class T>
using undeduced =
  typename std::enable_if<std::is_same<T, T>::value, T>::type;

template <class T>
SWIFT_CC(swift)
static void futureTaskInvokeFunction(AsyncTask *task, ExecutorRef executor,
                                     AsyncContext *context) {
  auto futureContext = static_cast<FutureContext<T>*>(context);
  futureContext->storedInvokeFn(task, executor, futureContext);

  // Return to finish off the task.
  // In a normal situation we'd need to free the context, but here
  // we know we're at the top level.
  futureContext->ResumeParent(task, executor, futureContext);
}

template <class T>
static void withFutureTask(const Metadata *resultType,
                           undeduced<InvokeFunctionRef<T>> invokeFn,
                           BodyFunctionRef body) {
  JobFlags flags = JobKind::Task;
  flags.task_setIsFuture(true);

  auto taskAndContext =
    swift_task_create_future_f(flags, /*parent*/ nullptr, resultType,
                               &futureTaskInvokeFunction<T>,
                               sizeof(FutureContext<T>),
                               offsetof(PODFutureContext<T>, resultStorage),
                               offsetof(PODFutureContext<T>, errorStorage));

  auto futureContext =
    static_cast<FutureContext<T>*>(taskAndContext.InitialContext);
  futureContext->getStorage() = 42; // Magic number.
  futureContext->storedInvokeFn = invokeFn;

  // Forward our owning reference to the task into its execution,
  // causing it to be destroyed when it completes.
  body(taskAndContext.Task);
}

static ExecutorRef createFakeExecutor(uintptr_t value) {
  return {reinterpret_cast<Executor*>(value)};
}
}

extern const FullMetadata<OpaqueMetadata> METADATA_SYM(Si);

TEST(TaskFutureTest, intFuture) {
  auto createdExecutor = createFakeExecutor(1234);
  bool hasRun = false;

  withFutureTask<intptr_t>(
      reinterpret_cast<const Metadata *>(&METADATA_SYM(Si)),
      [&](AsyncTask *task, ExecutorRef executor,
          FutureContext<intptr_t> *context) {
    // The storage should be what we initialized it to earlier.
    EXPECT_EQ(42, context->getStorage());

    // The error storage should have been cleared out for us.
    EXPECT_EQ(nullptr, context->errorStorage);

    // Store something in the future.
    context->getStorage() = 17;

    hasRun = true;
  }, [&](AsyncTask *task) {
    // Run the task, which should fill in the future.
    EXPECT_FALSE(hasRun);
    task->run(createdExecutor);
    EXPECT_TRUE(hasRun);

    // "Wait" for the future, which must have completed by now.
    auto waitResult = swift_task_future_wait(task, nullptr);
    EXPECT_EQ(TaskFutureWaitResult::Success, waitResult.kind);

    // Make sure we got the result value we expect.
    EXPECT_EQ(17, *reinterpret_cast<intptr_t *>(waitResult.storage));
  });
}

