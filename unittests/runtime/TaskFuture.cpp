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

template <class Storage> struct FutureContext : FutureAsyncContext {
  InvokeFunctionRef<Storage> storedInvokeFn;

  Storage& getStorage() {
    return *reinterpret_cast<Storage *>(this->indirectResult);
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
                           const T& initialValue,
                           undeduced<InvokeFunctionRef<T>> invokeFn,
                           BodyFunctionRef body) {
  JobFlags flags = JobKind::Task;
  flags.task_setIsFuture(true);

  auto taskAndContext =
    swift_task_create_future_f(flags, /*parent*/ nullptr, resultType,
                               &futureTaskInvokeFunction<T>,
                               sizeof(FutureContext<T>));

  auto futureContext =
    static_cast<FutureContext<T>*>(taskAndContext.InitialContext);
  futureContext->getStorage() = initialValue; // Magic number.
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

struct TestObject : HeapObject {
  constexpr TestObject(HeapMetadata const *newMetadata)
    : HeapObject(newMetadata, InlineRefCounts::Immortal)
    , Addr(NULL), Value(0) {}

  size_t *Addr;
  size_t Value;
};

static SWIFT_CC(swift) void destroyTestObject(SWIFT_CONTEXT HeapObject *_object) {
  auto object = static_cast<TestObject*>(_object);
  assert(object->Addr && "object already deallocated");
  *object->Addr = object->Value;
  object->Addr = nullptr;
  swift_deallocObject(object, sizeof(TestObject), alignof(TestObject) - 1);
}

static const FullMetadata<ClassMetadata> TestClassObjectMetadata = {
  { { &destroyTestObject }, { &VALUE_WITNESS_SYM(Bo) } },
  { { nullptr }, ClassFlags::UsesSwiftRefcounting, 0, 0, 0, 0, 0, 0 }
};

/// Create an object that, when deallocated, stores the given value to
/// the given pointer.
static TestObject *allocTestObject(size_t *addr, size_t value) {
  auto result =
    static_cast<TestObject *>(swift_allocObject(&TestClassObjectMetadata,
                                                sizeof(TestObject),
                                                alignof(TestObject) - 1));
  result->Addr = addr;
  result->Value = value;
  return result;
}

TEST(TaskFutureTest, objectFuture) {
  auto createdExecutor = createFakeExecutor(1234);
  bool hasRun = false;

  size_t objectValueOnComplete = 7;
  TestObject *object = nullptr;
  withFutureTask<TestObject *>(
      &TestClassObjectMetadata, nullptr,
      [&](AsyncTask *task, ExecutorRef executor,
          FutureContext<TestObject *> *context) {
    object = allocTestObject(&objectValueOnComplete, 25);

    // The error storage should have been cleared out for us.
    EXPECT_EQ(nullptr, context->errorResult);

    // Store the object in the future.
    context->getStorage() = object;

    hasRun = true;
  }, [&](AsyncTask *task) {
    // Retain the task, so it won't be destroyed when it is executed.
    swift_retain(task);

    // Run the task, which should fill in the future.
    EXPECT_FALSE(hasRun);
    task->run(createdExecutor);
    EXPECT_TRUE(hasRun);

    // "Wait" for the future, which must have completed by now.
    auto waitResult = swift_task_future_wait(task, nullptr);
    EXPECT_EQ(TaskFutureWaitResult::Success, waitResult.kind);

    // Make sure we got the result value we expect.
    EXPECT_EQ(object, *reinterpret_cast<TestObject **>(waitResult.storage));

    // Make sure the object hasn't been destroyed.
    EXPECT_EQ(size_t(7), objectValueOnComplete);

    // Okay, release the task. This should destroy the object.
    swift_release(task);
    assert(objectValueOnComplete == 25);
  });
}
