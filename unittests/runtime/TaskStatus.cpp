//===--- TaskStatus.cpp - Unit tests for the task-status API --------------===//
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
#include "swift/Basic/STLExtras.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {
template <class T> struct ValueContext;

template <class T>
using InvokeFunctionRef =
  llvm::function_ref<void(AsyncTask *task,
                          ExecutorRef executor,
                          ValueContext<T> *context)>;
using BodyFunctionRef =
  llvm::function_ref<void(AsyncTask *task)>;

template <class Storage> struct ValueContext : AsyncContext, Storage {
  InvokeFunctionRef<Storage> StoredInvokeFn;
  ValueContext(JobFlags flags, InvokeFunctionRef<Storage> invokeFn,
               Storage &&value)
    : AsyncContext(AsyncContextKind::Ordinary, nullptr, nullptr),
      Storage(std::forward<Storage>(value)),
      StoredInvokeFn(invokeFn) {}
};

// Disable template argument deduction.
template <class T>
using undeduced =
  typename std::enable_if<std::is_same<T, T>::value, T>::type;

template <class T>
SWIFT_CC(swift)
static void simpleTaskInvokeFunction(AsyncTask *task, ExecutorRef executor,
                                     AsyncContext *context) {
  auto valueContext = static_cast<ValueContext<T>*>(context);
  valueContext->StoredInvokeFn(task, executor, valueContext);
}

template <class T>
static void withSimpleTask(JobFlags flags, T &&value,
                           undeduced<InvokeFunctionRef<T>> invokeFn,
                           BodyFunctionRef body) {
  TaskContinuationFunction *invokeFP = &simpleTaskInvokeFunction<T>;
  ValueContext<T> initialContext(flags, invokeFn, std::forward<T>(value));
  AsyncTask task(flags, invokeFP, &initialContext);
  body(&task);
}

template <class T>
static void withSimpleTask(T &&value,
                           undeduced<InvokeFunctionRef<T>> invokeFn,
                           BodyFunctionRef bodyFn) {
  withSimpleTask(JobKind::Task, std::forward<T>(value), invokeFn, bodyFn);
}

static ExecutorRef createFakeExecutor(uintptr_t value) {
  return reinterpret_cast<ExecutorRef>(value);
}

} // end anonymous namespace

TEST(TaskStatusTest, basicTasks) {
  AsyncTask *createdTask = nullptr;
  auto createdExecutor = createFakeExecutor(1234);
  bool hasRun = false;

  struct Storage { int value; };
  withSimpleTask(Storage{47},
    [&](AsyncTask *task, ExecutorRef executor,
        ValueContext<Storage> *context) {
    // The task passed in should be the task we created.
    EXPECT_EQ(createdTask, task);
    EXPECT_EQ(createdExecutor, executor);

    if (hasRun) {
      EXPECT_EQ(94, context->value);
    } else {
      EXPECT_EQ(47, context->value);
      context->value *= 2;
    }

    hasRun = true;

  }, [&](AsyncTask *task) {
    createdTask = task;

    EXPECT_FALSE(hasRun);
    createdTask->run(createdExecutor);
    EXPECT_TRUE(hasRun);
    createdTask->run(createdExecutor);
    EXPECT_TRUE(hasRun);
  });

  EXPECT_TRUE(hasRun);
}

TEST(TaskStatusTest, cancellation_simple) {
  struct Storage { int value; };
  withSimpleTask(Storage{47},
    [&](AsyncTask *task, ExecutorRef executor,
        ValueContext<Storage> *context) {
    EXPECT_FALSE(task->isCancelled());
    swift_task_cancel(task);
    EXPECT_TRUE(task->isCancelled());
    swift_task_cancel(task);
    EXPECT_TRUE(task->isCancelled());
  }, [&](AsyncTask *task) {
    task->run(createFakeExecutor(1234));
  });
}

// Test basic deadline mechanics (other than actually setting up
// something to cancel the task).  Also tests adding and removing
// records quite a bit.
TEST(TaskStatusTest, deadline) {
  struct Storage { int value; };
  withSimpleTask(Storage{47},
    [&](AsyncTask *task, ExecutorRef executor,
        ValueContext<Storage> *context) {
    EXPECT_FALSE(task->isCancelled());

    TaskDeadline deadlineOne = { 1234 };
    TaskDeadline deadlineTwo = { 2345 };
    DeadlineStatusRecord recordOne(deadlineOne);
    DeadlineStatusRecord recordTwo(deadlineTwo);
    bool result;

    NearestTaskDeadline nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::None, nearest.ValueKind);

    // Add deadline 1.  Check that we haven't been cancelled yet.
    result = swift_task_addStatusRecord(task, &recordOne);
    EXPECT_TRUE(result);

    // There should now be an active deadline.
    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::Active, nearest.ValueKind);
    EXPECT_EQ(deadlineOne, nearest.Value);

    // Remove deadline 1.  Check that we haven't been cancelled yet.
    result = swift_task_removeStatusRecord(task, &recordOne);
    EXPECT_TRUE(result);

    // There shouldn't be an active deadline anymore.
    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::None, nearest.ValueKind);

    // Add deadline 1, then 2.
    result = swift_task_addStatusRecord(task, &recordOne);
    EXPECT_TRUE(result);
    result = swift_task_addStatusRecord(task, &recordTwo);
    EXPECT_TRUE(result);

    // The nearest deadline should be deadline 1.
    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::Active, nearest.ValueKind);
    EXPECT_EQ(deadlineOne, nearest.Value);

    // Remove the deadlines.
    result = swift_task_removeStatusRecord(task, &recordTwo);
    EXPECT_TRUE(result);
    result = swift_task_removeStatusRecord(task, &recordOne);
    EXPECT_TRUE(result);

    // Add deadline 2, then 1s.
    result = swift_task_addStatusRecord(task, &recordTwo);
    EXPECT_TRUE(result);

    // In the middle, the nearest deadline should be deadline 2.
    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::Active, nearest.ValueKind);
    EXPECT_EQ(deadlineTwo, nearest.Value);

    result = swift_task_addStatusRecord(task, &recordOne);
    EXPECT_TRUE(result);

    // The nearest deadline should be deadline 1.
    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::Active, nearest.ValueKind);
    EXPECT_EQ(deadlineOne, nearest.Value);

    // Remove the deadlines.
    result = swift_task_removeStatusRecord(task, &recordOne);
    EXPECT_TRUE(result);
    result = swift_task_removeStatusRecord(task, &recordTwo);
    EXPECT_TRUE(result);

    // Do the same thing with tryAddStatus.
    result = swift_task_tryAddStatusRecord(task, &recordTwo);
    EXPECT_TRUE(result);
    result = swift_task_tryAddStatusRecord(task, &recordOne);
    EXPECT_TRUE(result);
    // The nearest deadline should be deadline 1.
    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::Active, nearest.ValueKind);
    EXPECT_EQ(deadlineOne, nearest.Value);
    result = swift_task_removeStatusRecord(task, &recordOne);
    EXPECT_TRUE(result);
    result = swift_task_removeStatusRecord(task, &recordTwo);
    EXPECT_TRUE(result);

    // Remove out of order.
    result = swift_task_addStatusRecord(task, &recordTwo);
    EXPECT_TRUE(result);
    result = swift_task_addStatusRecord(task, &recordOne);
    EXPECT_TRUE(result);
    // The nearest deadline should be deadline 1.
    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::Active, nearest.ValueKind);
    EXPECT_EQ(deadlineOne, nearest.Value);
    result = swift_task_removeStatusRecord(task, &recordTwo);
    EXPECT_TRUE(result);
    result = swift_task_removeStatusRecord(task, &recordOne);
    EXPECT_TRUE(result);

    // Add deadline 2, then cancel.
    result = swift_task_addStatusRecord(task, &recordTwo);
    EXPECT_TRUE(result);

    // The nearest deadline should be deadline 2.
    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::Active, nearest.ValueKind);
    EXPECT_EQ(deadlineTwo, nearest.Value);

    // Cancel.
    swift_task_cancel(task);
    EXPECT_TRUE(task->isCancelled());

    // We should report already cancelled now.
    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::AlreadyCancelled, nearest.ValueKind);

    // Add deadline 1.
    result = swift_task_addStatusRecord(task, &recordOne);
    EXPECT_FALSE(result);

    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::AlreadyCancelled, nearest.ValueKind);

    result = swift_task_removeStatusRecord(task, &recordOne);
    EXPECT_FALSE(result);

    result = swift_task_tryAddStatusRecord(task, &recordOne);
    EXPECT_FALSE(result);

    result = swift_task_removeStatusRecord(task, &recordTwo);
    EXPECT_FALSE(result);

    nearest = swift_task_getNearestDeadline(task);
    EXPECT_EQ(NearestTaskDeadline::AlreadyCancelled, nearest.ValueKind);

    EXPECT_TRUE(task->isCancelled());
  }, [&](AsyncTask *task) {
    task->run(createFakeExecutor(1234));
  });
}
