//===--- Concurrency-libdispatch.cpp --------------------------------------===//
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

#include "SourceKit/Support/Concurrency.h"
#include "SourceKit/Config/config.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Threading.h"

#include <dispatch/dispatch.h>
#include <Block.h>

using namespace SourceKit;

void *Semaphore::Impl::create(long count) {
  return dispatch_semaphore_create(count);
}

void Semaphore::Impl::signal(Ty Obj) {
  dispatch_semaphore_signal(dispatch_semaphore_t(Obj));
}

bool Semaphore::Impl::wait(Ty Obj) {
  return dispatch_semaphore_wait(dispatch_semaphore_t(Obj), 
                                 DISPATCH_TIME_FOREVER);
}

bool Semaphore::Impl::wait(Ty Obj, long milliseconds) {
  dispatch_time_t timeout = dispatch_time(DISPATCH_TIME_NOW, 
                                          milliseconds * NSEC_PER_MSEC);
  return dispatch_semaphore_wait(dispatch_semaphore_t(Obj), timeout);
}

void Semaphore::Impl::retain(Ty Obj) {
  dispatch_retain(dispatch_semaphore_t(Obj));
}

void Semaphore::Impl::release(Ty Obj) {
  dispatch_release(dispatch_semaphore_t(Obj));
}


static dispatch_queue_priority_t toDispatchPriority(WorkQueue::Priority Prio) {
  switch (Prio) {
  case WorkQueue::Priority::High: return DISPATCH_QUEUE_PRIORITY_HIGH;
  case WorkQueue::Priority::Default: return DISPATCH_QUEUE_PRIORITY_DEFAULT;
  case WorkQueue::Priority::Low: return DISPATCH_QUEUE_PRIORITY_LOW;
  case WorkQueue::Priority::Background:
    return DISPATCH_QUEUE_PRIORITY_BACKGROUND;
  }
  llvm_unreachable("Invalid priority");
}

static dispatch_queue_attr_t toDispatchDequeuing(WorkQueue::Dequeuing DeqKind) {
  switch (DeqKind) {
  case WorkQueue::Dequeuing::Concurrent: return DISPATCH_QUEUE_CONCURRENT;
  case WorkQueue::Dequeuing::Serial: return DISPATCH_QUEUE_SERIAL;
  }
  llvm_unreachable("Invalid dequeuing kind");
}

static dispatch_queue_t getDispatchGlobalQueue(WorkQueue::Priority Prio) {
  return dispatch_get_global_queue(toDispatchPriority(Prio), 0);
}

void *WorkQueue::Impl::create(Dequeuing DeqKind, Priority Prio,
                              llvm::StringRef Label) {
  const char *LabelCStr = 0;
  llvm::SmallString<128> LabelStr(Label);
  if (!Label.empty()) {
    LabelStr.push_back('\0');
    LabelCStr = LabelStr.begin();
  }
  dispatch_queue_t queue =
      dispatch_queue_create(LabelCStr, toDispatchDequeuing(DeqKind));
  setPriority(queue, Prio);
  return queue;
}

namespace {
struct ExecuteOnLargeStackInfo {
  dispatch_block_t BlockToRun;
  
  ~ExecuteOnLargeStackInfo() {
    Block_release(BlockToRun);
  }
};
}

static void executeBlock(void *Data) {
  auto ExecuteInfo = (ExecuteOnLargeStackInfo*)Data;
  ExecuteInfo->BlockToRun();
  delete ExecuteInfo;
}

static void executeOnLargeStackThread(void *Data) {
  static const size_t ThreadStackSize = 8 << 20; // 8 MB.
  llvm::llvm_execute_on_thread(executeBlock, Data, ThreadStackSize);
}

static std::pair<void *, WorkQueue::DispatchFn>
toCFunction(void *Ctx, WorkQueue::DispatchFn Fn, bool isStackDeep) {
  if (!isStackDeep)
    return std::make_pair(Ctx, Fn);

  auto ExecuteInfo = new ExecuteOnLargeStackInfo;
#if HAVE_DISPATCH_BLOCK_CREATE
  // Transfer attributes of the calling thread, such as QOS class,
  // os_activity_t, etc.
  if (&dispatch_block_create) {
    ExecuteInfo->BlockToRun = dispatch_block_create(DISPATCH_BLOCK_ASSIGN_CURRENT,
                                                    ^{ Fn(Ctx); });
  } else {
    ExecuteInfo->BlockToRun = Block_copy(^{ Fn(Ctx); });
  }
#else
  ExecuteInfo->BlockToRun = Block_copy(^{ Fn(Ctx); });
#endif
  
  return std::make_pair(ExecuteInfo, executeOnLargeStackThread);
}

void WorkQueue::Impl::dispatch(Ty Obj, const DispatchData &Fn) {
  void *Context;
  WorkQueue::DispatchFn CFn;
  std::tie(Context, CFn) = toCFunction(Fn.getContext(), Fn.getFunction(),
                                       Fn.isStackDeep());
  dispatch_queue_t queue = dispatch_queue_t(Obj);
  dispatch_async_f(queue, Context, CFn);
}

void WorkQueue::Impl::dispatchSync(Ty Obj, const DispatchData &Fn) {
  void *Context;
  WorkQueue::DispatchFn CFn;
  std::tie(Context, CFn) = toCFunction(Fn.getContext(), Fn.getFunction(),
                                       Fn.isStackDeep());
  dispatch_queue_t queue = dispatch_queue_t(Obj);
  dispatch_sync_f(queue, Context, CFn);
}

void WorkQueue::Impl::dispatchBarrier(Ty Obj, const DispatchData &Fn) {
  void *Context;
  WorkQueue::DispatchFn CFn;
  std::tie(Context, CFn) = toCFunction(Fn.getContext(), Fn.getFunction(),
                                       Fn.isStackDeep());
  dispatch_queue_t queue = dispatch_queue_t(Obj);
  dispatch_barrier_async_f(queue, Context, CFn);
}

void WorkQueue::Impl::dispatchBarrierSync(Ty Obj, const DispatchData &Fn) {
  void *Context;
  WorkQueue::DispatchFn CFn;
  std::tie(Context, CFn) = toCFunction(Fn.getContext(), Fn.getFunction(),
                                       Fn.isStackDeep());
  dispatch_queue_t queue = dispatch_queue_t(Obj);
  dispatch_barrier_sync_f(queue, Context, CFn);
}

void WorkQueue::Impl::dispatchOnMain(const DispatchData &Fn) {
  void *Context;
  WorkQueue::DispatchFn CFn;
  std::tie(Context, CFn) = toCFunction(Fn.getContext(), Fn.getFunction(),
                                       Fn.isStackDeep());
  dispatch_async_f(dispatch_get_main_queue(), Context, CFn);
}

void WorkQueue::Impl::dispatchConcurrent(Priority Prio, const DispatchData &Fn) {
  void *Context;
  WorkQueue::DispatchFn CFn;
  std::tie(Context, CFn) = toCFunction(Fn.getContext(), Fn.getFunction(),
                                       Fn.isStackDeep());
  dispatch_async_f(getDispatchGlobalQueue(Prio), Context, CFn);
}

void WorkQueue::Impl::suspend(Ty Obj) {
  dispatch_queue_t queue = dispatch_queue_t(Obj);
  dispatch_suspend(queue);
}

void WorkQueue::Impl::resume(Ty Obj) {
  dispatch_queue_t queue = dispatch_queue_t(Obj);
  dispatch_resume(queue);
}

void WorkQueue::Impl::setPriority(Ty Obj, Priority Prio) {
  dispatch_queue_t queue = dispatch_queue_t(Obj);
  dispatch_set_target_queue(queue, getDispatchGlobalQueue(Prio));
}

llvm::StringRef WorkQueue::Impl::getLabel(const Ty Obj) {
  dispatch_queue_t queue = dispatch_queue_t(Obj);
  return dispatch_queue_get_label(queue);
}

void WorkQueue::Impl::retain(Ty Obj) {
  dispatch_queue_t queue = dispatch_queue_t(Obj);
  dispatch_retain(queue);
}

void WorkQueue::Impl::release(Ty Obj) {
  dispatch_queue_t queue = dispatch_queue_t(Obj);
  dispatch_release(queue);
}
