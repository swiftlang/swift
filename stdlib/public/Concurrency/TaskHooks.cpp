//===--- TaskHooks.cpp - Swift Task Hooks ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift Task observation callbacks and customization hooks.
//
//===----------------------------------------------------------------------===//

#include "TaskPrivate.h"

namespace swift {
namespace {
std::vector<TaskHooks> hookList;

template <typename PtrType, PtrType TaskHooks::*Member, typename... Args>
void callHooks(Args... args) {
  for (auto &hooks : hookList) {
    auto ptr = hooks.*Member;
    if (ptr) {
      ptr(args...);
    }
  }
}

template <typename PtrType, PtrType TaskHooks::*Member, typename... Args>
bool callHooksUntilHandled(Args... args) {
  for (auto &hooks : hookList) {
    auto ptr = hooks.*Member;
    if (ptr) {
      bool handled = ptr(args...);
      if (handled) {
        return true;
      }
    }
  }
  return false;
}
} // anonymous namespace

constexpr TaskHooks _taskHooks = {
    .hook_enqueueGlobal =
        callHooksUntilHandled<bool (*)(Job *), &TaskHooks::hook_enqueueGlobal, Job *>,
    .cb_enqueue =
        callHooks<void (*)(Job *, ExecutorRef), &TaskHooks::cb_enqueue, Job *, ExecutorRef>,
    .cb_runJobBegin =
        callHooks<void (*)(Job *, ExecutorRef), &TaskHooks::cb_runJobBegin, Job *, ExecutorRef>,
    .cb_runJobEnd =
        callHooks<void (*)(Job *, ExecutorRef), &TaskHooks::cb_runJobEnd, Job *, ExecutorRef>,
    .cb_completeFuture =
        callHooks<void (*)(AsyncTask *), &TaskHooks::cb_completeFuture, AsyncTask *>,
    .cb_completeFutureWaiter =
        callHooks<void (*)(AsyncTask *, AsyncTask *), &TaskHooks::cb_completeFutureWaiter, AsyncTask *, AsyncTask *>,
    .cb_waitFuture =
        callHooks<void (*)(AsyncTask *, AsyncTask *, AsyncTask::FutureFragment::Status status), &TaskHooks::cb_waitFuture, AsyncTask *, AsyncTask *, AsyncTask::FutureFragment::Status>,
};

void swift_task_set_hooks(const TaskHooks *hooks, size_t structSize) {
  TaskHooks tmp = {};
  memcpy(&tmp, hooks, structSize);
  hookList.push_back(tmp);
}
} // namespace swift
