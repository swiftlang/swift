//===-- llvm/Support/Threading.cpp- Control multithreading mode --*- C++ -*-==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines helper functions for running LLVM in a multi-threaded
// environment.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/Threading.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Config/config.h"
#include "llvm/Support/Host.h"

#include <cassert>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

using namespace llvm;

//===----------------------------------------------------------------------===//
//=== WARNING: Implementation here must contain only TRULY operating system
//===          independent code.
//===----------------------------------------------------------------------===//

bool llvm::llvm_is_multithreaded() {
#if LLVM_ENABLE_THREADS != 0
  return true;
#else
  return false;
#endif
}

#if LLVM_ENABLE_THREADS == 0 ||                                                \
    (!defined(_WIN32) && !defined(HAVE_PTHREAD_H))
// Support for non-Win32, non-pthread implementation.
void llvm::llvm_execute_on_thread(void (*Fn)(void *), void *UserData,
                                  llvm::Optional<unsigned> StackSizeInBytes) {
  (void)StackSizeInBytes;
  Fn(UserData);
}

uint64_t llvm::get_threadid() { return 0; }

uint32_t llvm::get_max_thread_name_length() { return 0; }

void llvm::set_thread_name(const Twine &Name) {}

void llvm::get_thread_name(SmallVectorImpl<char> &Name) { Name.clear(); }

llvm::BitVector llvm::get_thread_affinity_mask() { return {}; }

unsigned llvm::ThreadPoolStrategy::compute_thread_count() const {
  // When threads are disabled, ensure clients will loop at least once.
  return 1;
}

#if LLVM_ENABLE_THREADS == 0
void llvm::llvm_execute_on_thread_async(
    llvm::unique_function<void()> Func,
    llvm::Optional<unsigned> StackSizeInBytes) {
  (void)Func;
  (void)StackSizeInBytes;
  report_fatal_error("Spawning a detached thread doesn't make sense with no "
                     "threading support");
}
#else
// Support for non-Win32, non-pthread implementation.
void llvm::llvm_execute_on_thread_async(
    llvm::unique_function<void()> Func,
    llvm::Optional<unsigned> StackSizeInBytes) {
  (void)StackSizeInBytes;
  std::thread(std::move(Func)).detach();
}
#endif

#else

int computeHostNumHardwareThreads();

unsigned llvm::ThreadPoolStrategy::compute_thread_count() const {
  int MaxThreadCount = UseHyperThreads ? computeHostNumHardwareThreads()
                                       : sys::getHostNumPhysicalCores();
  if (MaxThreadCount <= 0)
    MaxThreadCount = 1;
  if (ThreadsRequested == 0)
    return MaxThreadCount;
  if (!Limit)
    return ThreadsRequested;
  return std::min((unsigned)MaxThreadCount, ThreadsRequested);
}

namespace {
struct SyncThreadInfo {
  void (*UserFn)(void *);
  void *UserData;
};

using AsyncThreadInfo = llvm::unique_function<void()>;

enum class JoiningPolicy { Join, Detach };
} // namespace

// Include the platform-specific parts of this class.
#ifdef LLVM_ON_UNIX
#include "Unix/Threading.inc"
#endif
#ifdef _WIN32
#include "Windows/Threading.inc"
#endif

void llvm::llvm_execute_on_thread(void (*Fn)(void *), void *UserData,
                                  llvm::Optional<unsigned> StackSizeInBytes) {

  SyncThreadInfo Info = {Fn, UserData};
  llvm_execute_on_thread_impl(threadFuncSync, &Info, StackSizeInBytes,
                              JoiningPolicy::Join);
}

void llvm::llvm_execute_on_thread_async(
    llvm::unique_function<void()> Func,
    llvm::Optional<unsigned> StackSizeInBytes) {
  llvm_execute_on_thread_impl(&threadFuncAsync,
                              new AsyncThreadInfo(std::move(Func)),
                              StackSizeInBytes, JoiningPolicy::Detach);
}

#endif

Optional<ThreadPoolStrategy>
llvm::get_threadpool_strategy(StringRef Num, ThreadPoolStrategy Default) {
  if (Num == "all")
    return llvm::hardware_concurrency();
  if (Num.empty())
    return Default;
  unsigned V;
  if (Num.getAsInteger(10, V))
    return None; // malformed 'Num' value
  if (V == 0)
    return Default;

  // Do not take the Default into account. This effectively disables
  // heavyweight_hardware_concurrency() if the user asks for any number of
  // threads on the cmd-line.
  ThreadPoolStrategy S = llvm::hardware_concurrency();
  S.ThreadsRequested = V;
  return S;
}
