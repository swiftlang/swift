//===--- CrashHandlerMacOS.cpp - Swift crash handler for macOS ----------- ===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The macOS crash handler implementation.
//
// We use signal handling rather than trying to use Mach exceptions here,
// because the latter would entail running a separate Mach server thread, and
// creates a much greater risk of interfering with the system wide Crash
// Reporter, which is a no-no.
//
//===----------------------------------------------------------------------===//

#ifdef __APPLE__

#include <TargetConditionals.h>

#if TARGET_OS_OSX || TARGET_OS_MACCATALYST

#include <mach/mach.h>
#include <mach/task.h>
#include <mach/thread_act.h>

#include <sys/mman.h>
#include <sys/ucontext.h>
#include <sys/wait.h>

#include <os/lock.h>

#include <errno.h>
#include <signal.h>
#include <spawn.h>
#include <unistd.h>

#include "swift/Runtime/Backtrace.h"

#include <cstring>

#include "BacktracePrivate.h"

#ifndef lengthof
#define lengthof(x)     (sizeof(x) / sizeof(x[0]))
#endif

using namespace swift::runtime::backtrace;

namespace {

void handle_fatal_signal(int signum, siginfo_t *pinfo, void *uctx);
void suspend_other_threads();
void resume_other_threads();

CrashInfo crashInfo;

os_unfair_lock crashLock = OS_UNFAIR_LOCK_INIT;

const int signalsToHandle[] = {
  SIGQUIT,
  SIGABRT,
  SIGBUS,
  SIGFPE,
  SIGILL,
  SIGSEGV,
  SIGTRAP
};

} // namespace

namespace swift {
namespace runtime {
namespace backtrace {

SWIFT_RUNTIME_STDLIB_INTERNAL int
_swift_installCrashHandler()
{
  stack_t ss;

  // See if an alternate signal stack already exists
  if (sigaltstack(NULL, &ss) < 0)
    return errno;

  if (ss.ss_sp == 0) {
    // No, so set one up
    ss.ss_flags = 0;
    ss.ss_size = SIGSTKSZ;
    ss.ss_sp = mmap(0, ss.ss_size, PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (ss.ss_sp == MAP_FAILED)
      return errno;

    if (sigaltstack(&ss, NULL) < 0)
      return errno;
  }

  // Now register signal handlers
  struct sigaction sa;

  sigfillset(&sa.sa_mask);
  for (unsigned n = 0; n < lengthof(signalsToHandle); ++n) {
    sigdelset(&sa.sa_mask, signalsToHandle[n]);
  }

  sa.sa_handler = NULL;
  sa.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_NODEFER;
  sa.sa_sigaction = handle_fatal_signal;

  for (unsigned n = 0; n < lengthof(signalsToHandle); ++n) {
    struct sigaction osa;

    // See if a signal handler for this signal is already installed
    if (sigaction(signalsToHandle[n], NULL, &osa) < 0)
      return errno;

    if (osa.sa_handler == SIG_DFL) {
      // No, so install ours
      if (sigaction(signalsToHandle[n], &sa, NULL) < 0)
        return errno;
    }
  }

  return 0;
}

} // namespace backtrace
} // namespace runtime
} // namespace swift

namespace {

void
suspend_other_threads()
{
  os_unfair_lock_lock(&crashLock);

  thread_t self = mach_thread_self();
  thread_act_array_t threads;
  mach_msg_type_number_t count = 0;

  kern_return_t kr = task_threads(mach_task_self(), &threads, &count);

  if (kr != KERN_SUCCESS)
    return;

  for (unsigned n = 0; n < count; ++n) {
    if (threads[n] == self)
      continue;

    // Ignore the results of these two; if they fail there's nothing we can do
    (void)thread_suspend(threads[n]);
    (void)mach_port_deallocate(mach_task_self(), threads[n]);
  }

  vm_deallocate(mach_task_self(),
                (vm_address_t)threads,
                count * sizeof(threads[0]));

  os_unfair_lock_unlock(&crashLock);
}

void
resume_other_threads()
{
  os_unfair_lock_lock(&crashLock);

  thread_t self = mach_thread_self();
  thread_act_array_t threads;
  mach_msg_type_number_t count = 0;

  kern_return_t kr = task_threads(mach_task_self(), &threads, &count);

  if (kr != KERN_SUCCESS)
    return;

  for (unsigned n = 0; n < count; ++n) {
    if (threads[n] == self)
      continue;

    // Ignore the results of these two; if they fail there's nothing we can do
    (void)thread_resume(threads[n]);
    (void)mach_port_deallocate(mach_task_self(), threads[n]);
  }

  vm_deallocate(mach_task_self(),
                (vm_address_t)threads,
                count * sizeof(threads[0]));

  os_unfair_lock_unlock(&crashLock);
}

void
handle_fatal_signal(int signum,
                    siginfo_t *pinfo,
                    void *uctx)
{
  int old_err = errno;

  // Prevent this from exploding if more than one thread gets here at once
  suspend_other_threads();

  // Remove our signal handlers; crashes should kill us here
  for (unsigned n = 0; n < lengthof(signalsToHandle); ++n)
    signal(signalsToHandle[n], SIG_DFL);

  // Get our thread identifier
  thread_identifier_info_data_t ident_info;
  mach_msg_type_number_t ident_size = THREAD_IDENTIFIER_INFO_COUNT;

  int ret = thread_info(mach_thread_self(),
                        THREAD_IDENTIFIER_INFO,
                        (int *)&ident_info,
                        &ident_size);
  if (ret != KERN_SUCCESS)
    return;

  // Fill in crash info
  crashInfo.crashing_thread = ident_info.thread_id;
  crashInfo.signal = signum;
  crashInfo.fault_address = (uint64_t)pinfo->si_addr;
  crashInfo.mctx = (uint64_t)(((ucontext_t *)uctx)->uc_mcontext);

  // Display a progress message
  void *pc = 0;
  ucontext_t *ctx = (ucontext_t *)uctx;

#if defined(__arm64__) && __DARWIN_OPAQUE_ARM_THREAD_STATE64
#define THREAD_STATE_MEMBER(x) __opaque_##x
#elif __DARWIN_UNIX03
#define THREAD_STATE_MEMBER(x) __##x
#else
#define THREAD_STATE_MEMBER(x) x
#endif

#if __DARWIN_UNIX03
#define CTX_MEMBER(x) __##x
#else
#define CTX_MEMBER(x) x
#endif

#if defined(__x86_64__)
  pc = (void *)(ctx->uc_mcontext->CTX_MEMBER(ss).THREAD_STATE_MEMBER(rip));
#elif defined(__arm64__)
  pc = (void *)(ctx->uc_mcontext->CTX_MEMBER(ss).THREAD_STATE_MEMBER(pc));
#endif

  _swift_displayCrashMessage(signum, pc);

  /* Start the backtracer; this will suspend the process, so there's no need
     to try to suspend other threads from here. */
  if (!_swift_spawnBacktracer(&crashInfo)) {
    const char *message = _swift_backtraceSettings.color == OnOffTty::On
      ? " failed\n\n" : " failed ***\n\n";
    if (_swift_backtraceSettings.outputTo == OutputTo::Stderr)
      write(STDERR_FILENO, message, strlen(message));
    else
      write(STDOUT_FILENO, message, strlen(message));
  }

  // Restart the other threads
  resume_other_threads();

  // Restore errno and exit (to crash)
  errno = old_err;
}

} // namespace

#endif // TARGET_OS_OSX || TARGET_OS_MACCATALYST

#endif // __APPLE__
