//===- Unix/Threading.inc - Unix Threading Implementation ----- -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides the Unix specific implementation of Threading functions.
//
//===----------------------------------------------------------------------===//

#include "Unix.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"

#if defined(__APPLE__)
#include <mach/mach_init.h>
#include <mach/mach_port.h>
#endif

#include <pthread.h>

#if defined(__FreeBSD__) || defined(__OpenBSD__)
#include <pthread_np.h> // For pthread_getthreadid_np() / pthread_set_name_np()
#endif

#if defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
#include <errno.h>
#include <sys/sysctl.h>
#include <sys/user.h>
#include <unistd.h>
#endif

#if defined(__NetBSD__)
#include <lwp.h> // For _lwp_self()
#endif

#if defined(__linux__)
#include <sched.h>       // For sched_getaffinity
#include <sys/syscall.h> // For syscall codes
#include <unistd.h>      // For syscall()
#endif

static void *threadFuncSync(void *Arg) {
  SyncThreadInfo *TI = static_cast<SyncThreadInfo *>(Arg);
  TI->UserFn(TI->UserData);
  return nullptr;
}

static void *threadFuncAsync(void *Arg) {
  std::unique_ptr<AsyncThreadInfo> Info(static_cast<AsyncThreadInfo *>(Arg));
  (*Info)();
  return nullptr;
}

static void
llvm_execute_on_thread_impl(void *(*ThreadFunc)(void *), void *Arg,
                            llvm::Optional<unsigned> StackSizeInBytes,
                            JoiningPolicy JP) {
  int errnum;

  // Construct the attributes object.
  pthread_attr_t Attr;
  if ((errnum = ::pthread_attr_init(&Attr)) != 0) {
    ReportErrnumFatal("pthread_attr_init failed", errnum);
  }

  auto AttrGuard = llvm::make_scope_exit([&] {
    if ((errnum = ::pthread_attr_destroy(&Attr)) != 0) {
      ReportErrnumFatal("pthread_attr_destroy failed", errnum);
    }
  });

  // Set the requested stack size, if given.
  if (StackSizeInBytes) {
    if ((errnum = ::pthread_attr_setstacksize(&Attr, *StackSizeInBytes)) != 0) {
      ReportErrnumFatal("pthread_attr_setstacksize failed", errnum);
    }
  }

  // Construct and execute the thread.
  pthread_t Thread;
  if ((errnum = ::pthread_create(&Thread, &Attr, ThreadFunc, Arg)) != 0)
    ReportErrnumFatal("pthread_create failed", errnum);

  if (JP == JoiningPolicy::Join) {
    // Wait for the thread
    if ((errnum = ::pthread_join(Thread, nullptr)) != 0) {
      ReportErrnumFatal("pthread_join failed", errnum);
    }
  }
}

uint64_t llvm::get_threadid() {
#if defined(__APPLE__)
  // Calling "mach_thread_self()" bumps the reference count on the thread
  // port, so we need to deallocate it. mach_task_self() doesn't bump the ref
  // count.
  thread_port_t Self = mach_thread_self();
  mach_port_deallocate(mach_task_self(), Self);
  return Self;
#elif defined(__FreeBSD__)
  return uint64_t(pthread_getthreadid_np());
#elif defined(__NetBSD__)
  return uint64_t(_lwp_self());
#elif defined(__ANDROID__)
  return uint64_t(gettid());
#elif defined(__linux__)
  return uint64_t(syscall(SYS_gettid));
#else
  return uint64_t(pthread_self());
#endif
}


static constexpr uint32_t get_max_thread_name_length_impl() {
#if defined(__NetBSD__)
  return PTHREAD_MAX_NAMELEN_NP;
#elif defined(__APPLE__)
  return 64;
#elif defined(__linux__)
#if HAVE_PTHREAD_SETNAME_NP
  return 16;
#else
  return 0;
#endif
#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
  return 16;
#elif defined(__OpenBSD__)
  return 32;
#else
  return 0;
#endif
}

uint32_t llvm::get_max_thread_name_length() {
  return get_max_thread_name_length_impl();
}

void llvm::set_thread_name(const Twine &Name) {
  // Make sure the input is null terminated.
  SmallString<64> Storage;
  StringRef NameStr = Name.toNullTerminatedStringRef(Storage);

  // Truncate from the beginning, not the end, if the specified name is too
  // long.  For one, this ensures that the resulting string is still null
  // terminated, but additionally the end of a long thread name will usually
  // be more unique than the beginning, since a common pattern is for similar
  // threads to share a common prefix.
  // Note that the name length includes the null terminator.
  if (get_max_thread_name_length() > 0)
    NameStr = NameStr.take_back(get_max_thread_name_length() - 1);
  (void)NameStr;
#if defined(__linux__)
#if (defined(__GLIBC__) && defined(_GNU_SOURCE)) || defined(__ANDROID__)
#if HAVE_PTHREAD_SETNAME_NP
  ::pthread_setname_np(::pthread_self(), NameStr.data());
#endif
#endif
#elif defined(__FreeBSD__) || defined(__OpenBSD__)
  ::pthread_set_name_np(::pthread_self(), NameStr.data());
#elif defined(__NetBSD__)
  ::pthread_setname_np(::pthread_self(), "%s",
    const_cast<char *>(NameStr.data()));
#elif defined(__APPLE__)
  ::pthread_setname_np(NameStr.data());
#endif
}

void llvm::get_thread_name(SmallVectorImpl<char> &Name) {
  Name.clear();

#if defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
  int pid = ::getpid();
  uint64_t tid = get_threadid();

  struct kinfo_proc *kp = nullptr, *nkp;
  size_t len = 0;
  int error;
  int ctl[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PID | KERN_PROC_INC_THREAD,
    (int)pid };

  while (1) {
    error = sysctl(ctl, 4, kp, &len, nullptr, 0);
    if (kp == nullptr || (error != 0 && errno == ENOMEM)) {
      // Add extra space in case threads are added before next call.
      len += sizeof(*kp) + len / 10;
      nkp = (struct kinfo_proc *)::realloc(kp, len);
      if (nkp == nullptr) {
        free(kp);
        return;
      }
      kp = nkp;
      continue;
    }
    if (error != 0)
      len = 0;
    break;
  }

  for (size_t i = 0; i < len / sizeof(*kp); i++) {
    if (kp[i].ki_tid == (lwpid_t)tid) {
      Name.append(kp[i].ki_tdname, kp[i].ki_tdname + strlen(kp[i].ki_tdname));
      break;
    }
  }
  free(kp);
  return;
#elif defined(__NetBSD__)
  constexpr uint32_t len = get_max_thread_name_length_impl();
  char buf[len];
  ::pthread_getname_np(::pthread_self(), buf, len);

  Name.append(buf, buf + strlen(buf));
#elif defined(__OpenBSD__)
  constexpr uint32_t len = get_max_thread_name_length_impl();
  char buf[len];
  ::pthread_get_name_np(::pthread_self(), buf, len);

  Name.append(buf, buf + strlen(buf));
#elif defined(__linux__)
#if HAVE_PTHREAD_GETNAME_NP
  constexpr uint32_t len = get_max_thread_name_length_impl();
  char Buffer[len] = {'\0'};  // FIXME: working around MSan false positive.
  if (0 == ::pthread_getname_np(::pthread_self(), Buffer, len))
    Name.append(Buffer, Buffer + strlen(Buffer));
#endif
#endif
}

SetThreadPriorityResult llvm::set_thread_priority(ThreadPriority Priority) {
#if defined(__linux__) && defined(SCHED_IDLE)
  // Some *really* old glibcs are missing SCHED_IDLE.
  // http://man7.org/linux/man-pages/man3/pthread_setschedparam.3.html
  // http://man7.org/linux/man-pages/man2/sched_setscheduler.2.html
  sched_param priority;
  // For each of the above policies, param->sched_priority must be 0.
  priority.sched_priority = 0;
  // SCHED_IDLE    for running very low priority background jobs.
  // SCHED_OTHER   the standard round-robin time-sharing policy;
  return !pthread_setschedparam(
             pthread_self(),
             Priority == ThreadPriority::Background ? SCHED_IDLE : SCHED_OTHER,
             &priority)
             ? SetThreadPriorityResult::SUCCESS
             : SetThreadPriorityResult::FAILURE;
#elif defined(__APPLE__)
  // https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/getpriority.2.html
  // When setting a thread into background state the scheduling priority is set
  // to lowest value, disk and network IO are throttled. Network IO will be
  // throttled for any sockets the thread opens after going into background
  // state. Any previously opened sockets are not affected.

  // https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/getiopolicy_np.3.html
  // I/Os with THROTTLE policy are called THROTTLE I/Os. If a THROTTLE I/O
  // request occurs within a small time window (usually a fraction of a second)
  // of another NORMAL I/O request, the thread that issues the THROTTLE I/O is
  // forced to sleep for a certain interval. This slows down the thread that
  // issues the THROTTLE I/O so that NORMAL I/Os can utilize most of the disk
  // I/O bandwidth.
  return !setpriority(PRIO_DARWIN_THREAD, 0,
                      Priority == ThreadPriority::Background ? PRIO_DARWIN_BG
                                                             : 0)
             ? SetThreadPriorityResult::SUCCESS
             : SetThreadPriorityResult::FAILURE;
#endif
  return SetThreadPriorityResult::FAILURE;
}

#include <thread>

int computeHostNumHardwareThreads() {
#ifdef __linux__
  cpu_set_t Set;
  if (sched_getaffinity(0, sizeof(Set), &Set) == 0)
    return CPU_COUNT(&Set);
#endif
  // Guard against std::thread::hardware_concurrency() returning 0.
  if (unsigned Val = std::thread::hardware_concurrency())
    return Val;
  return 1;
}

void llvm::ThreadPoolStrategy::apply_thread_strategy(
    unsigned ThreadPoolNum) const {}

llvm::BitVector llvm::get_thread_affinity_mask() {
  // FIXME: Implement
  llvm_unreachable("Not implemented!");
}

unsigned llvm::get_cpus() { return 1; }
