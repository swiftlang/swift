//===--- CrashHandlerFreeBSD.cpp - Swift crash handler for FreeBSD ------- ===//
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
// The FreeBSD crash handler implementation.
//
// We use signal handling rather than trying to use Mach exceptions here,
// because the latter would entail running a separate Mach server thread, and
// creates a much greater risk of interfering with the system wide Crash
// Reporter, which is a no-no.
//
//===----------------------------------------------------------------------===//

#ifdef __FreeBSD__
#include <sys/param.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/sysctl.h>
#include <sys/thr.h>
#include <sys/ucontext.h>
#include <sys/user.h>
#include <sys/umtx.h>

#include <errno.h>
#include <pthread.h>
#include <pthread_np.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
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
const struct kinfo_proc *suspend_other_threads(struct swift::runtime::backtrace::thread *, size_t*);
void resume_other_threads();
bool run_backtracer(int);

int memserver_start();
void* memserver_entry(void *fd);

ssize_t safe_read(int fd, void *buf, size_t len) {
  uint8_t *ptr = (uint8_t *)buf;
  uint8_t *end = ptr + len;
  ssize_t total = 0;

  while (ptr < end) {
    ssize_t ret;
    do {
      ret = read(fd, buf, len);
    } while (ret < 0 && errno == EINTR);
    if (ret < 0)
      return ret;
    total += ret;
    ptr += ret;
    len -= ret;
  }

  return total;
}

ssize_t safe_write(int fd, const void *buf, size_t len) {
  const uint8_t *ptr = (const uint8_t *)buf;
  const uint8_t *end = ptr + len;
  ssize_t total = 0;

  while (ptr < end) {
    ssize_t ret;
    do {
      ret = write(fd, buf, len);
    } while (ret < 0 && errno == EINTR);
    if (ret < 0)
      return ret;
    total += ret;
    ptr += ret;
    len -= ret;
  }

  return total;
}

CrashInfo crashInfo;

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

uint32_t threads_paused = 0;

void
warn(const char *str) {
  write(STDERR_FILENO, str, strlen(str));
}

size_t _getpagesize()
{
  int mib[] = {CTL_HW, HW_PAGESIZE};
  size_t page_size = 0, size_size = sizeof(page_size);
  if (sysctl(mib, 2, &page_size, &size_size, NULL, 0) == -1) {
    page_size = 4096;
  }
  return page_size;
}

int foreach_other_thread_kinfo(pid_t pid,
                               long tid,
                               const struct kinfo_proc ** kpp,
                               size_t *threads_count,
                               int (*perform)(const struct kinfo_proc* const))
{
  struct kinfo_proc *kp = NULL;
  size_t page_size = _getpagesize();
  size_t len = 0, count = 0, map_size = 0, sum = 0;
  int mib[] = {CTL_KERN, KERN_PROC, KERN_PROC_PID | KERN_PROC_INC_THREAD, pid};

  if (sysctl(mib, 4, NULL, &len, NULL, 0) == -1) {
    return -1;
  }

  count = len / sizeof(struct kinfo_proc);
  map_size = ((len / page_size) + (len % page_size ? 1 : 0)) * page_size;
  kp = static_cast<struct kinfo_proc*>(mmap(NULL, map_size,
                                            PROT_READ|PROT_WRITE,
                                            MAP_PRIVATE|MAP_ANON, -1, 0));

  if (sysctl(mib, 4, kp, &len, NULL, 0) == -1) {
    munmap(kp, map_size);
    return -1;
  }

  *kpp = kp;
  *threads_count = count;

  for (unsigned i = 0; i < count; ++i) {
    if (kp[i].ki_tid != tid) {
      if (perform(&kp[i]) != -1) {
        sum++;
      }
    }
  }

  return sum;
}

void
reset_threads(struct swift::runtime::backtrace::thread *first) {
  __atomic_store_n(&crashInfo.thread_list, (uint64_t)first, __ATOMIC_RELEASE);
}

void
add_thread(struct swift::runtime::backtrace::thread *thread) {
  uint64_t next = __atomic_load_n(&crashInfo.thread_list, __ATOMIC_ACQUIRE);
  do {
    thread->next = next;
  } while (!__atomic_compare_exchange_n(&crashInfo.thread_list, &next,
                                        (uint64_t)thread,
                                        false,
                                        __ATOMIC_RELEASE, __ATOMIC_ACQUIRE));
}

void
pause_thread(int signum __attribute__((unused)),
             siginfo_t *pinfo __attribute__((unused)),
             void *uctx)
{
  int old_err = errno;
  long tid;
  thr_self(&tid);

  struct swift::runtime::backtrace::thread self = {  0, (int64_t)tid, (uint64_t)uctx };
  add_thread(&self);

  __atomic_fetch_add(&threads_paused, 1, __ATOMIC_RELEASE);
  _umtx_op(&threads_paused, UMTX_OP_WAKE, 1, NULL, NULL);
  thr_suspend(NULL);
  errno = old_err;
}

void
wait_paused(uint32_t expected, const struct timespec *timeout)
{
  uint32_t current;
  do {
    current = __atomic_load_n(&threads_paused, __ATOMIC_ACQUIRE);
    if (current == expected)
      return;
  } while (!_umtx_op(&threads_paused, UMTX_OP_WAIT, current,
                     const_cast<struct timespec*>(timeout), NULL));
}

const struct kinfo_proc *
suspend_other_threads(struct swift::runtime::backtrace::thread* self, size_t *count)
{
  unsigned max_loops = 15;
  struct sigaction sa, sa_old_prof, sa_old_usr1, sa_old_usr2;
  pid_t pid = getpid();
  long tid;
  thr_self(&tid);

  const struct kinfo_proc *kpp = nullptr;

  reset_threads(self);

  sigfillset(&sa.sa_mask);
  sa.sa_flags = 0;
  sa.sa_handler = NULL;
  sa.sa_sigaction = pause_thread;

  sigaction(SIGPROF, &sa, &sa_old_prof);
  sigaction(SIGUSR1, &sa, &sa_old_usr1);
  sigaction(SIGUSR2, &sa, &sa_old_usr2);

  do {
    int pending = foreach_other_thread_kinfo(pid, tid, &kpp, count, [](auto ki) {
      sigset_t sigmask = ki->ki_sigmask;
      int sig = !sigismember(&sigmask, SIGUSR1) ? SIGUSR1
              : !sigismember(&sigmask, SIGUSR2) ? SIGUSR2
              : !sigismember(&sigmask, SIGPROF) ? SIGPROF
              : -1;
      if (sig != -1) {
        thr_kill(ki->ki_tid, sig);
        return 0;
      } else {
        warn("swift-runtime: failed to suspend thread ");
        warn(ki->ki_tdname);
        warn(" while processing a crash; backtraces will be missing "
            "information\n");
        return -1;
      }
    });

    if (pending) {
      struct timespec timeout = { .tv_sec = 2, .tv_nsec = 0 };
      wait_paused(pending, &timeout);
    } else {
      break;
    }
  } while (max_loops--);

  sigaction(SIGPROF, &sa_old_prof, NULL);
  sigaction(SIGUSR1, &sa_old_usr1, NULL);
  sigaction(SIGUSR2, &sa_old_usr2, NULL);

  return kpp;
}

void
resume_other_threads()
{
  pthread_resume_all_np();
}
void
reset_signal(int signum)
{
  struct sigaction sa;
  sa.sa_handler = SIG_DFL;
  sa.sa_flags = 0;
  sigemptyset(&sa.sa_mask);

  sigaction(signum, &sa, NULL);
}

void
handle_fatal_signal(int signum,
                    siginfo_t *pinfo,
                    void *uctx)
{
  int old_err = errno;
  long cur_thread;
  size_t threads_count = 0;

  thr_self(&cur_thread);

  struct swift::runtime::backtrace::thread self = { 0, (int64_t)cur_thread, (uint64_t)uctx };

  // Prevent this from exploding if more than one thread gets here at once
  const struct kinfo_proc *kp = suspend_other_threads(&self, &threads_count);

  for (unsigned n = 0; n < lengthof(signalsToHandle); ++n)
    signal(signalsToHandle[n], SIG_DFL);

  crashInfo.crashing_thread = cur_thread;
  crashInfo.signal = signum;
  crashInfo.fault_address = (uint64_t)(uintptr_t)pinfo->si_addr;
  crashInfo.kinfo_proc_list = (uint64_t)(uintptr_t)kp;
  crashInfo.kinfo_proc_count = (uint64_t)threads_count;

  int fd = memserver_start();

  void *pc = 0;
  ucontext_t *ctx = (ucontext_t *)uctx;

#if defined(__x86_64__)
  pc = (void *)ctx->uc_mcontext.mc_rip;
#elif defined(__aarch64__) || defined(__arm64__)
  pc = (void *)ctx->uc_mcontext.mc_gpregs.gp_x[15]
#endif

  _swift_displayCrashMessage(signum, pc);
  if (!run_backtracer(fd)) {
    const char *message = _swift_backtraceSettings.color == OnOffTty::On
      ? " failed\n\n" : " failed ***\n\n";
    if (_swift_backtraceSettings.outputTo == OutputTo::Stderr)
      write(STDERR_FILENO, message, strlen(message));
    else
      write(STDOUT_FILENO, message, strlen(message));
  }
  reset_signal(SIGSEGV);
  reset_signal(SIGBUS);

  // Restart the other threads
  resume_other_threads();

  // Restore errno and exit (to crash)
  errno = old_err;
}

char memserver_buffer[4096];
sigjmp_buf memserver_fault_buf;

int
memserver_start()
{
  int ret;
  int fds[2];
  pthread_t thread;
  ret = socketpair(AF_UNIX, SOCK_STREAM, 0, fds);
  if (ret < 0)
    return ret;
  // XXX: pthread_create is not async-signal-safe
  ret = pthread_create(&thread, NULL, memserver_entry, (void *)(intptr_t)fds[0]);
  if (ret < 0) {
    close(fds[0]);
    close(fds[1]);
    return ret;
  }
  return fds[1];
}


void
memserver_fault(int sig) {
  (void)sig;
  siglongjmp(memserver_fault_buf, -1);
}

ssize_t __attribute__((noinline))
memserver_read(void *to, const void *from, size_t len) {
  if (!sigsetjmp(memserver_fault_buf, 1)) {
    memcpy(to, from, len);
    return len;
  } else {
    return -1;
  }
}

void*
memserver_entry(void *fdp) {
  int fd = (int)(intptr_t)fdp;
  int result = -1;

  struct sigaction sa;
  sigfillset(&sa.sa_mask);
  sa.sa_handler = memserver_fault;
  sa.sa_flags = SA_NODEFER;
  sigaction(SIGSEGV, &sa, NULL);
  sigaction(SIGBUS, &sa, NULL);

  for (;;) {
    struct memserver_req req;
    ssize_t ret;

    ret = safe_read(fd, &req, sizeof(req));
    if (ret != sizeof(req))
      break;

    uint64_t addr = req.addr;
    uint64_t bytes = req.len;

    while (bytes) {
      uint64_t todo = (bytes < sizeof(memserver_buffer)
                       ? bytes : sizeof(memserver_buffer));
      ret = memserver_read(memserver_buffer, (void *)addr, (size_t)todo);
      struct memserver_resp resp;

      resp.addr = addr;
      resp.len = ret;
      ret = safe_write(fd, &resp, sizeof(resp));
      if (ret != sizeof(resp))
        goto fail;

      if (resp.len < 0)
        break;
      ret = safe_write(fd, memserver_buffer, resp.len);
      if (ret != resp.len)
        goto fail;

      addr += resp.len;
      bytes -= resp.len;
    }
  }

  result = 0;

fail:
  close(fd);

  return nullptr;
}

char addr_buf[18];
char timeout_buf[22];
char limit_buf[22];
char top_buf[22];
const char *backtracer_argv[] = {
  "swift-backtrace",            // 0
  "--unwind",                   // 1
  "precise",                    // 2
  "--demangle",                 // 3
  "true",                       // 4
  "--interactive",              // 5
  "true",                       // 6
  "--color",                    // 7
  "true",                       // 8
  "--timeout",                  // 9
  timeout_buf,                  // 10
  "--preset",                   // 11
  "friendly",                   // 12
  "--crashinfo",                // 13
  addr_buf,                     // 14
  "--threads",                  // 15
  "preset",                     // 16
  "--registers",                // 17
  "preset",                     // 18
  "--images",                   // 19
  "preset",                     // 20
  "--limit",                    // 21
  limit_buf,                    // 22
  "--top",                      // 23
  top_buf,                      // 24
  "--sanitize",                 // 25
  "preset",                     // 26
  "--cache",                    // 27
  "true",                       // 28
  "--output-to",                // 29
  "stdout",                     // 30
  "--symbolicate",              // 31
  "true",                       // 32
  NULL
};

const char *
trueOrFalse(bool b) {
  return b ? "true" : "false";
}

const char *
trueOrFalse(OnOffTty oot) {
  return trueOrFalse(oot == OnOffTty::On);
}

bool
run_backtracer(int memserver_fd)
{
  // Set-up the backtracer's command line arguments
  switch (_swift_backtraceSettings.algorithm) {
  case UnwindAlgorithm::Fast:
    backtracer_argv[2] = "fast";
    break;
  default:
    backtracer_argv[2] = "precise";
    break;
  }

  // (The TTY option has already been handled at this point, so these are
  //  all either "On" or "Off".)
  backtracer_argv[4] = trueOrFalse(_swift_backtraceSettings.demangle);
  backtracer_argv[6] = trueOrFalse(_swift_backtraceSettings.interactive);
  backtracer_argv[8] = trueOrFalse(_swift_backtraceSettings.color);

  switch (_swift_backtraceSettings.threads) {
  case ThreadsToShow::Preset:
    backtracer_argv[16] = "preset";
    break;
  case ThreadsToShow::All:
    backtracer_argv[16] = "all";
    break;
  case ThreadsToShow::Crashed:
    backtracer_argv[16] = "crashed";
    break;
  }

  switch (_swift_backtraceSettings.registers) {
  case RegistersToShow::Preset:
    backtracer_argv[18] = "preset";
    break;
  case RegistersToShow::None:
    backtracer_argv[18] = "none";
    break;
  case RegistersToShow::All:
    backtracer_argv[18] = "all";
    break;
  case RegistersToShow::Crashed:
    backtracer_argv[18] = "crashed";
    break;
  }

  switch (_swift_backtraceSettings.images) {
  case ImagesToShow::Preset:
    backtracer_argv[20] = "preset";
    break;
  case ImagesToShow::None:
    backtracer_argv[20] = "none";
    break;
  case ImagesToShow::All:
    backtracer_argv[20] = "all";
    break;
  case ImagesToShow::Mentioned:
    backtracer_argv[20] = "mentioned";
    break;
  }

  switch (_swift_backtraceSettings.preset) {
  case Preset::Friendly:
    backtracer_argv[12] = "friendly";
    break;
  case Preset::Medium:
    backtracer_argv[12] = "medium";
    break;
  default:
    backtracer_argv[12] = "full";
    break;
  }

  switch (_swift_backtraceSettings.sanitize) {
  case SanitizePaths::Preset:
    backtracer_argv[26] = "preset";
    break;
  case SanitizePaths::Off:
    backtracer_argv[26] = "false";
    break;
  case SanitizePaths::On:
    backtracer_argv[26] = "true";
    break;
  }

  switch (_swift_backtraceSettings.outputTo) {
  case OutputTo::Stdout:
    backtracer_argv[30] = "stdout";
    break;
  case OutputTo::Auto: // Shouldn't happen, but if it does pick stderr
  case OutputTo::Stderr:
    backtracer_argv[30] = "stderr";
    break;
  }

  backtracer_argv[28] = trueOrFalse(_swift_backtraceSettings.cache);

  switch (_swift_backtraceSettings.symbolicate) {
  case Symbolication::Off:
    backtracer_argv[32] = "off";
    break;
  case Symbolication::Fast:
    backtracer_argv[32] = "fast";
    break;
  case Symbolication::Full:
    backtracer_argv[32] = "full";
    break;
  }

  _swift_formatUnsigned(_swift_backtraceSettings.timeout, timeout_buf);

  if (_swift_backtraceSettings.limit < 0)
    std::strcpy(limit_buf, "none");
  else
    _swift_formatUnsigned(_swift_backtraceSettings.limit, limit_buf);

  _swift_formatUnsigned(_swift_backtraceSettings.top, top_buf);
  _swift_formatAddress(&crashInfo, addr_buf);

  // Actually execute it
  return _swift_spawnBacktracer(backtracer_argv, memserver_fd);
}

} // namespace

#endif // __FreeBSD__
