//===--- CrashRecoveryContext.cpp - Crash Recovery ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/CrashRecoveryContext.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/ThreadLocal.h"
#include <mutex>
#include <setjmp.h>
#if LLVM_ON_UNIX
#include <sysexits.h> // EX_IOERR
#endif

using namespace llvm;

namespace {

struct CrashRecoveryContextImpl;

static ManagedStatic<
    sys::ThreadLocal<const CrashRecoveryContextImpl> > CurrentContext;

struct CrashRecoveryContextImpl {
  // When threads are disabled, this links up all active
  // CrashRecoveryContextImpls.  When threads are enabled there's one thread
  // per CrashRecoveryContext and CurrentContext is a thread-local, so only one
  // CrashRecoveryContextImpl is active per thread and this is always null.
  const CrashRecoveryContextImpl *Next;

  CrashRecoveryContext *CRC;
  ::jmp_buf JumpBuffer;
  volatile unsigned Failed : 1;
  unsigned SwitchedThread : 1;
  unsigned ValidJumpBuffer : 1;

public:
  CrashRecoveryContextImpl(CrashRecoveryContext *CRC) noexcept
      : CRC(CRC), Failed(false), SwitchedThread(false), ValidJumpBuffer(false) {
    Next = CurrentContext->get();
    CurrentContext->set(this);
  }
  ~CrashRecoveryContextImpl() {
    if (!SwitchedThread)
      CurrentContext->set(Next);
  }

  /// Called when the separate crash-recovery thread was finished, to
  /// indicate that we don't need to clear the thread-local CurrentContext.
  void setSwitchedThread() {
#if defined(LLVM_ENABLE_THREADS) && LLVM_ENABLE_THREADS != 0
    SwitchedThread = true;
#endif
  }

  // If the function ran by the CrashRecoveryContext crashes or fails, then
  // 'RetCode' represents the returned error code, as if it was returned by a
  // process. 'Context' represents the signal type on Unix; on Windows, it is
  // the ExceptionContext.
  void HandleCrash(int RetCode, uintptr_t Context) {
    // Eliminate the current context entry, to avoid re-entering in case the
    // cleanup code crashes.
    CurrentContext->set(Next);

    assert(!Failed && "Crash recovery context already failed!");
    Failed = true;

    if (CRC->DumpStackAndCleanupOnFailure)
      sys::CleanupOnSignal(Context);

    CRC->RetCode = RetCode;

    // Jump back to the RunSafely we were called under.
    if (ValidJumpBuffer)
      longjmp(JumpBuffer, 1);

    // Otherwise let the caller decide of the outcome of the crash. Currently
    // this occurs when using SEH on Windows with MSVC or clang-cl.
  }
};
}

static ManagedStatic<std::mutex> gCrashRecoveryContextMutex;
static bool gCrashRecoveryEnabled = false;

static ManagedStatic<sys::ThreadLocal<const CrashRecoveryContext>>
       tlIsRecoveringFromCrash;

static void installExceptionOrSignalHandlers();
static void uninstallExceptionOrSignalHandlers();

CrashRecoveryContextCleanup::~CrashRecoveryContextCleanup() {}

CrashRecoveryContext::~CrashRecoveryContext() {
  // Reclaim registered resources.
  CrashRecoveryContextCleanup *i = head;
  const CrashRecoveryContext *PC = tlIsRecoveringFromCrash->get();
  tlIsRecoveringFromCrash->set(this);
  while (i) {
    CrashRecoveryContextCleanup *tmp = i;
    i = tmp->next;
    tmp->cleanupFired = true;
    tmp->recoverResources();
    delete tmp;
  }
  tlIsRecoveringFromCrash->set(PC);

  CrashRecoveryContextImpl *CRCI = (CrashRecoveryContextImpl *) Impl;
  delete CRCI;
}

bool CrashRecoveryContext::isRecoveringFromCrash() {
  return tlIsRecoveringFromCrash->get() != nullptr;
}

CrashRecoveryContext *CrashRecoveryContext::GetCurrent() {
  if (!gCrashRecoveryEnabled)
    return nullptr;

  const CrashRecoveryContextImpl *CRCI = CurrentContext->get();
  if (!CRCI)
    return nullptr;

  return CRCI->CRC;
}

void CrashRecoveryContext::Enable() {
  std::lock_guard<std::mutex> L(*gCrashRecoveryContextMutex);
  // FIXME: Shouldn't this be a refcount or something?
  if (gCrashRecoveryEnabled)
    return;
  gCrashRecoveryEnabled = true;
  installExceptionOrSignalHandlers();
}

void CrashRecoveryContext::Disable() {
  std::lock_guard<std::mutex> L(*gCrashRecoveryContextMutex);
  if (!gCrashRecoveryEnabled)
    return;
  gCrashRecoveryEnabled = false;
  uninstallExceptionOrSignalHandlers();
}

void CrashRecoveryContext::registerCleanup(CrashRecoveryContextCleanup *cleanup)
{
  if (!cleanup)
    return;
  if (head)
    head->prev = cleanup;
  cleanup->next = head;
  head = cleanup;
}

void
CrashRecoveryContext::unregisterCleanup(CrashRecoveryContextCleanup *cleanup) {
  if (!cleanup)
    return;
  if (cleanup == head) {
    head = cleanup->next;
    if (head)
      head->prev = nullptr;
  }
  else {
    cleanup->prev->next = cleanup->next;
    if (cleanup->next)
      cleanup->next->prev = cleanup->prev;
  }
  delete cleanup;
}

#if defined(_MSC_VER)

#include <windows.h> // for GetExceptionInformation

// If _MSC_VER is defined, we must have SEH. Use it if it's available. It's way
// better than VEH. Vectored exception handling catches all exceptions happening
// on the thread with installed exception handlers, so it can interfere with
// internal exception handling of other libraries on that thread. SEH works
// exactly as you would expect normal exception handling to work: it only
// catches exceptions if they would bubble out from the stack frame with __try /
// __except.

static void installExceptionOrSignalHandlers() {}
static void uninstallExceptionOrSignalHandlers() {}

// We need this function because the call to GetExceptionInformation() can only
// occur inside the __except evaluation block
static int ExceptionFilter(_EXCEPTION_POINTERS *Except) {
  // Lookup the current thread local recovery object.
  const CrashRecoveryContextImpl *CRCI = CurrentContext->get();

  if (!CRCI) {
    // Something has gone horribly wrong, so let's just tell everyone
    // to keep searching
    CrashRecoveryContext::Disable();
    return EXCEPTION_CONTINUE_SEARCH;
  }

  int RetCode = (int)Except->ExceptionRecord->ExceptionCode;
  if ((RetCode & 0xF0000000) == 0xE0000000)
    RetCode &= ~0xF0000000; // this crash was generated by sys::Process::Exit

  // Handle the crash
  const_cast<CrashRecoveryContextImpl *>(CRCI)->HandleCrash(
      RetCode, reinterpret_cast<uintptr_t>(Except));

  return EXCEPTION_EXECUTE_HANDLER;
}

#if defined(__clang__) && defined(_M_IX86)
// Work around PR44697.
__attribute__((optnone))
#endif
bool CrashRecoveryContext::RunSafely(function_ref<void()> Fn) {
  if (!gCrashRecoveryEnabled) {
    Fn();
    return true;
  }
  assert(!Impl && "Crash recovery context already initialized!");
  Impl = new CrashRecoveryContextImpl(this);
  __try {
    Fn();
  } __except (ExceptionFilter(GetExceptionInformation())) {
    return false;
  }
  return true;
}

#else // !_MSC_VER

#if defined(_WIN32)
// This is a non-MSVC compiler, probably mingw gcc or clang without
// -fms-extensions. Use vectored exception handling (VEH).
//
// On Windows, we can make use of vectored exception handling to catch most
// crashing situations.  Note that this does mean we will be alerted of
// exceptions *before* structured exception handling has the opportunity to
// catch it. Unfortunately, this causes problems in practice with other code
// running on threads with LLVM crash recovery contexts, so we would like to
// eventually move away from VEH.
//
// Vectored works on a per-thread basis, which is an advantage over
// SetUnhandledExceptionFilter. SetUnhandledExceptionFilter also doesn't have
// any native support for chaining exception handlers, but VEH allows more than
// one.
//
// The vectored exception handler functionality was added in Windows
// XP, so if support for older versions of Windows is required,
// it will have to be added.

#include "llvm/Support/Windows/WindowsSupport.h"

static LONG CALLBACK ExceptionHandler(PEXCEPTION_POINTERS ExceptionInfo)
{
  // DBG_PRINTEXCEPTION_WIDE_C is not properly defined on all supported
  // compilers and platforms, so we define it manually.
  constexpr ULONG DbgPrintExceptionWideC = 0x4001000AL;
  switch (ExceptionInfo->ExceptionRecord->ExceptionCode)
  {
  case DBG_PRINTEXCEPTION_C:
  case DbgPrintExceptionWideC:
  case 0x406D1388:  // set debugger thread name
    return EXCEPTION_CONTINUE_EXECUTION;
  }

  // Lookup the current thread local recovery object.
  const CrashRecoveryContextImpl *CRCI = CurrentContext->get();

  if (!CRCI) {
    // Something has gone horribly wrong, so let's just tell everyone
    // to keep searching
    CrashRecoveryContext::Disable();
    return EXCEPTION_CONTINUE_SEARCH;
  }

  // TODO: We can capture the stack backtrace here and store it on the
  // implementation if we so choose.

  int RetCode = (int)ExceptionInfo->ExceptionRecord->ExceptionCode;
  if ((RetCode & 0xF0000000) == 0xE0000000)
    RetCode &= ~0xF0000000; // this crash was generated by sys::Process::Exit

  // Handle the crash
  const_cast<CrashRecoveryContextImpl *>(CRCI)->HandleCrash(
      RetCode, reinterpret_cast<uintptr_t>(ExceptionInfo));

  // Note that we don't actually get here because HandleCrash calls
  // longjmp, which means the HandleCrash function never returns.
  llvm_unreachable("Handled the crash, should have longjmp'ed out of here");
}

// Because the Enable and Disable calls are static, it means that
// there may not actually be an Impl available, or even a current
// CrashRecoveryContext at all.  So we make use of a thread-local
// exception table.  The handles contained in here will either be
// non-NULL, valid VEH handles, or NULL.
static sys::ThreadLocal<const void> sCurrentExceptionHandle;

static void installExceptionOrSignalHandlers() {
  // We can set up vectored exception handling now.  We will install our
  // handler as the front of the list, though there's no assurances that
  // it will remain at the front (another call could install itself before
  // our handler).  This 1) isn't likely, and 2) shouldn't cause problems.
  PVOID handle = ::AddVectoredExceptionHandler(1, ExceptionHandler);
  sCurrentExceptionHandle.set(handle);
}

static void uninstallExceptionOrSignalHandlers() {
  PVOID currentHandle = const_cast<PVOID>(sCurrentExceptionHandle.get());
  if (currentHandle) {
    // Now we can remove the vectored exception handler from the chain
    ::RemoveVectoredExceptionHandler(currentHandle);

    // Reset the handle in our thread-local set.
    sCurrentExceptionHandle.set(NULL);
  }
}

#else // !_WIN32

// Generic POSIX implementation.
//
// This implementation relies on synchronous signals being delivered to the
// current thread. We use a thread local object to keep track of the active
// crash recovery context, and install signal handlers to invoke HandleCrash on
// the active object.
//
// This implementation does not attempt to chain signal handlers in any
// reliable fashion -- if we get a signal outside of a crash recovery context we
// simply disable crash recovery and raise the signal again.

#include <signal.h>

static const int Signals[] =
    { SIGABRT, SIGBUS, SIGFPE, SIGILL, SIGSEGV, SIGTRAP };
static const unsigned NumSignals = array_lengthof(Signals);
static struct sigaction PrevActions[NumSignals];

static void CrashRecoverySignalHandler(int Signal) {
  // Lookup the current thread local recovery object.
  const CrashRecoveryContextImpl *CRCI = CurrentContext->get();

  if (!CRCI) {
    // We didn't find a crash recovery context -- this means either we got a
    // signal on a thread we didn't expect it on, the application got a signal
    // outside of a crash recovery context, or something else went horribly
    // wrong.
    //
    // Disable crash recovery and raise the signal again. The assumption here is
    // that the enclosing application will terminate soon, and we won't want to
    // attempt crash recovery again.
    //
    // This call of Disable isn't thread safe, but it doesn't actually matter.
    CrashRecoveryContext::Disable();
    raise(Signal);

    // The signal will be thrown once the signal mask is restored.
    return;
  }

  // Unblock the signal we received.
  sigset_t SigMask;
  sigemptyset(&SigMask);
  sigaddset(&SigMask, Signal);
  sigprocmask(SIG_UNBLOCK, &SigMask, nullptr);

  // As per convention, -2 indicates a crash or timeout as opposed to failure to
  // execute (see llvm/include/llvm/Support/Program.h)
  int RetCode = -2;

  // Don't consider a broken pipe as a crash (see clang/lib/Driver/Driver.cpp)
  if (Signal == SIGPIPE)
    RetCode = EX_IOERR;

  if (CRCI)
    const_cast<CrashRecoveryContextImpl *>(CRCI)->HandleCrash(RetCode, Signal);
}

static void installExceptionOrSignalHandlers() {
  // Setup the signal handler.
  struct sigaction Handler;
  Handler.sa_handler = CrashRecoverySignalHandler;
  Handler.sa_flags = 0;
  sigemptyset(&Handler.sa_mask);

  for (unsigned i = 0; i != NumSignals; ++i) {
    sigaction(Signals[i], &Handler, &PrevActions[i]);
  }
}

static void uninstallExceptionOrSignalHandlers() {
  // Restore the previous signal handlers.
  for (unsigned i = 0; i != NumSignals; ++i)
    sigaction(Signals[i], &PrevActions[i], nullptr);
}

#endif // !_WIN32

bool CrashRecoveryContext::RunSafely(function_ref<void()> Fn) {
  // If crash recovery is disabled, do nothing.
  if (gCrashRecoveryEnabled) {
    assert(!Impl && "Crash recovery context already initialized!");
    CrashRecoveryContextImpl *CRCI = new CrashRecoveryContextImpl(this);
    Impl = CRCI;

    CRCI->ValidJumpBuffer = true;
    if (setjmp(CRCI->JumpBuffer) != 0) {
      return false;
    }
  }

  Fn();
  return true;
}

#endif // !_MSC_VER

LLVM_ATTRIBUTE_NORETURN
void CrashRecoveryContext::HandleExit(int RetCode) {
#if defined(_WIN32)
  // SEH and VEH
  ::RaiseException(0xE0000000 | RetCode, 0, 0, NULL);
#else
  // On Unix we don't need to raise an exception, we go directly to
  // HandleCrash(), then longjmp will unwind the stack for us.
  CrashRecoveryContextImpl *CRCI = (CrashRecoveryContextImpl *)Impl;
  assert(CRCI && "Crash recovery context never initialized!");
  CRCI->HandleCrash(RetCode, 0 /*no sig num*/);
#endif
  llvm_unreachable("Most likely setjmp wasn't called!");
}

// FIXME: Portability.
static void setThreadBackgroundPriority() {
#ifdef __APPLE__
  setpriority(PRIO_DARWIN_THREAD, 0, PRIO_DARWIN_BG);
#endif
}

static bool hasThreadBackgroundPriority() {
#ifdef __APPLE__
  return getpriority(PRIO_DARWIN_THREAD, 0) == 1;
#else
  return false;
#endif
}

namespace {
struct RunSafelyOnThreadInfo {
  function_ref<void()> Fn;
  CrashRecoveryContext *CRC;
  bool UseBackgroundPriority;
  bool Result;
};
}

static void RunSafelyOnThread_Dispatch(void *UserData) {
  RunSafelyOnThreadInfo *Info =
    reinterpret_cast<RunSafelyOnThreadInfo*>(UserData);

  if (Info->UseBackgroundPriority)
    setThreadBackgroundPriority();

  Info->Result = Info->CRC->RunSafely(Info->Fn);
}
bool CrashRecoveryContext::RunSafelyOnThread(function_ref<void()> Fn,
                                             unsigned RequestedStackSize) {
  bool UseBackgroundPriority = hasThreadBackgroundPriority();
  RunSafelyOnThreadInfo Info = { Fn, this, UseBackgroundPriority, false };
  llvm_execute_on_thread(RunSafelyOnThread_Dispatch, &Info,
                         RequestedStackSize == 0
                             ? llvm::None
                             : llvm::Optional<unsigned>(RequestedStackSize));
  if (CrashRecoveryContextImpl *CRC = (CrashRecoveryContextImpl *)Impl)
    CRC->setSwitchedThread();
  return Info.Result;
}
