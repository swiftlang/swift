#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

static void CrashCatcher(int Sig) {
  const char *Msg;
  switch (Sig) {
    case SIGILL:  Msg = "CRASHED: SIGILL\n";  break;
    case SIGTRAP: Msg = "CRASHED: SIGTRAP\n"; break;
    case SIGABRT: Msg = "CRASHED: SIGABRT\n"; break;
    case SIGFPE:  Msg = "CRASHED: SIGFPE\n";  break;
    case SIGBUS:  Msg = "CRASHED: SIGBUS\n";  break;
    case SIGSEGV: Msg = "CRASHED: SIGSEGV\n"; break;
    case SIGSYS:  Msg = "CRASHED: SIGSYS\n";  break;
    default:      Msg = "CRASHED: SIG????\n"; break;
  }
  write(STDERR_FILENO, Msg, strlen(Msg));
  _exit(0);
}

extern "C" void swift_stdlib_installTrapInterceptor() {
  // Disable buffering on stdout so that everything is printed before crashing.
  setbuf(stdout, 0);

  signal(SIGILL,  CrashCatcher);
  signal(SIGTRAP, CrashCatcher);
  signal(SIGABRT, CrashCatcher);
  signal(SIGFPE,  CrashCatcher);
  signal(SIGBUS,  CrashCatcher);
  signal(SIGSEGV, CrashCatcher);
  signal(SIGSYS,  CrashCatcher);
}

