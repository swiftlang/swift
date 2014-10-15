#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

static void CrashCatcher(int sig)
{
  const char *msg;
  switch (sig) {
    case SIGILL:  msg = "CRASHED: SIGILL\n";  break;
    case SIGTRAP: msg = "CRASHED: SIGTRAP\n"; break;
    case SIGABRT: msg = "CRASHED: SIGABRT\n"; break;
    case SIGFPE:  msg = "CRASHED: SIGFPE\n";  break;
    case SIGBUS:  msg = "CRASHED: SIGBUS\n";  break;
    case SIGSEGV: msg = "CRASHED: SIGSEGV\n"; break;
    case SIGSYS:  msg = "CRASHED: SIGSYS\n";  break;
    default:      msg = "CRASHED: SIG????\n"; break;
  }
  write(STDERR_FILENO, msg, strlen(msg));
  _exit(0);
}

static void CatchCrashes(void) __attribute__((used,constructor));
static void CatchCrashes(void)
{
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
