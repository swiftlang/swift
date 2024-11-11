#include <signal.h>

#include <sys/ptrace.h>
#include <sys/uio.h>

#include <linux/ptrace.h>

static inline
int ptrace_attach(pid_t pid) {
  return ptrace(PTRACE_ATTACH, pid, 0, 0);
}

static inline
int ptrace_detach(pid_t pid) {
  return ptrace(PTRACE_DETACH, pid, 0, 0);
}

static inline
int ptrace_continue(pid_t pid) {
  return ptrace(PTRACE_CONT, pid, 0, 0);
}

static inline
int ptrace_getsiginfo(pid_t pid, siginfo_t *siginfo) {
  return ptrace(PTRACE_GETSIGINFO, pid, 0, siginfo);
}

static inline
int ptrace_pokedata(pid_t pid, unsigned long addr, unsigned long value) {
  return ptrace(PTRACE_POKEDATA, pid, addr, value);
}

static inline
int ptrace_getregset(pid_t pid, int type, struct iovec *regs) {
  return ptrace(PTRACE_GETREGSET, pid, type, regs);
}

static inline
int ptrace_setregset(pid_t pid, int type, struct iovec *regs) {
  return ptrace(PTRACE_SETREGSET, pid, type, regs);
}
