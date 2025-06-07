//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#pragma once

#include <signal.h>

#include <sys/ptrace.h>
#include <sys/uio.h>

#include <linux/ptrace.h>

static inline
int ptrace_retry(int op, pid_t pid, void *addr, void *data) {
  int retries = 3;
  int result;
  do {
    result = ptrace(op, pid, addr, data);
  } while (result < 0 && (errno == EAGAIN || errno == EBUSY) && retries-- > 0);
  return result;
}

static inline
int ptrace_attach(pid_t pid) {
  return ptrace_retry(PTRACE_ATTACH, pid, 0, 0);
}

static inline
int ptrace_detach(pid_t pid) {
  return ptrace_retry(PTRACE_DETACH, pid, 0, 0);
}

static inline
int ptrace_cont(pid_t pid) {
  return ptrace_retry(PTRACE_CONT, pid, 0, 0);
}

static inline
int ptrace_interrupt(pid_t pid) {
  return ptrace_retry(PTRACE_INTERRUPT, pid, 0, 0);
}

static inline
int ptrace_getsiginfo(pid_t pid, siginfo_t *siginfo) {
  return ptrace_retry(PTRACE_GETSIGINFO, pid, 0, siginfo);
}

static inline
int ptrace_pokedata(pid_t pid, unsigned long addr, unsigned long value) {
  return ptrace_retry(PTRACE_POKEDATA, pid, (void*)(uintptr_t)addr, (void*)(uintptr_t)value);
}

static inline
int ptrace_getregset(pid_t pid, int type, struct iovec *regs) {
  return ptrace_retry(PTRACE_GETREGSET, pid, (void*)(uintptr_t)type, regs);
}

static inline
int ptrace_setregset(pid_t pid, int type, struct iovec *regs) {
  return ptrace_retry(PTRACE_SETREGSET, pid, (void*)(uintptr_t)type, regs);
}
