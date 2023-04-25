//===--- Program.cpp - Implement OS Program Concept -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Program.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/StringExtras.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Config/config.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Program.h"

#include <system_error>

#if HAVE_POSIX_SPAWN
#include <spawn.h>
#endif

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

using namespace swift;

int swift::ExecuteInPlace(const char *Program, const char **args,
                          const char **env) {
#if LLVM_ON_UNIX
  int result;
  if (env)
    result = execve(Program, const_cast<char **>(args),
                    const_cast<char **>(env));
  else
    result = execv(Program, const_cast<char **>(args));

  return result;
#else
  llvm::Optional<llvm::ArrayRef<llvm::StringRef>> Env = llvm::None;
  if (env)
    Env = llvm::toStringRefArray(env);
  int result =
      llvm::sys::ExecuteAndWait(Program, llvm::toStringRefArray(args), Env);
  if (result >= 0)
    exit(result);
  return result;
#endif
}

static const char **
toNullTerminatedCStringArray(ArrayRef<StringRef> array,
                             llvm::BumpPtrAllocator &Alloc) {
  size_t size = array.size();
  const char **result = Alloc.Allocate<const char *>(size + 1);
  for (size_t i = 0; i < size; ++i) {
    result[i] = NullTerminatedStringRef(array[i], Alloc).data();
  }
  result[size] = nullptr;
  return result;
}

#if HAVE_UNISTD_H

namespace {
struct Pipe {
  int read;
  int write;

  Pipe() {
    int fds[2];
    if (pipe(fds) == -1) {
      read = write = -1; // '-1' to inidicate the failure.
    }
    read = fds[0];
    write = fds[1];
  }

  operator bool() const { return read != -1 || write != -1; }
};
} // namespace

llvm::ErrorOr<swift::ChildProcessInfo>
swift::ExecuteWithPipe(llvm::StringRef program,
                       llvm::ArrayRef<llvm::StringRef> args,
                       llvm::Optional<llvm::ArrayRef<llvm::StringRef>> env) {
  Pipe p1; // Parent: write, child: read (child's STDIN).
  if (!p1)
    return std::error_code(errno, std::system_category());
  Pipe p2; // Parent: read, child: write (child's STDOUT).
  if (!p2)
    return std::error_code(errno, std::system_category());

  llvm::BumpPtrAllocator Alloc;
  const char **argv = toNullTerminatedCStringArray(args, Alloc);
  const char **envp = nullptr;
  if (env.has_value()) {
    envp = toNullTerminatedCStringArray(*env, Alloc);
  }
  const char *progCStr = args[0] == program
                             ? argv[0]
                             : NullTerminatedStringRef(program, Alloc).data();

  pid_t pid;

#if HAVE_POSIX_SPAWN
  posix_spawn_file_actions_t FileActions;
  posix_spawn_file_actions_init(&FileActions);

  posix_spawn_file_actions_adddup2(&FileActions, p1.read, STDIN_FILENO);
  posix_spawn_file_actions_addclose(&FileActions, p1.write);

  posix_spawn_file_actions_adddup2(&FileActions, p2.write, STDOUT_FILENO);
  posix_spawn_file_actions_addclose(&FileActions, p2.read);

  // Spawn the subtask.
  int error = posix_spawn(&pid, progCStr, &FileActions, nullptr,
                          const_cast<char **>(argv), const_cast<char **>(envp));

  posix_spawn_file_actions_destroy(&FileActions);

  close(p1.read);
  close(p2.write);

  if (error != 0 || pid == 0) {
    close(p1.write);
    close(p2.read);
    return std::error_code(error, std::system_category());
  }
#else
  // Create a child process.
  switch (pid = fork()) {
  // An error occurred:  Return to the caller.
  case -1:
    close(p1.read);
    close(p1.write);
    close(p2.read);
    close(p2.write);
    return std::error_code(errno, std::system_category());

  // Child process.
  case 0:
    close(p1.write);
    close(p2.read);

    // Redirect file descriptors...
    dup2(p1.read, STDIN_FILENO);
    dup2(p2.write, STDOUT_FILENO);

    // Execute the program.
    if (envp) {
      execve(progCStr, const_cast<char **>(argv), const_cast<char **>(envp));
    } else {
      execv(progCStr, const_cast<char **>(argv));
    }

    // If the execv()/execve() failed, we should exit. Follow Unix protocol and
    // return 127 if the executable was not found, and 126 otherwise. Use _exit
    // rather than exit so that atexit functions and static object destructors
    // cloned from the parent process aren't redundantly run, and so that any
    // data buffered in stdio buffers cloned from the parent aren't redundantly
    // written out.
    _exit(errno == ENOENT ? 127 : 126);

  // Parent process.
  default:
    break;
  }
#endif
  close(p1.read);
  close(p2.write);
  return ChildProcessInfo(pid, p1.write, p2.read);
}

#else // HAVE_UNISTD_H

llvm::ErrorOr<swift::ChildProcessInfo>
swift::ExecuteWithPipe(llvm::StringRef program,
                       llvm::ArrayRef<llvm::StringRef> args,
                       llvm::Optional<llvm::ArrayRef<llvm::StringRef>> env) {
  // Not supported.
  return std::errc::not_supported;
}

#endif
