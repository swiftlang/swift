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

#include "llvm/ADT/StringExtras.h"
#include "llvm/Config/config.h"
#include "llvm/Support/Program.h"

#if LLVM_ON_UNIX
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif

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
