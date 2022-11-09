//===--- Exclusivity.cpp --------------------------------------------------===//
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
//
// This implements the runtime support for dynamically tracking exclusivity.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Exclusivity.h"
#include "swift/Basic/Lazy.h"

#include <dlfcn.h>

void swift::swift_task_enterThreadLocalContextBackdeploy56(char *state) {
  const auto enterThreadLocalContext =
      reinterpret_cast<void(*)(char *state)>(SWIFT_LAZY_CONSTANT(
          dlsym(RTLD_DEFAULT, "swift_task_enterThreadLocalContext")));
  if (enterThreadLocalContext)
    enterThreadLocalContext(state);
}

void swift::swift_task_exitThreadLocalContextBackdeploy56(char *state) {
  const auto exitThreadLocalContext =
      reinterpret_cast<void(*)(char *state)>(SWIFT_LAZY_CONSTANT(
          dlsym(RTLD_DEFAULT, "swift_task_exitThreadLocalContext")));
  if (exitThreadLocalContext)
    exitThreadLocalContext(state);
}
