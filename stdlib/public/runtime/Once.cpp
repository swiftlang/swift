//===--- Once.cpp - Runtime support for lazy initialization ----------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift runtime functions in support of lazy initialization.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Once.h"
#include "Debug.h"
#include <type_traits>

using namespace swift;

#ifdef __APPLE__

// On OS X and and iOS, swift_once is implemented using GCD.

#include <dispatch/dispatch.h>
static_assert(std::is_same<swift_once_t, dispatch_once_t>::value,
              "swift_once_t and dispatch_once_t must stay in sync");
#endif
// The compiler generates the swift_once_t values as word-sized zero-initialized
// variables, so we want to make sure swift_once_t isn't larger than the
// platform word or the function below might overwrite something it shouldn't.
static_assert(sizeof(swift_once_t) <= sizeof(void*),
              "swift_once_t must be no larger than the platform word");

/// Runs the given function with the given context argument exactly once.
/// The predicate argument must point to a global or static variable of static
/// extent of type swift_once_t.
void swift::swift_once(swift_once_t *predicate, void (*fn)(void *)) {
#if __APPLE__
  dispatch_once_f(predicate, nullptr, fn);
#else
  // FIXME: We're relying here on the coincidence that libstdc++ uses pthread's
  // pthread_once, and that on glibc pthread_once follows a compatible init
  // process (the token is a word that is atomically incremented from 0 to
  // 1 to 2 during initialization) to work. We should implement our own version
  // that we can rely on to continue to work that way.
  // For more information, see rdar://problem/18499385
  std::call_once(*predicate, [fn]() { fn(nullptr); });
#endif
}
