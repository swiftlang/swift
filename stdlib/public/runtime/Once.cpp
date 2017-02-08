//===--- Once.cpp - Runtime support for lazy initialization ---------------===//
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
// Swift runtime functions in support of lazy initialization.
//
//===----------------------------------------------------------------------===//

#include "Private.h"
#include "swift/Runtime/Once.h"
#include "swift/Runtime/Debug.h"
#include <type_traits>

using namespace swift;

#ifdef __APPLE__

// On macOS and iOS, swift_once is implemented using GCD.
// The compiler emits an inline check matching the barrier-free inline fast
// path of dispatch_once(). See SwiftTargetInfo.OnceDonePredicateValue.

#include <dispatch/dispatch.h>
static_assert(std::is_same<swift_once_t, dispatch_once_t>::value,
              "swift_once_t and dispatch_once_t must stay in sync");
#else

// On non-Darwin platforms we do not assume any barrier-free inline path
// and SwiftTargetInfo.OnceDonePredicateValue is unset in the compiler.

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
#if defined(__APPLE__)
  dispatch_once_f(predicate, nullptr, fn);
#elif defined(__CYGWIN__)
  _swift_once_f(predicate, nullptr, fn);
#else
  std::call_once(*predicate, [fn]() { fn(nullptr); });
#endif
}
