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
#include <dispatch/dispatch.h>
#include <type_traits>

using namespace swift;

static_assert(std::is_same<swift_once_t, dispatch_once_t>::value,
              "swift_once_t and dispatch_once_t must stay in sync");

/// Runs the given function with the given context argument exactly once.
/// The predicate argument must point to a global or static variable of static
/// extent of type swift_once_t.

void swift::swift_once(swift_once_t *predicate, void (*fn)(HeapObject *ctx),
                       HeapObject *ctx) {
  // Swift convention passes ctx in here at +1, but 'fn' should also consume
  // its argument at +1, so calling it exactly once should perfectly balance
  // the retain count without any additional work.
  dispatch_once_f(predicate, reinterpret_cast<void*>(ctx),
                  reinterpret_cast<dispatch_function_t>(fn));
}
