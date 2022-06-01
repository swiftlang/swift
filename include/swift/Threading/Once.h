//===--- Once.h - Runtime support for lazy initialization -------*- C++ -*-===//
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

#ifndef SWIFT_THREADING_ONCE_H
#define SWIFT_THREADING_ONCE_H

#include "Impl.h"

namespace swift {

using once_t = threading_impl::once_t;

/// Runs the given function with the given context argument exactly once.
/// The predicate argument must refer to a global or static variable of static
/// extent of type swift::once_t.
inline void once(once_t &predicate, void (*fn)(void *),
                 void *context = nullptr) {
  threading_impl::once_impl(predicate, fn, context);
}

/// Executes the given callable exactly once.
/// The predicate argument must refer to a global or static variable of static
/// extent of type swift::once_t.
template <typename Callable>
inline void once(once_t &predicate, const Callable &callable) {
  once(predicate, [](void *ctx) {
    const Callable &callable = *(const Callable*)(ctx);
    callable();
  }, (void *)(&callable));
}

} // namespace swift

#endif // SWIFT_THREADING_ONCE_H
