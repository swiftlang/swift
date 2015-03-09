//===--- Lazy.h - A lazily-initialized object -----------------------------===//
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

#ifndef SWIFT_RUNTIME_LAZY_H
#define SWIFT_RUNTIME_LAZY_H

#ifdef __APPLE__
#include <dispatch/dispatch.h>
#else
#include <mutex>
#endif

namespace swift {

/// A template for lazily-constructed, zero-initialized global objects.
template <class T> class Lazy {
  T Value;
#ifdef __APPLE__
  dispatch_once_t OnceToken;
#else
  std::once_flag OnceToken;
#endif

public:
  T &get() {
#ifdef __APPLE__
    dispatch_once_f(&OnceToken, this, lazyInitCallback);
#else
    std::call_once(OnceToken, lazyInitCallback, this);
#endif
    return Value;
  }

private:
  static void lazyInitCallback(void *Argument) {
    auto self = reinterpret_cast<Lazy *>(Argument);
    ::new (&self->Value) T();
  }
};

} // namespace swift

#endif // SWIFT_RUNTIME_LAZY_H

