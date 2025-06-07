//===--- Thread.h - Thread abstraction ------------------------ -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Provides an abstract Thread class that identifies a system thread,
// and can fetch the current and main threads as well as being comparable
// with other Thread instances.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_THREAD_H
#define SWIFT_THREADING_THREAD_H

#include <optional>

#include "Impl.h"

namespace swift {

/// Identifies a thread
class Thread {
public:
  using Id = threading_impl::thread_id;
  using StackBounds = threading_impl::stack_bounds;

private:
  Id id_;

public:
  Thread() {}
  explicit Thread(Id platformId) : id_(platformId) {}
  Thread(const Thread &other) : id_(other.id_) {}
  Thread(Thread &&other) : id_(std::move(other.id_)) {}

  Thread &operator=(const Thread &other) {
    id_ = other.id_;
    return *this;
  }

  Thread &operator=(Thread &&other) {
    id_ = other.id_;
    return *this;
  }

  /// Returns the platform specific thread ID
  Id platformThreadId() const { return id_; }

  /// Returns the currently executing thread
  static Thread current() {
    return Thread(threading_impl::thread_get_current());
  }

  /// Returns true iff executed on the main thread
  static bool onMainThread() { return threading_impl::thread_is_main(); }

  /// Returns true if the two Thread values are equal
  bool operator==(const Thread &other) const {
    return threading_impl::threads_same(id_, other.id_);
  }
  bool operator!=(const Thread &other) const {
    return !threading_impl::threads_same(id_, other.id_);
  }

  // Retrieve the bounds of the current thread's stack
  static std::optional<StackBounds> stackBounds() {
    return threading_impl::thread_get_current_stack_bounds();
  }
};

} // namespace swift

#endif // SWIFT_THREADING_THREAD_H
