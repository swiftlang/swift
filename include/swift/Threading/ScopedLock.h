//===--- ScopedLock.h - ScopedLock ---------------------------- -*- C++ -*-===//
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
// Provides a ScopedLockT utility template that is used by Mutex and
// ConditionVariable.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_SCOPEDLOCK_H
#define SWIFT_THREADING_SCOPEDLOCK_H

namespace swift {

// -- ScopedLock ---------------------------------------------------------------

/// Compile time adjusted stack based object that locks/unlocks the supplied
/// Mutex type. Use the provided typedefs instead of this directly.
template <typename T, bool Inverted>
class ScopedLockT {
  ScopedLockT() = delete;
  ScopedLockT(const ScopedLockT &) = delete;
  ScopedLockT &operator=(const ScopedLockT &) = delete;
  ScopedLockT(ScopedLockT &&) = delete;
  ScopedLockT &operator=(ScopedLockT &&) = delete;

public:
  explicit ScopedLockT(T &l) : Lock(l) {
    if (Inverted) {
      Lock.unlock();
    } else {
      Lock.lock();
    }
  }

  ~ScopedLockT() {
    if (Inverted) {
      Lock.lock();
    } else {
      Lock.unlock();
    }
  }

private:
  T &Lock;
};

} // namespace swift

#endif // SWIFT_THREADING_SCOPEDLOCK_H
