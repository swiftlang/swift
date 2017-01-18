//===--- ThreadSafeRefCounted.h - Thread-safe Refcounting Base --*- C++ -*-===//
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

#ifndef SWIFT_BASIC_THREADSAFEREFCOUNTED_H
#define SWIFT_BASIC_THREADSAFEREFCOUNTED_H

#include <atomic>
#include <cassert>
#include "llvm/ADT/IntrusiveRefCntPtr.h"

namespace swift {

/// A class that has the same function as \c ThreadSafeRefCountedBase, but with
/// a virtual destructor.
///
/// Should be used instead of \c ThreadSafeRefCountedBase for classes that
/// already have virtual methods to enforce dynamic allocation via 'new'.
/// FIXME: This should eventually move to llvm.
class ThreadSafeRefCountedBaseVPTR {
  mutable std::atomic<unsigned> ref_cnt;
  virtual void anchor();

protected:
  ThreadSafeRefCountedBaseVPTR() : ref_cnt(0) {}
  virtual ~ThreadSafeRefCountedBaseVPTR() {}

public:
  void Retain() const {
    ref_cnt += 1;
  }

  void Release() const {
    int refCount = static_cast<int>(--ref_cnt);
    assert(refCount >= 0 && "Reference count was already zero.");
    if (refCount == 0) delete this;
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_THREADSAFEREFCOUNTED_H
