//===--- ThreadSafeRefCounted.h - Thread-safe Refcounting Base --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_THREADSAFEREFCOUNTED_H
#define SWIFT_BASIC_THREADSAFEREFCOUNTED_H

#include <atomic>
#include <cassert>

namespace swift {

/// A thread-safe version of \c llvm::RefCountedBase.
///
/// A generic base class for objects that wish to have their lifetimes managed
/// using reference counts. Classes subclass \c ThreadSafeRefCountedBase to
/// obtain such functionality, and are typically handled with
/// \c IntrusiveRefCntPtr "smart pointers" which automatically handle the
/// management of reference counts.
/// FIXME: This should eventually move to llvm.
template <class Derived>
class ThreadSafeRefCountedBase {
  mutable std::atomic<unsigned> ref_cnt;

protected:
  ThreadSafeRefCountedBase() : ref_cnt(0) {}

public:
  void Retain() const {
    ref_cnt.fetch_add(1, std::memory_order_acq_rel);
  }

  void Release() const {
    int refCount =
        static_cast<int>(ref_cnt.fetch_sub(1, std::memory_order_acq_rel));
    assert(refCount >= 0 && "Reference count was already zero.");
    if (refCount == 0) delete static_cast<const Derived*>(this);
  }
};

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
    ref_cnt.fetch_add(1, std::memory_order_acq_rel);
  }

  void Release() const {
    int refCount =
        static_cast<int>(ref_cnt.fetch_sub(1, std::memory_order_acq_rel));
    assert(refCount >= 0 && "Reference count was already zero.");
    if (refCount == 0) delete this;
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_THREADSAFEREFCOUNTED_H
