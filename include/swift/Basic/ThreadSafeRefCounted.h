//===--- ThreadSafeRefCounted.h - -------------------------------*- C++ -*-===//
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
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_THREADSAFEREFCOUNTED_H
#define SWIFT_BASIC_THREADSAFEREFCOUNTED_H

#include "llvm/Support/Atomic.h"
#include <cassert>

namespace swift {

/// \brief A thread-safe version of llvm::RefCountedBase.
/// FIXME: This should eventually move to llvm.
template <class Derived>
class ThreadSafeRefCountedBase {
  mutable llvm::sys::cas_flag ref_cnt;

protected:
  ThreadSafeRefCountedBase() : ref_cnt(0) {}

public:
  void Retain() const {
    llvm::sys::AtomicIncrement(&ref_cnt);
  }

  void Release() const {
    int refCount = static_cast<int>(llvm::sys::AtomicDecrement(&ref_cnt));
    assert(refCount >= 0 && "Reference count was already zero.");
    if (refCount == 0) delete static_cast<const Derived*>(this);
  }
};

class ThreadSafeRefCountedBaseVPTR {
  mutable llvm::sys::cas_flag ref_cnt;
  virtual void anchor();

protected:
  ThreadSafeRefCountedBaseVPTR() : ref_cnt(0) {}
  virtual ~ThreadSafeRefCountedBaseVPTR() {}

public:
  void Retain() const {
    llvm::sys::AtomicIncrement(&ref_cnt);
  }

  void Release() const {
    int refCount = static_cast<int>(llvm::sys::AtomicDecrement(&ref_cnt));
    assert(refCount >= 0 && "Reference count was already zero.");
    if (refCount == 0) delete this;
  }
};

} // end namespace swift.

#endif
