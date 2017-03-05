//===--- ThreadSafeRefCntPtr.h - --------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_SUPPORT_THREADSAFEREFCNTPTR_H
#define LLVM_SOURCEKIT_SUPPORT_THREADSAFEREFCNTPTR_H

#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/Support/Mutex.h"
#include <atomic>
#include <type_traits>
#include <utility>

namespace SourceKit {

class ThreadSafeRefCntPtrImpl {
protected:
  static llvm::sys::Mutex *getMutex(void *Ptr);
};

/// This is to be used judiciously as a global or member variable that can get
/// accessed by multiple threads. Don't use it for parameters or return
/// values, use IntrusiveRefCntPtr for that.
template <typename T>
class ThreadSafeRefCntPtr : ThreadSafeRefCntPtrImpl {
  std::atomic<T*> Obj;

public:
  typedef T element_type;

  ThreadSafeRefCntPtr() : Obj(nullptr) {}
  ThreadSafeRefCntPtr(std::nullptr_t) : Obj(nullptr) {}

  template <class X,
      class = typename std::enable_if<std::is_convertible<X*, T*>::value>::type>
  explicit ThreadSafeRefCntPtr(X* obj) : Obj(obj) {
    if (obj)
      obj->Retain();
  }

  ThreadSafeRefCntPtr(const ThreadSafeRefCntPtr& S) {
    llvm::IntrusiveRefCntPtr<T> Ref = S;
    Obj = Ref.get();
    Ref.resetWithoutRelease();
  }

  template <class X,
      class = typename std::enable_if<std::is_convertible<X*, T*>::value>::type>
  ThreadSafeRefCntPtr(const ThreadSafeRefCntPtr<X>& S) {
    llvm::IntrusiveRefCntPtr<T> Ref = S;
    Obj = Ref.get();
    Ref.resetWithoutRelease();
  }

  ThreadSafeRefCntPtr(ThreadSafeRefCntPtr&& S) : Obj(S.get()) {
    S.Obj = nullptr;
  }

  template <class X,
      class = typename std::enable_if<std::is_convertible<X*, T*>::value>::type>
  ThreadSafeRefCntPtr(ThreadSafeRefCntPtr<X>&& S) : Obj(S.get()) {
    S.Obj = nullptr;
  }

  template <class X,
      class = typename std::enable_if<std::is_convertible<X*, T*>::value>::type>
  ThreadSafeRefCntPtr(llvm::IntrusiveRefCntPtr<X> S) {
    Obj = S.get();
    S.resetWithoutRelease();
  }

  ThreadSafeRefCntPtr& operator=(const ThreadSafeRefCntPtr& S) {
    llvm::IntrusiveRefCntPtr<T> Ref = S;
    swap(Ref);
    return *this;
  }

  template <class X,
      class = typename std::enable_if<std::is_convertible<X*, T*>::value>::type>
  ThreadSafeRefCntPtr& operator=(const ThreadSafeRefCntPtr<X>& S) {
    llvm::IntrusiveRefCntPtr<T> Ref = S;
    swap(Ref);
    return *this;
  }

  ThreadSafeRefCntPtr& operator=(ThreadSafeRefCntPtr&& S) {
    llvm::sys::ScopedLock L(*getMutex(this));
    if (T *O = Obj.load())
      O->Release();
    Obj = S.Obj;
    S.Obj = nullptr;
    return *this;
  }

  template <class X,
      class = typename std::enable_if<std::is_convertible<X*, T*>::value>::type>
  ThreadSafeRefCntPtr& operator=(ThreadSafeRefCntPtr<X>&& S) {
    llvm::sys::ScopedLock L(*getMutex(this));
    if (T *O = Obj.load())
      O->Release();
    Obj = S.Obj;
    S.Obj = nullptr;
    return *this;
  }

  template <class X,
      class = typename std::enable_if<std::is_convertible<X*, T*>::value>::type>
  ThreadSafeRefCntPtr& operator=(llvm::IntrusiveRefCntPtr<X> S) {
    llvm::IntrusiveRefCntPtr<T> Ref = std::move(S);
    swap(Ref);
    return *this;
  }

  operator llvm::IntrusiveRefCntPtr<T>() const {
    llvm::sys::ScopedLock L(*getMutex((void*)this));
    llvm::IntrusiveRefCntPtr<T> Ref(Obj.load());
    return Ref;
  }

  ~ThreadSafeRefCntPtr() {
    if (T *O = Obj.load())
      O->Release();
  }

  T& operator*() const { return *Obj; }

  T* operator->() const { return Obj; }

  T* get() const { return Obj; }

  explicit operator bool() const { return get(); }

  void swap(llvm::IntrusiveRefCntPtr<T> &other) {
    llvm::sys::ScopedLock L(*getMutex(this));
    // FIXME: If ThreadSafeRefCntPtr has private access to IntrusiveRefCntPtr
    // we can eliminate the Retain/Release pair for this->Obj.
    llvm::IntrusiveRefCntPtr<T> Ref(Obj.load());
    Ref.swap(other);
    if (T *O = Obj.load())
      O->Release();
    Obj = Ref.get();
    Ref.resetWithoutRelease();
  }

  void reset() {
    llvm::IntrusiveRefCntPtr<T> NullRef;
    swap(NullRef);
  }

  void resetWithoutRelease() {
    llvm::IntrusiveRefCntPtr<T> Ref;
    swap(Ref);
    Ref.resetWithoutRelease();
  }

private:
  template <typename X>
  friend class ThreadSafeRefCntPtr;
};

template<class T, class U>
inline bool operator==(const ThreadSafeRefCntPtr<T>& A,
                       const ThreadSafeRefCntPtr<U>& B)
{
  return A.get() == B.get();
}

template<class T, class U>
inline bool operator!=(const ThreadSafeRefCntPtr<T>& A,
                       const ThreadSafeRefCntPtr<U>& B)
{
  return A.get() != B.get();
}

template<class T, class U>
inline bool operator==(const ThreadSafeRefCntPtr<T>& A,
                       U* B)
{
  return A.get() == B;
}

template<class T, class U>
inline bool operator!=(const ThreadSafeRefCntPtr<T>& A,
                       U* B)
{
  return A.get() != B;
}

template<class T, class U>
inline bool operator==(T* A,
                       const ThreadSafeRefCntPtr<U>& B)
{
  return A == B.get();
}

template<class T, class U>
inline bool operator!=(T* A,
                       const ThreadSafeRefCntPtr<U>& B)
{
  return A != B.get();
}

template<class T>
inline bool operator==(const ThreadSafeRefCntPtr<T>& A, std::nullptr_t) {
  return A.get() == nullptr;
}

template<class T>
inline bool operator!=(const ThreadSafeRefCntPtr<T>& A, std::nullptr_t) {
  return A.get() != nullptr;
}


} // namespace SourceKit

#endif
