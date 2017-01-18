//===--- Concurrency.h - ----------------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_SUPPORT_CONCURRENCY_H
#define LLVM_SOURCEKIT_SUPPORT_CONCURRENCY_H

#include "llvm/ADT/StringRef.h"

namespace SourceKit {

class Semaphore {
public:
  Semaphore(long count) {
    ImplObj = Impl::create(count);
  }
  ~Semaphore() {
    if (ImplObj)
      Impl::release(ImplObj);
  }

  void signal() {
    Impl::signal(ImplObj);
  }

  bool wait() {
    return Impl::wait(ImplObj);
  }

  bool wait(long milliseconds) {
    return Impl::wait(ImplObj, milliseconds);
  }

  Semaphore(const Semaphore &Other) = delete;
  Semaphore(Semaphore &&Other) {
    ImplObj = std::move(Other.ImplObj);
    Other.ImplObj = Impl::Ty();
  }

  Semaphore &operator=(const Semaphore &Other) = delete;
  Semaphore &operator=(Semaphore &&Other) {
    Semaphore Tmp(std::move(Other));
    std::swap(ImplObj, Tmp.ImplObj);
    return *this;
  }
private:
  // Platform-specific implementation.
  struct Impl {
    typedef void *Ty;
    static Ty create(long count);
    static void signal(Ty Obj);
    static bool wait(Ty Obj);
    static bool wait(Ty Obj, long milliseconds);
    static void retain(Ty Obj);
    static void release(Ty Obj);
  };

  Impl::Ty ImplObj;
};

class WorkQueue {
public:
  enum class Dequeuing {
    Serial,
    Concurrent
  };
  enum class Priority {
    High,
    Default,
    Low,
    Background
  };

  typedef void (*DispatchFn)(void *Context);

  WorkQueue() : ImplObj(0) { }
  WorkQueue(Dequeuing DeqKind, llvm::StringRef Label,
            Priority Prio = Priority::Default) {
    ImplObj = Impl::create(DeqKind, Prio, Label);
  }
  ~WorkQueue() {
    if (ImplObj)
      Impl::release(ImplObj);
  }

  llvm::StringRef getLabel() const {
    return Impl::getLabel(ImplObj);
  }

  void dispatch(void *Context, DispatchFn Fn, bool isStackDeep = false) {
    Impl::dispatch(ImplObj, DispatchData(Context, Fn, isStackDeep));
  }
  template <typename Callable>
  void dispatch(Callable &&Fn, bool isStackDeep = false) {
    Impl::dispatch(ImplObj, DispatchData(std::forward<Callable>(Fn),
                                         isStackDeep));
  }

  void dispatchSync(void *Context, DispatchFn Fn, bool isStackDeep = false) {
    Impl::dispatchSync(ImplObj, DispatchData(Context, Fn, isStackDeep));
  }
  template <typename Callable>
  void dispatchSync(Callable &&Fn, bool isStackDeep = false) {
    Impl::dispatchSync(ImplObj, DispatchData(std::forward<Callable>(Fn),
                                             isStackDeep));
  }

  void dispatchBarrier(void *Context, DispatchFn Fn, bool isStackDeep = false) {
    Impl::dispatchBarrier(ImplObj, DispatchData(Context, Fn, isStackDeep));
  }
  template <typename Callable>
  void dispatchBarrier(Callable &&Fn, bool isStackDeep = false) {
    Impl::dispatchBarrier(ImplObj, DispatchData(std::forward<Callable>(Fn),
                                                isStackDeep));
  }

  void dispatchBarrierSync(void *Context, DispatchFn Fn,
                           bool isStackDeep = false) {
    Impl::dispatchBarrierSync(ImplObj, DispatchData(Context, Fn, isStackDeep));
  }
  template <typename Callable>
  void dispatchBarrierSync(Callable &&Fn, bool isStackDeep = false) {
    Impl::dispatchBarrierSync(ImplObj, DispatchData(std::forward<Callable>(Fn),
                                                    isStackDeep));
  }

  static void dispatchOnMain(void *Context, DispatchFn Fn,
                             bool isStackDeep = false) {
    Impl::dispatchOnMain(DispatchData(Context, Fn, isStackDeep));
  }
  template <typename Callable>
  static void dispatchOnMain(Callable &&Fn, bool isStackDeep = false) {
    Impl::dispatchOnMain(DispatchData(std::forward<Callable>(Fn), isStackDeep));
  }

  static void dispatchConcurrent(void *Context, DispatchFn Fn,
                                 Priority Prio = Priority::Default,
                                 bool isStackDeep = false) {
    Impl::dispatchConcurrent(Prio, DispatchData(Context, Fn, isStackDeep));
  }
  template <typename Callable>
  static void dispatchConcurrent(Callable &&Fn,
                                 Priority Prio = Priority::Default,
                                 bool isStackDeep = false) {
    Impl::dispatchConcurrent(Prio, DispatchData(std::forward<Callable>(Fn),
                                                isStackDeep));
  }

  void suspend() {
    Impl::suspend(ImplObj);
  }
  void resume() {
    Impl::resume(ImplObj);
  }

  void setPriority(Priority Prio) {
    Impl::setPriority(ImplObj, Prio);
  }

  WorkQueue(const WorkQueue &Other) {
    ImplObj = Other.ImplObj;
    if (ImplObj)
      Impl::retain(ImplObj);
  }
  WorkQueue(WorkQueue &&Other) {
    ImplObj = std::move(Other.ImplObj);
    Other.ImplObj = Impl::Ty();
  }

  WorkQueue &operator=(const WorkQueue &Other) {
    WorkQueue Tmp(Other);
    std::swap(ImplObj, Tmp.ImplObj);
    return *this;
  }
  WorkQueue &operator=(WorkQueue &&Other) {
    WorkQueue Tmp(std::move(Other));
    std::swap(ImplObj, Tmp.ImplObj);
    return *this;
  }

private:
  class DispatchData {
  public:
    DispatchData(void *Context, DispatchFn Fn, bool isStackDeep = false)
      : Context(Context), CFn(Fn), IsStackDeep(isStackDeep) { }
    DispatchData(DispatchFn Fn, bool isStackDeep = false)
      : Context(0), CFn(Fn), IsStackDeep(isStackDeep) { }

    template <typename Callable>
    DispatchData(Callable Fn, bool isStackDeep = false) {
      Context = new Callable(std::move(Fn));
      CFn = &callAndDelete<Callable>;
      IsStackDeep = isStackDeep;
    }

    DispatchFn getFunction() const { return CFn; }
    void *getContext() const { return Context; }
    bool isStackDeep() const { return IsStackDeep; }

  private:
    template <typename Callable>
    static void callAndDelete(void *Ctx) {
      Callable *Fn = static_cast<Callable *>(Ctx);
      (*Fn)();
      delete Fn;
    }

    void *Context;
    DispatchFn CFn;
    bool IsStackDeep;
  };

  // Platform-specific implementation.
  struct Impl {
    typedef void *Ty;
    static Ty create(Dequeuing DeqKind, Priority Prio, llvm::StringRef Label);
    static void dispatch(Ty Obj, const DispatchData &Fn);
    static void dispatchSync(Ty Obj, const DispatchData &Fn);
    static void dispatchBarrier(Ty Obj, const DispatchData &Fn);
    static void dispatchBarrierSync(Ty Obj, const DispatchData &Fn);
    static void dispatchOnMain(const DispatchData &Fn);
    static void dispatchConcurrent(Priority Prio, const DispatchData &Fn);
    static void suspend(Ty Obj);
    static void resume(Ty Obj);
    static void setPriority(Ty Obj, Priority Prio);
    static llvm::StringRef getLabel(const Ty Obj);
    static void retain(Ty Obj);
    static void release(Ty Obj);
  };

  Impl::Ty ImplObj;
};

} // namespace SourceKit

#endif
