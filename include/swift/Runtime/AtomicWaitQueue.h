//===--- AtomicWaitQueue.h - A "condition variable" for atomics -*- C++ -*-===//
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
// This file declares the AtomicWaitQueue class template, which can be
// used to create a sort of condition variable associated with an atomic
// object which clients can wait on until some condition is satisfied.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_ATOMICWAITQUEUE_H
#define SWIFT_RUNTIME_ATOMICWAITQUEUE_H

#include "swift/Runtime/Heap.h"
#include "swift/Runtime/Mutex.h"
#include <assert.h>

namespace swift {

/// A wait queue designed to be used with an atomic object.  Somewhat
/// like a condition variable, it can be used to cause threads to block.
/// Unlike a condition variable, the queue is created on a particular
/// thread, called the worker thread, which is responsible for unblocking
/// all the waiting threads.  This means that wait queues can be used
/// without introducing priority inversions.
///
/// Wait queues are implemented as a shared object that stores a lock
/// held by the worker thread.  Becoming the worker thread therefore
/// requires an allocation.  Furthermore, because a shared reference
/// cannot be atomically read out of an atomic, a "global" lock (in
/// contrast with internal lock of the wait queue) must be acquired
/// around any change of the atomic's queue reference.  All of this
/// is suboptimal but unavoidable without introducing deeper problems.
/// The global lock could be avoided with generational approaches,
/// but these are not portably available.
///
/// AtomicWaitQueue imposes no constraints on the atomic object that
/// holds the wait queue reference, except that loads of the reference
/// which lead to uses by AtomicWaitQueue should happen-after the store
/// which originally set the reference in the atomic ("published" it).
/// Loads of the reference must happen while holding the global lock,
/// so changes to the reference must either also happen only while
/// holding the lock or must be release-ordered (and loads for AWQ's
/// purposes must therefore be acquires).
///
/// The API of this class attempts to tightly constrain the use of
/// the wait queue to avoid predictable errors in use.  The worker
/// thread creates a Worker object and then performs calls on it to
/// become the worker and then finish the work.
///
/// This class is a CRTP superclass, and clients should inherit from it:
///
/// ```
///   class MyQueue : public AtomicWaitQueue<MyQueue> {
///     ...
///   };
/// ```
template <class Impl, class GlobalLockType = Mutex>
class AtomicWaitQueue {
  Impl &asImpl() { return static_cast<Impl&>(*this); }

  /// The reference count; only manipulated under the associated
  /// global lock.
  size_t referenceCount = 1;

  /// The lock on which clients will wait.  This is not the global lock.
  Mutex WaitQueueLock;

  /// Add a reference to this queue.  Must be called while holding the
  /// globaal lock.
  void retain_locked() {
    referenceCount++;
  }

  /// Drop a reference to this queue.  Must be called while holding the
  /// global lock and while *not* holding the wait queue lock.
  void release_locked() {
    if (referenceCount == 1) {
      delete &asImpl();
    } else {
      referenceCount--;
    }
  }

  void wait(GlobalLockType &globalLock) {
    // Attempt to acquire the queue lock and then immediately discard it.
    WaitQueueLock.withLock([]{});

    // Acquire the global lock so that we can drop the reference count.
    globalLock.withLock([&]{
      release_locked();
    });
  }

public:
  /// Is this queue uniquely referenced?  This should only be called
  /// under the global lock.
  bool isUniquelyReferenced_locked() const {
    return referenceCount == 1;
  }

  /// This queue is being re-used with new construction arguments.
  /// Update it appropriately.
  void updateForNewArguments() {
    // We intentionally take no arguments so that only calls to
    // createQueue with no arguments will succeed at calling this no-op
    // implementation.  Queue types with construction arguments
    // will need to implement this method to take the appropriate
    // arguments.  Hopefully this discourages people from forgetting
    // that queues can be re-used if created in a loop.
  }

  /// An RAII helper class for signalling that the current thread is a
  /// worker thread which has acquired the lock.
  ///
  /// `AtomicWaitQueue` does not require the global lock to be held
  /// while creating or publishing the queue.  Clients taking advantage
  /// of this should inform the Worker class that a created queue has
  /// been published by calling `flagQueueIsPublished`.  Clients who
  /// wish to publish the queue while holding the global lock, perhaps
  /// to get a rule that all stores are done under the lock, may instead
  /// use `tryPublishQueue`.
  ///
  /// The expected use pattern is something like:
  ///
  /// ```
  ///   MyQueue::Worker worker(myGlobalLock);
  ///   auto oldStatus = myAtomic.load(std::memory_order_acquire);
  ///   while (true) {
  ///     if (oldStatus.isDone()) return;
  ///
  ///     if (oldStatus.hasWaitQueue()) {
  ///       bool waited = worker.tryReloadAndWait([&] {
  ///         oldStatus = myAtomic.load(std::memory_order_acquire);
  ///         return (oldStatus.hasWaitQueue() ? oldStatus.getWaitQueue()
  ///                                          : nullptr);
  ///       });
  ///
  ///       // If we waited, `oldStatus` will be out of date; reload it.
  ///       //
  ///       // (For the pattern in this example, where the worker thread
  ///       // always transitions the status to done, this is actually
  ///       // unnecessary: by virtue of having successfully waited, we've
  ///       // synchronized with the worker thread and know that the status
  ///       // is done, so we could just return.  But in general, this
  ///       // reload is necessary.)
  ///       if (waited)
  ///         oldStatus = myAtomic.load(std::memory_order_acquire);
  ///
  ///       // Go back and reconsider the updated status.
  ///       continue;
  ///     }
  ///
  ///     // Create a queue and try to publish it.  If this succeeds,
  ///     // we've become the worker thread.  We don't have to worry
  ///     // about the queue leaking if we don't use it; that's managed
  ///     // by the Worker class.
  ///     {
  ///       auto queue = worker.createQueue();
  ///       auto newStatus = oldStatus.withWaitQueue(queue);
  ///       if (!myAtomic.compare_exchange_weak(oldStatus, newStatus,
  ///                                 /*success*/ std::memory_order_release,
  ///                                 /*failure*/ std::memory_order_acquire))
  ///         continue;
  ///       worker.flagQueueIsPublished(queue);
  ///     }
  ///
  ///     // Do the actual work here.
  ///
  ///     // Report that the work is done and "unpublish" the queue from
  ///     // the atomic.
  ///     myAtomic.store(oldStatus.withDone(true), std::memory_order_release);
  ///     worker.finishAndUnpublishQueue([]{});
  ///     return;
  ///   }
  /// ```
  class Worker {
  protected:
    typename Impl::Worker &asImpl() {
      return static_cast<typename Impl::Worker&>(*this);
    }

    GlobalLockType &GlobalLock;

    /// The queue object.  The current thread always holds the lock on
    /// this if it's non-null.
    Impl *CurrentQueue = nullptr;

    /// True if the queue has been published and may have other references
    /// to it.
    bool Published = false;
  public:
    explicit Worker(GlobalLockType &globalLock) : GlobalLock(globalLock) {}

    Worker(const Worker &) = delete;
    Worker &operator=(const Worker &) = delete;

    ~Worker() {
      assert(!Published &&
             "should not allow Worker object to go out of scope after "
             "publishing without somehow finishing the work");

      // If we created the queue but never published it, destroy it.
      if (CurrentQueue) {
        CurrentQueue->WaitQueueLock.unlock();
        delete CurrentQueue;
      }
    }

    /// Is this thread the worker thread, meaning that it holds the
    /// lock on a published queue?
    ///
    /// Generally, this should only be used for assertions.
    bool isWorkerThread() const {
      return Published;
    }

    /// Given that this thread is not the worker thread and there seems
    /// to be a wait queue in place, try to wait on it.
    ///
    /// Acquire the global lock and call the given function.  If it
    /// returns a wait queue, wait on that queue and return true;
    /// otherwise, return false.
    template <class Fn>
    bool tryReloadAndWait(Fn &&fn) {
      assert(!isWorkerThread());
      typename Impl::Waiter waiter(GlobalLock);
      return waiter.tryReloadAndWait(std::forward<Fn>(fn));
    }

    /// Given that this thread is the worker thread, return the queue
    /// that's been created and published for it.
    Impl *getPublishedQueue() const {
      assert(CurrentQueue && Published);
      return CurrentQueue;
    }

    /// Given that this thread is not (yet) the worker thread, create
    /// a queue that can be published to make this the worker thread.
    /// Usually this will be called before or during `tryPublishQueue()`.
    ///
    /// The Worker object takes ownership of the queue until it's
    /// published, so you can safely call this even if publishing
    /// might fail.
    ///
    /// Note that the same queue will be returned on successive
    /// invocations.  Queues that accept arguments for construction
    /// should implement `updateForNewArguments`.
    template <class... Args>
    Impl *createQueue(Args &&...args) {
      assert(!Published);
      if (!CurrentQueue)
        CurrentQueue = asImpl().createNewQueue(std::forward<Args>(args)...);
      else
        CurrentQueue->updateForNewArguments(std::forward<Args>(args)...);
      return CurrentQueue;
    }

    /// Given that this Worker object owns a queue that was created
    /// with `createQueue()` but not yet published, flag that the
    /// queue been published, transferring ownership to the atomic;
    /// this is now the worker thread.
    void flagQueueIsPublished(Impl *publishedQueue) {
      assert(CurrentQueue);
      assert(CurrentQueue == publishedQueue);
      assert(!Published);
      Published = true;
    }

    /// Flag that the created queue has been published.  Necessary
    /// because of some awkward abstraction in MetadataCache;
    /// generally prefer to use flagQueueIsPublished.
    void flagCreatedQueueIsPublished() {
      assert(CurrentQueue);
      assert(!Published);
      Published = true;
    }

    /// Try to publish a queue.  The queue will be passed to the
    /// argument function, which should return true if the queue was
    /// published.  Do not also call `flagQueueIsPublished` when
    /// using this.
    template <class Fn>
    bool tryPublishQueue(Fn &&fn) {
      return GlobalLock.withLock([&]{
        if (fn(CurrentQueue))
          asImpl().flagQueueIsPublished(CurrentQueue);
        return Published;
      });
    }

    /// Given that this is the worker thread, create a replacement
    /// queue.  This should be used with `maybeReplaceQueue`.  The
    /// caller owns the replacement queue until it publishes it as
    /// a replacement.
    template <class... Args>
    Impl *createReplacementQueue(Args &&...args) {
      assert(CurrentQueue && Published);
      return asImpl().createNewQueue(std::forward<Args>(args)...);
    }

    /// Given that the queue has been published and so we've become
    /// the worker thread, possibly replace the queue with a new
    /// queue returned by the given function.  The function will be
    /// called under the global lock, so it is legal to call
    /// `isUniquelyReferenced_locked()` on the current queue.
    ///
    /// If replacement is required, the function should create it
    /// with `createReplacementQueue()`.  The replacement queue should
    /// published before returning from the function.  The reference
    /// to the old queue will be destroyed, and pointers to it
    /// should be considered invalidated.  If the function returns
    /// null, the original queue is left in place.
    template <class Fn>
    void maybeReplaceQueue(Fn &&fn) {
      assert(CurrentQueue && Published);

      GlobalLock.withLock([&] {
        if (auto newQueue = fn()) {
          assert(newQueue != CurrentQueue &&
                 "replacement queue is current queue?");
          CurrentQueue->WaitQueueLock.unlock();
          CurrentQueue->release_locked();
          CurrentQueue = newQueue;
        }
      });
    }

    /// Given that the queue has been published and so we've become
    /// the worker thread, finish the work, calling the given function
    /// while holding the global lock.
    ///
    /// The actual unpublishing doesn't have to happen during this
    /// operation, but it might help to create a general rule that
    /// all modifications are done while holding the lock.  (The lock
    /// has to be acquired anyway in order to drop the reference to
    /// the queue.)
    template <class Fn>
    void finishAndUnpublishQueue(Fn &&fn) {
      assert(CurrentQueue && Published);
      GlobalLock.withLock([&] {
        fn();
        CurrentQueue->WaitQueueLock.unlock();
        CurrentQueue->release_locked();
      });
      Published = false;
      CurrentQueue = nullptr;
    }

    /// A helper class for `withLock`.
    class Operation {
      friend class Worker;
      Worker &TheWorker;
      Impl *QueueToAwait = nullptr;

      Operation(Worker &worker) : TheWorker(worker) {}

      Operation(const Operation &) = delete;
      Operation &operator=(const Operation &) = delete;

    public:
      /// Tell the worker to wait on the given queue and then call
      /// the callback function again.
      void waitAndRepeat(Impl *queue) {
        // Take a reference to the queue.
        queue->retain_locked();

        // Set the queue to await.
        assert(!QueueToAwait);
        QueueToAwait = queue;
      }

      /// Create a wait queue that can be published.
      Impl *createQueue() {
        return TheWorker.asImpl().createQueue();
      }

      /// Record that we've published the wait queue.
      void flagQueueIsPublished(Impl *queue) {
        TheWorker.asImpl().flagQueueIsPublished(queue);
      }
    };

    /// Perform a complex operation under the global lock by making
    /// calls on the Operation object that is passed to the function.
    template <class Fn>
    void withLock(Fn &&fn) {
      assert(!Published);

      Operation operation(*this);
      Impl *queueToDrop = nullptr;

      while (true) {
        GlobalLock.withLock([&] {
          // If we have an awaited queue from a previous iteration,
          // drop the reference to it now that we're holding the lock.
          if (queueToDrop) {
            queueToDrop->release_locked();
          }

          // Perform the operation.
          fn(operation);
        });

        // We're done until waitAndRepeat was called.
        queueToDrop = operation.QueueToAwait;
        if (!queueToDrop)
          return;

        // Wait on the queue and then repeat the operation.
        // We'll drop the reference count when we get the lock again.
        operation.QueueToAwait = nullptr;
        queueToDrop->WaitQueueLock.withLock([]{});
      }
    }

  private:
    template <class... Args>
    static Impl *createNewQueue(Args &&...args) {
#if !defined(__cpp_aligned_new)
      static_assert(!swift::requires_aligned_alloc<std::alignment_of<Impl>::value>::value ||
                     is_aligned_alloc_aware<Impl>::value,
                    "type is over-aligned for non-alignment aware operator new");
#endif
      auto queue = new Impl(std::forward<Args>(args)...);
      queue->WaitQueueLock.lock();
      return queue;
    }
  };

  /// An RAII helper class for waiting for the worker thread to finish.
  ///
  /// The expected use pattern is:
  ///
  /// ```
  ///   MyQueue::Waiter waiter(myGlobalLock);
  ///
  ///   auto status = myAtomic.load(std::memory_order_acquire);
  ///   while (status.isLocked()) {
  ///     if (waiter.tryReloadAndWait([&] {
  ///       status = myAtomic.load(std::memory_order_acquire);
  ///       return (status.isLocked() ? status.getLock() : nullptr);
  ///     }) {
  ///       status = myAtomic.load(std::memory_order_acquire);
  ///     }
  ///   }
  /// ```
  class Waiter {
    GlobalLockType &GlobalLock;
  public:
    explicit Waiter(GlobalLockType &globalLock) : GlobalLock(globalLock) {}

    /// Acquire the global lock and call the given function.  If it
    /// returns a wait queue, wait on that queue and return true;
    /// otherwise, return false.
    template <class Fn>
    bool tryReloadAndWait(Fn &&fn) {
      Impl *queue;
      GlobalLock.withLock([&] {
        queue = fn();
        if (queue) {
          queue->retain_locked();
        }
      });

      if (!queue) return false;

      // Wait for the queue lock.
      queue->WaitQueueLock.withLock([]{});

      // Release the queue.
      GlobalLock.withLock([&] {
        queue->release_locked();
      });

      return true;
    }
  };
};

template <class GlobalLockType = Mutex>
struct SimpleAtomicWaitQueue
  : AtomicWaitQueue<SimpleAtomicWaitQueue<GlobalLockType>, GlobalLockType> {};

} // end namespace swift
#endif
