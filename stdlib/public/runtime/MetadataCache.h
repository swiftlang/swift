//===--- MetadataCache.h - Implements the metadata cache --------*- C++ -*-===//
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
#ifndef SWIFT_RUNTIME_METADATACACHE_H
#define SWIFT_RUNTIME_METADATACACHE_H

#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/STLExtras.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include <condition_variable>
#include <thread>

#ifndef SWIFT_DEBUG_RUNTIME
#define SWIFT_DEBUG_RUNTIME 0
#endif

namespace swift {

class MetadataAllocator : public llvm::AllocatorBase<MetadataAllocator> {
public:
  void Reset() {}

  LLVM_ATTRIBUTE_RETURNS_NONNULL void *Allocate(size_t size, size_t alignment);
  using AllocatorBase<MetadataAllocator>::Allocate;

  void Deallocate(const void *Ptr, size_t size);
  using AllocatorBase<MetadataAllocator>::Deallocate;

  void PrintStats() const {}
};

/// A typedef for simple global caches.
template <class EntryTy>
using SimpleGlobalCache =
  ConcurrentMap<EntryTy, /*destructor*/ false, MetadataAllocator>;

template <class T, bool ProvideDestructor = true>
class StaticOwningPointer {
  T *Ptr;
public:
  StaticOwningPointer(T *ptr = nullptr) : Ptr(ptr) {}
  StaticOwningPointer(const StaticOwningPointer &) = delete;
  StaticOwningPointer &operator=(const StaticOwningPointer &) = delete;
  ~StaticOwningPointer() { delete Ptr; }

  T *operator->() const { return Ptr; }
};

template <class T>
class StaticOwningPointer<T, false> {
  T *Ptr;
public:
  StaticOwningPointer(T *ptr = nullptr) : Ptr(ptr) {}
  StaticOwningPointer(const StaticOwningPointer &) = delete;
  StaticOwningPointer &operator=(const StaticOwningPointer &) = delete;

  T *operator->() const { return Ptr; }
};

enum class ConcurrencyRequest {
  /// No special requests; proceed to calling finish.
  None,

  /// Acquire the lock and call the appropriate function.
  AcquireLockAndCallBack,

  /// Notify all waiters on the condition variable without acquiring the lock.
  NotifyAll,
};

/// A map for which there is a phase of initialization that is guaranteed
/// to be performed exclusively.
///
/// In addition to the requirements of ConcurrentMap, the entry type must
/// provide the following members:
///
///   /// An encapsulation of the status of the entry.
///   using Status = ...;
///
///   /// Check the current status of the entry.  If 'locked' is true,
///   /// the entry's lock is held.
///   Status checkStatus(bool locked, ArgTys...);
///
///   /// Given the current status of an entry, decide whether we should
///   /// block for the given request.
///   bool shouldWait(Status status, ArgTys...);
///
///   /// Prepare to wait for the given request.  The lock is held.
///   /// If this returns false, the wait is cancelled.
///   bool prepareToWaitWithLock(Status status, ArgTys...);
///
///   /// An encapsulation of the result of allocation.
///   using AllocationResult = ...;
///
///   /// Perform allocation.  The lock is not held.
///   std::pair<AllocationResult, ConcurrencyRequest>
///   beginAllocation(ArgTys...);
///
///   /// Finish allocation.  The lock is held.  This is only called
///   /// if beginAllocation requests it by returning AcquireLockAndCallBack.
///   ///
///   /// If this returns true, all waiters on the condition variable will
///   /// be notified.
///   bool finishAllocationWithLock(AllocationResult);
///
///   /// An encapsulation of the result of initialization.
///   using InitializationResult = ...;
///
///   /// Attempt to initialize an entry.  The entry's lock is not held,
///   /// but only one thread will be trying this at once.  This is called
///   /// once for the entry, immediately after construction, by the thread
///   /// that successfully constructed the entry.
///   std::pair<InitializationResult, ConcurrencyRequest>
///   beginInitialization(ArgTys...);
///
///   /// Attempt to resume initializing an entry.  The entry's lock is
///   /// not held, but only one thread will be trying this at once.
///   /// This only need to be implemented if resumeInitialization is
///   /// called on the map.
///   std::pair<InitializationResult, ConcurrencyRequest>
///   resumeInitialization(ArgTys...);
///
///   /// Perform a final phase of initialization with the entry's lock
///   /// acquired.  This is only performed if beginInitialization or
///   /// resumeInitialization requests it by returning AcquireLockAndCallBack.
///   ///
///   /// If this returns true, all waiters on the condition variable will
///   /// be notified.
///   bool finishInitializationWithLock(InitializationResult &result);
///
///   /// Conclude an initialization attempt.  The entry's lock is not held.
///   /// This is called after finishInitializationWithLock.
///   Status finishInitialization(InitializationResult &result,
///                                   ConcurrencyRequest request);
///
///   /// Perform an "enqueueWithLock" operation.  The entry's lock is held.
///   /// This only needs to be implemented if enqueue is called on the map.
///   bool enqueueWithLock(ArgTys...);
template <class EntryType, bool ProvideDestructor = true>
class LockingConcurrentMap {
  ConcurrentMap<EntryType, ProvideDestructor, MetadataAllocator> Map;

  struct ConcurrencyControl {
    Mutex Lock;
    ConditionVariable Queue;
  };

  using Status = typename EntryType::Status;
  using InitializationResult = typename EntryType::InitializationResult;

  StaticOwningPointer<ConcurrencyControl, ProvideDestructor> Concurrency;
public:
  LockingConcurrentMap() : Concurrency(new ConcurrencyControl()) {}

  MetadataAllocator &getAllocator() { return Map.getAllocator(); }

  template <class KeyType, class... ExtraArgTys>
  std::pair<EntryType*, Status>
  getOrInsert(KeyType key, ExtraArgTys &&...extraArgs) {
    auto result = Map.getOrInsert(key, extraArgs...);
    auto entry = result.first;

    // If we are not inserting the entry, we need to check whether the entry
    // currently satisfies our conditions.
    if (!result.second) {
      // Check status without the lock, just in case we're already in an
      // acceptable state.
      Status status = entry->checkStatus(false, extraArgs...);
      if (!entry->shouldWait(status, extraArgs...)) {
        return { entry, std::move(status) };
      }

      // Check status with the lock and potentially block this thread.
      // FIXME: priority inversion?
      // TODO: Try to steal work from other threads instead of letting
      //   some other thread do all the work.
      Concurrency->Lock.withLockOrWait(Concurrency->Queue, [&] {
        status = entry->checkStatus(true, extraArgs...);
        bool shouldWait = entry->shouldWait(status, extraArgs...);
        if (shouldWait)
          shouldWait = entry->prepareToWaitWithLock(status, extraArgs...);
        return !shouldWait;
      });

      return { entry, std::move(status) };
    }

    // Okay, we inserted.  We own the right to initialize the entry.
    // Eagerly try to initialize it.
    auto allocationResult = entry->beginAllocation(extraArgs...);
    if (allocationResult.second == ConcurrencyRequest::AcquireLockAndCallBack) {
      // Notify anyone who might be waiting.
      Concurrency->Lock.withLock([&] {
        if (entry->finishAllocationWithLock(allocationResult.first))
          Concurrency->Queue.notifyAll();
      });
    } else if (allocationResult.second == ConcurrencyRequest::NotifyAll) {
      Concurrency->Queue.notifyAll();
    }

    auto initResult =
      entry->beginInitialization(std::forward<ExtraArgTys>(extraArgs)...);
    return finishInitialization(entry, std::move(initResult));
  }

  template <class KeyType, class... ExtraArgTys>
  std::pair<EntryType*, Status>
  resumeInitialization(KeyType key, ExtraArgTys &&...extraArgs) {
    auto entry = Map.find(key);
    assert(entry && "entry doesn't already exist!");

    auto initResult =
      entry->resumeInitialization(std::forward<ExtraArgTys>(extraArgs)...);
    return finishInitialization(entry, std::move(initResult));
  }

  template <class KeyType, class... ExtraArgTys>
  bool enqueue(KeyType key, ExtraArgTys &&...extraArgs) {
    auto entry = Map.find(key);
    assert(entry && "entry doesn't already exist!");

    return Concurrency->Lock.withLock([&] {
      return entry->enqueueWithLock(std::forward<ExtraArgTys>(extraArgs)...);
    });
  }

private:
  std::pair<EntryType*, Status>
  finishInitialization(EntryType *entry,
                       std::pair<InitializationResult,
                                 ConcurrencyRequest> &&result) {
    if (result.second == ConcurrencyRequest::AcquireLockAndCallBack) {
      // Notify anyone who might be waiting.
      Concurrency->Lock.withLock([&] {
        if (entry->finishInitializationWithLock(result.first))
          Concurrency->Queue.notifyAll();
      });
    } else if (result.second == ConcurrencyRequest::NotifyAll) {
      Concurrency->Queue.notifyAll();
    }

    return { entry, entry->finishInitialization(std::move(result.first),
                                                result.second) };
  }
};

// A wrapper around a pointer to a metadata cache entry that provides
// DenseMap semantics that compare values in the key vector for the metadata
// instance.
//
// This is stored as a pointer to the arguments buffer, so that we can save
// an offset while looking for the matching argument given a key.
class KeyDataRef {
  const void * const *Args;
  unsigned Length;

  KeyDataRef(const void * const *args, unsigned length)
    : Args(args), Length(length) {}

public:
  static KeyDataRef forArguments(const void * const *args,
                                 unsigned numArguments) {
    return KeyDataRef(args, numArguments);
  }

  bool operator==(KeyDataRef rhs) const {
    // Compare the sizes.
    unsigned asize = size(), bsize = rhs.size();
    if (asize != bsize) return false;

    // Compare the content.
    auto abegin = begin(), bbegin = rhs.begin();
    for (unsigned i = 0; i < asize; ++i)
      if (abegin[i] != bbegin[i]) return false;
    return true;
  }

  int compare(KeyDataRef rhs) const {
    // Compare the sizes.
    unsigned asize = size(), bsize = rhs.size();
    if (asize != bsize) {
      return (asize < bsize ? -1 : 1);
    }

    // Compare the content.
    auto abegin = begin(), bbegin = rhs.begin();
    for (unsigned i = 0; i < asize; ++i) {
      if (abegin[i] != bbegin[i])
        return (uintptr_t(abegin[i]) < uintptr_t(bbegin[i]) ? -1 : 1);
    }

    return 0;
  }

  size_t hash() {
    size_t H = 0x56ba80d1 * Length ;
    for (unsigned i = 0; i < Length; i++) {
      H = (H >> 10) | (H << ((sizeof(size_t) * 8) - 10));
      H ^= ((size_t)Args[i]) ^ ((size_t)Args[i] >> 19);
    }
    H *= 0x27d4eb2d;
    return (H >> 10) | (H << ((sizeof(size_t) * 8) - 10));
  }

  const void * const *begin() const { return Args; }
  const void * const *end() const { return Args + Length; }
  unsigned size() const { return Length; }
};

/// A key value as provided to the concurrent map.
struct MetadataCacheKey {
  size_t Hash;
  KeyDataRef KeyData;

  MetadataCacheKey(KeyDataRef data) : Hash(data.hash()), KeyData(data) {}
  MetadataCacheKey(const void *const *data, size_t size)
    : MetadataCacheKey(KeyDataRef::forArguments(data, size)) {}
};

/// A helper class for ConcurrentMap entry types which allows trailing objects
/// objects and automatically implements the getExtraAllocationSize methods
/// in terms of numTrailingObjects calls.
///
/// For each trailing object type T, the subclass must provide:
///   size_t numTrailingObjects(OverloadToken<T>) const;
///   static size_t numTrailingObjects(OverloadToken<T>, ...) const;
/// where the arguments to the latter are the arguments to getOrInsert,
/// including the key.
template <class Impl, class... Objects>
struct ConcurrentMapTrailingObjectsEntry
    : swift::ABI::TrailingObjects<Impl, Objects...> {
protected:
  using TrailingObjects =
      swift::ABI::TrailingObjects<Impl, Objects...>;

  Impl &asImpl() { return static_cast<Impl &>(*this); }
  const Impl &asImpl() const { return static_cast<const Impl &>(*this); }

  template<typename T>
  using OverloadToken = typename TrailingObjects::template OverloadToken<T>;

public:
  template <class... Args>
  static size_t getExtraAllocationSize(const MetadataCacheKey &key,
                                       Args &&...args) {
    return TrailingObjects::template additionalSizeToAlloc<Objects...>(
        Impl::numTrailingObjects(OverloadToken<Objects>(), key, args...)...);
  }
  size_t getExtraAllocationSize() const {
    return TrailingObjects::template additionalSizeToAlloc<Objects...>(
        asImpl().numTrailingObjects(OverloadToken<Objects>())...);
  }
};

enum class MetadataState : uint8_t {
  /// The metadata is being allocated.
  Allocating,

  /// The metadata is being allocated, and there are threads waiting for it.
  AllocatingWithWaiters,

  /// The metadata has been allocated, but is not yet complete for
  /// external layout: that is, it does not have a size.
  Abstract,
  AbstractWithWaiters,

  /// The metadata has a complete external layout, but may not have
  /// been fully initialized.
  LayoutComplete,
  LayoutCompleteWithWaiters,

  /// The metadata has a complete external layout and has been fully
  /// initialized.  There should no longer be waiters.
  Complete
};
inline bool operator<=(MetadataState lhs, MetadataState rhs) {
  return unsigned(lhs) <= unsigned(rhs);
}
inline bool operator>=(MetadataState lhs, MetadataState rhs) {
  return unsigned(lhs) >= unsigned(rhs);
}

inline MetadataState
getMetadataStateForRequestState(MetadataRequest::BasicKind state) {
  switch (state) {
  case MetadataRequest::Abstract:
    return MetadataState::Abstract;
  case MetadataRequest::LayoutComplete:
    return MetadataState::LayoutComplete;
  case MetadataRequest::Complete:
    return MetadataState::Complete;
  }
  swift_runtime_unreachable("bad state");
}

inline bool satisfies(MetadataState state,
                      MetadataRequest::BasicKind requirement) {
  switch (requirement) {
  case MetadataRequest::Abstract:
    return state >= MetadataState::Abstract;
  case MetadataRequest::LayoutComplete:
    return state >= MetadataState::LayoutComplete;
  case MetadataRequest::Complete:
    return state >= MetadataState::Complete;
  }
  swift_runtime_unreachable("unsupported requirement kind");
}

inline bool shouldWait(MetadataState state, MetadataRequest request) {
  // Always wait if we're allocating.  Non-blocking requests still need
  // to have an allocation that the downstream consumers can report
  // a dependency on.
  if (state == MetadataState::Allocating)
    return true;

  // Otherwise, if it's a non-blocking request, we do not need to block.
  if (request.isNonBlocking())
    return false;

  return !satisfies(state, request.getBasicKind());
}

/// Does the given metadata state say that there are other threads or
/// metadata waiting for the metadata to advance?
inline bool hasWaiters(MetadataState state) {
  switch (state) {
  case MetadataState::Allocating:
  case MetadataState::Abstract:
  case MetadataState::LayoutComplete:
  case MetadataState::Complete:
    return false;

  case MetadataState::AllocatingWithWaiters:
  case MetadataState::AbstractWithWaiters:
  case MetadataState::LayoutCompleteWithWaiters:
    return true;
  }
  swift_runtime_unreachable("bad kind");
}

inline MetadataState addWaiters(MetadataState state) {
  switch (state) {
  case MetadataState::Allocating:
  case MetadataState::AllocatingWithWaiters:
    return MetadataState::AllocatingWithWaiters;
  case MetadataState::Abstract:
  case MetadataState::AbstractWithWaiters:
    return MetadataState::AbstractWithWaiters;
  case MetadataState::LayoutComplete:
  case MetadataState::LayoutCompleteWithWaiters:
    return MetadataState::LayoutCompleteWithWaiters;
  case MetadataState::Complete:
    return MetadataState::Complete;
  }
  swift_runtime_unreachable("bad kind");
}

inline MetadataState removeWaiters(MetadataState state) {
  switch (state) {
  case MetadataState::Allocating:
  case MetadataState::AllocatingWithWaiters:
    return MetadataState::Allocating;
  case MetadataState::Abstract:
  case MetadataState::AbstractWithWaiters:
    return MetadataState::Abstract;
  case MetadataState::LayoutComplete:
  case MetadataState::LayoutCompleteWithWaiters:
    return MetadataState::LayoutComplete;
  case MetadataState::Complete:
    return MetadataState::Complete;
  }
  swift_runtime_unreachable("bad kind");
}

struct MetadataCompletionQueueEntry {
  /// The metadata whose completion is blocked.
  Metadata * const Value;

  /// The next entry in the completion queue.
  std::unique_ptr<MetadataCompletionQueueEntry> Next;

  /// The saved state of the completion function.
  MetadataCompletionContext CompletionContext;

  Metadata *Dependency = nullptr;
  MetadataRequest::BasicKind DependencyRequirement = MetadataRequest::Abstract;

  MetadataCompletionQueueEntry(Metadata *value,
                               const MetadataCompletionContext &context)
    : Value(value), CompletionContext(context) {}
};

/// Add the given queue entry to the queue for the given metadata.
///
/// \return false if the entry was not added because the dependency
///   has already reached the desired requirement
bool addToMetadataQueue(std::unique_ptr<MetadataCompletionQueueEntry> &&queueEntry,
                        Metadata *dependency,
                        MetadataRequest::BasicKind dependencyRequirement);


void resumeMetadataCompletion(
                    std::unique_ptr<MetadataCompletionQueueEntry> &&queueEntry);

/// A base class offerring a reasonable default implementation for entries
/// in a generic metadata cache.  Supports variably-sized keys.
///
/// The value type may be an arbitrary type, but it must be contextually
/// convertible to bool, and it must be default-constructible in a false
/// state.
///
/// Concrete implementations should provide:
///   /// A name describing the map; used in debugging diagnostics.
///   static const char *getName();
///
///   /// A constructor which should set up an entry.  Note that this phase
///   /// of initialization may race with other threads attempting to set up
///   /// the same entry; do not do anything during it which might block or
///   /// need to be reverted.
///   /// The extra arguments are those provided to getOrInsert.
///   Entry(MetadataCacheKey key, ExtraArgTys...);
///
///   /// Allocate the metadata.
///   AllocationResult allocate(ExtraArgTys...);
///
///   /// Try to initialize the metadata.
///   TryInitializeResult tryInitialize(Metadata *metadata,
///                                     MetadataCompletionContext *context,
///                                     ExtraArgTys...);
template <class Impl, class... Objects>
class MetadataCacheEntryBase
    : public ConcurrentMapTrailingObjectsEntry<Impl, const void *, Objects...> {
  using super =
             ConcurrentMapTrailingObjectsEntry<Impl, const void *, Objects...>;
  friend super;
  using TrailingObjects = typename super::TrailingObjects;
  friend TrailingObjects;

public:
  using ValueType = Metadata *;
  struct Status {
    ValueType Value;
    MetadataState State;
  };

protected:
  template<typename T>
  using OverloadToken = typename TrailingObjects::template OverloadToken<T>;

  size_t numTrailingObjects(OverloadToken<const void *>) const {
    return KeyLength;
  }

  template <class... Args>
  static size_t numTrailingObjects(OverloadToken<const void *>,
                                   const MetadataCacheKey &key,
                                   Args &&...extraArgs) {
    return key.KeyData.size();
  }

  using super::asImpl;

private:
  /// These are set during construction and never changed.
  const size_t Hash;
  const uint16_t KeyLength;

  /// What kind of data is stored in the LockedStorage field below?
  ///
  /// This is only ever modified under the lock.
  enum class LSK {
    AllocatingThread,
    CompletionQueue,
  };
  LSK LockedStorageKind;

  /// Does this entry have a value, or is it currently undergoing
  /// initialization?
  ///
  /// This (and the following field) can be modified while not under the lock,
  /// but the change from Allocating to Abstract happens under the lock.
  std::atomic<MetadataState> State;

  /// Valid if State >= MetadataState::Abstract.
  ValueType Value;

  /// Additional storage that is only ever accessed under the lock.
  union LockedStorage_t {
    /// The thread that is allocating the entry.
    std::thread::id AllocatingThread;

    /// The completion queue.  This is only ever accessed under the lock.
    std::unique_ptr<MetadataCompletionQueueEntry> CompletionQueue;

    LockedStorage_t() {}
    ~LockedStorage_t() {}
  } LockedStorage;

public:
  MetadataCacheEntryBase(const MetadataCacheKey &key)
      : Hash(key.Hash), KeyLength(key.KeyData.size()),
        State(MetadataState::Allocating) {
    LockedStorageKind = LSK::AllocatingThread;
    LockedStorage.AllocatingThread = std::this_thread::get_id();
    memcpy(this->template getTrailingObjects<const void*>(),
           key.KeyData.begin(),
           KeyLength * sizeof(void*));
  }

  ~MetadataCacheEntryBase() {
    if (LockedStorageKind == LSK::CompletionQueue)
      LockedStorage.CompletionQueue.~unique_ptr();
  }

  bool isBeingAllocatedByCurrentThread() const {
    return LockedStorageKind == LSK::AllocatingThread &&
           LockedStorage.AllocatingThread == std::this_thread::get_id();
  }

  KeyDataRef getKeyData() const {
    return KeyDataRef::forArguments(
                              this->template getTrailingObjects<const void*>(),
                                    KeyLength);
  }

  intptr_t getKeyIntValueForDump() const {
    return Hash;
  }

  int compareWithKey(const MetadataCacheKey &key) const {
    // Order by hash first, then by the actual key data.
    if (auto comparison = compareIntegers(key.Hash, Hash)) {
      return comparison;
    } else {
      return key.KeyData.compare(getKeyData());
    }
  }

  template <class... Args>
  Status checkStatus(bool locked, Args &&...extraArgs) const {
    auto state = State.load(std::memory_order_acquire);
    if (state == MetadataState::Complete) {
      return { Value, state };
    }

    if (!locked && state >= MetadataState::Abstract) {
      return { Value, state };
    }

    // As a QoI safe-guard against the simplest form of cyclic
    // dependency, check whether this thread is the one responsible
    // for allocating the metadata.
    if (locked && isBeingAllocatedByCurrentThread()) {
      fprintf(stderr,
              "%s(%p): cyclic metadata dependency detected, aborting\n",
              Impl::getName(), static_cast<const void*>(this));
      abort();
    }

    // TODO: we should have all the information we need to check for a
    // perfect cycle.

    return { ValueType(), state };
  }

  template <class... Args>
  static bool shouldWait(const Status &status,
                         MetadataRequest request, Args &&...extraArgs) {
    return swift::shouldWait(status.State, request);
  }

  template <class... Args>
  bool prepareToWaitWithLock(Status &status, Args &&...extraArgs) {
    // Add the has-waiters bit to the current state.
    auto oldState = status.State;
    auto newState = addWaiters(oldState);

    // If the current state doesn't already have that bit, try to set it.
    while (oldState != newState &&
           !State.compare_exchange_weak(oldState, newState,
                                        std::memory_order_release,
                                        std::memory_order_acquire)) {
      // That can fail, and it might fail due to the state being updated
      // to something we should no longer wait on.  In this case, update
      // the status and abort the wait.
      if (!shouldWait(status, extraArgs...)) {
        assert(oldState >= MetadataState::Abstract);
        status.State = oldState;
        status.Value = Value;
        return false;
      }

      // Otherwise, try again with whatever the new state is.
      newState = addWaiters(oldState);
    }

    // Wait.
    return true;
  }

  /// Publish a new metadata state, preserving the existence of waiters.
  ///
  /// This is the initializing thread.  The lock is not held.
  void publishMetadataState(MetadataState &oldState,
                            MetadataState &newState,
                            bool &isCompleteAndHadWaiters) {
    while (!State.compare_exchange_weak(oldState, newState,
                                        std::memory_order_release,
                                        std::memory_order_relaxed)) {
      // If the exchange failed because we have waiters, remember
      // that we have waiters in the new state.
      if (hasWaiters(oldState)) {
        if (newState == MetadataState::Complete) {
          isCompleteAndHadWaiters = true;
        } else {
          newState = addWaiters(newState);
        }
      }
    }
  }

  struct AllocationResult {
    ValueType Value;
    MetadataRequest::BasicKind State;
  };

  /// Allocate the metadata.
  ///
  /// This is the initializing thread.  The lock is not held.
  template <class... Args>
  std::pair<AllocationResult, ConcurrencyRequest>
  beginAllocation(Args &&...args) {
    AllocationResult allocationResult =
      asImpl().allocate(std::forward<Args>(args)...);

    // Publish the value.
    Value = allocationResult.Value;
    auto oldState = MetadataState::Allocating;
    auto newState = getMetadataStateForRequestState(allocationResult.State);
    bool isComplete = false;
    publishMetadataState(oldState, newState, isComplete);

    return { allocationResult,
             isComplete ? ConcurrencyRequest::NotifyAll
                        : ConcurrencyRequest::None };
  }

  /// Finish allocation with the lock held.  This is currently never called.
  ///
  /// This is the initializing thread.  The lock is held.
  bool finishAllocationWithLock(AllocationResult result) {
    swift_runtime_unreachable("never used");
  }

  struct InitializationResult {
    MetadataCacheEntryBase::Status Status;
    bool ShouldNotifyAll = false;
    std::unique_ptr<MetadataCompletionQueueEntry> CompletionsToResume;
  };

  /// Begin initialization immediately after allocation.
  ///
  /// This is the initializing thread.  The lock is not held.
  template <class... Args>
  std::pair<InitializationResult, ConcurrencyRequest>
  beginInitialization(Args &&...args) {
    return initializeFromEntry(nullptr, std::forward<Args>(args)...);
  }

  /// Resume initialization after a previous failure resulted in the
  /// metadata being enqueued on another metadata cache.
  ///
  /// This is the initializing thread.  The lock is not held.
  ///
  /// We expect the first argument here to be of type
  /// std::unique_ptr<MetadataCompletionQueueEntry> &&.
  template <class... Args>
  std::pair<InitializationResult, ConcurrencyRequest>
  resumeInitialization(Args &&...args) {
    return initializeFromEntry(std::forward<Args>(args)...);
  }

  struct TryInitializeResult {
    MetadataRequest::BasicKind NewState;
    MetadataRequest::BasicKind DependencyRequirement;
    Metadata *Dependency;
  };

  /// Try to complete the metadata.
  ///
  /// This is the initializing thread.  The lock is not held.
  template <class... Args>
  std::pair<InitializationResult, ConcurrencyRequest>
  initializeFromEntry(std::unique_ptr<MetadataCompletionQueueEntry> &&queueEntry,
                      Args &&...args) {
    // We should always have fully synchronized with any previous threads
    // that were processing the initialization, so a relaxed load is fine
    // here.  (This ordering is achieved by the locking which occurs as part
    // of queuing and dequeuing metadata.)
    auto origState = State.load(std::memory_order_relaxed);
    assert(origState >= MetadataState::Abstract);
    auto value = Value;

    std::pair<InitializationResult, ConcurrencyRequest> result;
    result.first.Status.Value = value;
    auto &curState = result.first.Status.State;
    curState = origState;

    // The state is only already in the initialized state in specific
    // circumstances where allocation was able to put it into that state.
    // In such a world, we don't need to do anything else.
    if (origState == MetadataState::Complete) {
      result.second = ConcurrencyRequest::None;
      return result;
    }

    // Figure out the completion context.
    MetadataCompletionContext scratchContext;
    MetadataCompletionContext *context;
    if (queueEntry) {
      context = &queueEntry->CompletionContext;
    } else {
      memset(&scratchContext, 0, sizeof(MetadataCompletionContext));
      context = &scratchContext;
    }

    bool isCompleteAndHadWaiters = false;

    // Try the complete the metadata.  This only loops if completion
    // doesn't succeed, but the new dependency is resolved when we go to
    // add ourselves to its queue.
    while (true) {
      TryInitializeResult tryInitializeResult =
        asImpl().tryInitialize(value, context, args...);
      MetadataState newState =
        getMetadataStateForRequestState(tryInitializeResult.NewState);

      // Publish the current state of the metadata.
      publishMetadataState(curState, newState, isCompleteAndHadWaiters);

      // If we don't have a dependency, we're finished.
      if (!tryInitializeResult.Dependency) {
        assert(newState == MetadataState::Complete);
        break;
      }

      // Otherwise, we need to block this metadata on the dependency's queue.

      // Create a queue entry if necessary.
      if (!queueEntry) {
        queueEntry.reset(
          new MetadataCompletionQueueEntry(value, scratchContext));
        context = &queueEntry->CompletionContext;
      }

      // Try to block this metadata initialization on that queue.
      // If this succeeds, we aren't really the initializing thread anymore.
      // The small amount of notification we do afterwards should be
      // okay to race with another thread potentially taking over
      // initialization.
      if (addToMetadataQueue(std::move(queueEntry),
                             tryInitializeResult.Dependency,
                             tryInitializeResult.DependencyRequirement))
        break;

      // If that failed, we should still have ownership of the entry.
      assert(queueEntry);
    }

    // If the state was updated, we need to notify waiters of all kinds.
    // That means threads and, potentially, metadata in our completion queue.
    result.second = ConcurrencyRequest::None;
    if (origState != curState) {
      result.first.ShouldNotifyAll =
        (isCompleteAndHadWaiters || hasWaiters(curState));
      result.second = ConcurrencyRequest::AcquireLockAndCallBack;
    }

    return result;
  }

  /// Wake waiters of all kinds because we've made progress with the metadata.
  /// Called after initializeFromEntry when it returns AcquireLockAndCallBack.
  ///
  /// The lock is held.  This thread is not necessarily the initializing
  /// thread anymore.
  bool finishInitializationWithLock(InitializationResult &result) {
    // Collect anything in the metadata's queue whose target state has been
    // reached to the queue in result.  Note that we repurpose the Next field
    // in the collected entries.

    // If we're not even currently storing a completion queue,
    // there's nothing to do but wake waiting threads.
    if (LockedStorageKind != LSK::CompletionQueue) {
      return claimWaitersWithLock(result);
    }

    auto nextToResume = &result.CompletionsToResume;
    assert(!*nextToResume && "already items in queue?");

    // Walk the completion queue.
    auto *nextWaiter = &LockedStorage.CompletionQueue;
    while (auto waiter = nextWaiter->get()) {
      // If the new state of this entry doesn't satisfy the waiter's
      // requirements, skip over it.
      if (!satisfies(result.Status.State, waiter->DependencyRequirement)) {
        nextWaiter = &waiter->Next;
        continue;
      }

      // Add the waiter to the end of the next-to-resume queue, and update
      // the end to the waiter's Next field.
      *nextToResume = std::move(*nextWaiter); // owning pointer to waiter
      nextToResume = &waiter->Next;

      // Splice the waiter out of the completion queue.
      *nextWaiter = std::move(waiter->Next);

      assert(!*nextToResume);
    }

    // Wake waiting threads.
    return claimWaitersWithLock(result);
  }

  /// Check whether we need to notify any waiters.  If so, clear the
  /// has-waiters bit.
  ///
  /// The lock is held.  This thread is not necessarily the initializing
  /// thread anymore.
  bool claimWaitersWithLock(InitializationResult &result) {
    if (!result.ShouldNotifyAll) return false;

    // Crucially, we never get a new waiting thread while we're holding
    // the lock.

    MetadataState curState = result.Status.State;
    auto newState = removeWaiters(curState);

    // If the state without waiters is the same as the old state,
    // we must be complete, and we don't need to update State.
    if (curState == newState) {
      assert(curState == MetadataState::Complete);
    } else {
      // Because we're not necessarily the initializing thread anymore,
      // we can't just store this; we need to do a compare-and-swap to
      // protect against possibly reversing work done by the initializing
      // thread.  If this doesn't work, it doesn't matter: all it means
      // is that the new initializing thread might think that it has
      // waiters when it doesn't, which will lead to nothing more harmful
      // than some wasteful extra notifies.
      (void) State.compare_exchange_strong(curState, newState,
                                           std::memory_order_relaxed,
                                           std::memory_order_relaxed);
    }

    return true;
  }

  /// Finish an initialization attempt by resuming any unblocked metadata
  /// initializations.
  ///
  /// The lock is not held.  This thread is not necessarily the initializing
  /// thread anymore.
  static Status finishInitialization(InitializationResult &&result,
                                     ConcurrencyRequest concurrentRequest) {
    // Process all the waiters we removed from the completion queue.
    while (auto cur = std::move(result.CompletionsToResume)) {
      result.CompletionsToResume = std::move(cur->Next);

      resumeMetadataCompletion(std::move(cur));
    }

    return result.Status;
  }

  /// Block a metadata initialization on the completion of this
  /// initialization.
  ///
  /// This is always called from the initializing thread.  The lock is not held.
  bool enqueueWithLock(
                  std::unique_ptr<MetadataCompletionQueueEntry> &&queueEntry) {
    assert(queueEntry);
    assert(!queueEntry->Next);

    auto curState = State.load(std::memory_order_acquire);
    if (satisfies(curState, queueEntry->DependencyRequirement))
      return false;

    // Note that we don't set the waiters bit because we're not actually
    // blocking any threads.

    // Transition the locked storage to the completion queue.
    if (LockedStorageKind != LSK::CompletionQueue) {
      LockedStorageKind = LSK::CompletionQueue;
      new (&LockedStorage.CompletionQueue)
        std::unique_ptr<MetadataCompletionQueueEntry>();
    }

    queueEntry->Next = std::move(LockedStorage.CompletionQueue);
    LockedStorage.CompletionQueue = std::move(queueEntry);
    return true;
  }
};

template <class EntryType, bool ProvideDestructor = true>
class MetadataCache :
    public LockingConcurrentMap<EntryType, ProvideDestructor> {
};

} // namespace swift

#endif // SWIFT_RUNTIME_METADATACACHE_H
