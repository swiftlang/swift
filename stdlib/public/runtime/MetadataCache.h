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
private:
  uint16_t Tag;

public:
  constexpr MetadataAllocator(uint16_t tag) : Tag(tag) {}
  MetadataAllocator() = delete;

  void Reset() {}

  LLVM_ATTRIBUTE_RETURNS_NONNULL void *Allocate(size_t size, size_t alignment);
  using AllocatorBase<MetadataAllocator>::Allocate;

  void Deallocate(const void *Ptr, size_t size, size_t Alignment);
  using AllocatorBase<MetadataAllocator>::Deallocate;

  void PrintStats() const {}
  
  MetadataAllocator withTag(uint16_t Tag) {
    MetadataAllocator Allocator = *this;
    Allocator.Tag = Tag;
    return Allocator;
  }
};

template<uint16_t StaticTag>
class TaggedMetadataAllocator: public MetadataAllocator {
public:
  constexpr TaggedMetadataAllocator() : MetadataAllocator(StaticTag) {}
};

/// A typedef for simple global caches.
template <class EntryTy, uint16_t Tag>
using SimpleGlobalCache =
  ConcurrentMap<EntryTy, /*destructor*/ false, TaggedMetadataAllocator<Tag>>;

template <class T, bool ProvideDestructor = true>
class StaticOwningPointer {
  T *Ptr;
public:
  StaticOwningPointer(T *ptr = nullptr) : Ptr(ptr) {}
  StaticOwningPointer(const StaticOwningPointer &) = delete;
  StaticOwningPointer &operator=(const StaticOwningPointer &) = delete;
  ~StaticOwningPointer() { delete Ptr; }

  T &operator*() const { return *Ptr; }
  T *operator->() const { return Ptr; }
};

template <class T>
class StaticOwningPointer<T, false> {
  T *Ptr;
public:
  StaticOwningPointer(T *ptr = nullptr) : Ptr(ptr) {}
  StaticOwningPointer(const StaticOwningPointer &) = delete;
  StaticOwningPointer &operator=(const StaticOwningPointer &) = delete;

  T &operator*() const { return *Ptr; }
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

struct ConcurrencyControl {
  Mutex Lock;
  ConditionVariable Queue;

  ConcurrencyControl() = default;
};

template <class EntryType, uint16_t Tag, bool ProvideDestructor = true>
class LockingConcurrentMapStorage {
  ConcurrentMap<EntryType, ProvideDestructor,
                TaggedMetadataAllocator<Tag>> Map;
  StaticOwningPointer<ConcurrencyControl, ProvideDestructor> Concurrency;

public:
  LockingConcurrentMapStorage() : Concurrency(new ConcurrencyControl()) {}

  MetadataAllocator &getAllocator() { return Map.getAllocator(); }

  ConcurrencyControl &getConcurrency() { return *Concurrency; }

  template <class KeyType, class... ArgTys>
  std::pair<EntryType*, bool>
  getOrInsert(KeyType key, ArgTys &&...args) {
    return Map.getOrInsert(key, args...);
  }

  template <class KeyType>
  EntryType *find(KeyType key) {
    return Map.find(key);
  }

  /// A default implementation for resolveEntry that assumes that the
  /// key type is a lookup key for the map.
  template <class KeyType>
  EntryType *resolveExistingEntry(KeyType key) {
    auto entry = Map.find(key);
    assert(entry && "entry doesn't already exist!");
    return entry;
  }
};

/// A map for which there is a phase of initialization that is guaranteed
/// to be performed exclusively.
///
/// In addition to the requirements of ConcurrentMap, the entry type must
/// provide the following members:
///
///   /// An encapsulation of the status of the entry.  The result type
///   /// of most operations.
///   using Status = ...;
///
///   /// Given that this is not the thread currently responsible for
///   /// initializing the entry, wait for the entry to complete.
///   Status await(ConcurrencyControl &concurrency, ArgTys...);
///
///   /// Perform allocation.  If this returns a status, initialization
///   /// is skipped.
///   Optional<Status>
///   beginAllocation(ConcurrencyControl &concurrency, ArgTys...);
///
///   /// Attempt to initialize an entry.  This is called once for the entry,
///   /// immediately after construction, by the thread that successfully
///   /// constructed the entry.
///   Status beginInitialization(ConcurrencyControl &concurrency, ArgTys...);
///
///   /// Attempt to resume initializing an entry.  Only one thread will be
///   /// trying this at once.  This only need to be implemented if
///   /// resumeInitialization is called on the map.
///   Status resumeInitialization(ConcurrencyControl &concurrency, ArgTys...);
///
///   /// Perform an enqueue operation.
///   /// This only needs to be implemented if enqueue is called on the map.
///   bool enqueue(ConcurrencyControl &concurrency, ArgTys...);
///
///   /// Perform a checkDependency operation.  This only needs to be
///   /// implemented if checkDependency is called on the map.
///   MetadataDependency checkDependency(ConcurrencyControl &concurrency,
///                                      ArgTys...);
template <class EntryType,
          class StorageType = LockingConcurrentMapStorage<EntryType, true>>
class LockingConcurrentMap {
  StorageType Storage;
  using Status = typename EntryType::Status;

public:
  LockingConcurrentMap() = default;

  MetadataAllocator &getAllocator() { return Storage.getAllocator(); }

  template <class KeyType, class... ArgTys>
  std::pair<EntryType*, Status>
  getOrInsert(KeyType key, ArgTys &&...args) {
    auto result = Storage.getOrInsert(key, args...);
    auto entry = result.first;

    // If we are not inserting the entry, we need to potentially block on 
    // currently satisfies our conditions.
    if (!result.second) {
      auto status =
        entry->await(Storage.getConcurrency(), std::forward<ArgTys>(args)...);
      return { entry, status };
    }

    // Okay, we inserted.  We are responsible for allocating and
    // subsequently trying to initialize the entry.

    // Allocation.  This can fast-path and bypass initialization by returning
    // a status.
    if (auto status =
          entry->beginAllocation(Storage.getConcurrency(), args...)) {
      return { entry, *status };
    }

    // Initialization.
    auto status = entry->beginInitialization(Storage.getConcurrency(),
                                             std::forward<ArgTys>(args)...);
    return { entry, status };
  }

  template <class KeyType>
  EntryType *find(KeyType key) {
    return Storage.find(key);
  }

  template <class KeyType, class... ArgTys>
  std::pair<EntryType*, Status>
  resumeInitialization(KeyType key, ArgTys &&...args) {
    EntryType *entry = Storage.resolveExistingEntry(key);
    auto status =
      entry->resumeInitialization(Storage.getConcurrency(),
                                  std::forward<ArgTys>(args)...);
    return { entry, status };
  }

  template <class KeyType, class... ArgTys>
  bool enqueue(KeyType key, ArgTys &&...args) {
    EntryType *entry = Storage.resolveExistingEntry(key);
    return entry->enqueue(Storage.getConcurrency(),
                          std::forward<ArgTys>(args)...);
  }

  /// Given that an entry already exists, await it.
  template <class KeyType, class... ArgTys>
  Status await(KeyType key, ArgTys &&...args) {
    EntryType *entry = Storage.resolveExistingEntry(key);
    return entry->await(Storage.getConcurrency(),
                        std::forward<ArgTys>(args)...);
  }

  /// If an entry already exists, await it; otherwise report failure.
  template <class KeyType, class... ArgTys>
  llvm::Optional<Status> tryAwaitExisting(KeyType key, ArgTys &&... args) {
    EntryType *entry = Storage.find(key);
    if (!entry) return None;
    return entry->await(Storage.getConcurrency(),
                        std::forward<ArgTys>(args)...);
  }

  /// Given that an entry already exists, check whether it has an active
  /// dependency.
  template <class KeyType, class... ArgTys>
  MetadataDependency checkDependency(KeyType key, ArgTys &&...args) {
    EntryType *entry = Storage.resolveExistingEntry(key);
    return entry->checkDependency(Storage.getConcurrency(),
                                  std::forward<ArgTys>(args)...);
  }
};

/// A base class for metadata cache entries which supports an unfailing
/// one-phase allocation strategy.
///
/// In addition to the requirements of ConcurrentMap, subclasses should
/// provide:
///
///   /// Allocate the cached entry.  This is not allowed to fail.
///   ValueType allocate(ArgTys...);
template <class Impl, class ValueType>
class SimpleLockingCacheEntryBase {
  static_assert(std::is_pointer<ValueType>::value,
                "value type must be a pointer type");

  static const uintptr_t Empty_NoWaiters = 0;
  static const uintptr_t Empty_HasWaiters = 1;
  static bool isSpecialValue(uintptr_t value) {
    return value <= Empty_HasWaiters;
  }

  std::atomic<uintptr_t> Value;

protected:
  Impl &asImpl() { return static_cast<Impl &>(*this); }
  const Impl &asImpl() const { return static_cast<const Impl &>(*this); }

  SimpleLockingCacheEntryBase() : Value(Empty_NoWaiters) {}

public:
  using Status = ValueType;

  template <class... ArgTys>
  Status await(ConcurrencyControl &concurrency, ArgTys &&...args) {
    // Load the value.  If this is not a special value, we're done.
    auto value = Value.load(std::memory_order_acquire);
    if (!isSpecialValue(value)) {
      return reinterpret_cast<ValueType>(value);
    }

    // The initializing thread will try to atomically swap in a valid value.
    // It can do that while we're holding the lock.  If it sees that there
    // aren't any waiters, it will not acquire the lock and will not try
    // to notify any waiters.  If it does see that there are waiters, it will
    // acquire the lock before notifying them in order to ensure that it
    // catches them all.  On the waiter side, we must set the has-waiters
    // flag while holding the lock.  This is because we otherwise can't be
    // sure that we'll have started waiting before the initializing thread
    // notifies the queue.
    //
    // We're adding a bit of complexity here for the advantage that, in the
    // absence of early contention, we never touch the lock at all.
    concurrency.Lock.withLockOrWait(concurrency.Queue, [&] {
      // Reload the current value.
      value = Value.load(std::memory_order_acquire);

      // If the value is still no-waiters, try to flag that
      // there's a waiter.  If that succeeds, we can go ahead and wait.
      if (value == Empty_NoWaiters &&
          Value.compare_exchange_strong(value, Empty_HasWaiters,
                                        std::memory_order_relaxed,
                                        std::memory_order_acquire))
        return false; // wait

      assert(value != Empty_NoWaiters);

      // If the value is already in the has-waiters state, we can go
      // ahead and wait.
      if (value == Empty_HasWaiters)
        return false; // wait

      // Otherwise, the initializing thread has finished, and we must not wait.
      return true;
    });

    return reinterpret_cast<ValueType>(value);
  }

  template <class... ArgTys>
  llvm::Optional<Status> beginAllocation(ConcurrencyControl &concurrency,
                                         ArgTys &&... args) {
    // Delegate to the implementation class.
    ValueType origValue = asImpl().allocate(std::forward<ArgTys>(args)...);

    auto value = reinterpret_cast<uintptr_t>(origValue);
    assert(!isSpecialValue(value) && "allocate returned a special value");

    // Publish the value.
    auto oldValue = Value.exchange(value, std::memory_order_release);
    assert(isSpecialValue(oldValue));

    // If there were any waiters, acquire the lock and notify the queue.
    if (oldValue != Empty_NoWaiters) {
      concurrency.Lock.withLockThenNotifyAll(concurrency.Queue, []{});
    }

    return origValue;
  }

  template <class... ArgTys>
  Status beginInitialization(ConcurrencyControl &concurrency,
                             ArgTys &&...args) {
    swift_unreachable("beginAllocation always short-circuits");
  }
};

/// A key value as provided to the concurrent map.
class MetadataCacheKey {
  const void * const *Data;
  uint16_t NumKeyParameters;
  uint16_t NumWitnessTables;
  uint32_t Hash;

  /// Compare two witness tables, which may involving checking the
  /// contents of their conformance descriptors.
  static int compareWitnessTables(const WitnessTable *awt,
                                  const WitnessTable *bwt) {
    if (awt == bwt)
      return 0;

    auto *aDescription = awt->getDescription();
    auto *bDescription = bwt->getDescription();
    return compareProtocolConformanceDescriptors(aDescription, bDescription);
  }

public:
  /// Compare two conformance descriptors, checking their contents if necessary.
  static int compareProtocolConformanceDescriptors(
      const ProtocolConformanceDescriptor *aDescription,
      const ProtocolConformanceDescriptor *bDescription) {
    if (aDescription == bDescription)
      return 0;

    if (!aDescription->isSynthesizedNonUnique() ||
        !bDescription->isSynthesizedNonUnique())
      return comparePointers(aDescription, bDescription);

    auto aType = aDescription->getCanonicalTypeMetadata();
    auto bType = bDescription->getCanonicalTypeMetadata();
    if (!aType || !bType)
      return comparePointers(aDescription, bDescription);

    if (int result = comparePointers(aType, bType))
      return result;

    return comparePointers(aDescription->getProtocol(),
                           bDescription->getProtocol());
  }

private:
  /// Compare the content from two keys.
  static int compareContent(const void * const *adata,
                            const void * const *bdata,
                            unsigned numKeyParameters,
                            unsigned numWitnessTables) {
    // Compare generic arguments for key parameters.
    for (unsigned i = 0; i != numKeyParameters; ++i) {
      if (auto result = comparePointers(*adata++, *bdata++))
        return result;
    }

    // Compare witness tables.
    for (unsigned i = 0; i != numWitnessTables; ++i) {
      if (auto result =
              compareWitnessTables((const WitnessTable *)*adata++,
                                   (const WitnessTable *)*bdata++))
        return result;
    }

    return 0;
  }

public:
  MetadataCacheKey(uint16_t numKeyParams,
                   uint16_t numWitnessTables,
                   const void * const *data)
      : Data(data), NumKeyParameters(numKeyParams),
        NumWitnessTables(numWitnessTables), Hash(computeHash()) { }

  MetadataCacheKey(uint16_t numKeyParams,
                   uint16_t numWitnessTables,
                   const void * const *data,
                   uint32_t hash)
    : Data(data), NumKeyParameters(numKeyParams),
      NumWitnessTables(numWitnessTables), Hash(hash) {}

  bool operator==(MetadataCacheKey rhs) const {
    // Compare the hashes.
    if (hash() != rhs.hash()) return false;

    // Compare the sizes.
    if (NumKeyParameters != rhs.NumKeyParameters) return false;
    if (NumWitnessTables != rhs.NumWitnessTables) return false;

    // Compare the content.
    return compareContent(begin(), rhs.begin(), NumKeyParameters,
                          NumWitnessTables) == 0;
  }

  int compare(const MetadataCacheKey &rhs) const {
    // Compare the hashes.
    if (auto hashComparison = compareIntegers(Hash, rhs.Hash)) {
      return hashComparison;
    }

    // Compare the # of key parameters.
    if (auto keyParamsComparison =
            compareIntegers(NumKeyParameters, rhs.NumKeyParameters)) {
      return keyParamsComparison;
    }

    // Compare the # of witness tables.
    if (auto witnessTablesComparison =
            compareIntegers(NumWitnessTables, rhs.NumWitnessTables)) {
      return witnessTablesComparison;
    }

    // Compare the content.
    return compareContent(begin(), rhs.begin(), NumKeyParameters,
                          NumWitnessTables);
  }

  uint16_t numKeyParameters() const { return NumKeyParameters; }
  uint16_t numWitnessTables() const { return NumWitnessTables; }

  uint32_t hash() const {
    return Hash;
  }

  const void * const *begin() const { return Data; }
  const void * const *end() const { return Data + size(); }
  unsigned size() const { return NumKeyParameters + NumWitnessTables; }

private:
  uint32_t computeHash() const {
    size_t H = 0x56ba80d1u * NumKeyParameters;
    for (unsigned index = 0; index != NumKeyParameters; ++index) {
      H = (H >> 10) | (H << ((sizeof(size_t) * 8) - 10));
      H ^= (reinterpret_cast<size_t>(Data[index])
            ^ (reinterpret_cast<size_t>(Data[index]) >> 19));
    }

    H *= 0x27d4eb2d;

    // Rotate right by 10 and then truncate to 32 bits.
    return uint32_t((H >> 10) | (H << ((sizeof(size_t) * 8) - 10)));
  }
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
  template <class KeyType, class... Args>
  static size_t getExtraAllocationSize(const KeyType &key,
                                       Args &&...args) {
    return TrailingObjects::template additionalSizeToAlloc<Objects...>(
        Impl::numTrailingObjects(OverloadToken<Objects>(), key, args...)...);
  }
  size_t getExtraAllocationSize() const {
    return TrailingObjects::template additionalSizeToAlloc<Objects...>(
        asImpl().numTrailingObjects(OverloadToken<Objects>())...);
  }
};

using RawPrivateMetadataState = uint8_t;
enum class PrivateMetadataState : RawPrivateMetadataState {
  /// The metadata is being allocated.
  Allocating,

  /// The metadata has been allocated, but is not yet complete for
  /// external layout: that is, it does not have a size.
  Abstract,

  /// The metadata has a complete external layout, but may not have
  /// been fully initialized.
  LayoutComplete,

  /// The metadata has a complete external layout and has been fully
  /// initialized, but has not yet satisfied its transitive completeness
  /// requirements.
  NonTransitiveComplete,

  /// The metadata is fully complete.  There should no longer be waiters.
  Complete
};
inline bool operator<(PrivateMetadataState lhs, PrivateMetadataState rhs) {
  return RawPrivateMetadataState(lhs) < RawPrivateMetadataState(rhs);
}
inline bool operator<=(PrivateMetadataState lhs, PrivateMetadataState rhs) {
  return RawPrivateMetadataState(lhs) <= RawPrivateMetadataState(rhs);
}
inline bool operator>(PrivateMetadataState lhs, PrivateMetadataState rhs) {
  return RawPrivateMetadataState(lhs) > RawPrivateMetadataState(rhs);
}
inline bool operator>=(PrivateMetadataState lhs, PrivateMetadataState rhs) {
  return RawPrivateMetadataState(lhs) >= RawPrivateMetadataState(rhs);
}
inline bool satisfies(PrivateMetadataState state, MetadataState requirement) {
  switch (requirement) {
  case MetadataState::Abstract:
    return state >= PrivateMetadataState::Abstract;
  case MetadataState::LayoutComplete:
    return state >= PrivateMetadataState::LayoutComplete;
  case MetadataState::NonTransitiveComplete:
    return state >= PrivateMetadataState::NonTransitiveComplete;
  case MetadataState::Complete:
    return state >= PrivateMetadataState::Complete;
  }
  swift_unreachable("unsupported requirement kind");
}

class PrivateMetadataTrackingInfo {
public:
  using RawType = RawPrivateMetadataState;

private:
  enum : RawType {
    State_mask =      0x7,
    HasWaiters_mask = 0x8,
  };

  RawType Data;

public:
  explicit constexpr PrivateMetadataTrackingInfo(RawType data)
    : Data(data) {}
  explicit constexpr PrivateMetadataTrackingInfo(PrivateMetadataState state)
    : Data(RawType(state)) {}

  static constexpr PrivateMetadataTrackingInfo initial() {
    return PrivateMetadataTrackingInfo(PrivateMetadataState::Allocating);
  }

  PrivateMetadataState getState() const {
    return PrivateMetadataState(Data & State_mask);
  }

  /// Does the state mean that we've allocated metadata?
  bool hasAllocatedMetadata() const {
    return getState() != PrivateMetadataState::Allocating;
  }

  bool isComplete() const {
    return getState() == PrivateMetadataState::Complete;
  }

  bool hasWaiters() const { return Data & HasWaiters_mask; }
  PrivateMetadataTrackingInfo addWaiters() const {
    assert(!isComplete() && "adding waiters to completed state");
    return PrivateMetadataTrackingInfo(Data | HasWaiters_mask);
  }
  PrivateMetadataTrackingInfo removeWaiters() const {
    return PrivateMetadataTrackingInfo(Data & ~HasWaiters_mask);
  }

  MetadataState getAccomplishedRequestState() const {
    switch (getState()) {
    case PrivateMetadataState::Allocating:
      swift_unreachable("cannot call on allocating state");
    case PrivateMetadataState::Abstract:
      return MetadataState::Abstract;
    case PrivateMetadataState::LayoutComplete:
      return MetadataState::LayoutComplete;
    case PrivateMetadataState::NonTransitiveComplete:
      return MetadataState::NonTransitiveComplete;
    case PrivateMetadataState::Complete:
      return MetadataState::Complete;
    }
    swift_unreachable("bad state");
  }

  bool satisfies(MetadataState requirement) {
    return swift::satisfies(getState(), requirement);
  }

  bool shouldWait(MetadataRequest request) {
    switch (getState()) {
    // Always wait if we're allocating.  Non-blocking requests still need
    // to have an allocation that the downstream consumers can report
    // a dependency on.
    case PrivateMetadataState::Allocating:
      return true;

    // We never need to wait if we're complete.  This is the most common
    // result.
    case PrivateMetadataState::Complete:
      return false;

    case PrivateMetadataState::Abstract:
    case PrivateMetadataState::LayoutComplete:
    case PrivateMetadataState::NonTransitiveComplete:
      // Otherwise, if it's a non-blocking request, we do not need to block.
      return (request.isBlocking() && !satisfies(request.getState()));
    }
    swift_unreachable("bad state");
  }

  constexpr RawType getRawValue() const { return Data; }
  RawType &getRawValueRef() { return Data; }
};

/// Reserve the runtime extra space to use for its own tracking.
struct PrivateMetadataCompletionContext {
  MetadataCompletionContext Public;
};

struct MetadataCompletionQueueEntry {
  /// The owning metadata, i.e. the metadata whose completion is blocked.
  Metadata * const Value;

  /// The completion queue for the blocked metadata.
  /// Only accessed under the lock for the owning metadata.
  MetadataCompletionQueueEntry *CompletionQueue = nullptr;

  /// The next entry in the completion queue.
  MetadataCompletionQueueEntry *Next = nullptr;

  /// The saved state of the completion function.
  PrivateMetadataCompletionContext CompletionContext;

  /// The metadata we're enqueued on and the state we're waiting for it
  /// to achieve.  These fields are only ever modified under the lock for
  /// the owning metadata, and only when the metadata is not enqueued
  /// on another metadata's completion queue.  This latter condition is
  /// important because it allows these fields to be read outside of the
  /// lock by the initializing thread of the dependent metadata.
  MetadataDependency Dependency;

  MetadataCompletionQueueEntry(Metadata *value,
                               const PrivateMetadataCompletionContext &context)
    : Value(value), CompletionContext(context) {}
};

/// Add the given queue entry to the queue for the given metadata.
///
/// \return false if the entry was not added because the dependency
///   has already reached the desired requirement
bool addToMetadataQueue(MetadataCompletionQueueEntry *queueEntry,
                        MetadataDependency dependency);

/// Resume completion of the given queue entry, given that it has been
/// removed from its dependency's metadata queue.
void resumeMetadataCompletion(MetadataCompletionQueueEntry *queueEntry);

/// Check for an unbreakable metadata-dependency cycle.
void checkMetadataDependencyCycle(const Metadata *start,
                                  MetadataDependency firstLink,
                                  MetadataDependency secondLink);

/// A cache entry class which provides the basic mechanisms for two-phase
/// metadata initialization.  Suitable for more heavyweight metadata kinds
/// such as generic types and tuples.  Does not provide the lookup-related
/// members.
///
/// The value type may be an arbitrary type, but it must be contextually
/// convertible to bool, and it must be default-constructible in a false
/// state.
///
/// In addition to the lookup members required by ConcurrentMap, concrete
/// implementations should provide:
///
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
///                                     PrivateMetadataState state,
///                                     PrivateMetadataCompletionContext *ctxt);
template <class Impl, class... Objects>
class MetadataCacheEntryBase
       : public ConcurrentMapTrailingObjectsEntry<Impl, Objects...> {
  using super = ConcurrentMapTrailingObjectsEntry<Impl, Objects...>;
public:
  using ValueType = Metadata *;
  using Status = MetadataResponse;

protected:
  using TrailingObjectsEntry = super;
  using super::asImpl;

private:
  #ifdef SWIFT_STDLIB_SINGLE_THREADED_RUNTIME
  using ThreadID = int;
  static ThreadID CurrentThreadID() {
    return 0;
  }
  #else
  using ThreadID = std::thread::id;
  static ThreadID CurrentThreadID() {
    return std::this_thread::get_id();
  }
  #endif

  /// Additional storage that is only ever accessed under the lock.
  union LockedStorage_t {
    /// The thread that is allocating the entry.
    ThreadID AllocatingThread;

    /// The completion queue.
    MetadataCompletionQueueEntry *CompletionQueue;

    /// The metadata's own queue entry.
    MetadataCompletionQueueEntry *QueueEntry;

    LockedStorage_t() {}
    ~LockedStorage_t() {}
  } LockedStorage;

  /// What kind of data is stored in the LockedStorage field below?
  ///
  /// This is only ever modified under the lock.
  enum class LSK : uint8_t {
    /// We're just storing the allocating thread.  The cache entry will be
    /// in this state initially, and it will stay in this state until either
    /// the metadata needs to enqueue itself on another metadata's completion
    /// queue or another metadata needs to enqueue itself on this metadata's
    /// completion queue.  It's possible (likely, even) that the cache entry
    /// will just stay in this state forever, e.g. if it manages to complete
    /// itself before another metadata needs to wait on it.
    AllocatingThread,

    /// We're storing a completion queue without having a queue entry
    /// ourselves.  This can happen if another metadata needs to add itself
    /// to the completion queue for this metadata during its first attempt
    /// at initialization.
    CompletionQueue,

    /// We're storing a queue entry, meaning that the metadata has set itself
    /// up to be enqueued at least once.  (It's possible that the actual
    /// enqueuing never actually succeeded.)  The metadata's completion
    /// queue can be found at LockedStorage.QueueEntry->CompletionQueue.
    /// 
    /// The cache entry owns its queue entry, but it must not destroy it
    /// while it is blocked on another queue.
    QueueEntry,

    /// We've completed ourselves and are storing nothing.
    Complete
  };
  LSK LockedStorageKind;

  /// The current state of this metadata cache entry.
  ///
  /// This has to be stored as a PrivateMetadataTrackingInfo::RawType
  /// instead of a PrivateMetadataTrackingInfo because some of our
  /// targets don't support interesting structs as atomic types.
  std::atomic<PrivateMetadataTrackingInfo::RawType> TrackingInfo;

  // Note that GenericMetadataCacheEntryBase is set up to place fields
  // into the tail padding of this class.

public:
  MetadataCacheEntryBase()
      : LockedStorageKind(LSK::AllocatingThread),
        TrackingInfo(PrivateMetadataTrackingInfo::initial().getRawValue()) {
    LockedStorage.AllocatingThread = CurrentThreadID();
  }

  // Note that having an explicit destructor here is important to make this
  // a non-POD class and allow subclass fields to be allocated in our
  // tail-padding.
  ~MetadataCacheEntryBase() {
    if (LockedStorageKind == LSK::QueueEntry)
      delete LockedStorage.QueueEntry;
  }

  bool isBeingAllocatedByCurrentThread() const {
    return LockedStorageKind == LSK::AllocatingThread &&
           LockedStorage.AllocatingThread == CurrentThreadID();
  }

  /// Given that this thread doesn't own the right to initialize the
  /// metadata, await the metadata being in the right state.
  template <class... Args>
  Status await(ConcurrencyControl &concurrency, MetadataRequest request,
               Args &&...extraArgs) {
    auto trackingInfo =
      PrivateMetadataTrackingInfo(TrackingInfo.load(std::memory_order_acquire));

    if (trackingInfo.shouldWait(request)) {
      awaitSatisfyingState(concurrency, request, trackingInfo);
    }

    assert(trackingInfo.hasAllocatedMetadata());
    return { asImpl().getValue(), trackingInfo.getAccomplishedRequestState() };
  }

  /// The expected return type of allocate.
  struct AllocationResult {
    Metadata *Value;
    PrivateMetadataState State;
  };

  /// Perform the allocation operation.
  template <class... Args>
  llvm::Optional<Status> beginAllocation(ConcurrencyControl &concurrency,
                                         MetadataRequest request,
                                         Args &&... args) {
    // Returning a non-None value here will preempt initialization, so we
    // should only do it if we're reached PrivateMetadataState::Complete.

    // Fast-track out if flagAllocatedDuringConstruction was called.
    if (Impl::MayFlagAllocatedDuringConstruction) {
      // This can be a relaxed load because beginAllocation is called on the
      // same thread that called the constructor.
      auto trackingInfo =
        PrivateMetadataTrackingInfo(
          TrackingInfo.load(std::memory_order_relaxed));

      // If we've already allocated metadata, we can skip the rest of
      // allocation.
      if (trackingInfo.hasAllocatedMetadata()) {
        // Skip initialization, too, if we're fully complete.
        if (trackingInfo.isComplete()) {
          return Status{asImpl().getValue(), MetadataState::Complete};

        // Otherwise go directly to the initialization phase.
        } else {
          return None;
        }
      }
    }

    // Allocate the metadata.
    AllocationResult allocationResult =
      asImpl().allocate(std::forward<Args>(args)...);

    // Publish the value.
    asImpl().setValue(const_cast<ValueType>(allocationResult.Value));
    PrivateMetadataState newState = allocationResult.State;
    publishPrivateMetadataState(concurrency, newState);

    // If allocation gave us completed metadata, short-circuit initialization.
    if (allocationResult.State == PrivateMetadataState::Complete) {
      return Status{allocationResult.Value, MetadataState::Complete};
    }

    return None;
  }

  enum : bool { MayFlagAllocatedDuringConstruction = false };

  /// As an alternative to allocate(), flag that allocation was
  /// completed within the entry's constructor.  This should only be
  /// called from within the constructor.
  ///
  /// If this is called, allocate() will not be called.
  ///
  /// If this is called, the subclass must define
  ///   enum { MayFlagAllocatedDuringConstruction = true };
  void flagAllocatedDuringConstruction(PrivateMetadataState state) {
    assert(Impl::MayFlagAllocatedDuringConstruction);
    assert(state != PrivateMetadataState::Allocating);
    TrackingInfo.store(PrivateMetadataTrackingInfo(state).getRawValue(),
                       std::memory_order_relaxed);
  }

  /// Begin initialization immediately after allocation.
  template <class... Args>
  Status beginInitialization(ConcurrencyControl &concurrency,
                             MetadataRequest request, Args &&...args) {
    // Note that we ignore the extra arguments; those are just for the
    // constructor and allocation.
    return doInitialization(concurrency, nullptr, request);
  }

  /// Resume initialization after a previous failure resulted in the
  /// metadata being enqueued on another metadata cache.
  Status resumeInitialization(ConcurrencyControl &concurrency,
                              MetadataCompletionQueueEntry *queueEntry) {
    return doInitialization(concurrency, queueEntry,
                            MetadataRequest(MetadataState::Complete,
                                            /*non-blocking*/ true));
  }

protected:
  /// The expected return type of tryInitialize.
  struct TryInitializeResult {
    PrivateMetadataState NewState;
    MetadataDependency Dependency;
  };

private:
  /// Try to complete the metadata.
  ///
  /// This is the initializing thread.  The lock is not held.
  Status doInitialization(ConcurrencyControl &concurrency,
                          MetadataCompletionQueueEntry *queueEntry,
                          MetadataRequest request) {
    // We should always have fully synchronized with any previous threads
    // that were processing the initialization, so a relaxed load is fine
    // here.  (This ordering is achieved by the locking which occurs as part
    // of queuing and dequeuing metadata.)
    auto curTrackingInfo =
      PrivateMetadataTrackingInfo(TrackingInfo.load(std::memory_order_relaxed));
    assert(curTrackingInfo.hasAllocatedMetadata());
    assert(!curTrackingInfo.isComplete());

    auto value = asImpl().getValue();

    // Figure out the completion context.
    PrivateMetadataCompletionContext scratchContext;
    PrivateMetadataCompletionContext *context;
    if (queueEntry) {
      context = &queueEntry->CompletionContext;
    } else {
      memset(&scratchContext, 0, sizeof(PrivateMetadataCompletionContext));
      context = &scratchContext;
    }

    bool hasProgressSinceLastEnqueueAttempt = false;
    MetadataCompletionQueueEntry *claimedQueue = nullptr;

    // Try the complete the metadata.  This only loops if initialization
    // has a dependency, but the new dependency is resolved when we go to
    // add ourselves to its queue.
    while (true) {
      TryInitializeResult tryInitializeResult =
        asImpl().tryInitialize(value, curTrackingInfo.getState(), context);
      auto newState = tryInitializeResult.NewState;

      assert(curTrackingInfo.getState() <= newState &&
             "initialization regressed to an earlier state");

      // Publish the new state of the metadata (waking any waiting
      // threads immediately) if we've made any progress.  This seems prudent,
      // but it might mean acquiring the lock multiple times.
      if (curTrackingInfo.getState() < newState) {
        hasProgressSinceLastEnqueueAttempt = true;
        curTrackingInfo = PrivateMetadataTrackingInfo(newState);
        publishPrivateMetadataState(concurrency, newState);
      }

      // If we don't have a dependency, we're finished.
      if (!tryInitializeResult.Dependency) {
        assert(newState == PrivateMetadataState::Complete &&
               "initialization didn't report a dependency but isn't complete");
        assert(hasProgressSinceLastEnqueueAttempt);

        // Claim any satisfied completion-queue entries (i.e. all of them).
        concurrency.Lock.withLock([&] {
          claimSatisfiedQueueEntriesWithLock(curTrackingInfo, claimedQueue);
        });

        // That will destroy the queue entry if we had one, so make sure we
        // don't try to use it.
        queueEntry = nullptr;
        break;
      }

      assert(newState != PrivateMetadataState::Complete &&
             "completed initialization reported a dependency");

      // Otherwise, we need to block this metadata on the dependency's queue.

      // Create a queue entry if necessary.  Start using its context
      // as the continuation context.
      if (!queueEntry) {
        queueEntry = new MetadataCompletionQueueEntry(value, scratchContext);
        context = &queueEntry->CompletionContext;
      }

      // Set the dependency on the queue entry.  This has to happen under
      // the lock to protect against other threads checking for dependency
      // cycles.
      concurrency.Lock.withLock([&] {
        prepareToEnqueueWithLock(queueEntry, tryInitializeResult.Dependency);
        assert(LockedStorageKind == LSK::QueueEntry);

        // Grab any satisfied queue entries while we have the lock.
        if (hasProgressSinceLastEnqueueAttempt) {
          hasProgressSinceLastEnqueueAttempt = false;
          claimSatisfiedQueueEntriesWithLock(curTrackingInfo, claimedQueue);
        }
      });

      // Try to block this metadata initialization on that queue.
      // If this succeeds, we can't consider ourselves the initializing
      // thread anymore.  The small amount of notification we do at the
      // end of this function is okay to race with another thread
      // potentially taking over initialization.
      if (addToMetadataQueue(queueEntry, tryInitializeResult.Dependency))
        break;

      // If that failed, we should still have ownership of the entry.
      assert(queueEntry);
    }

    // Immediately process all the queue entries we claimed.
    while (auto cur = claimedQueue) {
      claimedQueue = cur->Next;
      resumeMetadataCompletion(cur);
    }

    // If we're not actually satisfied by the current state, we might need
    // to block here.
    if (curTrackingInfo.shouldWait(request)) {
      awaitSatisfyingState(concurrency, request, curTrackingInfo);
    }

#if !NDEBUG
    verifyMangledNameRoundtrip(value);
#endif

    return { value, curTrackingInfo.getAccomplishedRequestState() };
  }

  /// Prepare to enqueue this metadata on another metadata's completion
  /// queue, given that we're holding the lock.
  void prepareToEnqueueWithLock(MetadataCompletionQueueEntry *queueEntry,
                                MetadataDependency dependency) {
    assert(dependency);
    queueEntry->Dependency = dependency;

    switch (LockedStorageKind) {
    case LSK::QueueEntry:
      assert(LockedStorage.QueueEntry == queueEntry);
      return;

    case LSK::CompletionQueue:
      // Move the existing completion queue to the cache entry.
      queueEntry->CompletionQueue = LockedStorage.CompletionQueue;
      SWIFT_FALLTHROUGH;

    case LSK::AllocatingThread:
      LockedStorageKind = LSK::QueueEntry;
      LockedStorage.QueueEntry = queueEntry;
      return;

    case LSK::Complete:
      swift_unreachable("preparing to enqueue when already complete?");
    }
    swift_unreachable("bad kind");
  }

  /// Claim all the satisfied completion queue entries, given that
  /// we're holding the lock.
  void claimSatisfiedQueueEntriesWithLock(PrivateMetadataTrackingInfo newInfo,
                                  MetadataCompletionQueueEntry *&claimedQueue) {
    // Collect anything in the metadata's queue whose target state has been
    // reached to the queue in result.  Note that we repurpose the Next field
    // in the collected entries.

    MetadataCompletionQueueEntry **completionQueue;
    if (LockedStorageKind == LSK::CompletionQueue) {
      completionQueue = &LockedStorage.CompletionQueue;
    } else if (LockedStorageKind == LSK::QueueEntry) {
      completionQueue = &LockedStorage.QueueEntry->CompletionQueue;
    } else {
      // If we're not even currently storing a completion queue,
      // there's nothing to do but wake waiting threads.
      return;
    }

    // We want to append to the claimed queue, so find the end.
    auto nextToResume = &claimedQueue;
    while (auto next = *nextToResume) {
      nextToResume = &next->Next;
    }
    assert(!*nextToResume);

    // If the new state is complete, we can just claim the entire queue
    // and destroy the metadata's own queue entry if it exists.
    if (newInfo.isComplete()) {
      *nextToResume = *completionQueue;
      *completionQueue = nullptr;

      if (LockedStorageKind == LSK::QueueEntry) {
        delete LockedStorage.QueueEntry;
      }

      // Mark that we're no longer storing a queue.
      LockedStorageKind = LSK::Complete;

      return;
    }

    // Otherwise, we have to walk the completion queue looking specifically
    // for entries that match.
    auto *nextWaiter = completionQueue;
    while (auto waiter = *nextWaiter) {
      // If the new state of this entry doesn't satisfy the waiter's
      // requirements, skip over it.
      if (!newInfo.satisfies(waiter->Dependency.Requirement)) {
        nextWaiter = &waiter->Next;
        continue;
      }

      // Add the waiter to the end of the next-to-resume queue, and update
      // the end to the waiter's Next field.
      *nextToResume = *nextWaiter;
      nextToResume = &waiter->Next;

      // Splice the waiter out of the completion queue.
      *nextWaiter = waiter->Next;

      assert(!*nextToResume);
    }
  }

  /// Publish a new metadata state.  Wake waiters if we had any.
  void publishPrivateMetadataState(ConcurrencyControl &concurrency,
                                   PrivateMetadataState newState) {
    auto newInfo = PrivateMetadataTrackingInfo(newState);
    assert(newInfo.hasAllocatedMetadata());
    assert(!newInfo.hasWaiters());

    auto oldInfo = PrivateMetadataTrackingInfo(
      TrackingInfo.exchange(newInfo.getRawValue(), std::memory_order_release));
    assert(!oldInfo.isComplete());

    // If we have existing waiters, wake them now, since we no longer
    // remember in State that we have any.
    if (oldInfo.hasWaiters()) {
      // We need to acquire the lock.  There could be an arbitrary number
      // of threads simultaneously trying to set the has-waiters flag, and we
      // have to make sure they start waiting before we notify the queue.
      concurrency.Lock.withLockThenNotifyAll(concurrency.Queue, [] {});
    }
  }

  /// Wait for the request to be satisfied by the current state.
  void awaitSatisfyingState(ConcurrencyControl &concurrency,
                            MetadataRequest request,
                            PrivateMetadataTrackingInfo &trackingInfo) {
    concurrency.Lock.withLockOrWait(concurrency.Queue, [&] {
      // Re-load the state now that we have the lock.  If we don't
      // need to wait, we're done.  Otherwise, flag the existence of a
      // waiter; if that fails, start over with the freshly-loaded state.
      trackingInfo = PrivateMetadataTrackingInfo(
                                  TrackingInfo.load(std::memory_order_acquire));
      while (true) {
        if (!trackingInfo.shouldWait(request))
          return true;

        if (trackingInfo.hasWaiters())
          break;

        // Try to swap in the has-waiters bit.  If this succeeds, we can
        // ahead and wait.
        if (TrackingInfo.compare_exchange_weak(trackingInfo.getRawValueRef(),
                                        trackingInfo.addWaiters().getRawValue(),
                                        std::memory_order_relaxed,
                                        std::memory_order_acquire))
          break;
      }

      // As a QoI safe-guard against the simplest form of cyclic
      // dependency, check whether this thread is the one responsible
      // for allocating the metadata.
      if (isBeingAllocatedByCurrentThread()) {
        fprintf(stderr,
                "%s(%p): cyclic metadata dependency detected, aborting\n",
                Impl::getName(), static_cast<const void*>(this));
        abort();
      }

      return false;
    });
  }

public:
  /// Block a metadata initialization on progress in the initialization
  /// of this metadata.
  ///
  /// That is, this cache entry is for metadata Y, and we have been
  /// handed a queue entry showing a dependency for a metadata X on Y
  /// reaching state S_Y.  Add the queue entry to the completion queue
  /// for Y (which is to say, on this cache entry) unless Y has already
  /// reached state S.  If it has reached that state, return false.
  ///
  /// This is always called from the initializing thread.  The lock is not held.
  bool enqueue(ConcurrencyControl &concurrency,
               MetadataCompletionQueueEntry *queueEntry,
               MetadataDependency dependency) {
    assert(queueEntry);
    assert(!queueEntry->Next);
    assert(dependency == queueEntry->Dependency);

    MetadataDependency otherDependency;
    bool success = concurrency.Lock.withLock([&] {
      auto curInfo = PrivateMetadataTrackingInfo(
                                  TrackingInfo.load(std::memory_order_acquire));
      if (curInfo.satisfies(dependency.Requirement))
        return false;

      // Note that we don't set the waiters bit because we're not actually
      // blocking any threads.

      // Ensure that there's a completion queue.
      MetadataCompletionQueueEntry **completionQueue;

      switch (LockedStorageKind) {
      case LSK::Complete:
        swift_unreachable("enqueuing on complete cache entry?");

      case LSK::AllocatingThread:
        LockedStorageKind = LSK::CompletionQueue;
        LockedStorage.CompletionQueue = nullptr;
        completionQueue = &LockedStorage.CompletionQueue;
        break;

      case LSK::CompletionQueue:
        completionQueue = &LockedStorage.CompletionQueue;
        break;

      case LSK::QueueEntry:
        otherDependency = LockedStorage.QueueEntry->Dependency;
        completionQueue = &LockedStorage.QueueEntry->CompletionQueue;
        break;
      }

      queueEntry->Next = *completionQueue;
      *completionQueue = queueEntry;
      return true;
    });

    // Diagnose unbreakable dependency cycles.
    //
    // Note that we only do this if we find a second dependency link ---
    // that is, if metadata Y is itself dependent on metadata Z reaching
    // state S_Z --- but that this will fire even only a cycle of length 1
    // (i.e. if X == Y) because of course Y will already be showing the
    // dependency on Y in this case.
    if (otherDependency) {
      checkMetadataDependencyCycle(queueEntry->Value, dependency,
                                   otherDependency);
    }

    return success;
  }

  MetadataDependency checkDependency(ConcurrencyControl &concurrency,
                                     MetadataState requirement) {
    return concurrency.Lock.withLock([&] {
      // Load the current state.
      auto curInfo = PrivateMetadataTrackingInfo(
                                  TrackingInfo.load(std::memory_order_acquire));

      // If the requirement is satisfied, there no further dependency for now.
      if (curInfo.satisfies(requirement))
        return MetadataDependency();

      // Check for an existing dependency.
      switch (LockedStorageKind) {
      case LSK::Complete:
        swift_unreachable("dependency on complete cache entry?");

      case LSK::AllocatingThread:
      case LSK::CompletionQueue:
        return MetadataDependency();

      case LSK::QueueEntry:
        return LockedStorage.QueueEntry->Dependency;
      }
    });
  }
};

/// An convenient subclass of MetadataCacheEntryBase which provides
/// metadata lookup using a variadic key.
template <class Impl, class... Objects>
class VariadicMetadataCacheEntryBase :
         public MetadataCacheEntryBase<Impl, const void *, Objects...> {
  using super = MetadataCacheEntryBase<Impl, const void *, Objects...>;

protected:
  using super::asImpl;

  using ValueType = typename super::ValueType;

  using TrailingObjectsEntry = typename super::TrailingObjectsEntry;
  friend TrailingObjectsEntry;

  using TrailingObjects = typename super::TrailingObjects;
  friend TrailingObjects;

  template<typename T>
  using OverloadToken = typename TrailingObjects::template OverloadToken<T>;

  size_t numTrailingObjects(OverloadToken<const void *>) const {
    return NumKeyParameters + NumWitnessTables;
  }

  template <class... Args>
  static size_t numTrailingObjects(OverloadToken<const void *>,
                                   const MetadataCacheKey &key,
                                   Args &&...extraArgs) {
    return key.size();
  }

private:
  // These are arranged to fit into the tail-padding of the superclass.

  /// These are set during construction and never changed.
  const uint16_t NumKeyParameters;
  const uint16_t NumWitnessTables;
  const uint32_t Hash;

  /// Valid if TrackingInfo.getState() >= PrivateMetadataState::Abstract.
  ValueType Value;

  friend super;
  ValueType getValue() {
    return Value;
  }
  void setValue(ValueType value) {
    Value = value;
  }

public:
  VariadicMetadataCacheEntryBase(const MetadataCacheKey &key)
      : NumKeyParameters(key.numKeyParameters()),
        NumWitnessTables(key.numWitnessTables()),
        Hash(key.hash()) {
    memcpy(this->template getTrailingObjects<const void *>(),
           key.begin(), key.size() * sizeof(const void *));
  }

  MetadataCacheKey getKey() const {
    return MetadataCacheKey(NumKeyParameters, NumWitnessTables,
                            this->template getTrailingObjects<const void*>(),
                            Hash);
  }

  intptr_t getKeyIntValueForDump() const {
    return Hash;
  }

  int compareWithKey(const MetadataCacheKey &key) const {
    return key.compare(getKey());
  }
};

template <class EntryType, uint16_t Tag, bool ProvideDestructor = true>
class MetadataCache :
    public LockingConcurrentMap<EntryType,
             LockingConcurrentMapStorage<EntryType, Tag, ProvideDestructor>> {
};

} // namespace swift

#endif // SWIFT_RUNTIME_METADATACACHE_H
