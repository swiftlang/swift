//===--- RuntimeInternals.h - Runtime Internal Structures -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Runtime data structures that Reflection inspects externally.
//
// FIXME: Ideally the original definitions would be templatized on a Runtime
// parameter and we could use the original definitions in both the runtime and
// in Reflection.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_RUNTIME_INTERNALS_H
#define SWIFT_REFLECTION_RUNTIME_INTERNALS_H

#include <stdint.h>

namespace swift {

namespace reflection {

template <typename Runtime>
struct ConformanceNode {
  typename Runtime::StoredPointer Left, Right;
  typename Runtime::StoredPointer Type;
  typename Runtime::StoredPointer Proto;
  typename Runtime::StoredPointer Description;
  typename Runtime::StoredSize FailureGeneration;
};

template <typename Runtime>
struct MetadataAllocation {
  uint16_t Tag;
  typename Runtime::StoredPointer Ptr;
  unsigned Size;
};

template <typename Runtime> struct MetadataCacheNode {
  typename Runtime::StoredPointer Left;
  typename Runtime::StoredPointer Right;
};

template <typename Runtime> struct ConcurrentHashMap {
  uint32_t ReaderCount;
  uint32_t ElementCount;
  typename Runtime::StoredPointer Elements;
  typename Runtime::StoredPointer Indices;
  // We'll ignore the remaining fields for now....
};

template <typename Runtime> struct ConformanceCacheEntry {
  typename Runtime::StoredPointer Type;
  typename Runtime::StoredSignedPointer Proto;
  typename Runtime::StoredPointer Witness;
};

template <typename Runtime>
struct HeapObject {
  typename Runtime::StoredSignedPointer Metadata;
  typename Runtime::StoredSize RefCounts;
};

template <typename Runtime>
struct Job {
  HeapObject<Runtime> HeapObject;
  typename Runtime::StoredPointer SchedulerPrivate[2];
  uint32_t Flags;
  uint32_t Id;
  typename Runtime::StoredPointer Reserved[2];
  typename Runtime::StoredSignedPointer RunJob;
};

template <typename Runtime>
struct StackAllocator {
  typename Runtime::StoredPointer LastAllocation;
  typename Runtime::StoredPointer FirstSlab;
  int32_t NumAllocatedSlabsAndFirstSlabIsPreallocated;

  struct Slab {
    typename Runtime::StoredPointer Metadata;
    typename Runtime::StoredPointer Next;
    uint32_t Capacity;
    uint32_t CurrentOffset;
  };
};

template <typename Runtime>
struct ActiveTaskStatusWithEscalation {
  uint32_t Flags[1];
  uint32_t ExecutionLock[(sizeof(typename Runtime::StoredPointer) == 8) ? 1 : 2];
  typename Runtime::StoredPointer Record;
};

template <typename Runtime>
struct ActiveTaskStatusWithoutEscalation {
  uint32_t Flags[sizeof(typename Runtime::StoredPointer) == 8 ? 2 : 1];
  typename Runtime::StoredPointer Record;
};

struct ActiveTaskStatusFlags {
  static const uint32_t PriorityMask = 0xFF;
  static const uint32_t IsCancelled = 0x100;
  static const uint32_t IsStatusRecordLocked = 0x200;
  static const uint32_t IsEscalated = 0x400;
  static const uint32_t IsRunning = 0x800;
  static const uint32_t IsEnqueued = 0x1000;
  static const uint32_t IsComplete = 0x2000;
};

template <typename Runtime, typename ActiveTaskStatus>
struct AsyncTaskPrivateStorage {
  typename Runtime::StoredPointer ExclusivityAccessSet[2];
  ActiveTaskStatus Status;
  StackAllocator<Runtime> Allocator;
  typename Runtime::StoredPointer Local;
  uint32_t Id;
  typename Runtime::StoredSize BasePriority;
  typename Runtime::StoredPointer DependencyRecord;
};

template <typename Runtime, typename ActiveTaskStatus>
struct AsyncTask: Job<Runtime> {
  // On 64-bit, there's a Reserved64 after ResumeContext.
  typename Runtime::StoredPointer ResumeContextAndReserved[
    sizeof(typename Runtime::StoredPointer) == 8 ? 2 : 1];
  AsyncTaskPrivateStorage<Runtime, ActiveTaskStatus> PrivateStorage;
};

template <typename Runtime>
struct AsyncContext {
  typename Runtime::StoredSignedPointer Parent;
  typename Runtime::StoredSignedPointer ResumeParent;
};

template <typename Runtime>
struct AsyncContextPrefix {
  typename Runtime::StoredSignedPointer AsyncEntryPoint;
  typename Runtime::StoredPointer ClosureContext;
  typename Runtime::StoredPointer ErrorResult;
};

template <typename Runtime>
struct FutureAsyncContextPrefix {
  typename Runtime::StoredPointer IndirectResult;
  typename Runtime::StoredSignedPointer AsyncEntryPoint;
  typename Runtime::StoredPointer ClosureContext;
  typename Runtime::StoredPointer ErrorResult;
};

template <typename Runtime>
struct alignas(2 * sizeof(typename Runtime::StoredPointer))
    ActiveActorStatusWithEscalation {
  uint32_t Flags[1];
  uint32_t DrainLock[(sizeof(typename Runtime::StoredPointer) == 8) ? 1 : 2];
  typename Runtime::StoredPointer FirstJob;
};

template <typename Runtime>
struct alignas(2 * sizeof(typename Runtime::StoredPointer))
    ActiveActorStatusWithoutEscalation {
  uint32_t Flags[sizeof(typename Runtime::StoredPointer) == 8 ? 2 : 1];
  typename Runtime::StoredPointer FirstJob;
};

template <typename Runtime, typename ActiveActorStatus>
struct DefaultActorImpl {
  HeapObject<Runtime> HeapObject;
  bool IsDistributedRemote;
  ActiveActorStatus Status;
};

template <typename Runtime>
struct TaskStatusRecord {
  typename Runtime::StoredSize Flags;
  typename Runtime::StoredPointer Parent;
};

template <typename Runtime>
struct ChildTaskStatusRecord : TaskStatusRecord<Runtime> {
  typename Runtime::StoredPointer FirstChild;
};

template <typename Runtime>
struct TaskGroupTaskStatusRecord : TaskStatusRecord<Runtime> {
  typename Runtime::StoredPointer FirstChild;
};

template <typename Runtime>
struct ChildFragment {
  typename Runtime::StoredPointer Parent;
  typename Runtime::StoredPointer NextChild;
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_RUNTIME_INTERNALS_H
