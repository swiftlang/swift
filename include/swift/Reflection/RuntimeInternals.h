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
  typename Runtime::StoredSize ReaderCount;
  typename Runtime::StoredSize ElementCount;
  typename Runtime::StoredPointer Elements;
  typename Runtime::StoredPointer Indices;
  // We'll ignore the remaining fields for now....
};

template <typename Runtime> struct ConformanceCacheEntry {
  typename Runtime::StoredPointer Type;
  typename Runtime::StoredPointer Proto;
  typename Runtime::StoredPointer Witness;
};

template <typename Runtime>
struct HeapObject {
  typename Runtime::StoredPointer Metadata;
  typename Runtime::StoredSize RefCounts;
};

template <typename Runtime>
struct Job {
  HeapObject<Runtime> HeapObject;
  typename Runtime::StoredPointer SchedulerPrivate[2];
  uint32_t Flags;
  uint32_t Id;
  typename Runtime::StoredPointer Reserved[2];
  typename Runtime::StoredPointer RunJob;
};

template <typename Runtime>
struct StackAllocator {
  typename Runtime::StoredPointer LastAllocation;
  typename Runtime::StoredPointer FirstSlab;
  int32_t NumAllocatedSlabs;
  bool FirstSlabIsPreallocated;

  struct Slab {
    typename Runtime::StoredPointer Next;
    uint32_t Capacity;
    uint32_t CurrentOffset;
  };
};

template <typename Runtime>
struct AsyncTaskPrivateStorage {
  typename Runtime::StoredSize Status;
  StackAllocator<Runtime> Allocator;
  typename Runtime::StoredPointer Local;
};

template <typename Runtime>
struct AsyncTask: Job<Runtime> {
  // On 64-bit, there's a Reserved64 after ResumeContext.  
  typename Runtime::StoredPointer ResumeContextAndReserved[
    sizeof(typename Runtime::StoredPointer) == 8 ? 2 : 1];

  AsyncTaskPrivateStorage<Runtime> PrivateStorage;
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_RUNTIME_INTERNALS_H
