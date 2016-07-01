//===--- Heap.cpp - Swift Language Heap Logic -----------------------------===//
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
//
// Implementations of the Swift heap
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Heap.h"
#include "Private.h"
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Metadata.h"
#include <stdlib.h>
#include <stdio.h>

using namespace swift;

SWIFT_RT_ENTRY_VISIBILITY
void *swift::swift_slowAlloc(size_t size, size_t alignMask)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
  // FIXME: use posix_memalign if alignMask is larger than the system guarantee.
  void *p = malloc(size);
  if (!p) swift::crash("Could not allocate memory.");
  return p;
}

SWIFT_RT_ENTRY_VISIBILITY
void swift::swift_slowDealloc(void *ptr, size_t bytes, size_t alignMask)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
  free(ptr);
}

#if false // Enable to debug the stack alloc functions.
#define SA_LOG(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)
#else
#define SA_LOG(fmt, ...)
#endif

//===----------------------------------------------------------------------===//
//      Runtime functions for stack allocation of non-fixed sized types
//===----------------------------------------------------------------------===//

// Calls to these runtime functions are generated for the SIL instruction
// alloc_stack, optionally in combination with a succeeding
// copy_addr [initialize].
//
// Up to two stack allocations can be combined into a single runtime call.
// Multiple runtime functions are provided to cover a whole set of different
// combinations. The naming scheme is:
//
//   swift_stackAllocXx
//
// where X is one of:
//   C ... allocate a buffer and copy data into it.
//   A ... just allocate the buffer
// and x is one of C, A or
//   c ... same as 'C' but re-use the Metadata pointer from the first allocation
//   a ... same as 'A' but re-use the Metadata pointer from the first allocation
//   empty, if only a single allocation is done.
//
// Depending on the variation of Xx, up to 2 Metadata pointers and up to 2
// copy-source addresses must be provided (as arguments #4 to #6).
// The first three parameters of swift_stackAllocXx are
// 1. A pointer to the AllocDescr, which is a small datastructure located on
//    the stack. It stores everything needed if heap allocation must be done.
// 2. A pointer to the fixed-sized inline buffer on the stack, which is either
//    3-words or 6-words of size, depending if a single or double allocation is
//    done.
// 3. An allocation ID, which is a unique number, identifying the runtime call
//    within a function. It is used to index into the AllocDescr.Entries array
//    to store heap allocated buffer pointers.
//
// The general allocation strategy is:
// 1. If the two allocations (or the single allocation) fit into the provided
//    fixed-sized buffer, then fine, just return pointers into that buffer (at
//    offset 0 and 3).
// 2. If not, check if both allocations in total fit in the fixed size buffer,
//    e.g. a 5 word + 1 word type.
// 3. If not, check if we already heap allocated a buffer for this allocation
//    ID, e.g. in a previous loop iteration. This is done by checking
//    AllocDescr.Entries[AllocID].Start.
//    If this is the first time we are dealing with heap allocation during the
//    function execution, all the AllocDescr.Entries[].Start pointers up to
//    AllocID are initialized with nullptr (lazy initialization).
// 4. If not, try to bump-allocate the memory in the current AllocPool.
// 5. If there is no pool yet or it is out of free memory, malloc a new pool.
//    We allocate more space then required, so that following allocations can
//    benefit from bump pointer allocation.
//
// At the function exit, swift_deallocStack is called, which frees all the
// allocated pools.
//
// Example: Let's assume we have 2 groups of 2 alloc_stacks each.
//
// First group (AllocID = 0)
//
// %a = alloc_stack $A  // sizeof(A) = 2 words
// copy_addr %srcForA to [initialization] %a : $*A
// %b = alloc_stack $B  // sizeof(B) = 3 words
//
// Second group (AllocID = 1)
//
// %c = alloc_stack $C  // sizeof(C) > 3 words
// %d = alloc_stack $D  // sizeof(D) > 3 words
//
//
// %0 = swift_stackAllocCA(allocdescr, inline_buf0, 0 /* allocID */,
//                         metadata(A), srcForA, metadata(B))
// ; returns { inline_buf0, inline_buf0 + 3 }
//
// %1 = swift_stackAllocAA(allocdescr, inline_buf1, 1 /* allocID */,
//                         metadata(C), metadata(D))
// ; returns { startofC, startofD }
//
// Memory layout:
//                          Stack                            Heap
//
//                                                           pool:
//                          | ... |                          +-----+
// AllocDescr: FirstPool:   |  *--|--------------> Next:     | null|
//             NumAllocIDs: |  2  |                BumpPtr:  |  *--|--+
//             Entries[0]:  | null|                EndPtr:   |  *--|--|-+
//                          |  ?  |        +-----> startofC: |  C  |  | |
//             Entries[1]:  |  *--|--------+                 | ... |  | |
//                          |  *--|--------+                 |  C  |  | |
//                          | ... |        +-----> startofD: |  D  |  | |
//                                                           | ... |  | |
//                                                           |  D  |  | |
//                          | ... |                free mem: |  ?  | <+ |
//             inline_buf0: |  A  |                          | ... |    |
//                          |  A  |                          |  ?  |    |
//                          |  ?  |                          +-----+ <--+
//                          |  B  |
//                          |  B  |
//                          |  B  |
//                          | ... |
//
//
//                          | ... |
//             inline_buf1: |  ?  |  Unused
//                          |  ?  |
//                          |  ?  |
//                          |  ?  |
//                          |  ?  |
//                          |  ?  |
//                          | ... |

/// Return type of the swift_stackAllocXx functions.
struct OpaquePtrPair {
  OpaqueValue *First;
  OpaqueValue *Second;
};

/// Header of an allocated pool.
/// The actual free memory is tail-allocated to this struct.
struct AllocPool {
  /// The next pool in the linked list of pools.
  AllocPool *Next;
  /// The current pointer to the next free memory.
  char *BumpPtr;
  /// The end of the available memory.
  char *EndPtr;
  
  /// Tail-allocated memory;
  char Memory[1];
};

/// A vector of AllocEntries is tail-allocated to the AllocDescr.
///
/// There is one entry for each allocation ID.
struct AllocEntry {
  char *Start;
  char *End;
};

/// The allocator descriptor, which resides on the stack.
///
/// It is used to store information about malloc'ed memory.
struct AllocDescr {
  /// A linked list of pools.
  AllocPool *FirstPool;
  /// The current number of initialized allocation entries.
  int NumAllocIDs;
  
  /// Tail-allocated entries.
  AllocEntry Entries[1];
};

enum {
  /// Number of times we allocate more memory in a pool than needed so that
  /// subsequent allocations can re-use the same pool.
  PreAllocFactor = 3
};

/// Round pointer up to a specific alignment.
inline static char *roundUp(char *Ptr, size_t AlignMask) {
  return (char *)(((uintptr_t)Ptr + AlignMask) & ~AlignMask);
}

static char *allocInPool(AllocDescr *Descr, int AllocID, size_t Size,
                           size_t AlignMask) {
  SA_LOG("  # slowalloc #%d, descr=%p, size=%zd, align=%zd\n",
         AllocID, Descr, Size, AlignMask);

  int NumAllocIDs;
  if (Descr->FirstPool == nullptr) {
    NumAllocIDs = 0;
  } else {
    NumAllocIDs = Descr->NumAllocIDs;
  }
  if (AllocID >= NumAllocIDs) {
    assert(sizeof(AllocDescr) == 4 * sizeof(void *));
    for (int Idx = NumAllocIDs; Idx <= AllocID; Idx++) {
      SA_LOG("  ... init-null #%d at %p\n", Idx, &Descr->Entries[Idx].Start);
      Descr->Entries[Idx].Start = nullptr;
    }
    Descr->NumAllocIDs = AllocID + 1;
  }

  char *AllocPtr = Descr->Entries[AllocID].Start;
  if (AllocPtr != nullptr) {
    // We came across the same alloc ID a second time, e.g. because the
    // allocation is in a loop.
    char *EndPtr = Descr->Entries[AllocID].End;
    AllocPtr = roundUp(AllocPtr, AlignMask);
    // This should always be the case because currently we allocate the same
    // types for the same AllocID. So the size should not change.
    if (AllocPtr + Size <= EndPtr) {
      SA_LOG("  -> re-use %p\n", AllocPtr);
      return AllocPtr;
    }
  }
  if (AllocPool *Pool = Descr->FirstPool) {
    AllocPtr = roundUp(Pool->BumpPtr, AlignMask);
    if (AllocPtr + Size < Pool->EndPtr) {
      // Still enough free memory in the current pool.
      Pool->BumpPtr = AllocPtr + Size;

      // Record the allocated memory for the AllocID.
      Descr->Entries[AllocID].Start = AllocPtr;
      Descr->Entries[AllocID].End = AllocPtr + Size;
      SA_LOG("  -> bump alloc: %p\n", AllocPtr);
      return AllocPtr;
    }
  }
  // We need a new pool. We allocate more memory than needed so that subsequent
  // allocations can still fit into this pool (reduces the number of malloc
  // calls).
  size_t PoolSize = sizeof(AllocPool) + Size * PreAllocFactor;
  AllocPool *NewPool = (AllocPool *)swift_slowAlloc(PoolSize, AlignMask);
  
  // Insert the new pool into the linked list of pools.
  NewPool->Next = Descr->FirstPool;
  Descr->FirstPool = NewPool;
  
  NewPool->EndPtr = (char *)NewPool + PoolSize;
  char *NewAllocPtr = roundUp(&NewPool->Memory[0], AlignMask);
  
  // Record the allocated memory for the AllocID.
  Descr->Entries[AllocID].Start = NewAllocPtr;
  Descr->Entries[AllocID].End = NewAllocPtr + Size;
  
  NewPool->BumpPtr = NewAllocPtr + Size;
  assert(NewPool->BumpPtr <= (char*)NewPool + PoolSize);
  SA_LOG("  -> in new pool: %p (pool=%p, size=%zd)\n", NewAllocPtr, NewPool, Size);
  return NewAllocPtr;
}

__attribute__((noinline)) OpaquePtrPair slowStackAlloc(char **DescrPtr,
                            char *StackSlot,
                            unsigned AllocID,
                            Metadata *MD0, OpaqueValue *CopySrc0,
                            Metadata *MD1, OpaqueValue *CopySrc1) {
  char *BumpPtr = StackSlot;
  
  SA_LOG("### slow stackalloc, id=%d, descr=%p, sslot=%p\n",
       AllocID, DescrPtr, StackSlot);
  
  char *EndPtr = StackSlot + (MD1 ? 2 : 1) * sizeof(ValueBuffer);

  Metadata *MD = MD0;
  const ValueWitnessTable *VW = MD->getValueWitnesses();
  size_t Size = VW->getSize();
  size_t AlignMask = VW->getAlignmentMask();
  
  OpaqueValue *CopySrc = CopySrc0;
  OpaqueValue *Buffer0 = nullptr;
  OpaqueValue *Buffer1 = nullptr;
  SA_LOG("  . initial read MD: %p, VW=%p, size=%zd\n", MD, VW, Size);
  bool isMalloced = false;
                                
  // This loop is executed a single or two times (depending on MD1).
  while (true) {
    BumpPtr = roundUp(BumpPtr, AlignMask);
    SA_LOG("  . alloc: %p\n", BumpPtr);
    if (BumpPtr + Size > EndPtr) {
      assert(!isMalloced);
      size_t RemainingSize = Size;
      if (MD1) {
        const ValueWitnessTable *VW1 = MD1->getValueWitnesses();
        AlignMask |= VW1->getAlignmentMask();
        RemainingSize = (RemainingSize + AlignMask) & ~AlignMask;
        RemainingSize += VW1->getSize();
      }
      BumpPtr = allocInPool((AllocDescr *)DescrPtr, AllocID, RemainingSize,
                            AlignMask);
      EndPtr = BumpPtr + RemainingSize;
      isMalloced = true;
    }
    if (!Buffer0) {
      // The first pointer is returned via the return value.
      Buffer0 = (OpaqueValue *)BumpPtr;
      SA_LOG("  ... return result #0: %p\n", BumpPtr);
    } else {
      Buffer1 = (OpaqueValue *)BumpPtr;
      SA_LOG("  ... return result #1: %p\n", BumpPtr);
    }
    if (CopySrc) {
      SA_LOG("  ... initializeWithCopy(%p <- %p, md=%p)\n", BumpPtr, CopySrc, MD);
      VW->initializeWithCopy((OpaqueValue *)BumpPtr, CopySrc, MD);
    }
    BumpPtr += Size;
    
    if (!MD1)
      break;
    
    if (MD1 != MD0) {
      MD = MD1;
      VW = MD->getValueWitnesses();
      Size = VW->getSize();
      AlignMask = VW->getAlignmentMask();
      CopySrc = CopySrc1;
    }
    MD1 = nullptr;
  }
  return {Buffer0, Buffer1};
}

SWIFT_RUNTIME_EXPORT
extern "C" void swift_stackDealloc(char **DescrPtr) {
  SA_LOG("### stack-dealloc\n");
  AllocDescr *Descr = (AllocDescr *)DescrPtr;
  AllocPool *Pool = Descr->FirstPool;
  while (Pool) {
    AllocPool *Next = Pool->Next;
    size_t Size = Pool->EndPtr - (char *)Pool;
    SA_LOG("  # dealloc %p, size=%zd\n", Pool, Size);
    swift_slowDealloc(Pool, Size, /* TODO */ 1);
    Pool = Next;
  }
}

SWIFT_RUNTIME_EXPORT
extern "C" OpaqueValue *swift_stackAllocC(char **DescrPtr, char *StackSlot,
                                          unsigned AllocID,
                                          Metadata *MD, OpaqueValue *CopySrc) {
  SA_LOG("### swift_stackAllocC, id=%d, descr=%p, sslot=%p\n",
         AllocID, DescrPtr, StackSlot);
  
  const ValueWitnessTable *VW = MD->getValueWitnesses();
  SA_LOG("  . read MD: %p, VW=%p\n", MD, VW);
  if (!VW->isValueInline()) {
    return slowStackAlloc(DescrPtr, StackSlot, AllocID,
                          MD, CopySrc, nullptr, nullptr).First;
  }
  OpaqueValue *Dest0 = (OpaqueValue *)StackSlot;
  VW->initializeWithCopy(Dest0, CopySrc, MD);
  return Dest0;
}

SWIFT_RUNTIME_EXPORT
extern "C" OpaqueValue *swift_stackAllocA(char **DescrPtr, char *StackSlot,
                                          unsigned AllocID,
                                          Metadata *MD) {
  SA_LOG("### swift_stackAllocA, id=%d, descr=%p, sslot=%p\n",
         AllocID, DescrPtr, StackSlot);
  
  const ValueWitnessTable *VW = MD->getValueWitnesses();
  SA_LOG("  . read MD: %p, VW=%p\n", MD, VW);
  if (!VW->isValueInline()) {
    return slowStackAlloc(DescrPtr, StackSlot, AllocID,
                          MD, nullptr, nullptr, nullptr).First;
  }
  return (OpaqueValue *)StackSlot;
}

SWIFT_RUNTIME_EXPORT
extern "C" OpaquePtrPair swift_stackAllocCc(char **DescrPtr, char *StackSlot,
                                           unsigned AllocID,
                                           Metadata *MD,
                                           OpaqueValue *CopySrc0,
                                           OpaqueValue *CopySrc1) {
  SA_LOG("### swift_stackAllocCc, id=%d, descr=%p, sslot=%p\n",
         AllocID, DescrPtr, StackSlot);
  
  const ValueWitnessTable *VW = MD->getValueWitnesses();
  SA_LOG("  . read MD: %p, VW=%p\n", MD, VW);
  if (!VW->isValueInline()) {
    return slowStackAlloc(DescrPtr, StackSlot, AllocID,
                          MD, CopySrc0, MD, CopySrc1);
  }
  OpaqueValue *Dest0 = (OpaqueValue *)StackSlot;
  VW->initializeWithCopy(Dest0, CopySrc0, MD);
  OpaqueValue *Dest1 = (OpaqueValue *)(StackSlot + sizeof(ValueBuffer));
  VW->initializeWithCopy(Dest1, CopySrc1, MD);
  return { Dest0, Dest1 };
}

SWIFT_RUNTIME_EXPORT
extern "C" OpaquePtrPair swift_stackAllocCA(char **DescrPtr, char *StackSlot,
                                           unsigned AllocID,
                                           Metadata *MD0,
                                           OpaqueValue *CopySrc0,
                                           Metadata *MD1) {
  SA_LOG("### swift_stackAllocCA, id=%d, descr=%p, sslot=%p\n",
         AllocID, DescrPtr, StackSlot);
  
  const ValueWitnessTable *VW0 = MD0->getValueWitnesses();
  const ValueWitnessTable *VW1 = MD1->getValueWitnesses();
  SA_LOG("  . read MD0: %p, VW=%p, MD1: %p, VW=%p\n", MD0, VW0, MD1, VW1);
  if (!VW0->isValueInline() || !VW1->isValueInline()) {
    return slowStackAlloc(DescrPtr, StackSlot, AllocID,
                          MD0, CopySrc0, MD1, nullptr);
  }
  OpaqueValue *Dest0 = (OpaqueValue *)StackSlot;
  VW0->initializeWithCopy(Dest0, CopySrc0, MD0);
  OpaqueValue *Dest1 = (OpaqueValue *)(StackSlot + sizeof(ValueBuffer));
  return { Dest0, Dest1 };
}

SWIFT_RUNTIME_EXPORT
extern "C" OpaquePtrPair swift_stackAllocCa(char **DescrPtr, char *StackSlot,
                                           unsigned AllocID,
                                           Metadata *MD,
                                           OpaqueValue *CopySrc0) {
  SA_LOG("### swift_stackAllocCa, id=%d, descr=%p, sslot=%p\n",
         AllocID, DescrPtr, StackSlot);
  
  const ValueWitnessTable *VW = MD->getValueWitnesses();
  SA_LOG("  . read MD: %p, VW=%p\n", MD, VW);
  if (!VW->isValueInline()) {
    return slowStackAlloc(DescrPtr, StackSlot, AllocID,
                          MD, CopySrc0, MD, nullptr);
  }
  OpaqueValue *Dest0 = (OpaqueValue *)StackSlot;
  VW->initializeWithCopy(Dest0, CopySrc0, MD);
  OpaqueValue *Dest1 = (OpaqueValue *)(StackSlot + sizeof(ValueBuffer));
  return { Dest0, Dest1 };
}

SWIFT_RUNTIME_EXPORT
extern "C" OpaquePtrPair swift_stackAllocAA(char **DescrPtr, char *StackSlot,
                                           unsigned AllocID,
                                           Metadata *MD0,
                                           Metadata *MD1) {
  SA_LOG("### swift_stackAllocAA, id=%d, descr=%p, sslot=%p\n",
         AllocID, DescrPtr, StackSlot);
  
  const ValueWitnessTable *VW0 = MD0->getValueWitnesses();
  const ValueWitnessTable *VW1 = MD1->getValueWitnesses();
  SA_LOG("  . read MD0: %p, VW=%p, MD1: %p, VW=%p\n", MD0, VW0, MD1, VW1);
  if (!VW0->isValueInline() || !VW1->isValueInline()) {
    return slowStackAlloc(DescrPtr, StackSlot, AllocID,
                          MD0, nullptr, MD1, nullptr);
  }
  OpaqueValue *Dest0 = (OpaqueValue *)StackSlot;
  OpaqueValue *Dest1 = (OpaqueValue *)(StackSlot + sizeof(ValueBuffer));
  return { Dest0, Dest1 };
}

SWIFT_RUNTIME_EXPORT
extern "C" OpaquePtrPair swift_stackAllocAa(char **DescrPtr, char *StackSlot,
                                           unsigned AllocID,
                                           Metadata *MD) {
  SA_LOG("### swift_stackAllocAa, id=%d, descr=%p, sslot=%p\n",
         AllocID, DescrPtr, StackSlot);
  
  const ValueWitnessTable *VW = MD->getValueWitnesses();
  SA_LOG("  . read MD: %p, VW=%p, size=%zd\n", MD, VW);
  if (!VW->isValueInline()) {
    return slowStackAlloc(DescrPtr, StackSlot, AllocID,
                          MD, nullptr, MD, nullptr);
  }
  OpaqueValue *Dest0 = (OpaqueValue *)StackSlot;
  OpaqueValue *Dest1 = (OpaqueValue *)(StackSlot + sizeof(ValueBuffer));
  return { Dest0, Dest1 };
}
