//===--- GlobalObjects.cpp - Statically-initialized objects ---------------===//
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
//  Objects that are allocated at global scope instead of on the heap,
//  and statically initialized to avoid synchronization costs, are
//  defined here.
//
//===----------------------------------------------------------------------===//

#include "../SwiftShims/GlobalObjects.h"
#include "../SwiftShims/Random.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Debug.h"
#include <stdlib.h>

namespace swift {
// FIXME(ABI)#76 : does this declaration need SWIFT_RUNTIME_STDLIB_API?
// _direct type metadata for Swift.__EmptyArrayStorage
SWIFT_RUNTIME_STDLIB_API
ClassMetadata CLASS_METADATA_SYM(s19__EmptyArrayStorage);

// _direct type metadata for Swift._EmptyDictionarySingleton
SWIFT_RUNTIME_STDLIB_API
ClassMetadata CLASS_METADATA_SYM(s25_EmptyDictionarySingleton);

// _direct type metadata for Swift._EmptySetSingleton
SWIFT_RUNTIME_STDLIB_API
ClassMetadata CLASS_METADATA_SYM(s18_EmptySetSingleton);
} // namespace swift

SWIFT_RUNTIME_STDLIB_API
swift::_SwiftEmptyArrayStorage swift::_swiftEmptyArrayStorage = {
  // HeapObject header;
  {
    &swift::CLASS_METADATA_SYM(s19__EmptyArrayStorage), // isa pointer
  },
  
  // _SwiftArrayBodyStorage body;
  {
    0, // int count;                                    
    1  // unsigned int _capacityAndFlags; 1 means elementTypeIsBridgedVerbatim
  }
};

SWIFT_RUNTIME_STDLIB_API
swift::_SwiftEmptyDictionarySingleton swift::_swiftEmptyDictionarySingleton = {
  // HeapObject header;
  {
    &swift::CLASS_METADATA_SYM(s25_EmptyDictionarySingleton), // isa pointer
  },
  
  // _SwiftDictionaryBodyStorage body;
  {
    // Setting the scale to 0 makes for a bucketCount of 1 -- so that the
    // storage consists of a single unoccupied bucket. The capacity is set to
    // 0 so that any insertion will lead to real storage being allocated.
    0, // int count;
    0, // int capacity;                                    
    0, // int8 scale;
    0, // int8 reservedScale;
    0, // int16 extra;
    0, // int32 age;
    0, // int seed;
    (void *)1, // void* keys; (non-null garbage)
    (void *)1  // void* values; (non-null garbage)
  },

  // bucket 0 is unoccupied; other buckets are out-of-bounds
  static_cast<__swift_uintptr_t>(~1) // int metadata; 
};

SWIFT_RUNTIME_STDLIB_API
swift::_SwiftEmptySetSingleton swift::_swiftEmptySetSingleton = {
  // HeapObject header;
  {
    &swift::CLASS_METADATA_SYM(s18_EmptySetSingleton), // isa pointer
  },
  
  // _SwiftSetBodyStorage body;
  {
    // Setting the scale to 0 makes for a bucketCount of 1 -- so that the
    // storage consists of a single unoccupied bucket. The capacity is set to
    // 0 so that any insertion will lead to real storage being allocated.
    0, // int count;
    0, // int capacity;                                    
    0, // int8 scale;
    0, // int8 reservedScale;
    0, // int16 extra;
    0, // int32 age;
    0, // int seed;
    (void *)1, // void *rawElements; (non-null garbage)
  },

  // bucket 0 is unoccupied; other buckets are out-of-bounds
  static_cast<__swift_uintptr_t>(~1) // int metadata;
};

static swift::_SwiftHashingParameters initializeHashingParameters() {
  // Setting the environment variable SWIFT_DETERMINISTIC_HASHING to "1"
  // disables randomized hash seeding. This is useful in cases we need to ensure
  // results are repeatable, e.g., in certain test environments.  (Note that
  // even if the seed override is enabled, hash values aren't guaranteed to
  // remain stable across even minor stdlib releases.)
  auto determinism = getenv("SWIFT_DETERMINISTIC_HASHING");
  if (determinism && 0 == strcmp(determinism, "1")) {
    return { 0, 0, true };
  }
  __swift_uint64_t seed0 = 0, seed1 = 0;
  swift::swift_stdlib_random(&seed0, sizeof(seed0));
  swift::swift_stdlib_random(&seed1, sizeof(seed1));
  return { seed0, seed1, false };
}

SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN
swift::_SwiftHashingParameters swift::_swift_stdlib_Hashing_parameters =
  initializeHashingParameters();
SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END


SWIFT_RUNTIME_STDLIB_API
void swift::_swift_instantiateInertHeapObject(void *address,
                                              const HeapMetadata *metadata) {
  ::new (address) HeapObject{metadata};
}

namespace llvm { namespace hashing { namespace detail {
  // An extern variable expected by LLVM's hashing templates. We don't link any
  // LLVM libs into the runtime, so define it as a weak symbol.
  //
  // Systems that compile this code into a dynamic library will do so with
  // hidden visibility, making this all internal to the dynamic library.
  // Systems that statically link the Swift runtime into applications (e.g. on
  // Linux) need this to handle the case when the app already uses LLVM.
  uint64_t LLVM_ATTRIBUTE_WEAK fixed_seed_override = 0;
} // namespace detail
} // namespace hashing
} // namespace llvm
