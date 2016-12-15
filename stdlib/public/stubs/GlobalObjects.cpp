//===--- GlobalObjects.cpp - Statically-initialized objects ---------------===//
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
//  Objects that are allocated at global scope instead of on the heap,
//  and statically initialized to avoid synchronization costs, are
//  defined here.
//
//===----------------------------------------------------------------------===//

#include "../SwiftShims/GlobalObjects.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Debug.h"
#include <stdlib.h>

namespace swift {
// FIXME(ABI): does this declaration need SWIFT_RUNTIME_STDLIB_INTERFACE?
// _direct type metadata for Swift._EmptyArrayStorage
SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" ClassMetadata _TMCs18_EmptyArrayStorage;
}

swift::_SwiftEmptyArrayStorage swift::_swiftEmptyArrayStorage = {
  // HeapObject header;
  {
    &_TMCs18_EmptyArrayStorage, // isa pointer
  },
  
  // _SwiftArrayBodyStorage body;
  {
    0, // int count;                                    
    1  // unsigned int _capacityAndFlags; 1 means elementTypeIsBridgedVerbatim
  }
};

static __swift_uint64_t randomUInt64() {
#if defined(__APPLE__)
  return static_cast<__swift_uint64_t>(arc4random()) |
         (static_cast<__swift_uint64_t>(arc4random()) << 32);
#else
  auto devUrandom = fopen("/dev/urandom", "r");
  if (!devUrandom) {
    swift::fatalError(/* flags = */ 0, "Opening \"/dev/urandom\" failed");
  }
  uint64_t result;
  if (fread(&result, sizeof(result), 1, devUrandom) != 1) {
    swift::fatalError(/* flags = */ 0, "Reading from \"/dev/urandom\" failed");
  }
  if (fclose(devUrandom)) {
    swift::fatalError(/* flags = */ 0, "Closing \"/dev/urandom\" failed");
  }
  return result;
#endif
}

SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN
swift::_SwiftHashingSecretKey swift::_swift_stdlib_Hashing_secretKey = {
  randomUInt64(), randomUInt64()
};
SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END

__swift_uint64_t swift::_swift_stdlib_HashingDetail_fixedSeedOverride = 0;

namespace llvm { namespace hashing { namespace detail {
  // An extern variable expected by LLVM's hashing templates. We don't link any
  // LLVM libs into the runtime, so define this here.
  size_t fixed_seed_override = 0;
} } }

