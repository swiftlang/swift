//===--- GlobalObjects.cpp - Statically-initialized objects ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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

namespace swift {

// _direct type metadata for Swift._EmptyArrayStorage
extern "C" FullMetadata<ClassMetadata> _TMdCSs18_EmptyArrayStorage;

extern "C" _SwiftEmptyArrayStorage _swiftEmptyArrayStorage = {
  // HeapObject header;
  {
    &_TMdCSs18_EmptyArrayStorage, // is-a pointer
  },
  
  // _SwiftArrayBodyStorage body;
  {
    0, // int count;                                    
    1  // unsigned int _capacityAndFlags; 1 means elementTypeIsBridgedVerbatim
  }
};

extern "C"
uint64_t _swift_stdlib_HashingDetail_fixedSeedOverride = 0;

}
