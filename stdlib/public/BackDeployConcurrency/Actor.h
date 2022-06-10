//===--- Actor.h - ABI structures for actors --------------------*- C++ -*-===//
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
// Swift ABI describing actors.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_ACTOR_BACKDEPLOYED_H
#define SWIFT_ABI_ACTOR_BACKDEPLOYED_H

#include "swift/ABI/HeapObject.h"
#include "swift/ABI/MetadataValues.h"
#include "ConditionVariable.h"

namespace swift {

/// The default actor implementation.  This is the layout of both
/// the DefaultActor and NSDefaultActor classes.
class alignas(Alignment_DefaultActor) DefaultActor : public HeapObject {
public:
  // These constructors do not initialize the actor instance, and the
  // destructor does not destroy the actor instance; you must call
  // swift_defaultActor_{initialize,destroy} yourself.
  constexpr DefaultActor(const HeapMetadata *metadata)
    : HeapObject(metadata), PrivateData{} {
    initHeapObject();
  }

  constexpr DefaultActor(const HeapMetadata *metadata,
                         InlineRefCounts::Immortal_t immortal)
    : HeapObject(metadata, immortal), PrivateData{} {
    // By a lucky coincidence, the modern bit pattern for immortal objects also
    // sets the immortal bit for older runtimes, so we don't need an immortal
    // equivalent to initHeapObject().
  }

  void initHeapObject() {
    _swift_instantiateInertHeapObject(this, metadata);
  }

  void *PrivateData[NumWords_DefaultActor];
};

} // end namespace swift

#endif
