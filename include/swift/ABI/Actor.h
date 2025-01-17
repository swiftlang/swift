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

#ifndef SWIFT_ABI_ACTOR_H
#define SWIFT_ABI_ACTOR_H

#include "swift/ABI/HeapObject.h"
#include "swift/ABI/MetadataValues.h"

// lldb knows about some of these internals. If you change things that lldb
// knows about (or might know about in the future, as a future lldb might be
// inspecting a process running an older Swift runtime), increment
// _swift_concurrency_debug_internal_layout_version and add a comment describing
// the new version.

namespace swift {

/// The default actor implementation.  This is the layout of both
/// the DefaultActor and NSDefaultActor classes.
class alignas(Alignment_DefaultActor) DefaultActor : public HeapObject {
public:
  // These constructors do not initialize the actor instance, and the
  // destructor does not destroy the actor instance; you must call
  // swift_defaultActor_{initialize,destroy} yourself.
  constexpr DefaultActor(const HeapMetadata *metadata)
    : HeapObject(metadata), PrivateData{} {}

  constexpr DefaultActor(const HeapMetadata *metadata,
                         InlineRefCounts::Immortal_t immortal)
    : HeapObject(metadata, immortal), PrivateData{} {}

  void *PrivateData[NumWords_DefaultActor];
};

/// The non-default distributed actor implementation.
class alignas(Alignment_NonDefaultDistributedActor) NonDefaultDistributedActor : public HeapObject {
public:
  // These constructors do not initialize the actor instance, and the
  // destructor does not destroy the actor instance; you must call
  // swift_nonDefaultDistributedActor_initialize yourself.
  constexpr NonDefaultDistributedActor(const HeapMetadata *metadata)
    : HeapObject(metadata), PrivateData{} {}

  constexpr NonDefaultDistributedActor(const HeapMetadata *metadata,
                                       InlineRefCounts::Immortal_t immortal)
    : HeapObject(metadata, immortal), PrivateData{} {}

  void *PrivateData[NumWords_NonDefaultDistributedActor];
};

} // end namespace swift

#endif
