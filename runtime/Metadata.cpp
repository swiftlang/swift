//===--- Metadata.cpp - Swift Language ABI Metdata Support ----------------===//
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
// Implementations of the metadata ABI functions.
//
//===----------------------------------------------------------------------===//

#include "Metadata.h"
#include <new>
#include <string.h>

namespace {
  class MetadataCacheEntry {
    const MetadataCacheEntry *Next;
    MetadataCacheEntry() = default;

  public:
    const MetadataCacheEntry *getNext() const { return Next; }

    void *getArgumentsBuffer() { return this + 1; }
    const void *getArgumentsBuffer() const { return this + 1; }

    SwiftHeapMetadata *getMetadataBuffer(SwiftGenericHeapMetadata *pattern) {
      return reinterpret_cast<SwiftHeapMetadata *>(
               (reinterpret_cast<void**>(getArgumentsBuffer())
                + pattern->NumArguments));
    }
    const SwiftHeapMetadata *
    getMetadataBuffer(SwiftGenericHeapMetadata *pattern) const {
      return const_cast<MetadataCacheEntry*>(this)->getMetadataBuffer(pattern);
    }

    /// Does this cache entry match the given set of arguments?
    bool matches(SwiftGenericHeapMetadata *pattern,
                 const void *arguments) const {
      const void *storedArguments = getArgumentsBuffer();

      // TODO: exploit alignment and size knowledge.
      // NumArguments is always non-zero but will typically be 1.
      return memcmp(storedArguments, arguments,
                    pattern->NumArguments * sizeof(void*));
    }
    
    /// Allocate and return a new metadata object for the given pattern.
    static const SwiftHeapMetadata *create(SwiftGenericHeapMetadata *pattern,
                                           const void *arguments);
  };

  /// The implementation of a metadata cache.  Note that all-zero must
  /// be a valid state for the cache.
  struct MetadataCache {
    /// The head of a linked list of metadata cache entries.
    const MetadataCacheEntry *Head;
  };
  MetadataCache &getCache(SwiftGenericHeapMetadata *metadata) {
    return *reinterpret_cast<MetadataCache*>(metadata->PrivateData);
  }
}

// Keep this assert even if you change the representation above.
static_assert(sizeof(MetadataCache) <=
              sizeof(SwiftGenericHeapMetadata::PrivateData),
              "metadata cache is larger than the allowed space");

const SwiftHeapMetadata *
MetadataCacheEntry::create(SwiftGenericHeapMetadata *pattern,
                           const void *arguments) {
  // Allocate the new entry.
  void *buffer = operator new(sizeof(MetadataCacheEntry) +
                              pattern->NumArguments * sizeof(void*) +
                              pattern->MetadataSize);
  MetadataCacheEntry *entry = new (buffer) MetadataCacheEntry();

  // Copy the arguments into the right place for the key.
  memcpy(entry->getArgumentsBuffer(), arguments,
         pattern->NumArguments * sizeof(void*));

  // Initialize the metadata by copying the template.
  SwiftHeapMetadata *metadata = entry->getMetadataBuffer(pattern);
  memcpy(metadata, pattern->getMetadataTemplate(), pattern->MetadataSize);

  // Fill in the missing spaces from the arguments.
  void * const *argumentsAsArray = reinterpret_cast<void * const *>(arguments);
  void **metadataAsArray = reinterpret_cast<void**>(metadata);
  for (auto i = pattern->fill_ops_begin(),
            e = pattern->fill_ops_end(); i != e; ++i) {
    metadataAsArray[i->ToIndex] = argumentsAsArray[i->FromIndex];
  }

  // The metadata is now valid.

  // Add the cache to the list.  This can in theory be made thread-safe,
  // but really this should use a non-linear lookup algorithm.
  entry->Next = getCache(pattern).Head;
  getCache(pattern).Head = entry;

  return metadata;
}

/// The primary entrypoint.
extern "C" const SwiftHeapMetadata *
swift_getGenericMetadata(SwiftGenericHeapMetadata *pattern,
                         const void *arguments) {
  // Check to see if an entry already exists for these arguments.
  for (auto entry = getCache(pattern).Head;
         entry != nullptr; entry = entry->getNext()) {
    if (entry->matches(pattern, arguments))
      return entry->getMetadataBuffer(pattern);
  }

  // Otherwise, create a new one.
  return MetadataCacheEntry::create(pattern, arguments);
}
