//===--- Metadata.h - Swift Language ABI Metadata Support -----------------===//
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
// Swift ABI for generating and uniquing class metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_METADATA_H
#define SWIFT_ABI_METADATA_H

#include <cstddef>
#include <cstdint>
#include "FastEntryPoints.h"

#ifdef __cplusplus
extern "C" {
#endif

/// A heap-metadata fill operation is an instruction to copy a pointer's
/// worth of data from the arguments into a particular position in the
/// allocated metadata.
struct SwiftGenericHeapMetadataFillOp {
  uint32_t FromIndex;
  uint32_t ToIndex;
};

/// \brief The header in front of a generic metadata template.
///
/// This is optimized so that the code generation pattern
/// requires the minimal number of independent arguments.
/// For example, we want to be able to allocate a generic class
/// Dictionary<T,U> like so:
///   extern struct SwiftGenericHeapMetadata Dictionary_metadata_header;
///   void *arguments[] = { typeid(T), typeid(U) };
///   void *metadata = swift_fetchGenericMetadata(&Dictionary_metadata_header,
///                                               &arguments);
///   void *object = swift_allocObject(metadata);
///
/// Note that the metadata header is *not* const data; it includes 8
/// pointers worth of implementation-private data.
///
/// Both the metadata header and the arguments buffer are guaranteed
/// to be pointer-aligned.
struct SwiftGenericHeapMetadata {
  /// The number of generic arguments that we need to unique on,
  /// in words.  The first 'NumArguments * sizeof(void*)' bytes of
  /// the arguments buffer are the key.
  uint32_t NumArguments;

  /// The number of fill operations following this header.
  /// See the 
  uint32_t NumFillOps;

  /// The size of the template in bytes.
  size_t MetadataSize;

  /// Data that the runtime can use for its own purposes.  It is guaranteed
  /// to be zero-filled by the compiler.
  void *PrivateData[8];

  // Here there is a variably-sized field:
  // struct SwiftGenericHeapMetadataFillOp FillOps[NumArguments];

  // Here there is a variably-sized field:
  // char MetadataTemplate[MetadataSize];

#ifdef __cplusplus
  typedef SwiftGenericHeapMetadataFillOp FillOp;
  typedef const FillOp *fill_ops_const_iterator;
  fill_ops_const_iterator fill_ops_begin() const {
    return reinterpret_cast<const FillOp *>(this + 1);
  }
  fill_ops_const_iterator fill_ops_end() const {
    return fill_ops_begin() + NumFillOps;
  }

  /// Return the starting address of the metadata template data.
  const void *getMetadataTemplate() const {
    return fill_ops_end();
  }
#endif
};

/// \brief Fetch a uniqued metadata object for a class.
///
/// The basic algorithm for fetching a metadata object is:
///   func swift_fetchGenericMetadata(header, arguments) {
///     if (metadata = getExistingMetadata(&header.PrivateData,
///                                        arguments[0..header.NumArguments]))
///       return metadata
///     metadata = malloc(header.MetadataSize)
///     memcpy(metadata, header.MetadataTemplate, header.MetadataSize)
///     for (i in 0..header.NumFillInstructions)
///       metadata[header.FillInstructions[i].ToIndex]
///         = arguments[header.FillInstructions[i].FromIndex]
///     setExistingMetadata(&header.PrivateData,
///                         arguments[0..header.NumArguments],
///                         metadata)
///     return metadata
///   }
const struct SwiftHeapMetadata *
swift_getGenericMetadata(struct SwiftGenericHeapMetadata *pattern,
                         const void *arguments);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* SWIFT_ABI_METADATA_H */
