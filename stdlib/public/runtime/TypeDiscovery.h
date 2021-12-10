//===--- TypeDiscovery.h - Dynamic type lookup at runtime--------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Functions to look up types in the Swift type system at runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_TYPE_DISCOVERY_H
#define SWIFT_RUNTIME_TYPE_DISCOVERY_H

#include "swift/Runtime/Config.h"
#include "swift/Runtime/Error.h"
#include "swift/Runtime/Metadata.h"

namespace swift {
  /// The function type used by \c swift_enumerateConformingTypesFromImage().
  ///
  /// \param name The name of the type being enumerated.
  /// \param tcd A pointer to the type context descriptor of the type being
  ///   enumerated.
  /// \param getType A function which, when passed \a tcd, will instantiate its
  ///   corresponding \c Metadata value.
  /// \param stop Whether the caller should continue enumerating types after
  ///   this function returns.
  /// \param context The Swift context pointer for this function. Ignored.
  /// \param error An error pointer passed to \a body. If \a body throws an
  /// 	error, enumeration is stopped and the error is rethrown.
  typedef void (* TypeEnumerationFunction)(
    const char *name,
    const TypeContextDescriptor *tcd,
    const Metadata *(* getType)(const TypeContextDescriptor *tcd),
    bool *stop,
    SWIFT_CONTEXT void *context,
    SWIFT_ERROR_RESULT SwiftError **error) SWIFT_CC(swift);

  /// Enumerate all types in a given image.
  ///
  /// \param imageAddress A platform-specific pointer to the image of interest.
  ///   The image must have been loaded into the current process. Do not pass
  ///   a handle acquired from \c dlopen() or platform equivalents. Pass
  ///   \c nullptr to search all loaded images.
  /// \param body A closure to invoke once per matching type.
  /// \param bodyContext The Swift context pointer for \a body.
  /// \param context The Swift context pointer for this function. Ignored.
  /// \param error An error pointer passed to \a body. If \a body throws an
  /// 	error, enumeration is stopped and the error is rethrown.
  ///
  /// This function walks all known types in the given image and passes them to
  /// \c body for evaluation.
  ///
  /// Generic types are not enumerated.
  ///
  /// \bug Objective-C class lookups are not supported yet.
  SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
  void swift_enumerateAllTypesFromImage(
    const void *imageAddress,
    TypeEnumerationFunction body,
    void *bodyContext,
    SWIFT_CONTEXT void *context,
    SWIFT_ERROR_RESULT SwiftError **error);
} // end namespace swift

#endif /* SWIFT_RUNTIME_TYPE_DISCOVERY_H */
