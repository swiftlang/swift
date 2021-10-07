//===--- BridgedSwiftObject.h - C header which defines SwiftObject --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This is a C header, which defines the SwiftObject header. For the C++ version
// see SwiftObjectHeader.h.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_BRIDGEDSWIFTOBJECT_H
#define SWIFT_SIL_BRIDGEDSWIFTOBJECT_H

#include <stdint.h>

// TODO: These macro definitions are duplicated in Visibility.h. Move
// them to a single file if we find a location that both Visibility.h and
// BridgedSwiftObject.h can import.
#if __has_feature(nullability)
// Provide macros to temporarily suppress warning about the use of
// _Nullable and _Nonnull.
# define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS                                   \
  _Pragma("clang diagnostic push")                                             \
  _Pragma("clang diagnostic ignored \"-Wnullability-extension\"")
# define SWIFT_END_NULLABILITY_ANNOTATIONS                                     \
  _Pragma("clang diagnostic pop")

#else
// #define _Nullable and _Nonnull to nothing if we're not being built
// with a compiler that supports them.
# define _Nullable
# define _Nonnull
# define _Null_unspecified
# define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS
# define SWIFT_END_NULLABILITY_ANNOTATIONS
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

typedef const void * _Nonnull SwiftMetatype;

/// The header of a Swift object.
///
/// This must be in sync with HeapObject, which is defined in the runtime lib.
/// It must be layout compatible with the Swift object header.
struct BridgedSwiftObject {
  SwiftMetatype metatype;
  int64_t refCounts;
};

typedef struct BridgedSwiftObject * _Nonnull SwiftObject;
typedef struct BridgedSwiftObject * _Nullable OptionalSwiftObject;

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
