//===--- SwiftRemoteMirrorLegacyInteropTypes.h - Legacy lib interop types. ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------------===//
///
/// \file
/// This header provides an interface for using multiple versions of the Swift remote
/// mirror library to inspect processes built with different versions of Swift, or
/// processes where different libraries are built with different versions of Swift.
///
//===----------------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_MIRROR_LEGACY_INTEROP_TYPES_H
#define SWIFT_REMOTE_MIRROR_LEGACY_INTEROP_TYPES_H

#include "SwiftRemoteMirror.h"

/// The remote representation of a Swift metada pointer as returned by
/// interop wrappers.
typedef struct swift_metadata_interop {
  uintptr_t Metadata;
  int Library;
} swift_metadata_interop_t;

/// The remote representation of a Swift typeref as returned by interop
/// wrappers.
typedef struct swift_typeref_interop {
  swift_typeref_t Typeref;
  int Library;
} swift_typeref_interop_t;

/// The description of a Swift type as returned by interop wrappers.
/// (NOTE: This is just a typedef, as interop requires no changes to the
/// underlying type. The typedef allows client code to use consistent
/// names, and transition gracefully if any differences are needed in
/// this type in the future.)
typedef swift_typeinfo_t swift_typeinfo_interop_t;

/// The representation of a child of a Swift entity as returned by
/// interop wrappers.
typedef struct swift_childinfo_interop {
  /// The memory for Name is owned by the reflection context.
  const char *Name;
  unsigned Offset;
  swift_layout_kind_t Kind;
  swift_typeref_interop_t TR;
} swift_childinfo_interop_t;

/// An opaque reflection context object.
typedef struct SwiftReflectionInteropContext *SwiftReflectionInteropContextRef;

#endif // SWIFT_REMOTE_MIRROR_LEGACY_INTEROP_TYPES_H

