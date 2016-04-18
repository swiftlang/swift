//===--- SwiftRemoteMirrorTypes.h - Remote reflection types -----*- C++ -*-===//
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
///
/// \file
/// This header declares types in the libswiftReflection library.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_MIRROR_TYPES_H
#define SWIFT_REMOTE_MIRROR_TYPES_H

#ifdef __cplusplus
extern "C" {
#endif

typedef uintptr_t swift_typeref_t;

/// \brief Represents the __swift{n}_reflect section of an image.
///
/// If this section is virtually mapped, the following corresponding sections
/// should also be mapped into the current address space:
///
/// __swift{n}_typeref
/// --swift{n}_reflstr
///
/// where {n} is SWIFT_REFLECTION_VERSION_MAJOR.
typedef struct swift_reflection_section_t {
  void *Begin;
  void *End;
} swift_reflection_section_t;

/// The kind of a Swift type.
typedef enum swift_layout_kind_t {
  SWIFT_STRONG_POINTER,
  SWIFT_WEAK_POINTER,
  SWIFT_UNOWNED_POINTER,
  SWIFT_UNMANAGED_POINTER,
  SWIFT_STRUCT,
  SWIFT_TUPLE,
  SWIFT_THICK_FUNCTION,
  SWIFT_THIN_FUNCTION,
  OBJC_BLOCK,
  SWIFT_CLOSURE_CONTEXT,
  SWIFT_BOX,
  SWIFT_UNKNOWN
} swift_layout_kind_t;

/// A basic description of a Swift type's.
typedef struct swift_typeinfo_t {
  swift_layout_kind_t LayoutKind;

  const char *MangledTypeName;

  size_t Size;
  size_t InstanceSize;
  size_t Alignment;
} swift_typeinfo_t;

typedef struct swift_childinfo_t {
  swift_typeref_t TypeRef;
  const char *Name;
  size_t Offset;
} swift_childinfo_t;

/// \brief An opaque pointer to a context which maintains state and
/// caching of reflection structure for heap instances.
typedef struct SwiftReflectionContext *SwiftReflectionContextRef;

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_REMOTE_MIRROR_TYPES_H
