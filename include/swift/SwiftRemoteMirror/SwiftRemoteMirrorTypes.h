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
typedef enum swift_typeref_kind_t {
  SWIFT_CLASS,
  SWIFT_STRUCT,
  SWIFT_TUPLE,
  SWIFT_CLOSURE_CONTEXT,
  SWIFT_DICTIONARY_OWNER,
  SWIFT_SET_OWNER,
  SWIFT_ARRAY_OWNER,
  SWIFT_UNKNOWN
} swift_layout_kind_t;

/// A basic description of a Swift type's.
typedef struct swift_typeinfo_t {
  swift_layout_kind_t LayoutKind;

  /// The demangled type name.
  const char *TypeName;

  size_t Size;
  size_t Stride;
} swift_typeinfo_t;

typedef enum swift_reference_kind_t {
  STRONG,
  WEAK,
  UNOWNED,
  UNMANAGED,
  NOTAPOINTER
} swift_reference_kind_t;

typedef struct swift_fieldinfo_t {
  swift_reference_kind_t ReferenceKind;
  swift_typeref_t TypeRef;
  const char *FieldName;
  size_t Offset;
  size_t Size;
} swift_fieldinfo_t;

/// \brief An opaque pointer to a context which maintains state and
/// caching of reflection structure for heap instances.
typedef struct SwiftReflectionContext *SwiftReflectionContextRef;


#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_REMOTE_MIRROR_TYPES_H
