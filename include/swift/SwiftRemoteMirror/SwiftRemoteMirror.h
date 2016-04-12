//===--- SwiftRemoteMirror.h - Public remote reflection interf. -*- C++ -*-===//
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
/// This header declares functions in the libswiftReflection library,
/// which provides mechanisms for reflecting heap information in a
/// remote Swift process.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_SWIFT_REFLECTION_H
#define SWIFT_REFLECTION_SWIFT_REFLECTION_H

/// Major version changes when there are ABI or source incompatible changes.
#define SWIFT_REFLECTION_VERSION_MAJOR 3

/// Minor version changes when new APIs are added in ABI- and source-compatible
/// way.
#define SWIFT_REFLECTION_VERSION_MINOR 0

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
  SWIFT_ARRAY_OWNER
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

/// \returns An opaque reflection context.
SwiftReflectionContextRef
swift_reflection_createReflectionContext(
    PointerSizeFunction getPointerSize,
    SizeSizeFunction getSizeSize,
    ReadBytesFunction readBytes,
    GetStringLengthFunction getStringLength,
    GetSymbolAddressFunction getSymbolAddress);

/// Destroys an opaque reflection context.
void
swift_reflection_destroyReflectionContext(SwiftReflectionContextRef Context);

/// Add reflection sections for a loaded Swift image.
void
swift_reflection_addReflectionInfo(SwiftReflectionContextRef ContextRef,
                                   const char *ImageName,
                                   swift_reflection_section_t fieldmd,
                                   swift_reflection_section_t typeref,
                                   swift_reflection_section_t reflstr,
                                   swift_reflection_section_t assocty);


/// Clear any caching of field type information for all known heap instances.
void swift_reflection_clearCaches(SwiftReflectionContextRef Context);

/// Returns an opaque type reference for a metadata pointer, or
/// NULL if one can't be constructed.
swift_typeref_t
swift_reflection_typeRefForMetadata(SwiftReflectionContextRef ContextRef,
                                    uintptr_t metadata);

/// Returns a structure describing the overall layout of a typeref.
swift_typeinfo_t
swift_reflection_infoForTypeRef(SwiftReflectionContextRef ContextRef);

/// Returns the information about a stored property by index.
swift_fieldinfo_t
swift_reflection_infoForField(SwiftReflectionContextRef ContextRef,
                              swift_typeref_t OpaqueTypeRef,
                              unsigned Index);

/// Returns a fully instantiated typeref for a generic argument by index.
swift_typeref_t
swift_reflection_genericArgumentOfTypeRef(swift_typeref_t OpaqueTypeRef,
                                          unsigned Index);
#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_REFLECTION_SWIFT_REFLECTION_H

