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

#include "SwiftRemoteMirrorTypes.h"

/// Major version changes when there are ABI or source incompatible changes.
#define SWIFT_REFLECTION_VERSION_MAJOR 3

/// Minor version changes when new APIs are added in ABI- and source-compatible
/// way.
#define SWIFT_REFLECTION_VERSION_MINOR 0

#ifdef __cplusplus
extern "C" {
#endif

/// \returns An opaque reflection context.
SwiftReflectionContextRef
swift_reflection_createReflectionContext(
    void *ReaderContext,
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
                                   swift_reflection_section_t builtin,
                                   swift_reflection_section_t assocty,
                                   swift_reflection_section_t typeref,
                                   swift_reflection_section_t reflstr);


/// Returns an opaque type reference for a metadata pointer, or
/// NULL if one can't be constructed.
swift_typeref_t
swift_reflection_typeRefForMetadata(SwiftReflectionContextRef ContextRef,
                                    uintptr_t metadata);

/// Returns a structure describing the overall layout of a typeref.
swift_typeinfo_t
swift_reflection_infoForTypeRef(SwiftReflectionContextRef ContextRef,
                                swift_typeref_t OpaqueTypeRef);

/// Returns the information about a child by index.
swift_childinfo_t
swift_reflection_infoForField(SwiftReflectionContextRef ContextRef,
                              swift_typeref_t OpaqueTypeRef,
                              unsigned Index);

/// Returns a fully instantiated typeref for a generic argument by index.
unsigned
swift_reflection_genericArgumentCountOfTypeRef(swift_typeref_t OpaqueTypeRef);

/// Returns a fully instantiated typeref for a generic argument by index.
swift_typeref_t
swift_reflection_genericArgumentOfTypeRef(swift_typeref_t OpaqueTypeRef,
                                          unsigned Index);
#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_REFLECTION_SWIFT_REFLECTION_H

