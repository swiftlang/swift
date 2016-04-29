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

#include "MemoryReaderInterface.h"
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


/// Returns a boolean indicating if the isa mask was successfully
/// read, in which case it is stored in the isaMask out parameter.
int
swift_reflection_readIsaMask(SwiftReflectionContextRef ContextRef,
                             uintptr_t *outIsaMask);

/// Returns an opaque type reference for a metadata pointer, or
/// NULL if one can't be constructed.
swift_typeref_t
swift_reflection_typeRefForMetadata(SwiftReflectionContextRef ContextRef,
                                    uintptr_t Metadata);

/// Returns a structure describing the layout of a value of a typeref.
/// For classes, this returns the reference value itself.
swift_typeinfo_t
swift_reflection_infoForTypeRef(SwiftReflectionContextRef ContextRef,
                                swift_typeref_t OpaqueTypeRef);

/// Returns the information about a child field of a value by index.
swift_childinfo_t
swift_reflection_childOfTypeRef(SwiftReflectionContextRef ContextRef,
                                swift_typeref_t OpaqueTypeRef,
                                unsigned Index);

/// Returns a structure describing the layout of a class instance
/// from the isa pointer of a class.
swift_typeinfo_t
swift_reflection_infoForMetadata(SwiftReflectionContextRef ContextRef,
                                 uintptr_t Metadata);

/// Returns the information about a child field of a class instance
/// by index.
swift_childinfo_t
swift_reflection_childOfMetadata(SwiftReflectionContextRef ContextRef,
                                 uintptr_t Metadata,
                                 unsigned Index);

/// Returns the number of generic arguments of a typeref.
unsigned
swift_reflection_genericArgumentCountOfTypeRef(swift_typeref_t OpaqueTypeRef);

/// Returns a fully instantiated typeref for a generic argument by index.
swift_typeref_t
swift_reflection_genericArgumentOfTypeRef(swift_typeref_t OpaqueTypeRef,
                                          unsigned Index);

/// Projects the type inside of an existential container.
///
/// Takes the address of the start of an existential container and the typeref
/// for the existential, and sets two out parameters:
///
/// - InstanceTypeRef: A type reference for the type inside of the existential
///   container.
/// - StartOfInstanceData: The address to the start of the inner type's instance
///   data, which may or may not be inside the container directly.
///   If the type is a reference type, the pointer to the instance is the first
///   word in the container.
///
///   If the existential contains a value type that can fit in the first three
///   spare words of the container, *StartOfInstanceData == InstanceAddress.
///   If it's a value type that can't fit in three words, the data will be in
///   its own heap box, so *StartOfInstanceData will be the address of that box.
///
/// Returns true if InstanceTypeRef and StartOfInstanceData contain valid
/// valid values.
int swift_reflection_projectExistential(SwiftReflectionContextRef ContextRef,
                                        addr_t InstanceAddress,
                                        swift_typeref_t ExistentialTypeRef,
                                        swift_typeref_t *OutInstanceTypeRef,
                                        addr_t *OutStartOfInstanceData);

/// Dump a brief description of the typeref as a tree to stderr.
void swift_reflection_dumpTypeRef(swift_typeref_t OpaqueTypeRef);

/// Dump information about the layout of a class instance from its isa pointer.
void swift_reflection_dumpInfoForMetadata(SwiftReflectionContextRef ContextRef,
                                          uintptr_t Metadata);

/// Dump information about the layout for a typeref.
void swift_reflection_dumpInfoForTypeRef(SwiftReflectionContextRef ContextRef,
                                         swift_typeref_t OpaqueTypeRef);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_REFLECTION_SWIFT_REFLECTION_H

