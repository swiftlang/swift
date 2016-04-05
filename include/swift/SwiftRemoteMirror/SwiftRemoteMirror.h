//===--- SwiftReflection.h - Public remote reflection interface -*- C++ -*-===//
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

/// \brief Represents the __swift{n}_reflect section of an image.
///
/// If this section is virtually mapped, the following corresponding sections
/// should also be mapped into the current address space:
///
/// __swift{n}_typeref
/// --swift{n}_reflstr
///
/// where {n} is SWIFT_REFLECTION_VERSION_MAJOR.
typedef struct ReflectionSection {
  const char *ImageName;
  void *Begin;
  void *End;
} ReflectionSection;

/// \brief An opaque pointer to a context which maintains state and
/// caching of reflection structure for heap instances.
typedef struct SwiftReflectionContext *SwiftReflectionContextRef;

/// \returns An opaque reflection context.
SwiftReflectionContextRef
swift_reflection_createReflectionContext(PointerSizeFunction getPointerSize,
                                         SizeSizeFunction getSizeSize,
                                         ReadBytesFunction readBytes,
                                         GetStringLengthFunction getStringLength,
                                         GetSymbolAddressFunction getSymbolAddress);

/// Destroys an opaque reflection context.
void swift_reflection_destroyReflectionContext(SwiftReflectionContextRef Context);

/// Clear any caching of field type information for all known heap instances.
void swift_reflection_clearCaches(SwiftReflectionContextRef Context);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_REFLECTION_SWIFT_REFLECTION_H

