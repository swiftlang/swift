//===--- Visibility.h - Visibility macros for runtime exports ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  These macros are used to declare symbols that should be exported from the
//  Swift runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_VISIBILITY_H
#define SWIFT_STDLIB_SHIMS_VISIBILITY_H

#if !defined(__has_feature)
#define __has_feature(x) 0
#endif

#if !defined(__has_attribute)
#define __has_attribute(x) 0
#endif

#if __has_feature(nullability)
// Provide macros to temporarily suppress warning about the use of
// _Nullable and _Nonnull.
# define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS                        \
  _Pragma("clang diagnostic push")                                  \
  _Pragma("clang diagnostic ignored \"-Wnullability-extension\"")
# define SWIFT_END_NULLABILITY_ANNOTATIONS                          \
  _Pragma("clang diagnostic pop")

#else
// #define _Nullable and _Nonnull to nothing if we're not being built
// with a compiler that supports them.
# define _Nullable
# define _Nonnull
# define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS
# define SWIFT_END_NULLABILITY_ANNOTATIONS
#endif

#if __has_attribute(pure)
#define SWIFT_READONLY __attribute__((__pure__))
#else
#define SWIFT_READONLY
#endif

#if __has_attribute(const)
#define SWIFT_READNONE __attribute__((__const__))
#else
#define SWIFT_READNONE
#endif

#if __has_attribute(always_inline)
#define SWIFT_ALWAYS_INLINE __attribute__((always_inline))
#else
#define SWIFT_ALWAYS_INLINE
#endif

#if __has_attribute(unavailable)
#define SWIFT_ATTRIBUTE_UNAVAILABLE __attribute__((__unavailable__))
#else
#define SWIFT_ATTRIBUTE_UNAVAILABLE
#endif

// TODO: support using shims headers in overlays by parameterizing
// SWIFT_RUNTIME_EXPORT on the library it's exported from, then setting
// protected vs. default based on the current value of __SWIFT_CURRENT_DYLIB.

/// Attribute used to export symbols from the runtime.
#if __MACH__
# define SWIFT_EXPORT_ATTRIBUTE __attribute__((__visibility__("default")))
#elif __ELF__

// Use protected visibility for ELF, since we don't want Swift symbols to be
// interposable. The relative relocations we form to metadata aren't
// valid in ELF shared objects, and leaving them relocatable at load time
// defeats the purpose of the relative references.
//
// Protected visibility on a declaration is interpreted to mean that the
// symbol is defined in the current dynamic library, so if we're building
// something else, we need to fall back on using default visibility.
#ifdef __SWIFT_CURRENT_DYLIB
# define SWIFT_EXPORT_ATTRIBUTE __attribute__((__visibility__("protected")))
#else
# define SWIFT_EXPORT_ATTRIBUTE __attribute__((__visibility__("default")))
#endif

#else
# if defined(__CYGWIN__)
#  define SWIFT_RUNTIME_EXPORT
# else
#  if defined(swiftCore_EXPORTS)
#   define SWIFT_EXPORT_ATTRIBUTE __declspec(dllexport)
#  else
#   define SWIFT_EXPORT_ATTRIBUTE __declspec(dllimport)
#  endif
# endif
#endif

#if defined(__cplusplus)
#define SWIFT_RUNTIME_EXPORT extern "C" SWIFT_EXPORT_ATTRIBUTE
#else
#define SWIFT_RUNTIME_EXPORT SWIFT_EXPORT_ATTRIBUTE
#endif

/// Attribute for runtime-stdlib SPI interfaces.
///
/// Since the stdlib is currently fully fragile, runtime-stdlib SPI currently
/// needs to be exported from the core dylib. When the stdlib admits more
/// resilience we may be able to make this hidden.
#define SWIFT_RUNTIME_STDLIB_INTERFACE SWIFT_RUNTIME_EXPORT

#endif
