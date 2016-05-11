//===--- Visibility.h - Visibility macros for runtime exports ---*- C++ -*-===//
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
//
//  These macros are used to declare symbols that should be exported from the
//  Swift runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_VISIBILITY_H
#define SWIFT_STDLIB_SHIMS_VISIBILITY_H

#if defined(__has_feature) && __has_feature(nullability)
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

/// Attribute used to export symbols from the runtime.
#if __MACH__
# define SWIFT_RUNTIME_EXPORT __attribute__((__visibility__("default")))
#elif __ELF__
// Use protected visibility for ELF, since we don't want Swift symbols to be
// interposable. The relative relocations we form to metadata aren't
// valid in ELF shared objects, and leaving them relocatable at load time
// defeats the purpose of the relative references.
# define SWIFT_RUNTIME_EXPORT __attribute__((__visibility__("protected")))
#elif __CYGWIN__
# define SWIFT_RUNTIME_EXPORT 
#else
// __dllexport/__dllimport for Windows?
# error "Unimplemented object format"
#endif

/// Attribute for runtime-stdlib SPI interfaces.
///
/// Since the stdlib is currently fully fragile, runtime-stdlib SPI currently
/// needs to be exported from the core dylib. When the stdlib admits more
/// resilience we may be able to make this hidden.
#define SWIFT_RUNTIME_STDLIB_INTERFACE SWIFT_RUNTIME_EXPORT

#endif
