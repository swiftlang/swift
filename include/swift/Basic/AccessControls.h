//===--- AccessControls.h ---------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines macros that help control access to APIs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ACCESSCONTROLS_H
#define SWIFT_ACCESSCONTROLS_H

/// Deprecation warnings
#if defined(__clang__) || defined(__GNUC__)
  #if !defined(SWIFT_DEPRECATED)
    #define SWIFT_DEPRECATED __attribute__((deprecated))
  #endif
  #if !defined(SWIFT_DEPRECATED_MSG)
    #define SWIFT_DEPRECATED_MSG(...) __attribute__((deprecated(__VA_ARGS__)))
  #endif
#else
  #if !defined(SWIFT_DEPRECATED)
    #define SWIFT_DEPRECATED
  #endif
  #if !defined(SWIFT_DEPRECATED_MSG)
    #define SWIFT_DEPRECATED_MSG(...)
  #endif
#endif


/// Unavailable errors
#if defined(__clang__) || defined(__GNUC__)
  #if !defined(SWIFT_UNAVAILABLE)
    #define SWIFT_UNAVAILABLE __attribute__((unavailable))
  #endif
  #if !defined(SWIFT_UNAVAILABLE_MSG)
    #define SWIFT_UNAVAILABLE_MSG(msg) __attribute__((unavailable(msg)))
  #endif
#else
  #if !defined(SWIFT_UNAVAILABLE)
    #define SWIFT_UNAVAILABLE
  #endif
  #if !defined(SWIFT_UNAVAILABLE_MSG)
    #define SWIFT_UNAVAILABLE_MSG(msg)
  #endif
#endif


// Access controls that are only active when included in SILGen sources.
#if defined(SWIFT_INCLUDED_IN_SILGEN_SOURCES)

// Override any prior definitions with these.
#define SWIFT_DEPRECATED_IN_SILGEN SWIFT_DEPRECATED
#define SWIFT_DEPRECATED_IN_SILGEN_MSG(...) SWIFT_DEPRECATED_MSG(__VA_ARGS__)
#define SWIFT_UNAVAILABLE_IN_SILGEN SWIFT_UNAVAILABLE
#define SWIFT_UNAVAILABLE_IN_SILGEN_MSG(MSG) SWIFT_UNAVAILABLE_MSG(MSG)

#else

#if !defined(SWIFT_DEPRECATED_IN_SILGEN)
  #define SWIFT_DEPRECATED_IN_SILGEN
#endif
#if !defined(SWIFT_DEPRECATED_IN_SILGEN_MSG)
  #define SWIFT_DEPRECATED_IN_SILGEN_MSG(...)
#endif
#if !defined(SWIFT_UNAVAILABLE_IN_SILGEN)
  #define SWIFT_UNAVAILABLE_IN_SILGEN
#endif
#if !defined(SWIFT_UNAVAILABLE_IN_SILGEN_MSG)
  #define SWIFT_UNAVAILABLE_IN_SILGEN_MSG(MSG)
#endif
#endif // SWIFT_INCLUDED_IN_SILGEN_SOURCES

#endif // SWIFT_ACCESSCONTROLS_H
