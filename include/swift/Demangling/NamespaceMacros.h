//===--- NamespaceMacros.h - Macros for inline namespaces -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Macros that conditionally define an inline namespace so that symbols used in
// multiple places (such as in the compiler and in the runtime library) can be
// given distinct mangled names in different contexts without affecting client
// usage in source.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_NAMESPACE_MACROS_H
#define SWIFT_DEMANGLING_NAMESPACE_MACROS_H

#if defined(__cplusplus)

#if defined(SWIFT_INLINE_NAMESPACE)
#define SWIFT_BEGIN_INLINE_NAMESPACE inline namespace SWIFT_INLINE_NAMESPACE {
#define SWIFT_END_INLINE_NAMESPACE }
#else
#define SWIFT_BEGIN_INLINE_NAMESPACE
#define SWIFT_END_INLINE_NAMESPACE
#endif

#endif

#endif // SWIFT_DEMANGLING_NAMESPACE_MACROS_H
