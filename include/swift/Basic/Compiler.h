//===--- Compiler.h - Compiler specific definitions -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_COMPILER_H
#define SWIFT_BASIC_COMPILER_H

#if defined(_MSC_VER) && !defined(__clang__)
#define SWIFT_COMPILER_IS_MSVC 1
#else
#define SWIFT_COMPILER_IS_MSVC 0
#endif

// Workaround non-clang compilers
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

#if SWIFT_COMPILER_IS_MSVC && _MSC_VER < 1910
// Work around MSVC bug: attempting to reference a deleted function
// https://connect.microsoft.com/VisualStudio/feedback/details/3116505
#define SWIFT_DELETE_OPERATOR_DELETED                                          \
  { llvm_unreachable("Delete operator should not be called."); }
#else
#define SWIFT_DELETE_OPERATOR_DELETED = delete;
#endif

// __builtin_assume() is an optimization hint.
#if __has_builtin(__builtin_assume)
#define SWIFT_ASSUME(x) __builtin_assume(x)
#else
#define SWIFT_ASSUME(x)
#endif

#if __has_attribute(constructor)
#define SWIFT_CONSTRUCTOR __attribute__((constructor))
#else
#define SWIFT_CONSTRUCTOR
#endif

#endif // SWIFT_BASIC_COMPILER_H
