//===--- Target.h - Info about the current compilation target ---*- C++ -*-===//
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
//
// Info about the current compilation target.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_ABI_TARGET_H
#define SWIFT_STDLIB_SHIMS_ABI_TARGET_H

#if !defined(__has_builtin)
#define __has_builtin(x) 0
#endif

// Is the target platform a simulator? We can't use TargetConditionals
// when included from SwiftShims, so use the builtin.
#if __has_builtin(__is_target_environment)
# if __is_target_environment(simulator)
#  define SWIFT_TARGET_OS_SIMULATOR 1
# else
#  define SWIFT_TARGET_OS_SIMULATOR 0
# endif
#endif

// Is the target platform Darwin?
#if __has_builtin(__is_target_os)
# if __is_target_os(darwin)
#   define SWIFT_TARGET_OS_DARWIN 1
# else
#   define SWIFT_TARGET_OS_DARWIN 0
# endif
#else
# define SWIFT_TARGET_OS_DARWIN 0
#endif

#endif // SWIFT_STDLIB_SHIMS_ABI_TARGET_H
