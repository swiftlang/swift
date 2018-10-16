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

#ifndef SWIFT_ABI_TARGET_H
#define SWIFT_ABI_TARGET_H

#if !defined(__has_builtin)
#define __has_builtin(x) 0
#endif

// Is the target platform a simulator? We can't use TargetConditionals
// when included from SwiftShims, so use the builtin.
#if __has_builtin(__is_target_environment)
# if __is_target_environment(simulator)
#  define SWIFT_TARGET_OS_SIMULATOR 1
# endif
#endif

#endif /* SWIFT_ABI_TARGET_H */
