//===--- Privilege.h - Process privilege checks -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Checks for whether the current process holds privileges or protections that
// are relevant to runtime behavior.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_PRIVILEGE_H
#define SWIFT_RUNTIME_PRIVILEGE_H

#include "swift/shims/Visibility.h"

namespace swift {
namespace runtime {

/// True if the process gained credentials at exec that its invoker does not
/// hold: `AT_SECURE` on Linux, `issetugid()` elsewhere. Targets with no
/// equivalent always return false.
SWIFT_RUNTIME_STDLIB_INTERNAL
bool _swift_isPrivilegedProcess();

/// True if `_swift_isPrivilegedProcess()`, or, on macOS, if the process is a
/// platform binary or uses the hardened runtime.
///
/// Gates environment variables that disable a check that's important for
/// memory safety.
SWIFT_RUNTIME_STDLIB_SPI
bool _swift_isRestrictedProcess();

/// True if `_swift_isRestrictedProcess()`, or, on macOS, if the process
/// disallows task_for_pid.
///
/// Gates the backtracer, which is only active for processes with the
/// get-task-allow entitlement.
SWIFT_RUNTIME_STDLIB_INTERNAL
bool _swift_isRestrictedProcessForExec();

} // end namespace runtime
} // end namespace swift

#endif // SWIFT_RUNTIME_PRIVILEGE_H
