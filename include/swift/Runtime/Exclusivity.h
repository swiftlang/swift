//===--- Exclusivity.h - Swift exclusivity-checking support -----*- C++ -*-===//
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
// Swift runtime support for dynamic checking of the Law of Exclusivity.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_EXCLUSIVITY_H
#define SWIFT_RUNTIME_EXCLUSIVITY_H

#include <cstdint>

#include "swift/Runtime/Config.h"

namespace swift {

enum class ExclusivityFlags : uintptr_t;
template <typename Runtime> struct TargetValueBuffer;
struct InProcess;
using ValueBuffer = TargetValueBuffer<InProcess>;

/// Begin dynamically tracking an access.
///
/// The buffer is opaque scratch space that the runtime may use for
/// the duration of the access.
///
/// The PC argument is an instruction pointer to associate with the start
/// of the access.  If it is null, the return address of the call to
/// swift_beginAccess will be used.
SWIFT_RUNTIME_EXPORT
void swift_beginAccess(void *pointer, ValueBuffer *buffer,
                       ExclusivityFlags flags, void *pc);


/// Stop dynamically tracking an access.
SWIFT_RUNTIME_EXPORT
void swift_endAccess(ValueBuffer *buffer);

/// A flag which, if set, causes access tracking to be suspended.
/// Accesses which begin while this flag is set will not be tracked,
/// will not cause exclusivity failures, and do not need to be ended.
///
/// This is here to support tools like debuggers.  Debuggers need to
/// be able to run code at breakpoints that does things like read
/// from a variable while there are ongoing formal accesses to it.
/// Such code may also crash, and we need to be able to recover
/// without leaving various objects in a permanent "accessed"
/// state.  (We also need to not leave references to scratch
/// buffers on the stack sitting around in the runtime.)
SWIFT_RUNTIME_EXPORT
bool _swift_disableExclusivityChecking;

#ifndef NDEBUG

/// Dump all accesses currently tracked by the runtime.
///
/// This is a debug routine that is intended to be used from the debugger and is
/// compiled out when asserts are disabled. The intention is that it allows one
/// to dump the access state to easily see if/when exclusivity violations will
/// happen. This eases debugging.
SWIFT_RUNTIME_EXPORT
void swift_dumpTrackedAccesses();

#endif

// When building the concurrency library for back deployment, we rename these
// symbols unformly so they don't conflict with the real concurrency library.
#ifdef SWIFT_CONCURRENCY_BACK_DEPLOYMENT
#  define swift_task_enterThreadLocalContext swift_task_enterThreadLocalContextBackDeploy
#  define swift_task_exitThreadLocalContext swift_task_exitThreadLocalContextBackDeploy
#endif

/// Called when a task inits, resumes and returns control to caller synchronous
/// code to update any exclusivity specific state associated with the task.
///
/// State is assumed to point to a buffer of memory with
/// swift_task_threadLocalContextSize bytes that was initialized with
/// swift_task_initThreadLocalContext.
///
/// We describe the algorithm in detail on SwiftTaskThreadLocalContext in
/// Exclusivity.cpp.
SWIFT_RUNTIME_EXPORT
void swift_task_enterThreadLocalContext(char *state);

/// Called when a task suspends and returns control to caller synchronous code
/// to update any exclusivity specific state associated with the task.
///
/// State is assumed to point to a buffer of memory with
/// swift_task_threadLocalContextSize bytes that was initialized with
/// swift_task_initThreadLocalContext.
///
/// We describe the algorithm in detail on SwiftTaskThreadLocalContext in
/// Exclusivity.cpp.
SWIFT_RUNTIME_EXPORT
void swift_task_exitThreadLocalContext(char *state);

} // end namespace swift

#endif
