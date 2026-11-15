/*===------------------ Swift Platform Abstraction Layer -----------*- C -*-===*
 *
 * This source file is part of the Swift.org open source project
 *
 * Copyright (c) 2026 Apple Inc. and the Swift project authors
 * Licensed under Apache License v2.0 with Runtime Library Exception
 *
 * See https: *swift.org/LICENSE.txt for license information
 * See https: *swift.org/CONTRIBUTORS.txt for the list of Swift project authors
 *
 *===----------------------------------------------------------------------=== *
 *
 * The Swift platform abstraction layer (PAL) describes the interface that Swift
 * uses to interface with a given platform. A "platform" in this case is broadly
 * construed, and can be anything from a couple of function implementations
 * within a standalone Embedded Swift program to a full-fledged SDK for an
 * OS. Either way, any of these functions can be implemented either in Swift
 * (using `@c @implementation`) or in C and linked into the final executable.
 *
 * The PAL is separated into different components, each of which declares a PAL
 * entrypoints in the form of `_swift_*` function declares. A platform is
 * required to implement the PAL entrypoints for the components that a Swift
 * program may wish to use. For example, the header `SwiftPlatform/Heap.h`
 * declares the functions that a platform needs to implement for Swift to use
 * the heap, whereas `SwiftPlatform/Clocks.h` declares the functions that a
 * platform needs to implement for the Swift `ContinuousClock` and
 * `SuspendingClock` types. The different components include:
 *
 * * Base PAL: required to support Swift anywhere.
 *   - Types.h: types used in the definition of the PAL entrypoints. There are
 *     no platform-provided PAL entrypoints declared here.
 *   - Errors.h: functions for reporting errors to the system.
 *   - ThreadLocalStorage.h: functions for managing thread-local storage needed
 *     by the Swift runtime.
 * * Clocks.h: functions for reporting time on the system.
 * * Heap.h: functions for allocating and deallocating heap memory.
 * * Random.h: functions for generating and seeding random numbers.
 * * StandardOutput.h: functions for printing to standard output.
 * * Mutex.h: functions for providing mutex lock/unlock, which are also used
 *   by the Concurrency PAL.
 * * Concurrency PAL: required to support the Swift Concurrency runtime.
 *   - ExecutorTypes.h: types used in the definition of concurrency PAL.
 *     There are no platform-provided PAL entrypoints declared here.
 *   - Concurrency.h: functions for enqueuing concurrency work.
 *
 *===----------------------------------------------------------------------===*/
#ifndef SWIFT_PLATFORM_H
#define SWIFT_PLATFORM_H

#include "Clocks.h"
#include "Concurrency.h"
#include "Errors.h"
#include "ExecutorTypes.h"
#include "Heap.h"
#include "Mutex.h"
#include "Random.h"
#include "StandardOutput.h"
#include "ThreadLocalStorage.h"
#include "Types.h"
#include "Version.h"

#endif
