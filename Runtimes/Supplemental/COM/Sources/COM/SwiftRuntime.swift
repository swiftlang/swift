//===----------------------------------------------------------------------===//
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

// MARK: - Runtime Imports

/// The normal runtime retain.  Returns the object pointer (the runtime marks
/// the argument `returned`); callers needing only the side effect ignore it.
/// Used for COM `AddRef`-on-return where the resulting count isn't needed —
/// e.g. `QueryInterface` retaining the interface pointer it hands back.
///
/// Referencing the reserved `swift_retain` symbol directly is intentional and
/// the accompanying diagnostic is expected: this module is part of the Swift
/// runtime.  `AddRef`/`Release`, which must return the count, use the
/// `*ReturningCount` entries below.
@_silgen_name("_swift_retain")
@usableFromInline
internal func swift_retain(_: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer

/// Retains the object and returns the new strong reference count.
///
/// This is the COM "CountingRR" entry point — distinct from the runtime's
/// `swift_retain`, which the compiler emits and whose return value the ABI
/// cannot widen on Windows x64. The COM `AddRef`/`Release` vtable thunks need
/// the post-operation count, so they use these dedicated entries instead.
@_silgen_name("_swift_retainReturningCount")
@usableFromInline
internal func swift_retainReturningCount(_: UnsafeMutableRawPointer) -> Int

/// Releases the object and returns the new strong reference count
/// (`0` indicates the object was deallocated).
@_silgen_name("_swift_releaseReturningCount")
@usableFromInline
internal func swift_releaseReturningCount(_: UnsafeMutableRawPointer) -> Int
