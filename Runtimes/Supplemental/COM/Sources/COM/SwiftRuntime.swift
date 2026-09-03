//===----------------------------------------------------------------------===//
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

// MARK: - Runtime Imports

/// Retains the object and returns the new strong reference count.
///
/// This is the COM "CountingRR" entry point — distinct from the runtime's
/// `swift_retain`, which the compiler emits and whose return value the ABI
/// cannot widen on Windows x64. The COM `AddRef`/`Release` vtable thunks need
/// the post-operation count, so they use these dedicated entries instead.
@usableFromInline
@_extern(c, "swift_retainReturningCount")
internal func swift_retainReturningCount(_: UnsafeMutableRawPointer) -> Int

/// Releases the object and returns the new strong reference count
/// (`0` indicates the object was deallocated).
@usableFromInline
@_extern(c, "swift_releaseReturningCount")
internal func swift_releaseReturningCount(_: UnsafeMutableRawPointer) -> Int
