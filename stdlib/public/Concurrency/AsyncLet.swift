//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

// ==== Async Let -------------------------------------------------------------
// Only has internal / builtin functions as it is not really accessible directly

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_asyncLet_start")
public func _asyncLetStart<T>(
  asyncLet: Builtin.RawPointer,
  options: Builtin.RawPointer?,
  operation: @Sendable () async throws -> T
)

/// Wait if necessary and then project the result value of an async let
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_asyncLet_get")
public func _asyncLet_get(_ asyncLet: Builtin.RawPointer, _ resultBuffer: Builtin.RawPointer) async

/// Wait if necessary and then project the result value of an async let that throws
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_asyncLet_get_throwing")
public func _asyncLet_get_throwing(_ asyncLet: Builtin.RawPointer, _ resultBuffer: Builtin.RawPointer) async throws

/// Wait if necessary and then tear down the async let task
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_asyncLet_finish")
public func _asyncLet_finish(_ asyncLet: Builtin.RawPointer, _ resultBuffer: Builtin.RawPointer) async

@_silgen_name("swift_asyncLet_extractTask")
func _asyncLetExtractTask(
  of asyncLet: Builtin.RawPointer
) -> Builtin.NativeObject
