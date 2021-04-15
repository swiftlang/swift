//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

// TODO: Custom Executors proposal will define these types
//       https://forums.swift.org/t/support-custom-executors-in-swift-concurrency/44425/38

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@frozen
public struct UnownedExecutorRef: Equatable {
  let identity: OpaquePointer
  let implementation: Int
}


@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_transparent
public // COMPILER_INTRINSIC
func _checkExpectedExecutor(Builtin
  _filenameStart: Builtin.RawPointer,
                            _filenameLength: Builtin.Word,
                            _filenameIsASCII: Builtin.Int1,
                            _line: Builtin.Word,
                            _executor: Builtin.Executor) {
  if _taskIsCurrentExecutor(_executor) {
    return
  }

  _reportUnexpectedExecutor(
    _filenameStart, _filenameLength, _filenameIsASCII, _line, _executor)
}
