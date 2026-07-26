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

@_exported import SwiftAndroid // Clang module
import SwiftOverlayShims

// FILE is opaque on Android (bionic) - the underlying `struct __sFILE` is
// not exposed in a way that Clang can import into Swift, so use OpaquePointer.
// Client code that needs to interoperate with <stdio.h> functions can bridge
// via `typealias File = OpaquePointer` under `#if os(Android)`.
nonisolated(unsafe) public var stdin: OpaquePointer {
  OpaquePointer(_swift_stdlib_stdin())
}
nonisolated(unsafe) public var stdout: OpaquePointer {
  OpaquePointer(_swift_stdlib_stdout())
}
nonisolated(unsafe) public var stderr: OpaquePointer {
  OpaquePointer(_swift_stdlib_stderr())
}
