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

import SwiftShims

/// Command-line arguments for the current process.
@_frozen // namespace
public enum CommandLine {
  /// The backing static variable for argument count may come either from the
  /// entry point or it may need to be computed e.g. if we're in the REPL.
  @usableFromInline
  internal static var _argc: Int32 = Int32()

  /// The backing static variable for arguments may come either from the
  /// entry point or it may need to be computed e.g. if we're in the REPL.
  ///
  /// Care must be taken to ensure that `_swift_stdlib_getUnsafeArgvArgc` is
  /// not invoked more times than is necessary (at most once).
  @usableFromInline
  internal static var _unsafeArgv:
    UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>
      =  _swift_stdlib_getUnsafeArgvArgc(&_argc)

  /// Access to the raw argc value from C.
  public static var argc: Int32 {
    _ = CommandLine.unsafeArgv // Force evaluation of argv.
    return _argc
  }

  /// Access to the raw argv value from C. Accessing the argument vector
  /// through this pointer is unsafe.
  public static var unsafeArgv:
    UnsafeMutablePointer<UnsafeMutablePointer<Int8>?> {
    return _unsafeArgv
  }

  /// Access to the swift arguments, also use lazy initialization of static
  /// properties to safely initialize the swift arguments.
  public static var arguments: [String]
    = (0..<Int(argc)).map { String(cString: _unsafeArgv[$0]!) }
}
