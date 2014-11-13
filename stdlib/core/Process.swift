//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public enum Process {
  /// The list of command-line arguments with which the current
  /// process was invoked.
  public static let arguments: [String] = {
    // Use lazy initialization of static properties to safely initialize the
    // public 'arguments' property on first use.
    map(0..<Int(argc)) { i in
      String.fromCStringRepairingIllFormedUTF8(unsafeArgv[i]).0 ?? ""
    }
  }()

  internal static var _argc: CInt = CInt()
  internal static var _unsafeArgv:
    UnsafeMutablePointer<UnsafeMutablePointer<Int8>>
    = nil

  /// Access to the raw argc value from C.
  public static var argc: CInt {
    return _argc
  }

  /// Access to the raw argv value from C. Accessing the argument vector
  /// through this pointer is unsafe.
  public static var unsafeArgv:
    UnsafeMutablePointer<UnsafeMutablePointer<Int8>> {
    return _unsafeArgv
  }
}

/// Intrinsic entry point invoked on entry to a standalone program's "main".
@transparent
public // COMPILER_INTRINSIC
func _didEnterMain(
  argc: Int32, argv: UnsafeMutablePointer<UnsafeMutablePointer<Int8>>
) {
  // Initialize the Process.argc and Process.unsafeArgv variables with the
  // values that were passed in to main.
  Process._argc = CInt(argc)
  Process._unsafeArgv = UnsafeMutablePointer(argv)
}

/// Access to the raw argc value from C.
@availability(*, unavailable, renamed="Process.argc")
public var C_ARGC: CInt = CInt()

/// Access to the raw argv value from C. Accessing the argument vector
/// through this pointer is unsafe.
@availability(*, unavailable, renamed="Process.unsafeArgv")
public var C_ARGV: UnsafeMutablePointer<UnsafeMutablePointer<Int8>> = nil
