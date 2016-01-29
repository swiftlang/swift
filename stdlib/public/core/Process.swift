//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public enum Process {
  /// Initialize the swift argument with the list of command-line arguments
  /// with which the current process was invoked.
  public static func initArguments() {
    for i in 0..<Int(argc) {
      _arguments.append(
          String.fromCStringRepairingIllFormedUTF8(unsafeArgv[i]).0 ?? "")
    }
  }

  internal static var _argc: CInt = CInt()
  internal static var _unsafeArgv:
    UnsafeMutablePointer<UnsafeMutablePointer<Int8>>
    = nil

  internal static var _arguments : [String] = [String]() 

  /// Access to the swift arguments.
  public static var arguments : [String] {
    return _arguments;
  } 

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
@_transparent
public // COMPILER_INTRINSIC
func _didEnterMain(
  argc: Int32, argv: UnsafeMutablePointer<UnsafeMutablePointer<Int8>>
) {
  // Initialize the Process.argc and Process.unsafeArgv variables with the
  // values that were passed in to main.
  Process._argc = CInt(argc)
  Process._unsafeArgv = UnsafeMutablePointer(argv)

  // Initialize the swift arguments.
  Process.initArguments();
}

