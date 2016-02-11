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

/// Command-line arguments for the current process.
public enum Process {
  /// Return an array of string containing the list of command-line arguments
  /// with which the current process was invoked.
  internal static func _computeArguments() -> [String] {
    var result: [String] = []
    for i in 0..<Int(argc) {
      result.append(
       String.fromCStringRepairingIllFormedUTF8(unsafeArgv[i]).0 ?? "")
    }
    return result 
  }

  internal static var _argc: CInt = CInt()
  internal static var _unsafeArgv:
    UnsafeMutablePointer<UnsafeMutablePointer<Int8>>
    = nil

  internal enum _ProcessArgumentsState {
    case NotYetComputed
    case ComputedArguments([String])
  }

  internal static var _arguments: [String]? = nil 

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

  /// Access to the swift arguments, also use lazy initialization of static
  /// properties to safely initialize the swift arguments.
  ///
  /// NOTE: we can not use static lazy let initializer as they can be moved
  /// around by the optimizer which will break the data dependence on argc
  /// and argv.
  public static var arguments: [String] {
    if _arguments == nil { 
      _arguments = _computeArguments()
    }
    return _arguments!
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
}

