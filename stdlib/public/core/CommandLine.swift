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

#if SWIFT_STDLIB_HAS_COMMANDLINE

@_silgen_name("_swift_stdlib_getUnsafeArgvArgc")
internal func _swift_stdlib_getUnsafeArgvArgc(_: UnsafeMutablePointer<Int32>)
  -> UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>

#if os(Windows)
@_silgen_name("_swift_stdlib_withExecutablePath")
private func _withExecutablePath(_ body: (UnsafePointer<CWideChar>) -> Void)
#else
@_silgen_name("_swift_stdlib_withExecutablePath")
private func _withExecutablePath(_ body: (UnsafePointer<CChar>) -> Void)
#endif

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS) || os(visionOS)
@_extern(c, "_NSGetExecutablePath")
@usableFromInline
func _NSGetExecutablePath(
  _ buf: UnsafeMutablePointer<CChar>?,
  _ bufsize: UnsafeMutablePointer<UInt32>
) -> CInt
#endif

/// Command-line arguments for the current process.
@frozen // namespace
public enum CommandLine: ~BitwiseCopyable {}

extension CommandLine {
  /// The backing static variable for argument count may come either from the
  /// entry point or it may need to be computed e.g. if we're in the REPL.
  @usableFromInline
  internal static var _argc: Int32 = 0

  /// The backing static variable for arguments may come either from the
  /// entry point or it may need to be computed e.g. if we're in the REPL.
  ///
  /// Care must be taken to ensure that `_swift_stdlib_getUnsafeArgvArgc` is
  /// not invoked more times than is necessary (at most once).
  @usableFromInline
  internal static var _unsafeArgv:
    UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>
      = unsafe _swift_stdlib_getUnsafeArgvArgc(&_argc)

  /// Access to the raw argc value from C.
  public static var argc: Int32 {
    // We intentionally ignore the argc value given to us from
    // '_swift_stdlib_getUnsafeArgvArgc' because argv and argc are mutable, so
    // someone can mutate the contents of argv and never update the argc value.
    // This results in an out of sync argc which can lead to crashes on first
    // access to 'CommandLine.arguments' due to attempting to read '0 ..< argc'
    // strings.
    //
    // Note: It's still entirely possible that someone may update argv after
    // this iteration and before we actually read argv, but we have no control
    // over synchronizing access to argc and argv.
    var argc: Int32 = 0

    while let _ = unsafe _unsafeArgv[Int(argc)] {
      argc += 1
    }

    return argc
  }

  /// Access to the raw argv value from C.
  ///
  /// The value of this property is a `nil`-terminated C array. Including the
  /// trailing `nil`, there are ``argc`` `+ 1` elements in the array.
  ///
  /// - Note: Accessing the argument vector through this pointer is unsafe.
  ///   Where possible, use ``arguments`` instead.
  public static var unsafeArgv:
    UnsafeMutablePointer<UnsafeMutablePointer<Int8>?> {
    return unsafe _unsafeArgv
  }

  // This is extremely unsafe and allows for concurrent writes with no
  // synchronization to the underlying data. In a future version of Swift you
  // will not be able to write to 'CommandLine.arguments'.
  static nonisolated(unsafe) var _arguments: [String] = (0 ..< Int(argc)).map {
    unsafe String(cString: _unsafeArgv[$0]!)
  }

  /// An array that provides access to this program's command line arguments.
  ///
  /// Use `CommandLine.arguments` to access the command line arguments used
  /// when executing the current program. The name of the executed program is
  /// the first argument.
  ///
  /// The following example shows a command line executable that squares the
  /// integer given as an argument.
  ///
  ///     if CommandLine.arguments.count == 2,
  ///        let number = Int(CommandLine.arguments[1]) {
  ///         print("\(number) x \(number) is \(number * number)")
  ///     } else {
  ///         print(
  ///           """
  ///           Error: Please provide a number to square.
  ///           Usage: command <number>
  ///           """
  ///         )
  ///     }
  ///
  /// Running the program results in the following output:
  ///
  ///     $ command 5
  ///     5 x 5 is 25
  ///     $ command ZZZ
  ///     Error: Please provide a number to square.
  ///     Usage: command <number>
  public static var arguments: [String] {
    get {
      _arguments
    }

    @available(*, deprecated, message: "Do not modify CommandLine.arguments. It will become read-only in a future version of Swift.")
    @available(swift, obsoleted: 6.0)
    set {
      _arguments = newValue
    }
  }

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS) || os(visionOS)
  /// The path to the current executable.
  ///
  /// The value of this property may not be canonical. If you need the canonical
  /// path to the current executable, you can pass the value of this property to
  /// [`realpath()`](https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/realpath.3.html)
  /// or use [`URL`](https://developer.apple.com/documentation/foundation/url)
  /// to standardize the path.
  ///
  /// If the path to the current executable could not be determined, the value
  /// of this property is `nil`.
  ///
  /// - Important: On some systems, it is possible to move an executable file on
  ///   disk while it is running. If the current executable file is moved, the
  ///   value of this property is not updated to its new path.
  @_unavailableInEmbedded
  @_alwaysEmitIntoClient
  public static var executablePath: String? { // NOTE: can't be AEIC and stored!
    // _NSGetExecutablePath() returns non-zero if the provided buffer is too
    // small and updates its *bufsize argument to the required value. Call it
    // once to get the buffer size before allocating.
    var byteCount = UInt32(0)
    _ = unsafe _NSGetExecutablePath(nil, &byteCount)
    return unsafe withUnsafeTemporaryAllocation(
      of: CChar.self,
      capacity: Int(byteCount)
    ) { buffer in
      if (unsafe 0 == _NSGetExecutablePath(buffer.baseAddress!, &byteCount)) {
        return unsafe String(validatingCString: buffer.baseAddress!)
      }
      return nil
    }
  }
#else
  /// The path to the current executable.
  ///
  /// The value of this property may not be canonical. If you need the canonical
  /// path to the current executable, you can pass the value of this property to
  /// [`realpath()`](https://www.kernel.org/doc/man-pages/online/pages/man3/realpath.3.html)
  /// ([`_wfullpath()`](https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/fullpath-wfullpath?view=msvc-170)
  /// on Windows) or use [`URL`](https://developer.apple.com/documentation/foundation/url)
  /// to standardize the path.
  ///
  /// If the path to the current executable could not be determined, the value
  /// of this property is `nil`.
  ///
  /// - Important: On some systems, it is possible to move an executable file on
  ///   disk while it is running. If the current executable file is moved, the
  ///   value of this property is not updated to its new path.
  @_unavailableInEmbedded
#if os(WASI)
  @available(*, unavailable, message: "Unavailable on WASI")
#endif
  public static let executablePath: String? = {
    var result: String?

    unsafe _withExecutablePath { path in
      if unsafe path.pointee == 0 {
        return
      }
#if os(Windows)
      result = unsafe String.decodeCString(path, as: UTF16.self)?.result
#else
      result = unsafe String(validatingCString: path)
#endif
    }

    return result
  }()
#endif
}
#endif // SWIFT_STDLIB_HAS_COMMANDLINE
