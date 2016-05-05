//===--- Print.swift ------------------------------------------------------===//
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

/// Writes the textual representations of `items`, separated by
/// `separator` and terminated by `terminator`, into the standard
/// output.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(item)`.
///
/// - Note: To print without a trailing newline, pass `terminator: ""`
///
/// - SeeAlso: `debugPrint`, `Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
@inline(never)
@_semantics("stdlib_binary_only")
public func print(
  _ items: Any...,
  separator: String = " ",
  terminator: String = "\n"
) {
#if os(Windows)
  // FIXME: This fix is for 'crash at hook(output.left)' in cygwin.
  //        Proper fix is needed. see: https://bugs.swift.org/browse/SR-612
  let _playgroundPrintHook: ((String) -> Void)? = nil
#endif
  if let hook = _playgroundPrintHook {
    var output = _TeeStream(left: "", right: _Stdout())
    _print(
      items, separator: separator, terminator: terminator, to: &output)
    hook(output.left)
  }
  else {
    var output = _Stdout()
    _print(
      items, separator: separator, terminator: terminator, to: &output)
  }
}

/// Writes the textual representations of `items` most suitable for
/// debugging, separated by `separator` and terminated by
/// `terminator`, into the standard output.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(reflecting: item)`.
///
/// - Note: To print without a trailing newline, pass `terminator: ""`
///
/// - SeeAlso: `print`, `Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
@inline(never)
@_semantics("stdlib_binary_only")
public func debugPrint(
  _ items: Any...,
  separator: String = " ",
  terminator: String = "\n") {
  if let hook = _playgroundPrintHook {
    var output = _TeeStream(left: "", right: _Stdout())
    _debugPrint(
      items, separator: separator, terminator: terminator, to: &output)
    hook(output.left)
  }
  else {
    var output = _Stdout()
    _debugPrint(
      items, separator: separator, terminator: terminator, to: &output)
  }
}

/// Writes the textual representations of `items`, separated by
/// `separator` and terminated by `terminator`, into `output`.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(item)`.
///
/// - Note: To print without a trailing newline, pass `terminator: ""`
///
/// - SeeAlso: `debugPrint`, `Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
@inline(__always)
public func print<Target : OutputStream>(
  _ items: Any...,
  separator: String = " ",
  terminator: String = "\n",
  to output: inout Target
) {
  _print(items, separator: separator, terminator: terminator, to: &output)
}

/// Writes the textual representations of `items` most suitable for
/// debugging, separated by `separator` and terminated by
/// `terminator`, into `output`.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(reflecting: item)`.
///
/// - Note: To print without a trailing newline, pass `terminator: ""`
///
/// - SeeAlso: `print`, `Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
@inline(__always)
public func debugPrint<Target : OutputStream>(
  _ items: Any...,
  separator: String = " ",
  terminator: String = "\n",
  to output: inout Target
) {
  _debugPrint(
    items, separator: separator, terminator: terminator, to: &output)
}

@_versioned
@inline(never)
@_semantics("stdlib_binary_only")
internal func _print<Target : OutputStream>(
  _ items: [Any],
  separator: String = " ",
  terminator: String = "\n",
  to output: inout Target
) {
  var prefix = ""
  output._lock()
  defer { output._unlock() }
  for item in items {
    output.write(prefix)
    _print_unlocked(item, &output)
    prefix = separator
  }
  output.write(terminator)
}

@_versioned
@inline(never)
@_semantics("stdlib_binary_only")
internal func _debugPrint<Target : OutputStream>(
  _ items: [Any],
  separator: String = " ",
  terminator: String = "\n",
  to output: inout Target
) {
  var prefix = ""
  output._lock()
  defer { output._unlock() }
  for item in items {
    output.write(prefix)
    _debugPrint_unlocked(item, &output)
    prefix = separator
  }
  output.write(terminator)
}

//===----------------------------------------------------------------------===//
//===--- Migration Aids ---------------------------------------------------===//

@available(*, unavailable, message: "Please use 'terminator: \"\"' instead of 'appendNewline: false': 'print((...), terminator: \"\")'")
public func print<T>(_: T, appendNewline: Bool = true) {}
@available(*, unavailable, message: "Please use 'terminator: \"\"' instead of 'appendNewline: false': 'debugPrint((...), terminator: \"\")'")
public func debugPrint<T>(_: T, appendNewline: Bool = true) {}


//===--- FIXME: Not working due to <rdar://22101775> ----------------------===//
@available(*, unavailable, message: "Please use the 'to' label for the target stream: 'print((...), to: &...)'")
public func print<T>(_: T, _: inout OutputStream) {}
@available(*, unavailable, message: "Please use the 'to' label for the target stream: 'debugPrint((...), to: &...))'")
public func debugPrint<T>(_: T, _: inout OutputStream) {}

@available(*, unavailable, message: "Please use 'terminator: \"\"' instead of 'appendNewline: false' and use the 'toStream' label for the target stream: 'print((...), terminator: \"\", toStream: &...)'")
public func print<T>(_: T, _: inout OutputStream, appendNewline: Bool = true) {}
@available(*, unavailable, message: "Please use 'terminator: \"\"' instead of 'appendNewline: false' and use the 'toStream' label for the target stream: 'debugPrint((...), terminator: \"\", toStream: &...)'")
public func debugPrint<T>(
  _: T, _: inout OutputStream, appendNewline: Bool = true
) {}
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
