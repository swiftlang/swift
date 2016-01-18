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
  items: Any...,
  separator: String = " ",
  terminator: String = "\n"
) {
  if let hook = _playgroundPrintHook {
    var output = _TeeStream(left: "", right: _Stdout())
    _print(
      items, separator: separator, terminator: terminator, toStream: &output)
    hook(output.left)
  }
  else {
    var output = _Stdout()
    _print(
      items, separator: separator, terminator: terminator, toStream: &output)
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
  items: Any...,
  separator: String = " ",
  terminator: String = "\n") {
  if let hook = _playgroundPrintHook {
    var output = _TeeStream(left: "", right: _Stdout())
    _debugPrint(
      items, separator: separator, terminator: terminator, toStream: &output)
    hook(output.left)
  }
  else {
    var output = _Stdout()
    _debugPrint(
      items, separator: separator, terminator: terminator, toStream: &output)
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
public func print<Target: OutputStreamType>(
  items: Any...,
  separator: String = " ",
  terminator: String = "\n",
  inout toStream output: Target
) {
  _print(items, separator: separator, terminator: terminator, toStream: &output)
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
public func debugPrint<Target: OutputStreamType>(
  items: Any...,
  separator: String = " ",
  terminator: String = "\n",
  inout toStream output: Target
) {
  _debugPrint(
    items, separator: separator, terminator: terminator, toStream: &output)
}

@inline(never)
@_semantics("stdlib_binary_only")
internal func _print<Target: OutputStreamType>(
  items: [Any],
  separator: String = " ",
  terminator: String = "\n",
  inout toStream output: Target
) {
  var prefix = ""
  output._lock()
  for item in items {
    output.write(prefix)
    _print_unlocked(item, &output)
    prefix = separator
  }
  output.write(terminator)
  output._unlock()
}

@inline(never)
@_semantics("stdlib_binary_only")
internal func _debugPrint<Target: OutputStreamType>(
  items: [Any],
  separator: String = " ",
  terminator: String = "\n",
  inout toStream output: Target
) {
  var prefix = ""
  output._lock()
  for item in items {
    output.write(prefix)
    _debugPrint_unlocked(item, &output)
    prefix = separator
  }
  output.write(terminator)
  output._unlock()
}

//===----------------------------------------------------------------------===//
//===--- Migration Aids ---------------------------------------------------===//

@available(*, unavailable, message="Please wrap your tuple argument in parentheses: 'print((...))'")
public func print<T>(_: T) {}
@available(*, unavailable, message="Please wrap your tuple argument in parentheses: 'debugPrint((...))'")
public func debugPrint<T>(_: T) {}

@available(*, unavailable, message="Please use 'terminator: \"\"' instead of 'appendNewline: false': 'print((...), terminator: \"\")'")
public func print<T>(_: T, appendNewline: Bool) {}
@available(*, unavailable, message="Please use 'terminator: \"\"' instead of 'appendNewline: false': 'debugPrint((...), terminator: \"\")'")
public func debugPrint<T>(_: T, appendNewline: Bool) {}


//===--- FIXME: Not working due to <rdar://22101775> ----------------------===//
@available(*, unavailable, message="Please use the 'toStream' label for the target stream: 'print((...), toStream: &...)'")
public func print<T>(_: T, inout _: OutputStreamType) {}
@available(*, unavailable, message="Please use the 'toStream' label for the target stream: 'debugPrint((...), toStream: &...))'")
public func debugPrint<T>(_: T, inout _: OutputStreamType) {}

@available(*, unavailable, message="Please use 'terminator: \"\"' instead of 'appendNewline: false' and use the 'toStream' label for the target stream: 'print((...), terminator: \"\", toStream: &...)'")
public func print<T>(_: T, inout _: OutputStreamType, appendNewline: Bool) {}
@available(*, unavailable, message="Please use 'terminator: \"\"' instead of 'appendNewline: false' and use the 'toStream' label for the target stream: 'debugPrint((...), terminator: \"\", toStream: &...)'")
public func debugPrint<T>(
  _: T, inout _: OutputStreamType, appendNewline: Bool
) {}
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
