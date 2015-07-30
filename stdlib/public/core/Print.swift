//===--- Print.swift ------------------------------------------------------===//
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

/// Writes the textual representations of `items`, separated by
/// `separator` and terminated by `terminator`, into the standard
/// output.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(item)`.
///
/// - SeeAlso: `debugPrint`, Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
@inline(never)
@_semantics("stdlib_binary_only")
public func _prext_print(
  items: Any...,
  separator: String = " ",
  terminator: String = "\n"
) {
  if let hook = _playgroundPrintHook {
    var output = _TeeStream(left: "", right: _Stdout())
    _print(
      items, toStream: &output, separator: separator, terminator: terminator)
    hook(output.left)
  }
  else {
    var output = _Stdout()
    _print(
      items, toStream: &output, separator: separator, terminator: terminator)
  }
}

/// Writes the textual representations of `items` most suitable for
/// debugging, separated by `separator` and terminated by
/// `terminator`, into the standard output.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(reflecting: item)`.
///
/// - SeeAlso: `print`, Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
@inline(never)
@_semantics("stdlib_binary_only")
public func _prext_debugPrint(
  items: Any...,
  separator: String = " ",
  terminator: String = "\n") {
  if let hook = _playgroundPrintHook {
    var output = _TeeStream(left: "", right: _Stdout())
    _debugPrint(
      items, toStream: &output, separator: separator, terminator: terminator)
    hook(output.left)
  }
  else {
    var output = _Stdout()
    _debugPrint(
      items, toStream: &output, separator: separator, terminator: terminator)
  }
}


/// Writes the textual representations of `items`, separated by
/// `separator` and terminated by `terminator`, into `output`.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(item)`.
///
/// - SeeAlso: `debugPrint`, Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
@inline(__always)
public func _prext_print<Target: OutputStreamType>(
  items: Any...,
  inout toStream output: Target,
  separator: String = " ",
  terminator: String = "\n"
) {
  _print(items, toStream: &output, separator: separator, terminator: terminator)
}

/// Writes the textual representations of `items` most suitable for
/// debugging, separated by `separator` and terminated by
/// `terminator`, into `output`.
///
/// The textual representations are obtained for each `item` via
/// the expression `String(reflecting: item)`.
///
/// - SeeAlso: `print`, Streamable`, `CustomStringConvertible`,
///   `CustomDebugStringConvertible`
@inline(__always)
public func _prext_debugPrint<Target: OutputStreamType>(
  items: Any...,
  inout toStream output: Target,
  separator: String = " ",
  terminator: String = "\n"
) {
  _debugPrint(
    items, toStream: &output, separator: separator, terminator: terminator)
}

@inline(never)
@_semantics("stdlib_binary_only")
internal func _print<Target: OutputStreamType>(
  items: [Any],
  inout toStream output: Target,
  separator: String = " ",
  terminator: String = "\n"
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
  inout toStream output: Target,
  separator: String = " ",
  terminator: String = "\n"
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

