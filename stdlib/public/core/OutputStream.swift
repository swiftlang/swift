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

import SwiftShims // for putchar

//===----------------------------------------------------------------------===//
// Input/Output interfaces
//===----------------------------------------------------------------------===//

public protocol _OutputStreamDefaultsType {}
extension _OutputStreamDefaultsType {
  final public mutating func _lock() {}
  final public mutating func _unlock() {}
}

/// A target of text streaming operations.
public protocol OutputStreamType : _OutputStreamDefaultsType {
  mutating func _lock()
  mutating func _unlock()

  /// Append the given `string` to this stream.
  mutating func write(string: String)
}

/// A source of text streaming operations.  `Streamable` instances can
/// be written to any *output stream*.
///
/// For example: `String`, `Character`, `UnicodeScalar`.
public protocol Streamable {
  /// Write a textual representation of `self` into `target`
  func writeTo<Target : OutputStreamType>(inout target: Target)
}

/// A type with a customized textual representation.
///
/// This textual representation is used when values are written to an
/// *output stream*, for example, by `print`.
///
/// - Note: `String(instance)` will work for an `instance` of *any*
///   type, returning its `description` if the `instance` happens to be
///   `CustomStringConvertible`.  Using `CustomStringConvertible` as a
/// generic constraint, or accessing a conforming type's `description`
/// directly, is therefore discouraged.
///
/// - SeeAlso: `String.init<T>(T)`, `CustomDebugStringConvertible`
public protocol CustomStringConvertible {
  /// A textual representation of `self`.
  var description: String { get }
}

/// A type with a customized textual representation suitable for
/// debugging purposes.
///
/// This textual representation is used when values are written to an
/// *output stream* by `debugPrint` and `debugPrintln`, and is
/// typically more verbose than the text provided by a
/// `CustomStringConvertible`'s `description` property.
///
/// - Note: `String(reflecting: instance)` will work for an `instance`
///   of *any* type, returning its `debugDescription` if the `instance`
///   happens to be `CustomDebugStringConvertible`.  Using
/// `CustomDebugStringConvertible` as a generic constraint, or
/// accessing a conforming type's `debugDescription` directly, is
/// therefore discouraged.
///
/// - see also: `String.init<T>(reflecting: T)`,
///   `CustomStringConvertible`
public protocol CustomDebugStringConvertible {
  /// A textual representation of `self`, suitable for debugging.
  var debugDescription: String { get }
}

@available(*, unavailable, renamed="CustomDebugStringConvertible")
public typealias DebugPrintable = CustomDebugStringConvertible
@available(*, unavailable, renamed="CustomStringConvertible")
public typealias Printable = CustomStringConvertible

//===----------------------------------------------------------------------===//
// Default (ad-hoc) printing
//===----------------------------------------------------------------------===//

/// Do our best to print a value that can not be printed directly.
internal func _adHocPrint<T, TargetStream : OutputStreamType>(
    value: T, inout _ target: TargetStream
) {
  let mirror = reflect(value)
  // Checking the mirror kind is not a good way to implement this, but we don't
  // have a more expressive reflection API now.
  if mirror is _TupleMirror {
    target.write("(")
    var first = true
    for i in 0..<mirror.count {
      if first {
        first = false
      } else {
        target.write(", ")
      }
      let (_, elementMirror) = mirror[i]
      let elt = elementMirror.value
      debugPrint(elt, &target, appendNewline: false)
    }
    target.write(")")
    return
  }
  if mirror is _StructMirror {
    print(mirror.summary, &target, appendNewline: false)
    target.write("(")
    var first = true
    for i in 0..<mirror.count {
      if first {
        first = false
      } else {
        target.write(", ")
      }
      let (label, elementMirror) = mirror[i]
      print(label, &target, appendNewline: false)
      target.write(": ")
      debugPrint(elementMirror.value, &target, appendNewline: false)
    }
    target.write(")")
    return
  }
  if mirror is _EnumMirror {
    print(mirror.summary, &target, appendNewline: false)
    if mirror.count == 0 {
      return
    }
    let (tag, payload) = mirror[0]
    target.write(".")
    target.write(tag)
    if payload is _TupleMirror {
      if payload.count == 0 {
        return
      }
      debugPrint(payload.value, &target, appendNewline: false)
      return
    }
    target.write("(")
    debugPrint(payload.value, &target, appendNewline: false)
    target.write(")")
    return
  }
  print(mirror.summary, &target, appendNewline: false)
}

@inline(never)
internal func _print_unlocked<T, TargetStream : OutputStreamType>(
  value: T, inout _ target: TargetStream
) {
  if case let streamableObject as Streamable = value {
    streamableObject.writeTo(&target)
    return
  }

  if case let printableObject as CustomStringConvertible = value {
    printableObject.description.writeTo(&target)
    return
  }

  if case let debugPrintableObject as CustomDebugStringConvertible = value {
    debugPrintableObject.debugDescription.writeTo(&target)
    return
  }

  _adHocPrint(value, &target)
}

/// Returns the result of `print`'ing `x` into a `String`
///
/// Exactly the same as `String`, but annotated 'readonly' to allow
/// the optimizer to remove calls where results are unused.
///
/// This function is forbidden from being inlined because when building the
/// standard library inlining makes us drop the special semantics.
@inline(never) @effects(readonly)
func _toStringReadOnlyStreamable<T : Streamable>(x: T) -> String {
  var result = ""
  x.writeTo(&result)
  return result
}

@inline(never) @effects(readonly)
func _toStringReadOnlyPrintable<T : CustomStringConvertible>(x: T) -> String {
  return x.description
}

//===----------------------------------------------------------------------===//
// `debugPrint`
//===----------------------------------------------------------------------===//

@inline(never)
public func _debugPrint_unlocked<T, TargetStream : OutputStreamType>(
    value: T, inout _ target: TargetStream
) {
  if let debugPrintableObject = value as? CustomDebugStringConvertible {
    debugPrintableObject.debugDescription.writeTo(&target)
    return
  }

  if let printableObject = value as? CustomStringConvertible {
    printableObject.description.writeTo(&target)
    return
  }

  if let streamableObject = value as? Streamable {
    streamableObject.writeTo(&target)
    return
  }

  _adHocPrint(value, &target)
}

//===----------------------------------------------------------------------===//
// OutputStreams
//===----------------------------------------------------------------------===//

internal struct _Stdout : OutputStreamType {
  mutating func _lock() {
    _swift_stdlib_flockfile_stdout()
  }

  mutating func _unlock() {
    _swift_stdlib_funlockfile_stdout()
  }

  mutating func write(string: String) {
    // FIXME: buffering?
    // It is important that we use stdio routines in order to correctly
    // interoperate with stdio buffering.
    for c in string.utf8 {
      putchar(Int32(c))
    }
  }
}

extension String : OutputStreamType {
  /// Append `other` to this stream.
  public mutating func write(other: String) {
    self += other
  }
}

//===----------------------------------------------------------------------===//
// Streamables
//===----------------------------------------------------------------------===//

extension String : Streamable {
  /// Write a textual representation of `self` into `target`
  public func writeTo<Target : OutputStreamType>(inout target: Target) {
    target.write(self)
  }
}

extension Character : Streamable {
  /// Write a textual representation of `self` into `target`
  public func writeTo<Target : OutputStreamType>(inout target: Target) {
    target.write(String(self))
  }
}

extension UnicodeScalar : Streamable {
  /// Write a textual representation of `self` into `target`
  public func writeTo<Target : OutputStreamType>(inout target: Target) {
    target.write(String(Character(self)))
  }
}

//===----------------------------------------------------------------------===//
// Compatibility APIs
//===----------------------------------------------------------------------===//

/// Writes the textual representation of `value` and a newline character into
/// the stream `target`.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `CustomStringConvertible`, `CustomDebugStringConvertible`.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
@available(*, unavailable, renamed="print")
@inline(never)
public func println<T, TargetStream : OutputStreamType>(
    value: T, inout _ target: TargetStream
) {
  target._lock()
  _print_unlocked(value, &target)
  target.write("\n")
  target._unlock()
}

/// Writes the textual representation of `value` and a newline character into
/// the standard output.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `CustomStringConvertible`, `CustomDebugStringConvertible`.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
@available(*, unavailable, renamed="print")
@inline(never)
@_semantics("stdlib_binary_only")
public func println<T>(value: T) {
  var target = _Stdout()
  target._lock()
  _print_unlocked(value, &target)
  target.write("\n")
  target._unlock()
}

/// Writes a single newline character into the standard output.
@available(*, unavailable, message="use print(\"\")")
@inline(never)
@_semantics("stdlib_binary_only")
public func println() {
  var target = _Stdout()
  target._lock()
  target.write("\n")
  target._unlock()
}

/// Returns the result of `print`'ing `x` into a `String`
@available(*, unavailable, renamed="String")
@inline(never)
public func toString<T>(x: T) -> String {
  var result = ""
  print(x, &result, appendNewline: false)
  return result
}

/// Write to `target` the textual representation of `x` most suitable
/// for debugging, followed by a newline.
///
/// * If `T` conforms to `CustomDebugStringConvertible`, write `x.debugDescription`
/// * Otherwise, if `T` conforms to `CustomStringConvertible`, write `x.description`
/// * Otherwise, if `T` conforms to `Streamable`, write `x`
/// * Otherwise, fall back to a default textual representation.
///
/// - SeeAlso: `debugPrint(x, &target)`
@available(*, unavailable, message="use debugPrint()")
@inline(never)
public func debugPrintln<T, TargetStream : OutputStreamType>(
    x: T, inout _ target: TargetStream
) {
  target._lock()
  _debugPrint_unlocked(x, &target)
  target.write("\n")
  target._unlock()
}

/// Write to the console the textual representation of `x` most suitable
/// for debugging, followed by a newline.
///
/// * If `T` conforms to `CustomDebugStringConvertible`, write `x.debugDescription`
/// * Otherwise, if `T` conforms to `CustomStringConvertible`, write `x.description`
/// * Otherwise, if `T` conforms to `Streamable`, write `x`
/// * Otherwise, fall back to a default textual representation.
///
/// - SeeAlso: `debugPrint(x)`
@available(*, unavailable, renamed="debugPrint")
@inline(never)
public func debugPrintln<T>(x: T) {
  var target = _Stdout()
  target._lock()
  _debugPrint_unlocked(x, &target)
  target.write("\n")
  target._unlock()
}

/// Returns the result of `debugPrint`'ing `x` into a `String`
@available(*, unavailable, message="use String(reflecting:)")
public func toDebugString<T>(x: T) -> String {
  var result = ""
  debugPrint(x, &result)
  return result
}

//===----------------------------------------------------------------------===//
// print()
//===----------------------------------------------------------------------===//

/// Writes the textual representation of `value`, and an optional newline,
/// into the stream `target`.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `CustomStringConvertible`, `CustomDebugStringConvertible`.  If none of
/// these conformances are found, a default text representation is constructed
/// in an implementation-defined way, based on the type kind and structure.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
///
/// - parameter appendNewline: iff `true` (the default), write a trailing
///   newline.
@inline(never)
@_semantics("stdlib_binary_only")
public func print<T, TargetStream : OutputStreamType>(
  value: T, inout _ target: TargetStream, appendNewline: Bool
) {
  target._lock()
  _print_unlocked(value, &target)
  if appendNewline {
    target.write("\n")
  }
  target._unlock()
}

@inline(never)
@_semantics("stdlib_binary_only")
public func print<T, TargetStream : OutputStreamType>(
  value: T, inout _ target: TargetStream
) {
  // FIXME: workaround for rdar://20775669
  print(value, &target, appendNewline: true)
}

/// Writes the textual representation of `value`, and an optional newline,
/// into the standard output.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `CustomStringConvertible`, `CustomDebugStringConvertible`.  If none of
/// these conformances are found, a default text representation is constructed
/// in an implementation-defined way, based on the type kind and structure.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
///
/// - parameter appendNewline: iff `true` (the default), write a trailing
///   newline.
@inline(never)
@_semantics("stdlib_binary_only")
public func print<T>(value: T, appendNewline: Bool) {
  var target = _Stdout()
  target._lock()
  _print_unlocked(value, &target)
  if appendNewline {
    target.write("\n")
  }
  target._unlock()
}

@inline(never)
@_semantics("stdlib_binary_only")
public func print<T>(value: T) {
  // FIXME: workaround for rdar://20775669
  print(value, appendNewline: true)
}

//===----------------------------------------------------------------------===//
// debugPrint()
//===----------------------------------------------------------------------===//

/// Writes the textual representation of `value` most suitable for debugging,
/// and an optional newline, into the stream `target`.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference:
/// `CustomDebugStringConvertible`, `CustomStringConvertible`, `Streamable`.
/// If none of these conformances are found, a default text representation is
/// constructed in an implementation-defined way, based on the type kind and
/// structure.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
///
/// - parameter appendNewline: iff `true` (the default), write a trailing
///   newline.
@inline(never)
@_semantics("stdlib_binary_only")
public func debugPrint<T, TargetStream : OutputStreamType>(
  value: T, inout _ target: TargetStream, appendNewline: Bool
) {
  target._lock()
  _debugPrint_unlocked(value, &target)
  if appendNewline {
    target.write("\n")
  }
  target._unlock()
}

@inline(never)
@_semantics("stdlib_binary_only")
public func debugPrint<T, TargetStream : OutputStreamType>(
  value: T, inout _ target: TargetStream
) {
  // FIXME: workaround for rdar://20775669
  debugPrint(value, &target, appendNewline: true)
}

/// Writes the textual representation of `value` most suitable for debugging,
/// and an optional newline, into the standard output.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference:
/// `CustomDebugStringConvertible`, `CustomStringConvertible`, `Streamable`.
/// If none of these conformances are found, a default text representation is
/// constructed in an implementation-defined way, based on the type kind and
/// structure.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
///
/// - parameter appendNewline: iff `true` (the default), write a trailing
///   newline.
@inline(never)
@_semantics("stdlib_binary_only")
public func debugPrint<T>(value: T, appendNewline: Bool) {
  var target = _Stdout()
  target._lock()
  _debugPrint_unlocked(value, &target)
  if appendNewline {
    target.write("\n")
  }
  target._unlock()
}

@inline(never)
@_semantics("stdlib_binary_only")
public func debugPrint<T>(value: T) {
  // FIXME: workaround for rdar://20775669
  debugPrint(value, appendNewline: true)
}

