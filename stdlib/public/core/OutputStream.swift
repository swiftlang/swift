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
/// *output stream*, for example, by `print` and `println`.
///
/// - note: `String(instance)` will work for an `instance` of *any*
/// type, returning its `description` if the `instance` happens to be
/// `CustomStringConvertible`.  Using `CustomStringConvertible` as a
/// generic constraint, or accessing a conforming type's `description`
/// directly, is therefore discouraged.
///
/// - seealso: `String.init<T>(T)`, `CustomDebugStringConvertible`
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
/// - note: `String(reflecting: instance)` will work for an `instance`
/// of *any* type, returning its `debugDescription` if the `instance`
/// happens to be `CustomDebugStringConvertible`.  Using
/// `CustomDebugStringConvertible` as a generic constraint, or
/// accessing a conforming type's `debugDescription` directly, is
/// therefore discouraged.
///
/// - see also: `String.init<T>(reflecting: T)`,
/// `CustomStringConvertible`
public protocol CustomDebugStringConvertible {
  /// A textual representation of `self`, suitable for debugging.
  var debugDescription: String { get }
}

@availability(*, unavailable, renamed="CustomDebugStringConvertible")
public typealias DebugPrintable = CustomDebugStringConvertible
@availability(*, unavailable, renamed="CustomStringConvertible")
public typealias Printable = CustomStringConvertible

//===----------------------------------------------------------------------===//
// `print`
//===----------------------------------------------------------------------===//

/// Do our best to print a value that can not be printed directly,
/// using one of its conformances to `Streamable`,
/// `CustomStringConvertible` or `CustomDebugStringConvertible`.
func _adHocPrint<T, TargetStream : OutputStreamType>(
    value: T, inout _ target: TargetStream
) {
  var mirror = reflect(value)
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
      var (label, elementMirror) = mirror[i]
      var elt = elementMirror.value
      debugPrint(elt, &target)
    }
    target.write(")")
    return
  }
  if mirror is _StructMirror {
    print(mirror.summary, &target)
    target.write("(")
    var first = true
    for i in 0..<mirror.count {
      if first {
        first = false
      } else {
        target.write(", ")
      }
      var (label, elementMirror) = mirror[i]
      print(label, &target)
      target.write(": ")
      debugPrint(elementMirror.value, &target)
    }
    target.write(")")
    return
  }
  print(mirror.summary, &target)
}

@inline(never)
public func _print_unlocked<T, TargetStream : OutputStreamType>(
    value: T, inout _ target: TargetStream
) {
  if case let streamableObject as Streamable = value {
    streamableObject.writeTo(&target)
    return
  }

  if case var printableObject as CustomStringConvertible = value {
    printableObject.description.writeTo(&target)
    return
  }

  if case let debugPrintableObject as CustomDebugStringConvertible = value {
    debugPrintableObject.debugDescription.writeTo(&target)
    return
  }

  _adHocPrint(value, &target)
}

/// Writes the textual representation of `value` into the stream `target`.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `CustomStringConvertible`, `CustomDebugStringConvertible`.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
@inline(never)
public func print<T, TargetStream : OutputStreamType>(
    value: T, inout _ target: TargetStream
) {
  target._lock()
  _print_unlocked(value, &target)
  target._unlock()
}

/// Writes the textual representation of `value` and a newline character into
/// the stream `target`.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `CustomStringConvertible`, `CustomDebugStringConvertible`.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
@inline(never)
public func println<T, TargetStream : OutputStreamType>(
    value: T, inout _ target: TargetStream
) {
  target._lock()
  _print_unlocked(value, &target)
  target.write("\n")
  target._unlock()
}

/// Writes the textual representation of `value` into the standard output.
///
/// The textual representation is obtained from the `value` using its protocol
/// conformances, in the following order of preference: `Streamable`,
/// `CustomStringConvertible`, `CustomDebugStringConvertible`.
///
/// Do not overload this function for your type.  Instead, adopt one of the
/// protocols mentioned above.
@inline(never)
@_semantics("stdlib_binary_only")
public func print<T>(value: T) {
  var target = _Stdout()
  target._lock()
  _print_unlocked(value, &target)
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
@inline(never)
@_semantics("stdlib_binary_only")
public func println() {
  var target = _Stdout()
  target._lock()
  target.write("\n")
  target._unlock()
}

/// Returns the result of `print`'ing `x` into a `String`
@inline(never)
public func toString<T>(x: T) -> String {
  var result = ""
  print(x, &result)
  return result
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

/// Returns the result of `debugPrint`'ing `x` into a `String`
public func toDebugString<T>(x: T) -> String {
  var result = ""
  debugPrint(x, &result)
  return result
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

  if var printableObject = value as? CustomStringConvertible {
    printableObject.description.writeTo(&target)
    return
  }

  if let streamableObject = value as? Streamable {
    streamableObject.writeTo(&target)
    return
  }

  _adHocPrint(value, &target)
}

/// Write to `target` the textual representation of `x` most suitable
/// for debugging.
///
/// * If `T` conforms to `CustomDebugStringConvertible`, write
///   `x.debugDescription`
/// * Otherwise, if `T` conforms to `CustomStringConvertible`, write
///   `x.description`
/// * Otherwise, if `T` conforms to `Streamable`, write `x`
/// * Otherwise, fall back to a default textual representation.
///
/// - seealso: `debugPrintln(x, &target)`
@inline(never)
public func debugPrint<T, TargetStream : OutputStreamType>(
    value: T, inout _ target: TargetStream
) {
  target._lock()
  _debugPrint_unlocked(value, &target)
  target._unlock()
}

/// Write to `target` the textual representation of `x` most suitable
/// for debugging, followed by a newline.
///
/// * If `T` conforms to `CustomDebugStringConvertible`, write `x.debugDescription`
/// * Otherwise, if `T` conforms to `CustomStringConvertible`, write `x.description`
/// * Otherwise, if `T` conforms to `Streamable`, write `x`
/// * Otherwise, fall back to a default textual representation.
///
/// - seealso: `debugPrint(x, &target)`
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
/// for debugging.
///
/// * If `T` conforms to `CustomDebugStringConvertible`, write `x.debugDescription`
/// * Otherwise, if `T` conforms to `CustomStringConvertible`, write `x.description`
/// * Otherwise, if `T` conforms to `Streamable`, write `x`
/// * Otherwise, fall back to a default textual representation.
///
/// - seealso: `debugPrintln(x)`
@inline(never)
public func debugPrint<T>(x: T) {
  var target = _Stdout()
  target._lock()
  _debugPrint_unlocked(x, &target)
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
/// - seealso: `debugPrint(x)`
@inline(never)
public func debugPrintln<T>(x: T) {
  var target = _Stdout()
  target._lock()
  _debugPrint_unlocked(x, &target)
  target.write("\n")
  target._unlock()
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
