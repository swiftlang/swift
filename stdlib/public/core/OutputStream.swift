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

import SwiftShims

//===----------------------------------------------------------------------===//
// Input/Output interfaces
//===----------------------------------------------------------------------===//

/// A target of text streaming operations.
public protocol OutputStreamType {
  mutating func _lock()
  mutating func _unlock()

  /// Append the given `string` to this stream.
  mutating func write(string: String)
}

extension OutputStreamType {
  public mutating func _lock() {}
  public mutating func _unlock() {}
}

/// A source of text streaming operations.  `Streamable` instances can
/// be written to any *output stream*.
///
/// For example: `String`, `Character`, `UnicodeScalar`.
public protocol Streamable {
  /// Write a textual representation of `self` into `target`.
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
///   generic constraint, or accessing a conforming type's `description`
///   directly, is therefore discouraged.
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
/// *output stream* by `debugPrint`, and is
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
/// - SeeAlso: `String.init<T>(reflecting: T)`,
///   `CustomStringConvertible`
public protocol CustomDebugStringConvertible {
  /// A textual representation of `self`, suitable for debugging.
  var debugDescription: String { get }
}

//===----------------------------------------------------------------------===//
// Default (ad-hoc) printing
//===----------------------------------------------------------------------===//

/// Do our best to print a value that cannot be printed directly.
internal func _adHocPrint<T, TargetStream : OutputStreamType>(
    value: T, inout _ target: TargetStream, isDebugPrint: Bool
) {
  func printTypeName(type: Any.Type) {
    // Print type names without qualification, unless we're debugPrint'ing.
    target.write(_typeName(type, qualified: isDebugPrint))
  }

  let mirror = _reflect(value)
  switch mirror {
  // Checking the mirror kind is not a good way to implement this, but we don't
  // have a more expressive reflection API now.
  case is _TupleMirror:
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
      debugPrint(elt, terminator: "", toStream: &target)
    }
    target.write(")")

  case is _StructMirror:
    printTypeName(mirror.valueType)
    target.write("(")
    var first = true
    for i in 0..<mirror.count {
      if first {
        first = false
      } else {
        target.write(", ")
      }
      let (label, elementMirror) = mirror[i]
      print(label, terminator: "", toStream: &target)
      target.write(": ")
      debugPrint(elementMirror.value, terminator: "", toStream: &target)
    }
    target.write(")")

  case let enumMirror as _EnumMirror:
    if let caseName = String.fromCString(enumMirror.caseName) {
      // Write the qualified type name in debugPrint.
      if isDebugPrint {
        target.write(_typeName(mirror.valueType))
        target.write(".")
      }
      target.write(caseName)
    } else {
      // If the case name is garbage, just print the type name.
      printTypeName(mirror.valueType)
    }

    if mirror.count == 0 {
      return
    }
    let (_, payload) = mirror[0]
    if payload is _TupleMirror {
      debugPrint(payload.value, terminator: "", toStream: &target)
      return
    }
    target.write("(")
    debugPrint(payload.value, terminator: "", toStream: &target)
    target.write(")")

  case is _MetatypeMirror:
    printTypeName(mirror.value as! Any.Type)

  default:
    print(mirror.summary, terminator: "", toStream: &target)
  }
}

@inline(never)
@_semantics("stdlib_binary_only")
internal func _print_unlocked<T, TargetStream : OutputStreamType>(
  value: T, inout _ target: TargetStream
) {
  // Optional has no representation suitable for display; therefore,
  // values of optional type should be printed as a debug
  // string. Check for Optional first, before checking protocol
  // conformance below, because an Optional value is convertible to a
  // protocol if its wrapped type conforms to that protocol.
  if _isOptional(value.dynamicType) {
    let debugPrintable = value as! CustomDebugStringConvertible
    debugPrintable.debugDescription.writeTo(&target)
    return
  }
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

  _adHocPrint(value, &target, isDebugPrint: false)
}

/// Returns the result of `print`'ing `x` into a `String`.
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

  _adHocPrint(value, &target, isDebugPrint: true)
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
      _swift_stdlib_putchar(Int32(c))
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
  /// Write a textual representation of `self` into `target`.
  public func writeTo<Target : OutputStreamType>(inout target: Target) {
    target.write(self)
  }
}

extension Character : Streamable {
  /// Write a textual representation of `self` into `target`.
  public func writeTo<Target : OutputStreamType>(inout target: Target) {
    target.write(String(self))
  }
}

extension UnicodeScalar : Streamable {
  /// Write a textual representation of `self` into `target`.
  public func writeTo<Target : OutputStreamType>(inout target: Target) {
    target.write(String(Character(self)))
  }
}

//===----------------------------------------------------------------------===//
// Unavailable APIs
//===----------------------------------------------------------------------===//

@available(*, unavailable, renamed="CustomDebugStringConvertible")
public typealias DebugPrintable = CustomDebugStringConvertible
@available(*, unavailable, renamed="CustomStringConvertible")
public typealias Printable = CustomStringConvertible

@available(*, unavailable, renamed="print")
public func println<T, TargetStream : OutputStreamType>(
    value: T, inout _ target: TargetStream
) {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, renamed="print")
public func println<T>(value: T) {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, message="use print(\"\")")
public func println() {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, renamed="String")
public func toString<T>(x: T) -> String {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, message="use debugPrint()")
public func debugPrintln<T, TargetStream : OutputStreamType>(
    x: T, inout _ target: TargetStream
) {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, renamed="debugPrint")
public func debugPrintln<T>(x: T) {
  fatalError("unavailable function can't be called")
}

/// Returns the result of `debugPrint`'ing `x` into a `String`.
@available(*, unavailable, message="use String(reflecting:)")
public func toDebugString<T>(x: T) -> String {
  fatalError("unavailable function can't be called")
}

/// A hook for playgrounds to print through.
public var _playgroundPrintHook : ((String) -> Void)? = {_ in () }

internal struct _TeeStream<
  L : OutputStreamType, 
  R : OutputStreamType
> : OutputStreamType {
  var left: L
  var right: R
  
  /// Append the given `string` to this stream.
  mutating func write(string: String)
  { left.write(string); right.write(string) }

  mutating func _lock() { left._lock(); right._lock() }
  mutating func _unlock() { left._unlock(); right._unlock() }
}

