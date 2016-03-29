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
public protocol OutputStream {
  mutating func _lock()
  mutating func _unlock()

  /// Append the given `string` to this stream.
  mutating func write(_ string: String)
}

extension OutputStream {
  public mutating func _lock() {}
  public mutating func _unlock() {}
}

/// A source of text streaming operations.  `Streamable` instances can
/// be written to any *output stream*.
///
/// For example: `String`, `Character`, `UnicodeScalar`.
public protocol Streamable {
  /// Write a textual representation of `self` into `target`.
  func write<Target : OutputStream>(to target: inout Target)
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
  /// A textual representation of the instance.
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
  /// A textual representation of the instance, suitable for debugging.
  var debugDescription: String { get }
}

//===----------------------------------------------------------------------===//
// Default (ad-hoc) printing
//===----------------------------------------------------------------------===//

@_silgen_name("swift_EnumCaseName")
func _getEnumCaseName<T>(_ value: T) -> UnsafePointer<CChar>?

@_silgen_name("swift_OpaqueSummary")
func _opaqueSummary(_ metadata: Any.Type) -> UnsafePointer<CChar>?

/// Do our best to print a value that cannot be printed directly.
internal func _adHocPrint_unlocked<T, TargetStream : OutputStream>(
    _ value: T, _ mirror: Mirror, _ target: inout TargetStream,
    isDebugPrint: Bool
) {
  func printTypeName(_ type: Any.Type) {
    // Print type names without qualification, unless we're debugPrint'ing.
    target.write(_typeName(type, qualified: isDebugPrint))
  }

  if let displayStyle = mirror.displayStyle {
    switch displayStyle {
      case .optional:
        if let child = mirror.children.first {
          _debugPrint_unlocked(child.1, &target)
        } else {
          _debugPrint_unlocked("nil", &target)
        }
      case .tuple:
        target.write("(")
        var first = true
        for (_, value) in mirror.children {
          if first {
            first = false
          } else {
            target.write(", ")
          }
          _debugPrint_unlocked(value, &target)
        }
        target.write(")")
      case .`struct`:
        printTypeName(mirror.subjectType)
        target.write("(")
        var first = true
        for (label, value) in mirror.children {
          if let label = label {
            if first {
              first = false
            } else {
              target.write(", ")
            }
            target.write(label)
            target.write(": ")
            _debugPrint_unlocked(value, &target)
          }
        }
        target.write(")")
      case .`enum`:
        if let cString = _getEnumCaseName(value),
            let caseName = String(validatingUTF8: cString) {
          // Write the qualified type name in debugPrint.
          if isDebugPrint {
            printTypeName(mirror.subjectType)
            target.write(".")
          }
          target.write(caseName)
        } else {
          // If the case name is garbage, just print the type name.
          printTypeName(mirror.subjectType)
        }
        if let (_, value) = mirror.children.first {
          if (Mirror(reflecting: value).displayStyle == .tuple) {
            _debugPrint_unlocked(value, &target)
          } else {
            target.write("(")
            _debugPrint_unlocked(value, &target)
            target.write(")")
          }
        }
      default:
        target.write(_typeName(mirror.subjectType))
    }
  } else if let metatypeValue = value as? Any.Type {
    // Metatype
    printTypeName(metatypeValue)
  } else {
    // Fall back to the type or an opaque summary of the kind
    if let cString = _opaqueSummary(mirror.subjectType),
        let opaqueSummary = String(validatingUTF8: cString) {
      target.write(opaqueSummary)
    } else {
      target.write(_typeName(mirror.subjectType, qualified: true))
    }
  }
}

@inline(never)
@_semantics("stdlib_binary_only")
internal func _print_unlocked<T, TargetStream : OutputStream>(
  _ value: T, _ target: inout TargetStream
) {
  // Optional has no representation suitable for display; therefore,
  // values of optional type should be printed as a debug
  // string. Check for Optional first, before checking protocol
  // conformance below, because an Optional value is convertible to a
  // protocol if its wrapped type conforms to that protocol.
  if _isOptional(value.dynamicType) {
    let debugPrintable = value as! CustomDebugStringConvertible
    debugPrintable.debugDescription.write(to: &target)
    return
  }
  if case let streamableObject as Streamable = value {
    streamableObject.write(to: &target)
    return
  }

  if case let printableObject as CustomStringConvertible = value {
    printableObject.description.write(to: &target)
    return
  }

  if case let debugPrintableObject as CustomDebugStringConvertible = value {
    debugPrintableObject.debugDescription.write(to: &target)
    return
  }

  let mirror = Mirror(reflecting: value)
  _adHocPrint_unlocked(value, mirror, &target, isDebugPrint: false)
}

/// Returns the result of `print`'ing `x` into a `String`.
///
/// Exactly the same as `String`, but annotated 'readonly' to allow
/// the optimizer to remove calls where results are unused.
///
/// This function is forbidden from being inlined because when building the
/// standard library inlining makes us drop the special semantics.
@inline(never) @effects(readonly)
func _toStringReadOnlyStreamable<T : Streamable>(_ x: T) -> String {
  var result = ""
  x.write(to: &result)
  return result
}

@inline(never) @effects(readonly)
func _toStringReadOnlyPrintable<T : CustomStringConvertible>(_ x: T) -> String {
  return x.description
}

//===----------------------------------------------------------------------===//
// `debugPrint`
//===----------------------------------------------------------------------===//

@inline(never)
public func _debugPrint_unlocked<T, TargetStream : OutputStream>(
    _ value: T, _ target: inout TargetStream
) {
  if let debugPrintableObject = value as? CustomDebugStringConvertible {
    debugPrintableObject.debugDescription.write(to: &target)
    return
  }

  if let printableObject = value as? CustomStringConvertible {
    printableObject.description.write(to: &target)
    return
  }

  if let streamableObject = value as? Streamable {
    streamableObject.write(to: &target)
    return
  }

  let mirror = Mirror(reflecting: value)
  _adHocPrint_unlocked(value, mirror, &target, isDebugPrint: true)
}

internal func _dumpPrint_unlocked<T, TargetStream : OutputStream>(
    _ value: T, _ mirror: Mirror, _ target: inout TargetStream
) {
  if let displayStyle = mirror.displayStyle {
    // Containers and tuples are always displayed in terms of their element count
    switch displayStyle {
    case .tuple:
      let count = mirror.children.count
      target.write(count == 1 ? "(1 element)" : "(\(count) elements)")
      return
    case .collection:
      let count = mirror.children.count
      target.write(count == 1 ? "1 element" : "\(count) elements")
      return
    case .dictionary:
      let count = mirror.children.count
      target.write(count == 1 ? "1 key/value pair" : "\(count) key/value pairs")
      return
    case .`set`:
      let count = mirror.children.count
      target.write(count == 1 ? "1 member" : "\(count) members")
      return
    default:
      break
    }
  }

  if let debugPrintableObject = value as? CustomDebugStringConvertible {
    debugPrintableObject.debugDescription.write(to: &target)
    return
  }

  if let printableObject = value as? CustomStringConvertible {
    printableObject.description.write(to: &target)
    return
  }

  if let streamableObject = value as? Streamable {
    streamableObject.write(to: &target)
    return
  }

  if let displayStyle = mirror.displayStyle {
    switch displayStyle {
    case .`class`, .`struct`:
      // Classes and structs without custom representations are displayed as
      // their fully qualified type name
      target.write(_typeName(mirror.subjectType, qualified: true))
      return
    case .`enum`:
      target.write(_typeName(mirror.subjectType, qualified: true))
      if let cString = _getEnumCaseName(value),
          let caseName = String(validatingUTF8: cString) {
        target.write(".")
        target.write(caseName)
      }
      return
    default:
      break
    }
  }

  _adHocPrint_unlocked(value, mirror, &target, isDebugPrint: true)
}

//===----------------------------------------------------------------------===//
// OutputStreams
//===----------------------------------------------------------------------===//

internal struct _Stdout : OutputStream {
  mutating func _lock() {
    _swift_stdlib_flockfile_stdout()
  }

  mutating func _unlock() {
    _swift_stdlib_funlockfile_stdout()
  }

  mutating func write(_ string: String) {
    if string.isEmpty { return }

    if string._core.isASCII {
      defer { _fixLifetime(string) }

      _swift_stdlib_fwrite_stdout(UnsafePointer(string._core.startASCII),
                                  string._core.count, 1)
      return
    }

    for c in string.utf8 {
      _swift_stdlib_putchar_unlocked(Int32(c))
    }
  }
}

extension String : OutputStream {
  /// Append `other` to this stream.
  public mutating func write(_ other: String) {
    self += other
  }
}

//===----------------------------------------------------------------------===//
// Streamables
//===----------------------------------------------------------------------===//

extension String : Streamable {
  /// Write a textual representation of `self` into `target`.
  public func write<Target : OutputStream>(to target: inout Target) {
    target.write(self)
  }
}

extension Character : Streamable {
  /// Write a textual representation of `self` into `target`.
  public func write<Target : OutputStream>(to target: inout Target) {
    target.write(String(self))
  }
}

extension UnicodeScalar : Streamable {
  /// Write a textual representation of `self` into `target`.
  public func write<Target : OutputStream>(to target: inout Target) {
    target.write(String(Character(self)))
  }
}

/// A hook for playgrounds to print through.
public var _playgroundPrintHook : ((String) -> Void)? = {_ in () }

internal struct _TeeStream<
  L : OutputStream, 
  R : OutputStream
> : OutputStream {
  var left: L
  var right: R
  
  /// Append the given `string` to this stream.
  mutating func write(_ string: String)
  { left.write(string); right.write(string) }

  mutating func _lock() { left._lock(); right._lock() }
  mutating func _unlock() { right._unlock(); left._unlock() }
}

@available(*, unavailable, renamed: "OutputStream")
public typealias OutputStreamType = OutputStream
