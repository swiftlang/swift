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

@_exported import Foundation // Clang module

//===----------------------------------------------------------------------===//
// Strings
//===----------------------------------------------------------------------===//

@available(*, unavailable, message: "Please use String or NSString")
public class NSSimpleCString {}

@available(*, unavailable, message: "Please use String or NSString")
public class NSConstantString {}

extension NSString : ExpressibleByStringLiteral {
  /// Create an instance initialized to `value`.
  public required convenience init(stringLiteral value: StaticString) {
    var immutableResult: NSString
    if value.hasPointerRepresentation {
      immutableResult = NSString(
        bytesNoCopy: UnsafeMutableRawPointer(mutating: value.utf8Start),
        length: Int(value.utf8CodeUnitCount),
        encoding: value.isASCII ? String.Encoding.ascii.rawValue : String.Encoding.utf8.rawValue,
        freeWhenDone: false)!
    } else {
      var uintValue = value.unicodeScalar
      immutableResult = NSString(
        bytes: &uintValue,
        length: 4,
        encoding: String.Encoding.utf32.rawValue)!
    }
    self.init(string: immutableResult as String)
  }
}

extension NSString : _HasCustomAnyHashableRepresentation {
  // Must be @nonobjc to prevent infinite recursion trying to bridge
  // AnyHashable to NSObject.
  @nonobjc
  public func _toCustomAnyHashable() -> AnyHashable? {
    // Consistently use Swift equality and hashing semantics for all strings.
    return AnyHashable(self as String)
  }
}

extension NSString {
  public convenience init(format: __shared NSString, _ args: CVarArg...) {
    // We can't use withVaList because 'self' cannot be captured by a closure
    // before it has been initialized.
    let va_args = getVaList(args)
    self.init(format: format as String, arguments: va_args)
  }

  public convenience init(
    format: __shared NSString, locale: Locale?, _ args: CVarArg...
  ) {
    // We can't use withVaList because 'self' cannot be captured by a closure
    // before it has been initialized.
    let va_args = getVaList(args)
    self.init(format: format as String, locale: locale, arguments: va_args)
  }

  public class func localizedStringWithFormat(
    _ format: NSString, _ args: CVarArg...
  ) -> Self {
    return withVaList(args) {
      self.init(format: format as String, locale: Locale.current, arguments: $0)
    }
  }

  public func appendingFormat(_ format: NSString, _ args: CVarArg...)
  -> NSString {
    return withVaList(args) {
      self.appending(NSString(format: format as String, arguments: $0) as String) as NSString
    }
  }
}

extension NSMutableString {
  public func appendFormat(_ format: NSString, _ args: CVarArg...) {
    return withVaList(args) {
      self.append(NSString(format: format as String, arguments: $0) as String)
    }
  }
}

extension NSString {
  /// Returns an `NSString` object initialized by copying the characters
  /// from another given string.
  ///
  /// - Returns: An `NSString` object initialized by copying the
  ///   characters from `aString`. The returned object may be different
  ///   from the original receiver.
  @nonobjc
  public convenience init(string aString: __shared NSString) {
    self.init(string: aString as String)
  }
}

extension NSString : _CustomPlaygroundQuickLookable {
  @available(*, deprecated, message: "NSString.customPlaygroundQuickLook will be removed in a future Swift version")
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(self as String)
  }
}
