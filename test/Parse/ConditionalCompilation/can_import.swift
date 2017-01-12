// RUN: %target-typecheck-verify-swift -parse-as-library
// RUN: %target-typecheck-verify-swift -D WITH_PERFORM -primary-file %s %S/Inputs/can_import_nonprimary_file.swift

// REQUIRES: can_import

public struct LibraryDependentBool : ExpressibleByBooleanLiteral {
#if canImport(Swift)
  var _description: String
#endif

  public let magicConstant: Int = {
#if canImport(Swift)
    return 42
#else
    return "" // Type error
#endif
  }()

#if canImport(AppKit) || (canImport(UIKit) && (arch(i386) || arch(arm)))
  // On OS X and 32-bit iOS, Objective-C's BOOL type is a "signed char".
  var _value: Int8

  init(_ value: Int8) {
    self._value = value
#if canImport(Swift)
    self._description = "\(value)"
#endif
  }

  public init(_ value: Bool) {
#if canImport(Swift)
  self._description = value ? "YES" : "NO"
#endif
    self._value = value ? 1 : 0
  }

#else
  // Everywhere else it is C/C++'s "Bool"
  var _value: Bool

  public init(_ value: Bool) {
#if canImport(Swift)
    self._description = value ? "YES" : "NO"
#endif
    self._value = value
  }
#endif

  /// The value of `self`, expressed as a `Bool`.
  public var boolValue: Bool {
#if canImport(AppKit) || (canImport(UIKit) && (arch(i386) || arch(arm)))
    return _value != 0
#else
    return _value
#endif
  }

  /// Create an instance initialized to `value`.
  public init(booleanLiteral value: Bool) {
    self.init(value)
  }

  func withBoolValue(_ f : (Bool) -> ()) {
    return f(self.boolValue)
  } 
}

#if canImport(Swift)
func topLevelFunction() {
  LibraryDependentBool(true).withBoolValue { b in
    let value: String
#if canImport(Swiftz)
#if canImport(ReactiveCocoa)
#if canImport(Darwin)
    value = NSObject() // Type error
#endif
#endif
#else
    value = ""
#endif
    print(value)
  }
}
#else
enum LibraryDependentBool {} // This should not happen
#endif

#if WITH_PERFORM
func performPerOS() -> Int {
  let value: Int
#if canImport(Argo)
  value = "" // Type error
#else
  value = 42
#endif
  return performFoo(withX: value, andY: value)
}
#endif

let osName: String = {
#if os(iOS)
  return "iOS"
#elseif os(watchOS)
  return "watchOS"
#elseif os(tvOS)
  return "tvOS"
#elseif os(OSX)
  return "OS X"
#elseif os(Linux)
  return "Linux"
#else
  return "Unknown"
#endif
}()
