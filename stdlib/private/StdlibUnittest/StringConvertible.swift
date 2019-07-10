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


/// A type that has different `CustomStringConvertible` and
/// `CustomDebugStringConvertible` representations.
///
/// This type also conforms to other protocols, to make it
/// usable in constrained contexts.  It is not intended to be a
/// minimal type that only conforms to certain protocols.
///
/// This type can be used to check that code uses the correct
/// kind of string representation.
public struct CustomPrintableValue
  : Equatable, Comparable, Hashable, Strideable
{
  public static var timesDescriptionWasCalled = ResettableValue(0)
  public static var timesDebugDescriptionWasCalled = ResettableValue(0)

  public static var descriptionImpl =
    ResettableValue<(_ value: Int, _ identity: Int) -> String>({
      (value: Int, identity: Int) -> String in
      if identity == 0 {
        return "(value: \(value)).description"
      } else {
        return "(value: \(value), identity: \(identity)).description"
      }
    })

  public static var debugDescriptionImpl =
    ResettableValue<(_ value: Int, _ identity: Int) -> String>({
      (value: Int, identity: Int) -> String in
      CustomPrintableValue.timesDescriptionWasCalled.value += 1
      if identity == 0 {
        return "(value: \(value)).debugDescription"
      } else {
        return "(value: \(value), identity: \(identity)).debugDescription"
      }
    })


  public var value: Int
  public var identity: Int

  public init(_ value: Int) {
    self.value = value
    self.identity = 0
  }

  public init(_ value: Int, identity: Int) {
    self.value = value
    self.identity = identity
  }

  public var hashValue: Int {
    return value.hashValue
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

  public typealias Stride = Int

  public func distance(to other: CustomPrintableValue) -> Stride {
    return other.value - self.value
  }

  public func advanced(by n: Stride) -> CustomPrintableValue {
    return CustomPrintableValue(self.value + n, identity: self.identity)
  }
}

public func == (
  lhs: CustomPrintableValue,
  rhs: CustomPrintableValue
) -> Bool {
  return lhs.value == rhs.value
}

public func < (
  lhs: CustomPrintableValue,
  rhs: CustomPrintableValue
) -> Bool {
  return lhs.value < rhs.value
}

extension CustomPrintableValue : CustomStringConvertible {
  public var description: String {
    CustomPrintableValue.timesDescriptionWasCalled.value += 1
    return CustomPrintableValue.descriptionImpl.value(
      value, identity)
  }
}

extension CustomPrintableValue : CustomDebugStringConvertible {
  public var debugDescription: String {
    CustomPrintableValue.timesDebugDescriptionWasCalled.value += 1
    return CustomPrintableValue.debugDescriptionImpl.value(
      value, identity)
  }
}

public func expectPrinted<T>(
  expectedOneOf patterns: [String], _ object: T,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  let actual = String(describing: object)
  if !patterns.contains(actual) {
    expectationFailure(
      "expected: any of \(String(reflecting: patterns))\n"
      + "actual: \(String(reflecting: actual))",
      trace: message(),
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))
  }
}

public func expectPrinted<T>(
  _ expected: String, _ object: T,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  expectPrinted(expectedOneOf: [expected], object, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))
}

public func expectDebugPrinted<T>(
  expectedOneOf patterns: [String], _ object: T,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  expectPrinted(expectedOneOf: patterns, String(reflecting: object), message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))
}

public func expectDebugPrinted<T>(
  _ expected: String, _ object: T,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  expectDebugPrinted(expectedOneOf: [expected], object, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))
}

public func expectDumped<T>(
  _ expected: String, _ object: T,
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  var actual = ""
  dump(object, to: &actual)
  expectEqual(expected, actual, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))
}
