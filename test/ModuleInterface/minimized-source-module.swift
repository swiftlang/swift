// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Step 1: Compile original sources to module + all outputs in one invocation
// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   %t/Types.swift %t/Uses.swift \
// RUN:   -module-name TestMod -enable-library-evolution \
// RUN:   -package-name TestPkg \
// RUN:   -emit-module-interface-path %t/Original.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Original.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Original.package.swiftinterface \
// RUN:   -emit-tbd-path %t/Original.tbd -tbd-install_name TestMod \
// RUN:   -emit-objc-header-path %t/Original.h \
// RUN:   -o %t/Original.swiftmodule

// Step 2: Minimize each source file
// RUN: %swift-interface-tool -action minimize-source %t/Types.swift > %t/Types.min.swift
// RUN: %swift-interface-tool -action minimize-source %t/Uses.swift > %t/Uses.min.swift

// Step 3: Compile minimized sources with -parse-as-interface, same outputs
// RUN: %target-swift-frontend -emit-module -parse-as-interface \
// RUN:   %t/Types.min.swift %t/Uses.min.swift \
// RUN:   -module-name TestMod -enable-library-evolution \
// RUN:   -package-name TestPkg \
// RUN:   -emit-module-interface-path %t/Minimized.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Minimized.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Minimized.package.swiftinterface \
// RUN:   -emit-tbd-path %t/Minimized.tbd -tbd-install_name TestMod \
// RUN:   -emit-objc-header-path %t/Minimized.h \
// RUN:   -o %t/Minimized.swiftmodule

// Step 4: Compare all outputs
// RUN: diff %t/Original.swiftinterface %t/Minimized.swiftinterface
// RUN: diff %t/Original.private.swiftinterface %t/Minimized.private.swiftinterface
// RUN: diff %t/Original.package.swiftinterface %t/Minimized.package.swiftinterface
// RUN: diff %t/Original.tbd %t/Minimized.tbd
// RUN: diff %t/Original.h %t/Minimized.h

// Test: -parse-as-interface rejects actions that require function bodies
// RUN: not %target-swift-frontend -emit-object -parse-as-interface %t/Types.min.swift \
// RUN:   -module-name TestMod 2>&1 | %FileCheck --check-prefix=CHECK-ERROR %s
// RUN: not %target-swift-frontend -emit-ir -parse-as-interface %t/Types.min.swift \
// RUN:   -module-name TestMod 2>&1 | %FileCheck --check-prefix=CHECK-ERROR %s
// RUN: not %target-swift-frontend -emit-sil -parse-as-interface %t/Types.min.swift \
// RUN:   -module-name TestMod 2>&1 | %FileCheck --check-prefix=CHECK-ERROR %s
// CHECK-ERROR: error: the requested action is not compatible with '-parse-as-interface'

// Test: -parse-as-interface -typecheck should succeed with multiple files
// RUN: %target-swift-frontend -typecheck -parse-as-interface \
// RUN:   %t/Types.min.swift %t/Uses.min.swift \
// RUN:   -module-name TestMod -package-name TestPkg

// REQUIRES: objc_interop
// REQUIRES: swift_swift_parser

//--- Types.swift
import Foundation

// --- Access levels ---

// Public struct with stored properties
public struct Point {
  public var x: Double
  public var y: Double

  public init(x: Double, y: Double) {
    self.x = x
    self.y = y
  }
}

// Package-level declarations (visible in package.swiftinterface)
package struct PackageConfig {
  package var name: String
  package var version: Int

  package init(name: String, version: Int) {
    self.name = name
    self.version = version
  }
}

package func packageHelper() -> Int {
  return 42
}

// Internal declarations (not visible in any interface)
struct InternalData {
  var value: Int
}

func internalHelper() -> Int {
  return 0
}

// Private declarations (not visible in any interface)
private struct PrivateData {
  var secret: Int
}

private func privateHelper() -> Int {
  return -1
}

// --- Enums and synthesis ---

// Simple enum (tests synthesized Equatable/Hashable)
public enum Direction {
  case north
  case south
  case east
  case west
}

// Frozen enum
@frozen public enum Ordering {
  case ascending
  case equal
  case descending
}

// Enum with raw type
public enum Planet: Int {
  case mercury = 1
  case venus
  case earth
}

// --- Generics ---

// Generic enum with constraints
public enum Outcome<Success, Failure: Error> {
  case success(Success)
  case failure(Failure)
}

// Generic struct
public struct Pair<First, Second> {
  public var first: First
  public var second: Second

  public init(first: First, second: Second) {
    self.first = first
    self.second = second
  }
}

// --- Protocols ---

// Protocol with associated type
public protocol Shape {
  associatedtype Unit: Numeric
  var area: Unit { get }
  func scaled(by factor: Unit) -> Self
}

public protocol Describable {
  var description: String { get }
}

// --- SPI declarations (visible in private.swiftinterface) ---

// SPI protocol
@_spi(Testing) public protocol TestOnly {
  func runTest() -> Bool
}

// SPI struct
@_spi(Testing) public struct TestConfig {
  @_spi(Testing) public var verbose: Bool

  @_spi(Testing) public init(verbose: Bool) {
    self.verbose = verbose
  }
}

//--- Uses.swift
import Foundation

// --- Cross-file type references ---

// Public type alias referencing Pair from Types.swift
public typealias IntPair = Pair<Int, Int>

// Class conforming to Shape protocol from Types.swift
public class Circle: Shape {
  public typealias Unit = Double
  public var radius: Double

  public init(radius: Double) {
    self.radius = radius
  }

  public var area: Double {
    return .pi * radius * radius
  }

  public func scaled(by factor: Double) -> Self {
    fatalError()
  }
}

// Extension on Pair (from Types.swift) with constrained conformance
extension Pair: Describable where First: Describable, Second: Describable {
  public var description: String {
    return "(\(first.description), \(second.description))"
  }
}

// Function taking Point (from Types.swift) as parameter
public func distance(from p1: Point, to p2: Point) -> Double {
  let dx = p1.x - p2.x
  let dy = p1.y - p2.y
  return (dx * dx + dy * dy).squareRoot()
}

// Function returning Outcome (from Types.swift)
public func parseDirection(_ string: String) -> Outcome<Direction, ParseError> {
  switch string {
  case "north": return .success(.north)
  case "south": return .success(.south)
  case "east": return .success(.east)
  case "west": return .success(.west)
  default:
    return .failure(ParseError.invalidInput)
  }
}

public enum ParseError: Error {
  case invalidInput
}

// Package function referencing cross-file package type
package func makeConfig() -> PackageConfig {
  return PackageConfig(name: "test", version: 1)
}

// @objc class
public class ObjCCompatible: NSObject {
  @objc public var name: String = ""

  @objc public func greet() -> String {
    return "Hello, \(name)"
  }
}

// --- Functions ---

// @inlinable function (body preserved in minimized output)
@inlinable
public func add(_ a: Int, _ b: Int) -> Int {
  return a + b
}

// Generic function with constraints
public func transform<T, U>(_ value: T, using f: (T) -> U) -> U {
  return f(value)
}

// Regular public function (body stripped)
public func multiply(_ a: Int, _ b: Int) -> Int {
  return a * b
}

// --- SPI declarations (visible in private.swiftinterface) ---

// SPI function referencing cross-file SPI type
@_spi(Testing) public func makeTestConfig() -> TestConfig {
  return TestConfig(verbose: true)
}

// SPI conformance on public type referencing cross-file SPI protocol
@_spi(Testing) extension Circle: TestOnly {
  @_spi(Testing) public func runTest() -> Bool {
    return area > 0
  }
}
