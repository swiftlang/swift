// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library -enable-experimental-feature Reparenting
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s --check-prefixes=CHECK --implicit-check-not '$Reparenting' < %t/Library.swiftinterface

// REQUIRES: swift_feature_Reparenting

// CHECK: #if compiler(>=5.3) && $Reparenting
// CHECK-NEXT: @reparentable public protocol Circle {
@reparentable
public protocol Circle {
  associatedtype Color
  func fill() -> Color
}

// CHECK: #if compiler(>=5.3) && $Reparenting
// CHECK-NEXT: extension Library::Circle where Self.Color == Swift::String {
extension Circle where Color == String {
  public func fill() -> Color { "blue" }
}

// CHECK: #if compiler(>=5.3) && $Reparenting
// CHECK-NEXT: public protocol Ball : Library::Circle {
// CHECK:         associatedtype Color = Swift::Int
public protocol Ball: Circle {
  associatedtype Color = Int
  func roll()
}

// CHECK: #if compiler(>=5.3) && $Reparenting
// CHECK-NEXT: extension Library::Ball : @reparented Library::Circle where Self.Color == Swift::Int {
// CHECK:         public func fill() -> Self.Color
// CHECK-NOT:     typealias
extension Ball: @reparented Circle where Color == Int {
  public func fill() -> Color { return 0xFFFD74 }
}

public struct Oval {}

extension Oval: Sendable {}

// CHECK: #if compiler(>=5.3) && $Reparenting
// CHECK-NEXT: extension Library::Oval : Library::Circle {
extension Oval: Circle {
  public typealias Color = Int
  public func fill() -> Color { 0 }
}

// No need to become viral and guard mentions of protocols downstream of a reparentable one.
// The expected failure condition is that they'll be missing from the interface according to old compilers.
public struct Sun: Ball {
  public typealias Color = Int
  public func fill() -> Color { 0 }
  public func roll() {}
}

public struct Moon {}

extension Moon: Ball {
  public typealias Color = Int
  public func fill() -> Color { 0 }
  public func roll() {}
}

// CHECK: #if compiler(>=5.3) && $Reparenting
// CHECK-NEXT: public func guarded<T>(_: T.Type) where T : Library::Circle
public func guarded<T>(_: T.Type) where T: Circle {}

// CHECK: #if compiler(>=5.3) && $Reparenting
// CHECK-NEXT: public func guarded2<T>(_: T.Type) where T : Library::Circle
public func guarded2<T: Circle>(_: T.Type) {}

public func notGuarded<T: Ball>(_: T.Type) {}


public protocol Unrelated {}

// CHECK: #if compiler(>=5.3) && $Reparenting
// CHECK-NEXT: extension Library::Unrelated where Self : Library::Circle {
extension Unrelated where Self: Circle {
  public func star() {}
}

public protocol Lake {
// CHECK:   #if compiler(>=5.3) && $Reparenting
// CHECK-NEXT: associatedtype Shape : Library::Circle
  associatedtype Shape: Circle
  associatedtype Depth: Comparable
}
