// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library -enable-experimental-feature Reparenting
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s --check-prefixes=CHECK < %t/Library.swiftinterface

// REQUIRES: swift_feature_Reparenting

// CHECK-LABEL: @reparentable public protocol Circle {

@reparentable
public protocol Circle {
  associatedtype Color
  func fill() -> Color
}

// CHECK-LABEL: public protocol Ball : Library::Circle {
// CHECK:         associatedtype Color = Swift::Int
public protocol Ball: Circle {
  associatedtype Color = Int
  func roll()
}

// CHECK-LABEL: extension Library::Ball : @reparented Library::Circle where Self.Color == Swift::Int {
// CHECK:         public func fill() -> Self.Color
// CHECK-NOT:     typealias
extension Ball: @reparented Circle where Color == Int {
  public func fill() -> Color { return 0xFFFD74 }
}
