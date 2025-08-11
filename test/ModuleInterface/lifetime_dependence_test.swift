// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module \
// RUN:     -enable-experimental-feature LifetimeDependence \
// RUN:     -suppress-warnings \
// RUN:     -o %t/lifetime_dependence.swiftmodule \
// RUN:     -emit-module-interface-path %t/lifetime_dependence.swiftinterface \
// RUN:     %S/Inputs/lifetime_dependence.swift

// Check the interfaces

// RUN: %FileCheck %s < %t/lifetime_dependence.swiftinterface

// See if we can compile a module through just the interface and typecheck using it.

// RUN: %target-swift-frontend -compile-module-from-interface \
// RUN:    -enable-experimental-feature LifetimeDependence \
// RUN:    %t/lifetime_dependence.swiftinterface -o %t/lifetime_dependence.swiftmodule

// RUN: %target-swift-frontend -typecheck -I %t %s \
// RUN:    -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_feature_LifetimeDependence

import lifetime_dependence
// CHECK: @lifetime(borrow a)
// CHECK-NEXT: @inlinable internal init(_ ptr: Swift.UnsafeRawBufferPointer, _ a: borrowing Swift.Array<Swift.Int>) {
// CHECK: @lifetime(copy a)
// CHECK-NEXT: @inlinable internal init(_ ptr: Swift.UnsafeRawBufferPointer, _ a: consuming lifetime_dependence.AnotherView) {

// CHECK: @lifetime(copy x)
// CHECK-NEXT: @inlinable public func derive(_ x: consuming lifetime_dependence.BufferView) -> lifetime_dependence.BufferView {

// CHECK: @lifetime(copy view)
// CHECK-NEXT: @inlinable public func consumeAndCreate(_ view: consuming lifetime_dependence.BufferView) -> lifetime_dependence.BufferView {

// CHECK: @lifetime(copy this, copy that)
// CHECK-NEXT: @inlinable public func deriveThisOrThat(_ this: consuming lifetime_dependence.BufferView, _ that: consuming lifetime_dependence.BufferView) -> lifetime_dependence.BufferView {

// Check that an implicitly dependent variable accessor is guarded by LifetimeDependence.
//
// CHECK: extension lifetime_dependence.Container {
// CHECK-NEXT: #if compiler(>=5.3) && $LifetimeDependence
// CHECK-NEXT:   public var storage: lifetime_dependence.BufferView {

// CHECK-LABEL: extension Swift.UnsafeMutableBufferPointer where Element : ~Copyable {
// CHECK:   #if compiler(>=5.3) && $LifetimeDependence
// CHECK:   public var span: Swift.Span<Element> {
// CHECK:     @lifetime(borrow self)
// CHECK:     @_alwaysEmitIntoClient get {
// CHECK:   #if compiler(>=5.3) && $NonescapableAccessorOnTrivial && $LifetimeDependence
// CHECK:   public var mutableSpan: Swift.MutableSpan<Element> {
// CHECK:     @lifetime(borrow self)
// CHECK:     @_alwaysEmitIntoClient get {
