// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module \
// RUN:     -enable-experimental-feature Lifetimes \
// RUN:     -enable-experimental-feature Lifetimes \
// RUN:     -o %t/lifetime_underscored_dependence.swiftmodule \
// RUN:     -emit-module-interface-path %t/lifetime_underscored_dependence.swiftinterface \
// RUN:     %S/Inputs/lifetime_underscored_dependence.swift

// Check the interfaces

// RUN: %FileCheck %s < %t/lifetime_underscored_dependence.swiftinterface

// See if we can compile a module through just the interface and typecheck using it.

// RUN: %target-swift-frontend -compile-module-from-interface \
// RUN:    -enable-experimental-feature Lifetimes \
// RUN:    %t/lifetime_underscored_dependence.swiftinterface -o %t/lifetime_underscored_dependence.swiftmodule

// RUN: %target-swift-frontend -typecheck -I %t %s \
// RUN:    -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

import lifetime_underscored_dependence
// CHECK:  #if compiler(>=5.3) && $Lifetimes
// CHECK:  @_lifetime(borrow a)
// CHECK:  @inlinable internal init(_ ptr: Swift.UnsafeRawBufferPointer, _ a: borrowing Swift.Array<Swift.Int>) {
// CHECK:    let bv = BufferView(ptr, a.count)
// CHECK:    self = _overrideLifetime(bv, borrowing: a)
// CHECK:  }
// CHECK:  #else
// CHECK:  @lifetime(borrow a)
// CHECK:  @inlinable internal init(_ ptr: Swift.UnsafeRawBufferPointer, _ a: borrowing Swift.Array<Swift.Int>) {
// CHECK:    let bv = BufferView(ptr, a.count)
// CHECK:    self = _overrideLifetime(bv, borrowing: a)
// CHECK:  }
// CHECK:  #endif

// CHECK:  #if compiler(>=5.3) && $Lifetimes
// CHECK:  @_lifetime(copy a)
// CHECK:  @inlinable internal init(_ ptr: Swift.UnsafeRawBufferPointer, _ a: consuming lifetime_underscored_dependence.AnotherView) {
// CHECK:    let bv = BufferView(ptr, a._count)
// CHECK:    self = _overrideLifetime(bv, copying: a)
// CHECK:  }
// CHECK:  #else
// CHECK:  @lifetime(copy a)
// CHECK:  @inlinable internal init(_ ptr: Swift.UnsafeRawBufferPointer, _ a: consuming lifetime_underscored_dependence.AnotherView) {
// CHECK:    let bv = BufferView(ptr, a._count)
// CHECK:    self = _overrideLifetime(bv, copying: a)
// CHECK:  }
// CHECK:  #endif

// CHECK:#if compiler(>=5.3) && $Lifetimes
// CHECK:@_lifetime(copy x)
// CHECK:@inlinable public func derive(_ x: consuming lifetime_underscored_dependence.BufferView) -> lifetime_underscored_dependence.BufferView {
// CHECK:  let pointer = x._ptr
// CHECK:  let bv = BufferView(pointer, x._count)
// CHECK:  return _overrideLifetime(bv, copying: x)
// CHECK:}
// CHECK:#else
// CHECK:@lifetime(copy x)
// CHECK:@inlinable public func derive(_ x: consuming lifetime_underscored_dependence.BufferView) -> lifetime_underscored_dependence.BufferView {
// CHECK:  let pointer = x._ptr
// CHECK:  let bv = BufferView(pointer, x._count)
// CHECK:  return _overrideLifetime(bv, copying: x)
// CHECK:}
// CHECK:#endif

// CHECK:#if compiler(>=5.3) && $Lifetimes
// CHECK:@_lifetime(copy view)
// CHECK:@inlinable public func consumeAndCreate(_ view: consuming lifetime_underscored_dependence.BufferView) -> lifetime_underscored_dependence.BufferView {
// CHECK:  let pointer = view._ptr
// CHECK:  let bv = BufferView(pointer, view._count)
// CHECK:  return _overrideLifetime(bv, copying: view)
// CHECK:}
// CHECK:#else
// CHECK:@lifetime(copy view)
// CHECK:@inlinable public func consumeAndCreate(_ view: consuming lifetime_underscored_dependence.BufferView) -> lifetime_underscored_dependence.BufferView {
// CHECK:  let pointer = view._ptr
// CHECK:  let bv = BufferView(pointer, view._count)
// CHECK:  return _overrideLifetime(bv, copying: view)
// CHECK:}
// CHECK:#endif

// CHECK:#if compiler(>=5.3) && $Lifetimes
// CHECK:@_lifetime(copy this, copy that)
// CHECK:@inlinable public func deriveThisOrThat(_ this: consuming lifetime_underscored_dependence.BufferView, _ that: consuming lifetime_underscored_dependence.BufferView) -> lifetime_underscored_dependence.BufferView {
// CHECK:  if (Int.random(in: 1..<100) == 0) {
// CHECK:    let thisView = BufferView(this._ptr, this._count)
// CHECK:    return _overrideLifetime(thisView, copying: this)
// CHECK:  }
// CHECK:  let thatView = BufferView(that._ptr, that._count)
// CHECK:  return _overrideLifetime(thatView, copying: that)
// CHECK:}
// CHECK:#else
// CHECK:@lifetime(copy this, copy that)
// CHECK:@inlinable public func deriveThisOrThat(_ this: consuming lifetime_underscored_dependence.BufferView, _ that: consuming lifetime_underscored_dependence.BufferView) -> lifetime_underscored_dependence.BufferView {
// CHECK:  if (Int.random(in: 1..<100) == 0) {
// CHECK:    let thisView = BufferView(this._ptr, this._count)
// CHECK:    return _overrideLifetime(thisView, copying: this)
// CHECK:  }
// CHECK:  let thatView = BufferView(that._ptr, that._count)
// CHECK:  return _overrideLifetime(thatView, copying: that)
// CHECK:}
// CHECK:#endif

// Check that an implicitly dependent variable accessor is guarded by LifetimeDependence.
//
// CHECK: extension lifetime_underscored_dependence.Container {
// CHECK-NEXT: #if compiler(>=5.3) && $NonescapableTypes && $LifetimeDependence
// CHECK-NEXT:   public var storage: lifetime_underscored_dependence.BufferView {
