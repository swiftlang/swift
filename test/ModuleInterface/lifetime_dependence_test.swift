// RUN: %empty-directory(%t)

// FIXME: Remove '-disable-experimental-parser-round-trip' (rdar://137636751).

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module \
// RUN:     -enable-experimental-feature NonescapableTypes \
// RUN:     -o %t/lifetime_dependence.swiftmodule \
// RUN:     -emit-module-interface-path %t/lifetime_dependence.swiftinterface \
// RUN:     -disable-experimental-parser-round-trip \
// RUN:     %S/Inputs/lifetime_dependence.swift
// REQUIRES: asserts

// Check the interfaces

// RUN: %FileCheck %s < %t/lifetime_dependence.swiftinterface

// See if we can compile a module through just the interface and typecheck using it.

// RUN: %target-swift-frontend -compile-module-from-interface \
// RUN:    -enable-experimental-feature NonescapableTypes \
// RUN:    -disable-experimental-parser-round-trip \
// RUN:    %t/lifetime_dependence.swiftinterface -o %t/lifetime_dependence.swiftmodule

// RUN: %target-swift-frontend -typecheck -I %t %s \
// RUN:    -disable-experimental-parser-round-trip \
// RUN:    -enable-experimental-feature NonescapableTypes

import lifetime_dependence
// CHECK: @lifetime(borrow a)
// CHECK-NEXT: @inlinable internal init(_ ptr: Swift.UnsafeRawBufferPointer, _ a: borrowing Swift.Array<Swift.Int>) {
// CHECK: @lifetime(a)
// CHECK-NEXT: @inlinable internal init(_ ptr: Swift.UnsafeRawBufferPointer, _ a: consuming lifetime_dependence.AnotherView) {

// CHECK: @lifetime(x)
// CHECK-NEXT: @inlinable public func derive(_ x: consuming lifetime_dependence.BufferView) -> lifetime_dependence.BufferView {

// CHECK: @lifetime(view)
// CHECK-NEXT: @inlinable public func consumeAndCreate(_ view: consuming lifetime_dependence.BufferView) -> lifetime_dependence.BufferView {

// CHECK: @lifetime(this, that)
// CHECK-NEXT: @inlinable public func deriveThisOrThat(_ this: consuming lifetime_dependence.BufferView, _ that: consuming lifetime_dependence.BufferView) -> lifetime_dependence.BufferView {
