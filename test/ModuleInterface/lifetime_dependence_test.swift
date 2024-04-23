// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module \
// RUN:     -enable-experimental-feature NoncopyableGenerics \
// RUN:     -enable-experimental-feature NonescapableTypes \
// RUN:     -o %t/lifetime_dependence.swiftmodule \
// RUN:     -emit-module-interface-path %t/lifetime_dependence.swiftinterface \
// RUN:     %S/Inputs/lifetime_dependence.swift
// REQUIRES: asserts

// Check the interfaces

// RUN: %FileCheck %s < %t/lifetime_dependence.swiftinterface

// See if we can compile a module through just the interface and typecheck using it.

// RUN: %target-swift-frontend -compile-module-from-interface \
// RUN:    -enable-experimental-feature NoncopyableGenerics \
// RUN:    -enable-experimental-feature NonescapableTypes \
// RUN:    %t/lifetime_dependence.swiftinterface -o %t/lifetime_dependence.swiftmodule

// RUN: %target-swift-frontend -typecheck -I %t %s \
// RUN:    -enable-experimental-feature NoncopyableGenerics \
// RUN:    -enable-experimental-feature NonescapableTypes

import lifetime_dependence

// CHECK: @inlinable internal init(_ ptr: Swift.UnsafeRawBufferPointer, _ a: borrowing Swift.Array<Swift.Int>) -> dependsOn(a) Self {
// CHECK: @inlinable internal init(_ ptr: Swift.UnsafeRawBufferPointer, _ a: consuming lifetime_dependence.AnotherView) -> dependsOn(a) Self {

// CHECK: @inlinable public func derive(_ x: consuming lifetime_dependence.BufferView) ->  dependsOn(x) lifetime_dependence.BufferView {

// CHECK: @inlinable public func consumeAndCreate(_ view: consuming lifetime_dependence.BufferView) ->  dependsOn(view) lifetime_dependence.BufferView {

// CHECK: @inlinable public func deriveThisOrThat(_ this: consuming lifetime_dependence.BufferView, _ that: consuming lifetime_dependence.BufferView) ->  dependsOn(this)  dependsOn(that) lifetime_dependence.BufferView {
