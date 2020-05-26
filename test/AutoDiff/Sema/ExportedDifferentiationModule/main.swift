// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/exports_differentiation.swift -o %t/exports_differentiation.swiftmodule
// RUN: %target-build-swift %s -I %t

// Test whether importing a module with `@_exported import _Differentiation`
// enables differentiable programming. This behavior is desirable.

import exports_differentiation

@differentiable
func id<T: Differentiable>(_ x: T) -> T { x }
