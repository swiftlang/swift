//===--- DifferentiationUtilities.swift -----------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Utilities for creating differentiable functions, debugging, and customizing
// derivatives.
//
//===----------------------------------------------------------------------===//

import Swift

//===----------------------------------------------------------------------===//
// Derivative customization
//===----------------------------------------------------------------------===//

/// Returns `x` like an identity function. When used in a context where `x` is
/// being differentiated with respect to, this function will not produce any 
/// derivative at `x`.
@inlinable
@inline(__always)
@_semantics("autodiff.nonvarying")
public func withoutDerivative<T>(at x: T) -> T {
  x
}

//===----------------------------------------------------------------------===//
// Diagnostics
//===----------------------------------------------------------------------===//

@_silgen_name("_fatalErrorForwardModeDifferentiationDisabled")
public func _fatalErrorForwardModeDifferentiationDisabled() -> Never {
  fatalError("""
    JVP does not exist. Use \
    '-Xfrontend -enable-experimental-forward-mode-differentiation' to enable \
    differential-first differentiation APIs.
    """)
}
