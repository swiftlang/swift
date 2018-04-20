//===--- AutoDiff.swift ---------------------------------------*- swift -*-===//
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
//
// SWIFT_ENABLE_TENSORFLOW
//
// This file defines support for automatic differentiation.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Compiler Protocols
//===----------------------------------------------------------------------===//

/// A type that represents a valid argument for automatic differentiation.
///
/// Types that conform to the `Differentiable` protocol can be differentiated
/// with-respect-to in `#gradient` and `#valueAndGradient` expressions.
///
/// Example:
///
///     struct Vector<Scalar> {
///         var elements: [Scalar]
///         init(_ elements: [Scalar]) { ... }
///     }
///
///     extension Vector: Numeric where Scalar: Numeric { ... }
///
///     extension Vector: Differentiable where Scalar: FloatingPoint {
///         associatedtype DifferentiationCurrency = Scalar
///
///         init(differentiationSeed: Scalar) {
///           self.init(differentiationSeed)
///         }
///
///         func combiningAsAdjoint(with newAdjoint: Self) -> Self {
///             return self + newAdjoint
///         }
///     }
///
public protocol Differentiable {
  /// The currency type in the mathematical model of differentiation. For
  /// example, the currency type of `Float` is `Float`, and the currency type
  /// of a vector of `Float` is still `Float`. The currency type is used to
  /// initialize intermediate values during automatic differentiation, such as
  /// the initial adjoint/tangent and the seed.
  associatedtype DifferentiationCurrency : FloatingPoint

  /// Creates a differentiation seed from a value of the currency type.
  ///
  /// - Parameter differentiationSeed: The seed.
  init(differentiationSeed: DifferentiationCurrency)

  /// Combining self with the given value as differentiated adjoint, producing
  /// a new adjoint.
  ///
  /// - Note: This will be used by the compiler during reverse-mode automatic
  /// differentiation, to combine back-propagated gradient values.
  func combiningAsAdjoint(with newAdjoint: Self) -> Self
}

public extension FloatingPoint {
  @_inlineable // FIXME(sil-serialize-all)
  @_transparent
  init(differentiationSeed: Self) {
    self = differentiationSeed
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_transparent
  func combiningAsAdjoint(with newAdjoint: Self) -> Self {
    return self + newAdjoint
  }
}

//===----------------------------------------------------------------------===//
// Differentiation Operators
//===----------------------------------------------------------------------===//

@_transparent @_versioned
func _gradientBodyUnreachable() {
  // This implementation is never used, since calls to `Swift.gradient(of:)` are
  // resolved as a special case by the type checker.
  Builtin.staticReport(_trueAfterDiagnostics(), true._value,
    ("internal consistency error: 'gradient(of:)' operation failed to resolve"
      as StaticString).utf8Start._rawValue)
  Builtin.unreachable()
}

@_transparent @_versioned
func _valueAndGradientBodyUnreachable() {
  // This implementation is never used, since calls to
  // `Swift.valueAndGradient(of:)` are resolved as a special case by the type
  // checker.
  Builtin.staticReport(_trueAfterDiagnostics(), true._value,
    ("internal consistency error: 'valueAndGradient(of:)' operation failed to resolve"
     as StaticString).utf8Start._rawValue)
  Builtin.unreachable()
}

@_inlineable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, Result>(of function: (T) -> Result) -> (T) -> T
  where T : Differentiable, Result : Differentiable {
  _gradientBodyUnreachable()
}

@_inlineable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, U, Result>(
  of function: (T, U) -> Result
) -> (T, U) -> (T, U)
  where T : Differentiable, U : Differentiable, Result : Differentiable {
  _gradientBodyUnreachable()
}

@_inlineable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, U, V, Result>(
  of function: (T, U, V) -> Result
) -> (T, U, V) -> (T, U, V)
  where T : Differentiable, U : Differentiable, V : Differentiable,
        Result : Differentiable {
  _gradientBodyUnreachable()
}

@_inlineable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.gradient(of:)")
public func gradient<T, U, V, W, Result>(
  of function: (T, U, V, W) -> Result
) -> (T, U, V, W) -> (T, U, V, W)
  where T : Differentiable, U : Differentiable, V : Differentiable,
        W : Differentiable, Result : Differentiable {
  _gradientBodyUnreachable()
}

@_inlineable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, Result>(
  of function: (T) -> Result
) -> (T) -> (value: Result, gradient: T)
  where T : Differentiable, Result : Differentiable {
  _valueAndGradientBodyUnreachable()
}

@_inlineable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, U, Result>(
  of function: (T, U) -> Result
) -> (T, U) -> (value: Result, gradient: (T, U))
  where T : Differentiable, U : Differentiable, Result : Differentiable {
  _valueAndGradientBodyUnreachable()
}

@_inlineable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, U, V, Result>(
  of function: (T, U, V) -> Result
) -> (T, U, V) -> (value: Result, gradient: (T, U, V))
  where T : Differentiable, U : Differentiable, V : Differentiable,
        Result : Differentiable {
  _valueAndGradientBodyUnreachable()
}

@_inlineable // FIXME(sil-serialize-all)
@_transparent
@_semantics("typechecker.valueAndGradient(of:)")
public func valueAndGradient<T, U, V, W, Result>(
  of function: (T, U, V, W) -> Result
) -> (T, U, V, W) -> (value: Result, gradient: (T, U, V, W))
  where T : Differentiable, U : Differentiable, V : Differentiable,
        W : Differentiable, Result : Differentiable {
  _valueAndGradientBodyUnreachable()
}

//===----------------------------------------------------------------------===//
// Runtime
//===----------------------------------------------------------------------===//

@_versioned
class _ADTape<Element> {
  @_versioned var elements: [Element] = []

  @inline(never)
  @_semantics("autodiff.create_tape")
  @_silgen_name("_swift_autodiff_CreateTape")
  init() {}
}

extension _ADTape {
  @_versioned
  var count: Int {
    @inline(never)
    @_semantics("autodiff.tape_element_count")
    @_silgen_name("_swift_autodiff_TapeElementCount")
    get {
      return elements.count
    }
  }

  @_versioned @inline(never)
  @_semantics("autodiff.push_to_tape")
  @_silgen_name("_swift_autodiff_PushToTape")
  func push(_ value: Element) {
    elements.append(value)
  }

  @_versioned @inline(never)
  @_semantics("autodiff.pop_from_tape")
  @_silgen_name("_swift_autodiff_PopFromTape")
  func pop() -> Element {
    guard let popped = elements.popLast() else {
      preconditionFailure("Cannot pop from an empty AD tape.")
    }
    return popped
  }
}
