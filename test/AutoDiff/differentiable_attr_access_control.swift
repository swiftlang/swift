// RUN: %target-swift-frontend -typecheck -verify %s

// If the original function is "exported" (public or @usableFromInline), then
// its primal/adjoint must also be exported.

// Ok: all public.
@differentiable(reverse, adjoint: dfoo1(_:primal:seed:))
public func foo1(_ x: Float) -> Float { return 1 }
public func dfoo1(_ x: Float, primal: Float, seed: Float) -> Float { return 1 }

// Ok: all internal.
struct CheckpointsFoo {}
@differentiable(reverse, primal: pfoo2(_:), adjoint: dfoo2(_:checkpoints:originalValue:seed:))
func foo2(_ x: Float) -> Float { return 1 }
func pfoo2(_ x: Float) -> (checkpoints: CheckpointsFoo, originalValue: Float) { return (CheckpointsFoo(), 1) }
func dfoo2(_ x: Float, checkpoints: CheckpointsFoo, originalValue: Float, seed: Float) -> Float { return 1 }

// Ok: all private.
@differentiable(reverse, adjoint: dfoo3(_:primal:seed:))
private func foo3(_ x: Float) -> Float { return 1 }
private func dfoo3(_ x: Float, primal: Float, seed: Float) -> Float { return 1 }

// Error: adjoint not exported.
@differentiable(reverse, adjoint: dbar1(_:primal:seed:)) // expected-error {{associated differentiation function 'dbar1(_:primal:seed:)' is required to either be public or @usableFromInline because the original function 'bar1' is public or @usableFromInline}}
public func bar1(_ x: Float) -> Float { return 1 }
private func dbar1(_ x: Float, primal: Float, seed: Float) -> Float { return 1 }

// Error: primal not exported.
@differentiable(reverse, primal: pbar2(_:), adjoint: dbar2(_:checkpoints:originalValue:seed:)) // expected-error {{associated differentiation function 'pbar2' is required to either be public or @usableFromInline because the original function 'bar2' is public or @usableFromInline}}
@usableFromInline func bar2(_ x: Float) -> Float { return 1 }
func pbar2(_ x: Float) -> (checkpoints: CheckpointsFoo, originalValue: Float) { return (CheckpointsFoo(), 1) }
func dbar2(_ x: Float, checkpoints: CheckpointsFoo, originalValue: Float, seed: Float) -> Float { return 1 }
