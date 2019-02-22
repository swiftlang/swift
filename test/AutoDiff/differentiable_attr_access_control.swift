// RUN: %target-swift-frontend -typecheck -verify %s

// If the original function is "exported" (public or @usableFromInline), then
// its JVP/VJP must also be exported.

// Ok: all public.
@differentiable(vjp: dfoo1)
public func foo1(_ x: Float) -> Float { return 1 }
public func dfoo1(x: Float) -> (Float, (Float) -> Float) { return (1, {$0}) }

// Ok: all internal.
struct CheckpointsFoo {}
@differentiable(vjp: dfoo2)
func foo2(_ x: Float) -> Float { return 1 }
func dfoo2(_ x: Float) -> (Float, (Float) -> Float) { return (1, {$0}) }

// Ok: all private.
@differentiable(vjp: dfoo3)
private func foo3(_ x: Float) -> Float { return 1 }
private func dfoo3(_ x: Float) -> (Float, (Float) -> Float) { return (1, {$0}) }

// Error: vjp not exported.
@differentiable(vjp: dbar1) // expected-error {{associated differentiation function 'dbar1' is required to either be public or @usableFromInline because the original function 'bar1' is public or @usableFromInline}}
public func bar1(_ x: Float) -> Float { return 1 }
private func dbar1(_ x: Float) -> (Float, (Float) -> Float) { return (1, {$0}) }
