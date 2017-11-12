// RUN: %target-swift-frontend -typecheck -swift-version 4 %s

// These are in a separate file -- the absence of diagnostics causes the
// AST verifier to check additional invariants

func takesAnyType(_: Any.Type) {}

class Base {}
class Derived : Base {}

let b: Base = Derived()

_ = [b].filter { type(of: $0) == Derived.self }

// Trailing closure...
let _: (() -> ()).Type = type { }
