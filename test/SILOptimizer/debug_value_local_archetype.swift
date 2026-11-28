// RUN: %target-swift-frontend -emit-sil -O -parse-stdlib -module-name Test -sil-verify-all %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -O -parse-stdlib -module-name Test -enable-sil-opaque-values -sil-verify-all %s -o /dev/null

// The verifier must accept a `debug_value undef : $()` for a variable whose
// alloc_stack has a type containing a local (opened) archetype.
//
// Once `open` is inlined into `test`, `x` is an alloc_stack of
// `@thick (@opened(...) Self).Type`. DeadStoreElimination leaves a debug_value
// for `x` next to that alloc_stack. When SILCombine then erases the
// instruction that debug_value uses, killDebugUses replaces it with
// `debug_value undef : $()`, because a variable with a local archetype in its
// type cannot be described by an operand. SILCloner replaces debug_values
// the same way when it inlines. The alloc_stack keeps its type, and the
// verifier rejected the two as "Two variables with different type but same
// scope".

public protocol P {}

func mutate<T>(_: inout T) {}

public func test(_ e: any P) {
  @inline(__always) func open<B: P>(_: B) { var x = B.self; mutate(&x) }
  open(e)
}
