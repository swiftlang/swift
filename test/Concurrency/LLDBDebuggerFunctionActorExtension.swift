// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -debugger-support
// REQUIRES: concurrency

// This test simulates LLDB's expression evaluator makeing an otherwise illegal
// synchronous call into an extension of an actor, as it would to run `p n` in
// this example.

actor A {
  var n : Int = 0
}

extension A {
  final func lldb_wrapped_expr() {
    n = 1
  }
}

@LLDBDebuggerFunction
func lldb_expr(a: A) {
  a.lldb_wrapped_expr()
}
