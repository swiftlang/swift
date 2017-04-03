// RUN: %target-swift-frontend %s -emit-ir -verify -enable-sil-ownership

// Verify we don't crash on this.
// rdar://15595118
infix operator ~>
protocol Target {}

func ~> <Target, Arg0, Result>(x: inout Target, f: @escaping (_: inout Target, _: Arg0) -> Result) -> (Arg0) -> Result {
  return { f(&x, $0) } // expected-error {{escaping closures can only capture inout parameters explicitly by value}}
}

func ~> (x: inout Int, f: @escaping (_: inout Int, _: Target) -> Target) -> (Target) -> Target {
  return { f(&x, $0) } // expected-error {{escaping closures can only capture inout parameters explicitly by value}}
}

