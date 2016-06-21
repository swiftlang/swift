// RUN: %target-swift-frontend %s -emit-ir -verify

// Verify we don't crash on this.
// rdar://15595118
infix operator ~> { precedence 255 }
protocol Target {}

func ~> <Target, Arg0, Result>(x: inout Target, f: (_: inout Target, _: Arg0) -> Result) -> (Arg0) -> Result {
  return { f(&x, $0) } // expected-error {{closure cannot implicitly capture an inout parameter unless @noescape}}
}

func ~> (x: inout Int, f: (_: inout Int, _: Target) -> Target) -> (Target) -> Target {
  return { f(&x, $0) } // expected-error {{closure cannot implicitly capture an inout parameter unless @noescape}}
}

