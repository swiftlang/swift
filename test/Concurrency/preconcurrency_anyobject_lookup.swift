// RUN: %target-swift-emit-silgen -verify -disable-objc-attr-requires-foundation-module %s
// RUN: %target-swift-emit-silgen -verify -swift-version 6 -disable-objc-attr-requires-foundation-module %s

// REQUIRES: objc_interop

@objc class C {
  @preconcurrency @objc func foo(_ x: Sendable) {}
}

func bar(_ fn: (Any) -> Void) {}
func bar(_ fn: (Sendable) -> Void) {}

// Make sure we can handle both the implicit unwrap and concurrency adjustment.
func foo(_ x: AnyObject) {
  bar(x.foo)
  let _ = AnyObject.foo
}
