// RUN: %target-typecheck-verify-swift

// REQUIRES: OS=macosx

struct A {} // expected-note * {{found this candidate}}
struct B {} // expected-note * {{found this candidate}}

func ambiguousInFarFuture(_: A) {}

@available(macOS 99, *)
func ambiguousInFarFuture(_: B) {}

struct S {
  func ambiguousInFarFuture(_: A) {}
}

@available(macOS 99, *)
extension S {
  func ambiguousInFarFuture(_: B) {}
}

func testDeploymentTarget(_ s: S) {
  ambiguousInFarFuture(.init())
  s.ambiguousInFarFuture(.init())
}

@available(macOS 99, *)
func testFarFuture(_ s: S) {
  ambiguousInFarFuture(.init()) // expected-error {{ambiguous use of 'init()'}}
  s.ambiguousInFarFuture(.init()) // expected-error {{ambiguous use of 'init()'}}
}

@available(macOS, unavailable)
func testUnavailable(_ s: S) {
  ambiguousInFarFuture(.init()) // expected-error {{ambiguous use of 'init()'}}
  s.ambiguousInFarFuture(.init()) // expected-error {{ambiguous use of 'init()'}}
}
