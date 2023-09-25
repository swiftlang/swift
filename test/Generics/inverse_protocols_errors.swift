// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics

// REQUIRES: asserts

protocol RegularProto {}
protocol NCProto: ~Copyable, RegularProto {} // FIXME: ought to diagnose

protocol Hello: ~Copyable {
  func greet(_ s: Self) // FIXME: should complain about no ownership!
}

struct RegularStruct: Hello {
    func greet(_ s: Self) {}
}

struct NCThinger<T: ~Copyable>: ~Copyable, Hello {
  let fd: Int = 0
  deinit {}

  func greet(_ f: Self) {}
  // expected-error@-1 {{noncopyable parameter must specify its ownership}}
  // expected-note@-2 {{add 'borrowing' for an immutable reference}}
  // expected-note@-3 {{add 'inout' for a mutable reference}}
  // expected-note@-4 {{add 'consuming' to take the value from the caller}}

  func setThinger(_ t: T) {} // FIXME: should complain about no ownership!
}
