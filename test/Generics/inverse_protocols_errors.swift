// RUN: %target-typecheck-verify-swift



protocol RegularProto {}
protocol NCProto: RegularProto // expected-error{{'Self' required to be 'Copyable' but is marked with '~Copyable'}}
  where Self: ~Copyable {
  func checkIfCopyableSelf(_ s: Self)
}

protocol Hello: ~Copyable {
  func greet(_ s: Self)
  // expected-error@-1 {{parameter of noncopyable type 'Self' must specify ownership}}
  // expected-note@-2 {{add 'borrowing' for an immutable reference}}
  // expected-note@-3 {{add 'inout' for a mutable reference}}
  // expected-note@-4 {{add 'consuming' to take the value from the caller}}

  func salute(_ s: borrowing Self)
}

struct RegularStruct: Hello {
    func greet(_ s: Self) {}
    func salute(_ s: Self) {}
}

struct NCThinger<T: ~Copyable>: ~Copyable, Hello {
  let fd: Int = 0
  deinit {}

  func greet(_ f: Self) {}
  // expected-error@-1 {{parameter of noncopyable type 'NCThinger<T>' must specify ownership}}
  // expected-note@-2 {{add 'borrowing' for an immutable reference}}
  // expected-note@-3 {{add 'inout' for a mutable reference}}
  // expected-note@-4 {{add 'consuming' to take the value from the caller}}

  func salute(_ s: borrowing Self) {}

  func setThinger(_ t: T) {}
  // expected-error@-1 {{parameter of noncopyable type 'T' must specify ownership}}
  // expected-note@-2 {{add 'borrowing' for an immutable reference}}
  // expected-note@-3 {{add 'inout' for a mutable reference}}
  // expected-note@-4 {{add 'consuming' to take the value from the caller}}
}
