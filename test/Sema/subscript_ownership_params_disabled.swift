// RUN: %target-typecheck-verify-swift

struct NC: ~Copyable {
  var value: Int
}

struct S {
  var slots: [Int] = [0]

  subscript(b i: borrowing Int) -> Int { return slots[i] }
  // expected-error@-1 {{'borrowing' may only be used on function or initializer parameters}}

  subscript(c i: consuming Int) -> Int { return slots[i] }
  // expected-error@-1 {{'consuming' may only be used on function or initializer parameters}}

  subscript(io i: inout Int) -> Int { return slots[i] }
  // expected-error@-1 {{'inout' may only be used on function or initializer parameters}}

  // A noncopyable index has no ownership specifier available to it, so there is
  // nothing to suggest here.
  subscript(nc n: NC) -> Int { return slots[n.value] }
  // expected-error@-1 {{subscripts cannot have noncopyable parameters yet}}

  // `inout` within the index type is still fine: that is a function type, not
  // the parameter itself.
  subscript(fn f: (inout Int) -> ()) -> Int { return 0 } // ok
}

func passInOut(s: S, i: inout Int) -> Int {
  return s[b: &i]
  // expected-error@-1 {{cannot pass an inout argument to a subscript; use 'withUnsafeMutablePointer' to explicitly convert argument to a pointer}}
}
