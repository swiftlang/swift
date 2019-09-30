// RUN: %target-typecheck-verify-swift

typealias Not<A> = (A) -> Never
typealias And<A, B> = (left: A, right: B)
indirect enum Or<A, B> {
  case inl(A)
  case inr(B)
}

// FIXME: Port non_nominal_no_initializers diagnostic
func deMorgan<A, B>(_ ne: Not<Or<A, B>>) -> And<Not<A>, Not<B>> {
  return And<Not<A>, Not<B>>(
    Not<A> { a in ne(.left(a)) },
    // expected-error@-1 {{type 'Not<A>' (aka '(A) -> Never') has no member 'init'}}
    // expected-error@-2 {{type 'Or<A, B>' has no member 'left'}}
    Not<B> { a in ne(.right(a)) }
    // expected-error@-1 {{type 'Not<B>' (aka '(B) -> Never') has no member 'init'}}
    // expected-error@-2 {{type 'Or<A, B>' has no member 'right'}}
  )
}

