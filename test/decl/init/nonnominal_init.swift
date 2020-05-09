// RUN: %target-typecheck-verify-swift

typealias Not<A> = (A) -> Never
typealias And<A, B> = (left: A, right: B)
indirect enum Or<A, B> {
  case inl(A)
  case inr(B)
}

func deMorgan<A, B>(_ ne: Not<Or<A, B>>) -> And<Not<A>, Not<B>> {
  // FIXME(diagnostics): The error message about initialization here is confusing
  return And<Not<A>, Not<B>>(
    Not<A> { a in ne(.left(a)) }, // expected-error {{type 'Not<A>' (aka '(A) -> Never') has no member 'init'}}
    // expected-error@-1 {{type 'Or<A, B>' has no member 'left'}}
    Not<B> { a in ne(.right(a)) } // expected-error {{type 'Not<B>' (aka '(B) -> Never') has no member 'init'}}
    // expected-error@-1 {{type 'Or<A, B>' has no member 'right'}}
  )
}

