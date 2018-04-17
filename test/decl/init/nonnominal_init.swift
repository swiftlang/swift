// RUN: %target-typecheck-verify-swift

typealias Not<A> = (A) -> Never
typealias And<A, B> = (left: A, right: B)
indirect enum Or<A, B> {
  case inl(A)
  case inr(B)
}

func deMorgan<A, B>(_ ne: Not<Or<A, B>>) -> And<Not<A>, Not<B>> {
  return And<Not<A>, Not<B>>(
    Not<A> { a in ne(.left(a)) }, // expected-error {{non-nominal type 'Not<A>' (aka '(A) -> Never') does not support explicit initialization}}
    Not<B> { a in ne(.right(a)) }
  )
}

