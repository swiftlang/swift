// RUN: %target-typecheck-verify-swift

struct A<T> {}

extension A where T == Int32 { // expected-note 3{{requirement specified as 'T' == 'Int32' [with T = Int]}}
  struct B : ExpressibleByIntegerLiteral {
    typealias E = Int
    typealias IntegerLiteralType = Int

    init(integerLiteral: IntegerLiteralType) {}
  }

  typealias C = Int
}

let _: A<Int>.B = 0
// expected-error@-1 {{'A<T>.B' requires the types 'Int' and 'Int32' be equivalent}}
let _: A<Int>.C = 0
// expected-error@-1 {{'A<T>.C' (aka 'Int') requires the types 'Int' and 'Int32' be equivalent}}
let _: A<Int>.B.E = 0
// expected-error@-1 {{'A<T>.B' requires the types 'Int' and 'Int32' be equivalent}}


protocol P {}

@propertyWrapper
struct Wrapper<T: P> { // expected-note {{where 'T' = 'Int'}}
  var wrappedValue: T
}

class C {
  static let i = 1

  @Wrapper // expected-error{{generic struct 'Wrapper' requires that 'Int' conform to 'P'}}
  var value = C.i
}
