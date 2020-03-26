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
// expected-error@-1 {{'A<Int>.B' requires the types 'Int' and 'Int32' be equivalent}}
let _: A<Int>.C = 0
// expected-error@-1 {{'A<Int>.C' (aka 'Int') requires the types 'Int' and 'Int32' be equivalent}}
let _: A<Int>.B.E = 0
// expected-error@-1 {{'A<Int>.B' requires the types 'Int' and 'Int32' be equivalent}}
