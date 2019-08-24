// RUN: %target-typecheck-verify-swift

struct A<T> {}

extension A where T == Int32 { // expected-note 2 {{where 'T' = 'Int'}}
  struct B : ExpressibleByIntegerLiteral { // expected-note {{where 'T' = 'Int'}}
    typealias E = Int
    typealias IntegerLiteralType = Int

    init(integerLiteral: IntegerLiteralType) {}
  }

  typealias C = Int
}

let _: A<Int>.B = 0
// expected-error@-1 {{referencing struct 'B' on 'A' requires the types 'Int' and 'Int32' be equivalent}}
let _: A<Int>.C = 0
// expected-error@-1 {{referencing type alias 'C' on 'A' requires the types 'Int' and 'Int32' be equivalent}}
let _: A<Int>.B.E = 0
// expected-error@-1 {{referencing type alias 'E' on 'A.B' requires the types 'Int' and 'Int32' be equivalent}}
