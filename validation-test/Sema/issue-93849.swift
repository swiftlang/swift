// rdar://191490858 (https://github.com/swiftlang/swift/issues/93849)
// RUN: %target-typecheck-verify-swift

let a: [Int?] = ["a"]
// expected-error@-1 {{cannot convert value of type 'String' to expected element type 'Int?'}}
let b: [String: Int?] = ["k": "a"]
// expected-error@-1 {{cannot convert value of type 'String' to expected dictionary value type 'Int?'}}
let c: Set<Int?> = ["a"]
// expected-error@-1 {{cannot convert value of type 'String' to expected element type 'Int?'}}
