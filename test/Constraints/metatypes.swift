// RUN: %target-parse-verify-swift

class A {}
class B : A {}

let test0 : A.Type = A.self
let test1 : A.Type = B.self
let test2 : B.Type = A.self // expected-error {{cannot convert initializer of type 'A.Type' to specified type 'B.Type'}}
let test3 : AnyClass = A.self
let test4 : AnyClass = B.self

struct S {}

let test5 : S.Type = S.self
let test6 : AnyClass = S.self // expected-error {{'S' does not conform to protocol 'AnyObject'}}
