// RUN: %target-typecheck-verify-swift

struct A {
  subscript(x: Int) -> Int { return x }
  var c: C? = C()
}

class C {
  var i = 0
}

// rdar://problem/32209039 - Improve diagnostic when unsupported tuple element references are used in key path literals
let _ = \(Int, String).0 // expected-error {{key path cannot reference tuple elements}}
let _ = \(a: Int, b: String).b // expected-error {{key path cannot reference tuple elements}}

struct TupleKeypath {
    let labeled: (foo: Int, bar: String)
    let unlabeled: (Int, Int)
}

let _: KeyPath<TupleKeypath, Int> = \TupleKeypath.labeled.foo // expected-error {{key path support for tuples is not implemented}}
let _: KeyPath<TupleKeypath, String> = \TupleKeypath.labeled.bar // expected-error {{key path support for tuples is not implemented}}
let _: KeyPath<TupleKeypath, Int> = \TupleKeypath.unlabeled.0 // expected-error {{key path support for tuples is not implemented}}
let _: KeyPath<TupleKeypath, String> = \TupleKeypath.unlabeled.1 // expected-error {{key path support for tuples is not implemented}}
