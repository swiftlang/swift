// RUN: %target-typecheck-verify-swift 

// https://github.com/apple/swift/issues/55666

struct W<T> {}

struct S<C1: Collection> {
  init(){}
  // expected-note@+2 {{where 'C1.Element' = 'W<C1>', 'main.W<C2.Element>' = 'main.W<C2.Element>'}}
  // expected-note@+1 {{where 'C1.Element' = 'W<String>', 'W<C2.Element>' = 'W<Array<Int>.Element>'}}
  init<C2>(_ c2: W<C2>) where C2: Collection, C1.Element == W<C2.Element> {}
  // expected-note@+1 {{where 'C1.Element' = 'W<String>', 'W<C2.Element>' = 'W<Array<Int>.Element>'}}
  static func f<C2>(_ c2: W<C2>) where C2: Collection, C1.Element == W<C2.Element> {}
  // expected-note@+1 {{where 'C1.Element' = 'W<String>', 'W<C2.Element>' = 'W<Array<Int>.Element>'}}
  func instancef<C2>(_ c2: W<C2>) where C2: Collection, C1.Element == W<C2.Element> {}
}
let _ = S<[W<String>]>(W<[Int]>()) // expected-error{{initializer 'init(_:)' requires the types 'W<String>' and 'W<Array<Int>.Element>' be equivalent}}
let _ = S<[W<String>]>.f(W<[Int]>()) // expected-error{{static method 'f' requires the types 'W<String>' and 'W<Array<Int>.Element>' be equivalent}}
let _ = S<[W<String>]>().instancef(W<[Int]>()) // expected-error{{instance method 'instancef' requires the types 'W<String>' and 'W<Array<Int>.Element>' be equivalent}}

// Archetypes requirement failure
func genericFunc<C1: Collection, C2: Collection>(_ c2: W<C2>, c1: C1.Type) where C1.Element == W<C2.Element> {
  let _ = S<[W<C1>]>(W<C2>()) // expected-error{{initializer 'init(_:)' requires the types 'W<C1>' and 'W<C2.Element>' be equivalent}}
}
