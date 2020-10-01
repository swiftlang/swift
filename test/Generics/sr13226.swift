// RUN: %target-typecheck-verify-swift 
struct W<T> {}

struct S<C1: Collection> {
  init(){}
  // expected-note@+2 {{where 'C1.Element' = 'String', 'W<C2.Element>' = 'Int'}}
  // expected-note@+1 {{where 'C1.Element' = 'C1', 'W<C2.Element>' = 'C2.Element'}}
  init<C2>(_ c2: W<C2>) where C2: Collection, C1.Element == W<C2.Element> {}
  // expected-note@+1 {{where 'C1.Element' = 'String', 'W<C2.Element>' = 'Int'}}
  static func f<C2>(_ c2: W<C2>) where C2: Collection, C1.Element == W<C2.Element> {}
  // expected-note@+1 {{where 'C1.Element' = 'String', 'W<C2.Element>' = 'Int'}}
  func instancef<C2>(_ c2: W<C2>) where C2: Collection, C1.Element == W<C2.Element> {}
}
let _ = S<[W<String>]>(W<[Int]>()) // expected-error{{initializer 'init(_:)' requires the types 'String' and 'Int' be equivalent}}
let _ = S<[W<String>]>.f(W<[Int]>()) // expected-error{{static method 'f' requires the types 'String' and 'Int' be equivalent}}
let _ = S<[W<String>]>().instancef(W<[Int]>()) // expected-error{{instance method 'instancef' requires the types 'String' and 'Int' be equivalent}}

// Archetypes requirement failure
func genericFunc<C1: Collection, C2: Collection>(_ c2: W<C2>, c1: C1.Type) where C1.Element == W<C2.Element> {
  let _ = S<[W<C1>]>(W<C2>()) // expected-error{{initializer 'init(_:)' requires the types 'C1' and 'C2.Element' be equivalent}}
}
