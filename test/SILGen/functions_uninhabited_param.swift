// RUN: %target-swift-emit-sil %s -o /dev/null -verify
// RUN: %target-swift-emit-sil %s -o /dev/null -verify -enable-actor-data-race-checks

//===--- Function declaration with uninhabited parameter type
                                   
func foo(baz: Never) -> Int { // expected-note {{'baz' is of type 'Never' which cannot be constructed because it is an enum with no cases}}
  print("I can't be called!") // expected-warning{{will never be executed}}
  return 0
}

func bar(baz: Never) -> Int {} // ok

// SR-13432
func map<T>(_ block: (Never) -> T) {}
map { arg in // expected-note {{'arg' is of type 'Never' which cannot be constructed because it is an enum with no cases}}
  5 // expected-warning {{will never be executed}}
}

map { arg in // expected-note {{'arg' is of type 'Never' which cannot be constructed because it is an enum with no cases}}
  return 5 // expected-warning {{will never be executed}}
}

// We used to crash when emitting the closures below.
enum E {
  static func f(_: E) {}
}

@available(SwiftStdlib 5.1, *)
@MainActor
class Bar {
  var foo: (E) -> Void = { _ in }
}

let _: (E.Type) -> (E) -> () = { s in { e in s.f(e) } }
// expected-warning@-1 {{will never be executed}}
// expected-note@-2 {{'e' is of type 'E' which cannot be constructed because it is an enum with no cases}}

func empty_product(_ xs: (Int, String, Never)) { // expected-note {{'xs' is of type '(Int, String, Never)' which cannot be constructed because it contains 'Never' which is an enum with no cases}}
  print() // expected-warning{{will never be executed}}
}
func empty_custom_product(_ xs: (E, Int)) { // expected-note {{'xs' is of type '(E, Int)' which cannot be constructed because it contains 'E' which is an enum with no cases}}
  print() // expected-warning{{will never be executed}}
}

