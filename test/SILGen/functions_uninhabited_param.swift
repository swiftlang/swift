// RUN: %target-swift-emit-sil %s -o /dev/null -verify

//===--- Function declaration with uninhabited parameter type
                                   
func foo(baz: Never) -> Int { // expected-note {{'baz' is uninhabited, so this function body can never be executed}}
  print("I can't be called!") // expected-warning{{will never be executed}}
  return 0
}

func bar(baz: Never) -> Int {} // ok

// SR-13432
func map<T>(_ block: (Never) -> T) {}
map { arg in // expected-note {{'arg' is uninhabited, so this function body can never be executed}}
  5 // expected-warning {{will never be executed}}
}

map { arg in // expected-note {{'arg' is uninhabited, so this function body can never be executed}}
  return 5 // expected-warning {{will never be executed}}
}

// We used to crash when emitting the closure below.
enum E {
  static func f(_: E) {}
}

let _: (E.Type) -> (E) -> () = { s in { e in s.f(e) } }
// expected-warning@-1 {{will never be executed}}
// expected-note@-2 {{'e' is uninhabited, so this function body can never be executed}}
