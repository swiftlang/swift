// RUN: %target-typecheck-verify-swift

func test1(_: ()) {} // expected-note {{'test1' declared here}}
test1() // expected-error {{missing argument for parameter #1 in call}}

enum Result<T> {
  case success(T) // expected-note {{'success' declared here}}
}
func test3(_: Result<()>) {}
test3(.success()) // expected-error {{missing argument for parameter #1 in call}}

func toString(indexes: Int?...) -> String {
  let _ = indexes.reduce(0) { print($0); return $0.0 + ($0.1 ?? 0)}
  // expected-error@-1 {{contextual closure type '(_, Int?) throws -> _' expects 2 arguments, but 1 was used in closure body}}
  let _ = indexes.reduce(0) { (true ? $0 : (1, 2)).0 + ($0.1 ?? 0) }
  // expected-error@-1 {{contextual closure type '(_, Int?) throws -> _' expects 2 arguments, but 1 was used in closure body}}
  _ = ["Hello", "Foo"].sorted { print($0); return $0.0.count > ($0).1.count }
  // expected-error@-1 {{argument passed to call that takes no arguments}}
}

func doit(_ x: Int) -> Bool { return x > 0 }
let _: ((String, Int)) -> [String:Bool] = { [$0: doit($1)] }
// expected-error@-1 {{closure tuple parameter '(String, Int)' does not support destructuring with implicit parameters}}
func returnClosure() -> ((Int, Int)) -> Bool { return {$1 > $0} }
// expected-error@-1 {{closure tuple parameter '(Int, Int)' does not support destructuring with implicit parameters}}
