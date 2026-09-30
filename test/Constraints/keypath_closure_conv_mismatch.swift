// RUN: %target-swift-frontend -typecheck -verify -primary-file %s

// rdar://problem/57930643
struct School {
  var name: String
}
func testKeyPathClosureLiteralError() -> [School] {
  let slist = [School(name:"AHS"), School(name:"BHS")]
  return slist.sorted(by: \School.name)
  // expected-error@-1 {{cannot convert key path literal to '(School, School) throws -> Bool', expected single-parameter function type '(School) -> String'}}
}

func f(_: () -> String) {}

func testEmptyParamListConversion() {
  f(\School.name)
  // expected-error@-1 {{cannot convert key path literal to '() -> String', expected single-parameter function type '(School) -> String'}}
}
