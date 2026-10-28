// RUN: %target-swift-frontend -typecheck -verify -primary-file %s

// rdar://problem/57930643
struct School {
  var name: String
}
func testKeyPathClosureLiteralError() {
  let slist = [School(name:"AHS"), School(name:"BHS")]
  _ = slist.sorted(by: \School.name)
  // expected-error@-1 {{cannot convert key path literal to '(School, School) throws -> Bool', expected single-parameter function type '(School) -> String'}}
}
