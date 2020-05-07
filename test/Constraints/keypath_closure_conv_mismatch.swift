// RUN: %target-swift-frontend -typecheck -verify -primary-file %s

// rdar://problem/57930643
struct School {
  var name: String
}
func testKeyPathClosureLiteralError() -> [School] {
  let slist = [School(name:"AHS"), School(name:"BHS")]
  return slist.sorted(by: \School.name) // expected-error {{cannot convert key path into a multi-argument function type '(School, School) throws -> Bool'}}
}
