// RUN: %target-typecheck-verify-swift

struct Description: Hashable {
  let name: String
  let id: Int
}

struct Value {
  let ID: Int?
}

func test(allValues: [Value]) {
  // Type for `return nil` cannot be inferred at the moment because there is no join for result expressions.
  let owners = Set(allValues.compactMap { // expected-error {{generic parameter 'Element' could not be inferred}}
      // expected-note@-1 {{explicitly specify the generic arguments to fix this issue}}
      guard let id = $0.ID else { return nil }
      return Description(name: "", id: id)
    })
}
