// RUN: %target-typecheck-verify-swift

struct Description: Hashable {
  let name: String
  let id: Int
}

struct Value {
  let ID: Int?
}

func test(allValues: [Value]) {
  _ = Set(allValues.compactMap {
      guard let id = $0.ID else { return nil } // Ok (result type is inferred by joining both `return` statements)
      return Description(name: "", id: id)
    })
}
