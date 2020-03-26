// RUN: %target-typecheck-verify-swift

protocol P {
  func isEqual(_ other: P) -> Bool
}

struct A {
  var value: P? = nil
}

struct B {
  func foo() throws -> A {}
}

struct E {
  func getB(_ flag: inout Bool) throws -> B {
    return B()
  }
}

func foo(arr: [E], other: P) -> Bool {
  return arr.compactMap { i in
    // expected-error@-1 {{generic parameter 'ElementOfResult' could not be inferred}}
    var flag = false
    return try? i.getB(&flag)
  }.compactMap { u -> P? in
    guard let a = try? u.foo() else { return nil }
    return a.value!
  }.contains {
    $0.isEqual(other)
  }
}
