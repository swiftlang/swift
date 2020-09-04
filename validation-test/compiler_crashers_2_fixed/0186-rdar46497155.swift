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
    var flag = false
    return try? i.getB(&flag)
  }.compactMap { u -> P? in // expected-error {{unable to infer type of a closure parameter 'u' in the current context}}
    guard let a = try? u.foo() else { return nil }
    return a.value!
  }.contains {
    $0.isEqual(other)
  }
}
