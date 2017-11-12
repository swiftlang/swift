// RUN: %target-typecheck-verify-swift

protocol Initable {
  init()
}

struct ConcreteType {
  let c: [Int]

  // Generic index type
  subscript<C : Collection>(indices: C) -> [Int]
    where C.Iterator.Element == Int {
    return indices.map { c[$0] }
  }

  // Generic element type
  subscript<I : Initable>(factory: I.Type) -> I {
    return factory.init()
  }
}

struct GenericType<T : Collection> {
  let c: T

  // Generic index type
  subscript<C : Collection>(indices: C) -> [T.Iterator.Element]
    where C.Iterator.Element == T.Index {
    return indices.map { c[$0] }
  }

  // Generic element type
  subscript<I : Initable>(factory: I.Type) -> I {
    return factory.init()
  }
}
