// RUN: not %target-swift-frontend %s -typecheck

class A {}

struct B {}

@dynamicMemberLookup
enum DynMember {
  subscript<T>(_: T.Type) -> T {
    get { fatalError() }
  }

  subscript<T>(dynamicMember keyPath: KeyPath<B, T>) -> T {
    fatalError()
  }
}

@dynamicMemberLookup
class Test {
  subscript<T>(_: KeyPath<A, T>) -> T {
    self[setting: T.self]
  }

  subscript<T>(setting: T.Type) -> T {
    get {
      fatalError()
    }
  }

  subscript<T>(dynamicMember keyPath: KeyPath<DynMember, T>) -> T {
    fatalError()
  }
}
