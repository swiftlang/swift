// RUN: %target-typecheck-verify-swift

protocol P {}
func takesP(_ x: some P) {}

struct Value {
  let prop: any P
}

@dynamicMemberLookup
struct ValueWithDynamicMember {
  let prop: any P

  subscript<U>(dynamicMember keyPath: KeyPath<Int, U>) -> U {
    fatalError()
  }
}

@dynamicMemberLookup
struct Lens<T> {
  subscript<U>(dynamicMember keyPath: KeyPath<T, U>) -> U {
    fatalError()
  }
}

func test(_ a: Lens<Value>, _ b: Lens<ValueWithDynamicMember>) {
  takesP(a.prop)
  takesP(b.prop)
}
