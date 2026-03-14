// RUN: %target-typecheck-verify-swift

struct D {}

class Future<T> {
  func then<U>(_ fn: @escaping (T) -> Future<U>) -> Future<U> { fatalError() } // expected-note {{in call to function 'then'}}
  func thenThrowing<U>(_ fn: @escaping (T) throws -> U) -> Future<U> { fatalError() }
  func whenFailure(_ fn: @escaping (Error) -> Void) {}

  func and<U>(result: U) -> Future<(T,U)> { fatalError() }
}

protocol P {
  func foo(arr: [D], data: ArraySlice<UInt8>) -> Future<D>
  func bar(root: D, from: P) -> Future<D>
}

extension P {
  func foo(arr: [D] = [], data: [UInt8]) -> Future<D> { fatalError() }
}

func crash(_ p: P, payload: [UInt8]) throws {
  p.foo(data: payload).then { _ in
    return Future<(D, [D])>()
  }.then { (id, arr) in
    p.foo(arr: arr, data: []).and(result: (id, arr))
  }.then { args0 in // expected-error {{generic parameter 'U' could not be inferred}}
    let (parentID, args1) = args0
    p.bar(root: parentID, from: p).and(result: args1)
  }.whenFailure { _ in }
}
