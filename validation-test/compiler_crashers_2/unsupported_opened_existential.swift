// RUN: not --crash %target-swift-frontend -emit-ir %s

// REQUIRES: asserts

func fetch() {
  sryMap { return "" }
  .napError{ $0.abc() }
}

func sryMap<String>(_ transform: () -> String) -> SryMap<String> {
  fatalError()
}

protocol MyError {}
extension MyError {
  func abc() -> Void { }
}

protocol MyProto {
  associatedtype Failure
}
extension MyProto {
  func napError(_ transform: (Self.Failure) -> Void) {}
}

struct SryMap<Output> : MyProto {
  typealias Failure = MyError & SomeClass<Output>
}

class SomeClass<T> {}
