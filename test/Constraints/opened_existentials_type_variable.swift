// RUN: %target-swift-frontend -emit-ir %s

public func fetch() {
  sryMap { return "" }
  .napError{ $0.abc() }
}

public func sryMap<String>(_ transform: () -> String) -> SryMap<String> {
  fatalError()
}

public protocol MyError {}
extension MyError {
  public func abc() -> Void { }
}

public protocol MyProto {
  associatedtype Failure
}
extension MyProto {
  public func napError(_ transform: (Self.Failure) -> Void) {}
}

public struct SryMap<Output> : MyProto {
  public typealias Failure = MyError & SomeClass<Output>
}

public class SomeClass<T> {}
