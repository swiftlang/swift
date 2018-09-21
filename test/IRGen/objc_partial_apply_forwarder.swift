// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

public class Foo<T> {
}

public func use(_: Any) {}

// Don't crash trying to generate IR.
// CHECK:  define{{.*}}swiftcc void @"$s28objc_partial_apply_forwarder13createClosure1a1tyAA3FooCyxGcyXl_xmtlF5localL_1xyAG_tlFTA"
public func createClosure<T>(a: AnyObject, t: T.Type) -> (Foo<T>) -> () {
  func local(x: Foo<T>) {
    use(a)
    use(x)
  }
  return local
}

// Don't crash.
public final class K<Error: Swift.Error> {
  func act(error: Error) {}
}

public struct A<Error: Swift.Error>{
  private let action: (K<Error>) -> Void

  public init(_ action: @escaping (K<Error>) -> Void) {
    self.action = action
  }

  public init(error: Error) {
    self.init { actor in
      actor.act(error: error)
    }
  }
}
