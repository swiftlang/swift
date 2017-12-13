// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

public class Foo<T> {
}

public func use(_: Any) {}

// Don't crash trying to generate IR.
// CHECK:  define{{.*}}swiftcc void @_T028objc_partial_apply_forwarder13createClosureyAA3FooCyxGcyXl1a_xm1ttlF5localL_yAE1x_tlFTA
public func createClosure<T>(a: AnyObject, t: T.Type) -> (Foo<T>) -> () {
  func local(x: Foo<T>) {
    use(a)
    use(x)
  }
  return local
}

