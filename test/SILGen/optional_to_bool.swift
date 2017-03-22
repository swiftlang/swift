// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

public protocol P {}
extension Int: P {}

public class A {}
public class B: A {
  // CHECK-LABEL: sil @_T016optional_to_bool1BC1x{{[_0-9a-zA-Z]*}}fg
  // CHECK:         select_enum {{%.*}} : $Optional<Int>
  public lazy var x: Int = 0
  // CHECK-LABEL: sil @_T016optional_to_bool1BC1y{{[_0-9a-zA-Z]*}}fg
  // CHECK:         select_enum_addr {{%.*}} : $*Optional<P>
  public lazy var y: P = 0
}

// Collection casting is not implemented in non-ObjC runtime
#if _runtime(_ObjC)

// CHECK-objc-LABEL: sil @_T016optional_to_bool3foo{{[_0-9a-zA-Z]*}}F
public func foo(x: inout [A]) -> Bool {
  // CHECK-objc:       select_enum {{%.*}} : $Optional<Array<B>>
  return x is [B]
}

#endif
