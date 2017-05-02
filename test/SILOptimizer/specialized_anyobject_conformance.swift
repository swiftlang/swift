// RUN: %target-swift-frontend  -O -sil-inline-threshold 0 -emit-sil -primary-file %s | %FileCheck %s

// rdar://problem/31910351

// Check that swift compiler does not crash on this input.

public protocol P {
  func use<T:AnyObject>(_ t: T)
}

public class C<T> {
}

public func callee(_ t: C<Int32>?, _ p: P) {
  // This call results in a creation of a specialized conformance of C<Int32> to AnyObject.
  p.use(t!)
}

// CHECK-LABEL: sil @_T033specialized_anyobject_conformance7caller1yAA1P_p1p_tF : $@convention(thin) (@in P) -> ()
public func caller1(p: P) {
  callee(C<Int32>(), p)
}

