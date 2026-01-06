// RUN: %target-swift-frontend -emit-sil %s -O -swift-version 5 | %FileCheck %s

public final class Class<T: Sendable> {
  @inline(never)
  public init() {}
}

public protocol P {}

// CHECK-LABEL: sil @$s27specialize_missing_sendable6callerAA5ClassCyAA1P_pGyF : $@convention(thin) () -> @owned Class<any P> {
public func caller() -> Class<any P> {
  // CHECK: function_ref @$s27specialize_missing_sendable5ClassCACyxGycfCAA1P_p_Ttg5 : $@convention(thin) () -> @owned Class<any P>
  // CHECK: return
  return Class<any P>()
}
