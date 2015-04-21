// RUN: %target-swift-frontend -O -emit-sil %s | FileCheck %s

public protocol Foo { 
  func foo(x:Int)->Int
}

public class C : Foo {
  @inline(never)
  public func foo(x:Int) -> Int {
    return 1111 + x
  }
}

@transparent
func callfoo(f: Foo)->Int {
  return f.foo(2) + f.foo(2)
}

// Check that calls to f.foo() get devirtalized and are not invoked
// via the expensive witness_method instruciton.
// To achieve that the information about a concrete type C should
// be propagated from init_existential_addr into witness_method and 
// apply instructions.

// CHECK-LABEL: sil [noinline] @_TF34devirt_protocol_method_invocations38test_devirt_protocol_method_invocationFCS_1CSi 
// CHECK-NOT: witness_method
// CHECK: checked_cast
// CHECK-NOT: checked_cast
// CHECK: bb1(
// CHECK-NOT: checked_cast
// CHECK: return
// CHECK: bb2(
// CHECK-NOT: checked_cast
// CHECK: function_ref
// CHECK: apply
// CHECK: apply
// CHECK: br bb1(
// CHECK: bb3
// CHECK-NOT: checked_cast
// CHECK: apply
// CHECK: apply
// CHECK: br bb1(
@inline(never)
public func test_devirt_protocol_method_invocation(c: C)->Int {
  return callfoo(c)
}

