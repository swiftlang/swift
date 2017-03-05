// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -O -emit-sil | %FileCheck %s

protocol Pingable {
 func ping(_ x : Int);
}
class Foo : Pingable {
  func ping(_ x : Int) { var t : Int }
}

// Everything gets devirtualized, inlined, and promoted to the stack.
// CHECK-LABEL: @_T024devirtualize_existential17interesting_stuffyyF
// CHECK-NOT: init_existential_addr
// CHECK-NOT: apply
// CHECK: return
public func interesting_stuff() {
 var x: Pingable = Foo()
 x.ping(1)
}

protocol Cloneable {
  func clone() -> Self
  func maybeClone() -> Self?
}

struct Bar : Cloneable {
  @inline(never)
  func clone() -> Bar { return self }

  @inline(never)
  func maybeClone() -> Bar? { return self }
}

// In this example, we don't eliminate the init_existential_addr, because
// of the stack allocated existential value used for the return.
//
// If the combiner was generalized to replace the opened existential type
// with the concrete type in all instructions that use it, instead of just
// special-casing witness_method and apply, we could eliminate the opened
// existential type completely.
//
// However even though IRGen still performs more work at runtime than is
// necessary here, the call is devirtualized.

// CHECK-LABEL: sil @_T024devirtualize_existential22more_interesting_stuffyyF
// CHECK: [[EXISTENTIAL:%.*]] = alloc_stack $Cloneable
// CHECK: [[EXISTENTIAL_ADDR:%.*]] = init_existential_addr [[EXISTENTIAL]]
// CHECK: [[VALUE:%.*]] = struct $Bar ()
// CHECK: [[RESULT_ADDR:%.*]] = unchecked_addr_cast [[EXISTENTIAL_ADDR:%.*]]
// CHECK: [[FN:%.*]] = function_ref @_T024devirtualize_existential3BarV5cloneACyF
// CHECK: [[RETURN:%.*]] = apply [[FN]]([[VALUE]])
// CHECK: store [[RETURN]] to [[RESULT_ADDR]]

// CHECK: [[ENUM:%.*]] = alloc_stack $Optional<Cloneable>
// CHECK: [[ENUM_ADDR:%.*]] = init_enum_data_addr [[ENUM]]
// CHECK: [[EXISTENTIAL_ADDR:%.*]] = init_existential_addr [[ENUM_ADDR]]
// CHECK: [[RESULT_ADDR:%.*]] = unchecked_addr_cast [[EXISTENTIAL_ADDR:%.*]]
// CHECK: [[FN:%.*]] = function_ref @_T024devirtualize_existential3BarV10maybeCloneACSgyF
// CHECK: [[RETURN:%.*]] = apply [[FN]]([[VALUE]])
// CHECK: store [[RETURN]] to [[RESULT_ADDR]]

// CHECK: return

public func more_interesting_stuff() {
  var x: Cloneable = Bar()

  x.clone()
  x.maybeClone()
}
