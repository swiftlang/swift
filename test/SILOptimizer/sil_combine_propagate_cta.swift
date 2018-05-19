// RUN: %target-swift-frontend %s -O -wmo -emit-sil -Xllvm -sil-disable-pass=DeadFunctionElimination | %FileCheck %s

internal protocol SomeProtocol : class {
  func foo(x:SomeProtocol)  -> Int
  func foo_internal()  -> Int
}
internal class SomeClass: SomeProtocol {
  func foo_internal() ->Int {
    return 10
  }
  func foo(x:SomeProtocol) -> Int {
    return x.foo_internal()
  }
}
internal protocol SomeNonClassProtocol {
  func bar(x:SomeNonClassProtocol)  -> Int
  func bar_internal()  -> Int
}
internal class SomeNonClass: SomeNonClassProtocol {
  func bar_internal() -> Int {
    return 20
  }
  func bar(x:SomeNonClassProtocol) -> Int {
    return x.bar_internal()
  }
}
internal protocol UnrelatedProtocol {
  func foo()  -> Int
}
internal class SomeUnrelatedClass: UnrelatedProtocol {
  func foo() -> Int {
    return 20
  }
}
internal class SomeUnrelatedClassDerived: SomeUnrelatedClass {
}
internal class Other {
   let x:SomeProtocol
   let y:SomeNonClassProtocol
   let z:UnrelatedProtocol
   init(x:SomeProtocol, y:SomeNonClassProtocol, z:UnrelatedProtocol) {
     self.x = x;
     self.y = y;
     self.z = z;
   }

// CHECK-LABEL: sil hidden [noinline] @$S25sil_combine_propagate_cta5OtherC6doWorkSiyF : $@convention(method) (@guaranteed Other) -> Int {
// CHECK: bb0
// CHECK: integer_literal
// CHECK: ref_element_addr
// CHECK: open_existential_addr
// CHECK: witness_method
// CHECK: apply
// CHECK: struct_extract
// CHECK: integer_literal
// CHECK: builtin
// CHECK: tuple_extract 
// CHECK: tuple_extract 
// CHECK: cond_fail
// CHECK: struct 
// CHECK: return
// CHECK: } // end sil function '$S25sil_combine_propagate_cta5OtherC6doWorkSiyF'
   @inline(never) func doWork () ->Int {
      return self.x.foo(x:self.x) + self.y.bar(x:self.y) + self.z.foo()
   }
}

