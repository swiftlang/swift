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

// CHECK-LABEL: sil hidden [noinline] @$S25sil_combine_propagate_cta5OtherC11doWorkClassSiyF : $@convention(method) (@guaranteed Other) -> Int {
// CHECK: bb0
// CHECK: debug_value
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
// CHECK: } // end sil function '$S25sil_combine_propagate_cta5OtherC11doWorkClassSiyF'
   @inline(never) func doWorkClass () ->Int {
      return self.x.foo(x:self.x) + self.y.bar(x:self.y) + self.z.foo()
   }
}

internal protocol PropProtocol {
  var arg1: Int { get set }
}

internal struct PropClass: PropProtocol {
  var arg1: Int
  init(arg1: Int) {
    self.arg1 = arg1
  }
}

internal class OtherClass {
  var arg: PropProtocol

  init(arg:PropProtocol) {
     self.arg = arg
  }

// CHECK-LABEL: sil hidden [noinline] @$S25sil_combine_propagate_cta10OtherClassC12doWorkStructSiyF : $@convention(method) (@guaranteed OtherClass) -> Int {
// CHECK: bb0
// CHECK: debug_value
// CHECK: alloc_stack
// CHECK: ref_element_addr
// CHECK: copy_addr
// CHECK: open_existential_addr
// CHECK: unchecked_addr_cast
// CHECK: struct_element_addr
// CHECK: load 
// CHECK: debug_value
// CHECK: destroy_addr
// CHECK: dealloc_stack
// CHECK: return
// CHECK: } // end sil function '$S25sil_combine_propagate_cta10OtherClassC12doWorkStructSiyF'
  @inline(never) func doWorkStruct () -> Int{
    let num = self.arg.arg1
    return num
  }
}

internal protocol AProtocol {
  var arg1: Int { get }
}
internal enum AnEnum : AProtocol {
    case avalue
    var arg1: Int {
        switch self {
        case .avalue:
            return 10
        }
    }
}

internal class OtherKlass {
  var arg: AProtocol

  init(arg:AProtocol) {
     self.arg = arg
  }

// CHECK-LABEL: sil hidden [noinline] @$S25sil_combine_propagate_cta10OtherKlassC10doWorkEnumSiyF : $@convention(method) (@guaranteed OtherKlass) -> Int {
// CHECK: bb0
// CHECK: integer_literal
// CHECK: struct
// CHECK: debug_value
// CHECK: return
// CHECK: } // end sil function '$S25sil_combine_propagate_cta10OtherKlassC10doWorkEnumSiyF'
  @inline(never) func doWorkEnum() -> Int {
    let num = self.arg.arg1
    return num
  }
}
