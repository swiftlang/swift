// RUN: %target-swift-frontend %s -O -wmo -emit-sil -Xllvm -sil-disable-pass=DeadFunctionElimination | %FileCheck %s

// case 1: class protocol -- should optimize
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

// case 2: non-class protocol -- should optimize
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

// case 3: class conforming to protocol has a derived class -- should not optimize
internal protocol DerivedProtocol {
  func foo()  -> Int
}
internal class SomeDerivedClass: DerivedProtocol {
  func foo() -> Int {
    return 20
  }
}
internal class SomeDerivedClassDerived: SomeDerivedClass {
}

// case 3: public protocol -- should not optimize
public protocol PublicProtocol {
  func foo()  -> Int
}
internal class SomePublicClass: PublicProtocol {
  func foo() -> Int {
    return 20
  }
}

// case 4: Chain of protocols P1->P2->C -- optimize
internal protocol MultiProtocolChain {
  func foo()  -> Int
}
internal protocol RefinedMultiProtocolChain : MultiProtocolChain {
  func bar()  -> Int
}
internal class SomeMultiClass: RefinedMultiProtocolChain {
  func foo() -> Int {
    return 20
  }
  func bar() -> Int {
    return 30
  }
}

// case 5: Generic conforming type -- should not  optimize
internal protocol GenericProtocol {
  func foo() -> Int 
}

internal class GenericClass<T> : GenericProtocol {
  var items = [T]()
  func foo() -> Int { 
    return items.count 
  }
}


// case 6: two classes conforming to protocol
internal protocol MultipleConformanceProtocol {
  func foo()  -> Int
}
internal class Klass1: MultipleConformanceProtocol {
  func foo() -> Int {
    return 20
  }
}
internal class Klass2: MultipleConformanceProtocol {
  func foo() -> Int {
    return 30
  }
}


internal class Other {
   let x:SomeProtocol
   let y:SomeNonClassProtocol
   let z:DerivedProtocol
   let p:PublicProtocol
   let q:MultiProtocolChain
   let r:GenericProtocol
   let s:MultipleConformanceProtocol
   init(x:SomeProtocol, y:SomeNonClassProtocol, z:DerivedProtocol, p:PublicProtocol, q:MultiProtocolChain, r:GenericProtocol, s:MultipleConformanceProtocol) {
     self.x = x;
     self.y = y;
     self.z = z;
     self.p = p;
     self.q = q;
     self.r = r;
     self.s = s;
   }

// CHECK-LABEL: sil hidden [noinline] @$S25sil_combine_protocol_conf5OtherC11doWorkClassSiyF : $@convention(method) (@guaranteed Other) -> Int {
// CHECK: bb0
// CHECK: debug_value
// CHECK: integer_literal
// CHECK: [[R1:%.*]] = ref_element_addr %0 : $Other, #Other.z
// CHECK: [[O1:%.*]] = open_existential_addr immutable_access [[R1]] : $*DerivedProtocol to $*@opened("{{.*}}") DerivedProtocol
// CHECK: [[W1:%.*]] = witness_method $@opened("{{.*}}") DerivedProtocol, #DerivedProtocol.foo!1 : <Self where Self : DerivedProtocol> (Self) -> () -> Int, [[O1]] : $*@opened("{{.*}}") DerivedProtocol : $@convention(witness_method: DerivedProtocol) <τ_0_0 where τ_0_0 : DerivedProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: apply [[W1]]<@opened("{{.*}}") DerivedProtocol>([[O1]]) : $@convention(witness_method: DerivedProtocol) <τ_0_0 where τ_0_0 : DerivedProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: struct_extract
// CHECK: integer_literal
// CHECK: builtin
// CHECK: tuple_extract 
// CHECK: tuple_extract 
// CHECK: cond_fail
// CHECK: [[R2:%.*]] = ref_element_addr %0 : $Other, #Other.p
// CHECK: [[O2:%.*]] = open_existential_addr immutable_access [[R2]] : $*PublicProtocol to $*@opened("{{.*}}") PublicProtocol
// CHECK: [[W2:%.*]] = witness_method $@opened("{{.*}}") PublicProtocol, #PublicProtocol.foo!1 : <Self where Self : PublicProtocol> (Self) -> () -> Int, [[O2]] : $*@opened("{{.*}}") PublicProtocol : $@convention(witness_method: PublicProtocol) <τ_0_0 where τ_0_0 : PublicProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: apply [[W2]]<@opened("{{.*}}") PublicProtocol>([[O2]]) : $@convention(witness_method: PublicProtocol) <τ_0_0 where τ_0_0 : PublicProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: struct_extract
// CHECK: builtin 
// CHECK: tuple_extract
// CHECK: tuple_extract
// CHECK: cond_fail
// CHECK: integer_literal
// CHECK: builtin 
// CHECK: tuple_extract
// CHECK: tuple_extract
// CHECK: cond_fail
// CHECK: [[R3:%.*]] = ref_element_addr %0 : $Other, #Other.r
// CHECK: [[O3:%.*]] = open_existential_addr immutable_access [[R3]] : $*GenericProtocol to $*@opened("{{.*}}") GenericProtocol
// CHECK: [[W3:%.*]] = witness_method $@opened("{{.*}}") GenericProtocol, #GenericProtocol.foo!1 : <Self where Self : GenericProtocol> (Self) -> () -> Int, [[O3]] : $*@opened("{{.*}}") GenericProtocol : $@convention(witness_method: GenericProtocol) <τ_0_0 where τ_0_0 : GenericProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: apply [[W3]]<@opened("{{.*}}") GenericProtocol>([[O3]]) : $@convention(witness_method: GenericProtocol) <τ_0_0 where τ_0_0 : GenericProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: struct_extract
// CHECK: builtin 
// CHECK: tuple_extract
// CHECK: tuple_extract
// CHECK: cond_fail
// CHECK: [[R4:%.*]] = ref_element_addr %0 : $Other, #Other.s
// CHECK: [[O4:%.*]] = open_existential_addr immutable_access %36 : $*MultipleConformanceProtocol to $*@opened("{{.*}}") MultipleConformanceProtocol
// CHECK: [[W4:%.*]] = witness_method $@opened("{{.*}}") MultipleConformanceProtocol, #MultipleConformanceProtocol.foo!1 : <Self where Self : MultipleConformanceProtocol> (Self) -> () -> Int, %37 : $*@opened("{{.*}}") MultipleConformanceProtocol : $@convention(witness_method: MultipleConformanceProtocol) <τ_0_0 where τ_0_0 : MultipleConformanceProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: apply [[W4]]<@opened("{{.*}}") MultipleConformanceProtocol>(%37) : $@convention(witness_method: MultipleConformanceProtocol) <τ_0_0 where τ_0_0 : MultipleConformanceProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: struct_extract
// CHECK: builtin
// CHECK: tuple_extract
// CHECK: tuple_extract
// CHECK: cond_fail
// CHECK: struct 
// CHECK: return
// CHECK: } // end sil function '$S25sil_combine_protocol_conf5OtherC11doWorkClassSiyF'
   @inline(never) func doWorkClass () ->Int {
      return self.x.foo(x:self.x) // optimize
             + self.y.bar(x:self.y) // optimize
             + self.z.foo() // do not optimize
             + self.p.foo() // do not optimize
             + self.q.foo() // optimize
             + self.r.foo() // do not optimize
             + self.s.foo() // do not optimize
   }
}

// case 1: struct -- optimize
internal protocol PropProtocol {
  var val: Int { get set }
}
internal struct PropClass: PropProtocol {
  var val: Int
  init(val: Int) {
    self.val = val
  }
}

// case 2: generic struct -- do not optimize
internal protocol GenericPropProtocol {
  var val: Int { get set }
}
internal struct GenericPropClass<T>: GenericPropProtocol {
  var val: Int
  init(val: Int) {
    self.val = val
  }
}

// case 3: nested struct -- optimize
internal protocol NestedPropProtocol {
  var val: Int { get }
}
struct Outer {
  struct Inner : NestedPropProtocol {
    var val: Int
    init(val: Int) {
      self.val = val
    }
  }
}

// case 4: generic nested struct -- do not optimize
internal protocol GenericNestedPropProtocol {
  var val: Int { get }
}
struct GenericOuter<T> {
  struct GenericInner : GenericNestedPropProtocol {
    var val: Int
    init(val: Int) {
      self.val = val
    }
  }
}

internal class OtherClass {
  var arg1: PropProtocol
  var arg2: GenericPropProtocol
  var arg3: NestedPropProtocol
  var arg4: GenericNestedPropProtocol

  init(arg1:PropProtocol, arg2:GenericPropProtocol, arg3: NestedPropProtocol, arg4: GenericNestedPropProtocol) {
     self.arg1 = arg1
     self.arg2 = arg2
     self.arg3 = arg3
     self.arg4 = arg4
  }

// CHECK-LABEL: sil hidden [noinline] @$S25sil_combine_protocol_conf10OtherClassC12doWorkStructSiyF : $@convention(method) (@guaranteed OtherClass) -> Int {
// CHECK: bb0
// CHECK: debug_value
// CHECK: [[A1:%.*]] = alloc_stack $PropProtocol
// CHECK: [[R1:%.*]] = ref_element_addr %0 : $OtherClass, #OtherClass.arg1
// CHECK: copy_addr [[R1]] to [initialization] [[A1]] : $*PropProtocol
// CHECK: [[O1:%.*]] = open_existential_addr immutable_access [[A1]] : $*PropProtocol to $*@opened("{{.*}}") PropProtocol
// CHECK: [[U1:%.*]] = unchecked_addr_cast [[O1]] : $*@opened("{{.*}}") PropProtocol to $*PropClass 
// CHECK: [[S1:%.*]] = struct_element_addr [[U1]] : $*PropClass, #PropClass.val
// CHECK: [[S11:%.*]] = struct_element_addr [[S1]] : $*Int, #Int._value
// CHECK: load [[S11]] 
// CHECK: destroy_addr [[A1]] : $*PropProtocol
// CHECK: [[A2:%.*]] = alloc_stack $GenericPropProtocol 
// CHECK: [[R2:%.*]] = ref_element_addr %0 : $OtherClass, #OtherClass.arg2
// CHECK: copy_addr  [[R2]] to [initialization] [[A2]] : $*GenericPropProtocol
// CHECK: [[O2:%.*]] = open_existential_addr immutable_access [[A2]] : $*GenericPropProtocol to $*@opened("{{.*}}") GenericPropProtocol
// CHECK: [[W2:%.*]] = witness_method $@opened("{{.*}}") GenericPropProtocol, #GenericPropProtocol.val!getter.1 : <Self where Self : GenericPropProtocol> (Self) -> () -> Int, [[O2]] : $*@opened("{{.*}}") GenericPropProtocol : $@convention(witness_method: GenericPropProtocol) <τ_0_0 where τ_0_0 : GenericPropProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: apply [[W2]]<@opened("{{.*}}") GenericPropProtocol>([[O2]]) : $@convention(witness_method: GenericPropProtocol) <τ_0_0 where τ_0_0 : GenericPropProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: destroy_addr [[A2]] : $*GenericPropProtocol
// CHECK: struct_extract
// CHECK: integer_literal
// CHECK: builtin
// CHECK: tuple_extract
// CHECK: tuple_extract
// CHECK: cond_fail
// CHECK: dealloc_stack [[A2]] : $*GenericPropProtocol
// CHECK: dealloc_stack [[A1]] : $*PropProtocol
// CHECK: [[A4:%.*]] = alloc_stack $NestedPropProtocol
// CHECK: [[R4:%.*]] = ref_element_addr %0 : $OtherClass, #OtherClass.arg3
// CHECK: copy_addr [[R4]] to [initialization] [[A4]] : $*NestedPropProtocol
// CHECK: [[O4:%.*]] = open_existential_addr immutable_access [[A4]] : $*NestedPropProtocol to $*@opened("{{.*}}") NestedPropProtocol
// CHECK: [[AA:%.*]] = alloc_stack $@opened("{{.*}}") NestedPropProtocol
// CHECK: copy_addr [[O4]] to [initialization] [[AA]] : $*@opened("{{.*}}") NestedPropProtocol
// CHECK: destroy_addr [[A4]] : $*NestedPropProtocol
// CHECK: [[U4:%.*]] = unchecked_addr_cast [[AA]] : $*@opened("{{.*}}") NestedPropProtocol to $*Outer.Inner
// CHECK: [[S4:%.*]] = struct_element_addr [[U4]] : $*Outer.Inner, #Outer.Inner.val
// CHECK: [[S41:%.*]] = struct_element_addr [[S4]] : $*Int, #Int._value
// CHECK: load [[S41]]
// CHECK: builtin
// CHECK: tuple_extract
// CHECK: tuple_extract
// CHECK: cond_fail
// CHECK: dealloc_stack [[A4]] : $*NestedPropProtocol
// CHECK: [[A5:%.*]] = alloc_stack $GenericNestedPropProtocol
// CHECK: [[R5:%.*]] = ref_element_addr %0 : $OtherClass, #OtherClass.arg4
// CHECK:  copy_addr [[R5]] to [initialization] [[A5]] : $*GenericNestedPropProtocol
// CHECK: [[O5:%.*]] = open_existential_addr immutable_access [[A5]] : $*GenericNestedPropProtocol to $*@opened("{{.*}}") GenericNestedPropProtocol
// CHECK: [[A6:%.*]] = alloc_stack $@opened("{{.*}}") GenericNestedPropProtocol
// CHECK: copy_addr [[O5]] to [initialization] [[A6]] : $*@opened("{{.*}}") GenericNestedPropProtocol
// CHECK: destroy_addr [[A5]] : $*GenericNestedPropProtocol
// CHECK: [[W5:%.*]] = witness_method $@opened("{{.*}}") GenericNestedPropProtocol, #GenericNestedPropProtocol.val!getter.1 : <Self where Self : GenericNestedPropProtocol> (Self) -> () -> Int, [[O5:%.*]] : $*@opened("{{.*}}") GenericNestedPropProtocol : $@convention(witness_method: GenericNestedPropProtocol) <τ_0_0 where τ_0_0 : GenericNestedPropProtocol> (@in_guaranteed τ_0_0) -> Int 
// CHECK: apply [[W5]]<@opened("{{.*}}") GenericNestedPropProtocol>([[A6]]) : $@convention(witness_method: GenericNestedPropProtocol) <τ_0_0 where τ_0_0 : GenericNestedPropProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: destroy_addr [[A6]] : $*@opened("{{.*}}") GenericNestedPropProtocol
// CHECK: struct_extract
// CHECK: builtin
// CHECK: tuple_extract
// CHECK: tuple_extract
// CHECK: cond_fail
// CHECK: struct
// CHECK: dealloc_stack [[A6]] : $*@opened("{{.*}}") GenericNestedPropProtocol
// CHECK: dealloc_stack [[A5]] : $*GenericNestedPropProtocol
// CHECK: return
// CHECK: } // end sil function '$S25sil_combine_protocol_conf10OtherClassC12doWorkStructSiyF'
  @inline(never) func doWorkStruct () -> Int{
    return self.arg1.val  // optimize
           + self.arg2.val  // do not optimize
           + self.arg3.val  // optimize
           + self.arg4.val  // do not optimize
  }
}

// case 1: enum -- optimize
internal protocol AProtocol {
  var val: Int { get }
}
internal enum AnEnum : AProtocol {
    case avalue
    var val: Int {
        switch self {
        case .avalue:
            return 10
        }
    }
}

// case 2: generic enum -- do not optimize
internal protocol AGenericProtocol {
  var val: Int { get }
}
internal enum AGenericEnum<T> : AGenericProtocol {
    case avalue
    var val: Int {
        switch self {
        case .avalue:
            return 10
        }
    }
}

internal class OtherKlass {
  var arg1: AProtocol
  var arg2: AGenericProtocol

  init(arg1:AProtocol, arg2:AGenericProtocol) {
     self.arg1 = arg1
     self.arg2 = arg2
  }

// CHECK-LABEL: sil hidden [noinline] @$S25sil_combine_protocol_conf10OtherKlassC10doWorkEnumSiyF : $@convention(method) (@guaranteed OtherKlass) -> Int {
// CHECK: bb0
// CHECK: debug_value
// CHECK: integer_literal
// CHECK: [[A1:%.*]] = alloc_stack $AGenericProtocol
// CHECK: [[R1:%.*]] = ref_element_addr %0 : $OtherKlass, #OtherKlass.arg2
// CHECK: copy_addr [[R1]] to [initialization] [[A1]] : $*AGenericProtocol 
// CHECK: [[O1:%.*]] = open_existential_addr immutable_access [[A1]] : $*AGenericProtocol to $*@opened("{{.*}}") AGenericProtocol
// CHECK: [[A2:%.*]] = alloc_stack $@opened("{{.*}}") AGenericProtocol
// CHECK: copy_addr [[O1]] to [initialization] [[A2]] : $*@opened("{{.*}}") AGenericProtocol
// CHECK: destroy_addr [[A1]] : $*AGenericProtocol
// CHECK: [[W1:%.*]] = witness_method $@opened("{{.*}}") AGenericProtocol, #AGenericProtocol.val!getter.1 : <Self where Self : AGenericProtocol> (Self) -> () -> Int, [[O1]] : $*@opened("{{.*}}") AGenericProtocol : $@convention(witness_method: AGenericProtocol) <τ_0_0 where τ_0_0 : AGenericProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: apply [[W1]]<@opened("{{.*}}") AGenericProtocol>([[A2]]) : $@convention(witness_method: AGenericProtocol) <τ_0_0 where τ_0_0 : AGenericProtocol> (@in_guaranteed τ_0_0) -> Int
// CHECK: destroy_addr [[A2]] : $*@opened("{{.*}}") AGenericProtocol
// CHECK: struct_extract
// CHECK: integer_literal
// CHECK: builtin
// CHECK: tuple_extract
// CHECK: tuple_extract
// CHECK: cond_fail
// CHECK: struct
// CHECK: dealloc_stack [[A2]] : $*@opened("{{.*}}") AGenericProtocol
// CHECK: dealloc_stack [[A1]] : $*AGenericProtocol
// CHECK: return
// CHECK: } // end sil function '$S25sil_combine_protocol_conf10OtherKlassC10doWorkEnumSiyF'
  @inline(never) func doWorkEnum() -> Int {
    return self.arg1.val  // optimize
           + self.arg2.val // do not optimize
  }
}

