// RUN: %target-swift-frontend -O -sil-existential-specializer -Xllvm -sil-disable-pass=GenericSpecializer -Xllvm -sil-disable-pass=FunctionSignatureOpts -Xllvm -sil-disable-pass=SILCombine -emit-sil -sil-verify-all %s | %FileCheck %s

internal protocol SomeProtocol : class {
  func foo()  -> Int
}
internal class SomeClass: SomeProtocol {
  init() {
  }
  @inline(never) func foo() -> Int {
   return 10
  }
}

@inline(never) internal func wrap_foo_cp(a:SomeProtocol ) -> Int{
 return a.foo()
}

// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform2cpyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK: alloc_ref
// CHECK: debug_value
// CHECK: init_existential_ref
// CHECK: debug_value
// CHECK: function_ref @$s21existential_transform11wrap_foo_cp1aSiAA12SomeProtocol_p_tFTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : SomeProtocol> (@guaranteed τ_0_0) -> Int
// CHECK: open_existential_ref
// CHECK: apply
// CHECK: set_deallocating
// CHECK: debug_value
// CHECK: debug_value
// CHECK: dealloc_ref
// CHECK: dealloc_ref
// CHECK: tuple
// CHECK: return
// CHECK-LABEL: } // end sil function '$s21existential_transform2cpyyF'
@inline(never) func cp() {
let magic1:SomeProtocol = SomeClass()
let _ = wrap_foo_cp(a:magic1)
}

/// Non Class Protocol
internal protocol SomeNoClassProtocol  {
  func foo()  -> Int
}
internal class SomeNoClass: SomeNoClassProtocol {
  init() {
  }
  @inline(never) func foo() -> Int {
   return 10
  }
}

@inline(never) internal func wrap_foo_ncp(a:SomeNoClassProtocol) -> Int{
 return a.foo() 
}

// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform3ncpyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK: alloc_stack
// CHECK: alloc_ref $SomeNoClass
// CHECK: debug_value
// CHECK: init_existential_addr
// CHECK: store
// CHECK: function_ref @$s21existential_transform12wrap_foo_ncp1aSiAA19SomeNoClassProtocol_p_tFTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : SomeNoClassProtocol> (@in_guaranteed τ_0_0) -> Int 
// CHECK: open_existential_addr
// CHECK: apply
// CHECK: destroy_addr
// CHECK: dealloc_stack
// CHECK: tuple
// CHECK: return
// CHECK-LABEL: } // end sil function '$s21existential_transform3ncpyyF'
@inline(never) func ncp() {
let magic2:SomeNoClassProtocol = SomeNoClass()
let _ = wrap_foo_ncp(a:magic2)
}


/// Class Protocol Composition
internal protocol SomeClassProtocolComp : class {
  func foo()  -> Int
}
internal protocol SomeOtherClassProtocolComp : class {
  func bar()  -> Int
}
internal class SomeClassComp: SomeClassProtocolComp, SomeOtherClassProtocolComp {
  init() {
  }
  @inline(never) func foo() -> Int {
   return 10
  }
  @inline(never) func bar() -> Int {
   return 20
  }
}
@inline(never) internal func wrap_foo_bar_cpc(a:SomeClassProtocolComp & SomeOtherClassProtocolComp) -> Int{
 return a.foo() + a.bar()
}

// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform3cpcyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   alloc_ref
// CHECK:   debug_value
// CHECK:   init_existential_ref
// CHECK:   debug_value
// CHECK:   function_ref @$s21existential_transform16wrap_foo_bar_cpc1aSiAA21SomeClassProtocolComp_AA0g5OtherhiJ0p_tFTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : SomeClassProtocolComp, τ_0_0 : SomeOtherClassProtocolComp> (@guaranteed τ_0_0) -> Int 
// CHECK:   open_existential_ref 
// CHECK:   apply
// CHECK:   set_deallocating
// CHECK:   debug_value
// CHECK:   debug_value
// CHECK:   dealloc_ref
// CHECK:   dealloc_ref
// CHECK:   tuple
// CHECK:   return
// CHECK-LABEL: } // end sil function '$s21existential_transform3cpcyyF'
@inline(never) func cpc() {
let magic3:SomeClassProtocolComp & SomeOtherClassProtocolComp = SomeClassComp()
let _ = wrap_foo_bar_cpc(a:magic3)
}

/// Non Class Protocol Comp
internal protocol SomeNoClassProtocolComp  {
  func foo()  -> Int
}
internal protocol SomeOtherNoClassProtocolComp  {
  func bar()  -> Int
}
internal class SomeNoClassComp: SomeNoClassProtocolComp, SomeOtherNoClassProtocolComp {
  init() {
  }
  @inline(never) func foo() -> Int {
   return 10
  }
  @inline(never) func bar() -> Int {
   return 20
  }
}
@inline(never) internal func wrap_no_foo_bar_comp_ncpc(a:SomeNoClassProtocolComp & SomeOtherNoClassProtocolComp) -> Int{
 return a.foo() + a.bar()
}

// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform4ncpcyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK: alloc_stack
// CHECK: alloc_ref
// CHECK: debug_value
// CHECK: init_existential_addr
// CHECK: store
// CHECK: function_ref @$s21existential_transform25wrap_no_foo_bar_comp_ncpc1aSiAA23SomeNoClassProtocolComp_AA0i5OtherjklM0p_tFTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : SomeNoClassProtocolComp, τ_0_0 : SomeOtherNoClassProtocolComp> (@in_guaranteed τ_0_0) -> Int
// CHECK: open_existential_addr
// CHECK: apply
// CHECK: destroy_addr
// CHECK: dealloc_stack
// CHECK: tuple
// CHECK: return
// CHECK-LABEL: } // end sil function '$s21existential_transform4ncpcyyF'
@inline(never) func ncpc() {
let magic4:SomeNoClassProtocolComp & SomeOtherNoClassProtocolComp = SomeNoClassComp()
let _ = wrap_no_foo_bar_comp_ncpc(a:magic4)
}

internal protocol P : class {
  func foo()  -> Int
}
internal class K : P {
  init() {
  }
  @inline(never) func foo() -> Int {
   return 10
  }
}

// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform18do_not_optimize_cp1aSiAA1P_p_tF : $@convention(thin) (@guaranteed P) -> Int {
// CHECK: bb0(%0 : $P):
// CHECK: debug_value
// CHECK: [[O1:%.*]] = open_existential_ref
// CHECK: witness_method $@opened("{{.*}}") P, #P.foo!1 : <Self where Self : P> (Self) -> () -> Int, [[O1]] : $@opened("{{.*}}") P : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@guaranteed τ_0_0) -> Int
// CHECK: apply
// CHECK: return
// CHECK-LABEL: } // end sil function '$s21existential_transform18do_not_optimize_cp1aSiAA1P_p_tF'
@inline(never) internal func do_not_optimize_cp(a:P ) -> Int{
 return a.foo()
}

internal protocol PP : class {
  func foo()  -> Int
}
internal class KK : PP {
  init() {
  }
 @inline(never)  func foo() -> Int {
   return 10
  }
}

// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform13wrap_inout_cp1aSiAA2PP_pz_tF : $@convention(thin) (@inout PP) -> Int {
// CHECK: bb0(%0 : $*PP):
// CHECK: debug_value_addr
// CHECK: load
// CHECK: [[O1:%.*]] = open_existential_ref
// CHECK:  witness_method $@opened("{{.*}}") PP, #PP.foo!1 : <Self where Self : PP> (Self) -> () -> Int, %3 : $@opened("{{.*}}PP : $@convention(witness_method: PP) <τ_0_0 where τ_0_0 : PP> (@guaranteed τ_0_0) -> Int 
// CHECK: apply
// CHECK: return
// CHECK-LABEL: } // end sil function '$s21existential_transform13wrap_inout_cp1aSiAA2PP_pz_tF'
@inline(never) internal func wrap_inout_cp(a: inout PP ) -> Int{
 return a.foo()
}

// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform24do_not_optimize_inout_cpyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK: alloc_stack
// CHECK: alloc_ref
// CHECK: debug_value
// CHECK: init_existential_ref
// CHECK: store
// CHECK: function_ref @$s21existential_transform13wrap_inout_cp1aSiAA2PP_pz_tF : $@convention(thin) (@inout PP) -> Int 
// CHECK: apply
// CHECK: set_deallocating
// CHECK: debug_value
// CHECK: debug_value
// CHECK: dealloc_ref
// CHECK: dealloc_ref
// CHECK: dealloc_stack
// CHECK: tuple
// CHECK: return
// CHECK-LABEL: } // end sil function '$s21existential_transform24do_not_optimize_inout_cpyyF'
@inline(never) func do_not_optimize_inout_cp() {
var magic5:PP = KK()
let _ = wrap_inout_cp(a: &magic5)
}

internal protocol PPP  {
  func foo()  -> Int
}
internal class KKKClass : PPP {
  init() {
  }
  @inline(never) func foo() -> Int {
   return 10
  }
}

@inline(never) internal func wrap_inout_ncp(a: inout PPP ) -> Int{
 return a.foo()
}

// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform9inout_ncpyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK: alloc_stack
// CHECK: alloc_ref
// CHECK: debug_value
// CHECK: init_existential_addr
// CHECK: store
// CHECK: function_ref @$s21existential_transform14wrap_inout_ncp1aSiAA3PPP_pz_tFTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : PPP> (@inout τ_0_0) -> Int
// CHECK: open_existential_addr
// CHECK: apply
// CHECK: destroy_addr 
// CHECK: dealloc_stack
// CHECK: tuple
// CHECK: return
// CHECK-LABEL: } // end sil function '$s21existential_transform9inout_ncpyyF'
@inline(never) func inout_ncp() {
var magic6:PPP = KKKClass()
let _ = wrap_inout_ncp(a: &magic6)
}

internal protocol PPPP  {
  func foo()  -> Int
}
internal struct SSSS : PPPP {
  init() {
  }
  @inline(never) func foo() -> Int {
   return 10
  }
}
@inline(never) internal func wrap_struct_inout_ncp(a: inout PPPP ) -> Int{
 return a.foo()
}

// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform16struct_inout_ncpyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK: alloc_stack
// CHECK: struct
// CHECK: init_existential_addr
// CHECK: store
// CHECK: function_ref @$s21existential_transform21wrap_struct_inout_ncp1aSiAA4PPPP_pz_tFTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : PPPP> (@inout τ_0_0) -> Int 
// CHECK: open_existential_addr
// CHECK: apply
// CHECK: destroy_addr
// CHECK: dealloc_stack
// CHECK: tuple
// CHECK: return
// CHECK-LABEL: } // end sil function '$s21existential_transform16struct_inout_ncpyyF'
@inline(never) func struct_inout_ncp() {
var magic7:PPPP = SSSS()
let _ = wrap_struct_inout_ncp(a: &magic7)
}

protocol GP {
func foo() -> Int
}

class GC: GP {
  @inline(never) func foo() ->Int {
    return 10
  }
}
func wrap_gcp<T:GP>(_ a:T,_ b:GP) -> Int {
  return a.foo() + b.foo()
}
// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform3gcpySixAA2GPRzlF : $@convention(thin) <T where T : GP> (@in_guaranteed T) -> Int {
// CHECK: bb0(%0 : $*T):
// CHECK: debug_value_addr 
// CHECK: alloc_ref 
// CHECK: debug_value
// CHECK: debug_value 
// CHECK: alloc_stack 
// CHECK: init_existential_addr 
// CHECK: store
// CHECK: function_ref @$s21existential_transform8wrap_gcpySix_AA2GP_ptAaCRzlFTf4ne_n : $@convention(thin) <τ_0_0 where τ_0_0 : GP><τ_1_0 where τ_1_0 : GP> (@in_guaranteed τ_0_0, @in_guaranteed τ_1_0) -> Int 
// CHECK: open_existential_addr 
// CHECK: strong_retain 
// CHECK: apply
// CHECK: destroy_addr 
// CHECK: dealloc_stack
// CHECK: strong_release 
// CHECK: return 
// CHECK: } // end sil function '$s21existential_transform3gcpySixAA2GPRzlF'
@inline(never) func gcp<T:GP>(_ a:T) -> Int {
  let k:GC = GC()
  return wrap_gcp(a, k)
}

func wrap_gcp_arch<T:GP>(_ a:T,_ b:GP, _ c:inout Array<T>) -> Int {
  return a.foo() + b.foo() + c[0].foo()
}
// CHECK-LABEL: sil hidden [noinline] @$s21existential_transform8gcp_archySix_SayxGztAA2GPRzlF : $@convention(thin) <T where T : GP> (@in_guaranteed T, @inout Array<T>) -> Int {
// CHECK: bb0(%0 : $*T, %1 : $*Array<T>):
// CHECK: debug_value_addr
// CHECK: debug_value_addr
// CHECK: alloc_ref
// CHECK: debug_value
// CHECK: debug_value
// CHECK: alloc_stack
// CHECK: init_existential_addr
// CHECK: store
// CHECK: function_ref @$s21existential_transform13wrap_gcp_archySix_AA2GP_pSayxGztAaCRzlFTf4nen_n : $@convention(thin) <τ_0_0 where τ_0_0 : GP><τ_1_0 where τ_1_0 : GP> (@in_guaranteed τ_0_0, @in_guaranteed τ_1_0, @inout Array<τ_0_0>) -> Int
// CHECK: open_existential_addr
// CHECK: strong_retain
// CHECK: apply
// CHECK: destroy_addr
// CHECK: dealloc_stack
// CHECK: strong_release
// CHECK: return
// CHECK-LABEL: } // end sil function '$s21existential_transform8gcp_archySix_SayxGztAA2GPRzlF'
@inline(never) func gcp_arch<T:GP>(_ a:T, _ b:inout Array<T>) -> Int {
  let k:GC = GC()
  return wrap_gcp_arch(a, k, &b)
}

@_optimize(none) public func foo() -> Int {
cp()
ncp()
cpc()
ncpc()
let p:P = K()
let x:Int = do_not_optimize_cp(a:p)
do_not_optimize_inout_cp()
inout_ncp()
struct_inout_ncp()
let y:Int = gcp(GC())
var a:Array<GC> = [GC()]
let z:Int = gcp_arch(GC(), &a) 
return x + y + z
}
