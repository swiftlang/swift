// RUN: %swift -O3 %s -emit-sil -sil-inline-threshold 0 | FileCheck %s

// CHECK-LABEL: sil shared @_TTSC28devirt_inherited_conformance2B2S0_S_1P___TF28devirt_inherited_conformance11doSomethingUS_1P__FT1tQ__T_ : $@thin (@in B2) -> () {
// CHECK: bb0([[INPUT_PTR:%[0-9]+]] : $*B2):
// CHECK-NEXT: function_ref protocol witness for devirt_inherited_conformance.P.doSomething <A : devirt_inherited_conformance.P>(@inout devirt_inherited_conformance.P.Self)() -> () in conformance devirt_inherited_conformance.B : devirt_inherited_conformance.P
// CHECK-NEXT: [[FUNCTION_REF:%[0-9]+]] = function_ref @_TTWC28devirt_inherited_conformance1BS_1PFS1_11doSomethingUS1___fRQPS1_FT_T_ : $@cc(witness_method) @thin (@inout B) -> ()
// CHECK-NEXT: [[CAST_PTR:%[0-9]+]] = upcast [[INPUT_PTR]] : $*B2 to $*B
// CHECK-NEXT: apply [[FUNCTION_REF]]([[CAST_PTR]]) : $@cc(witness_method) @thin (@inout B) -> ()
// CHECK-NEXT: load
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return

@asmname("unknown1")
func unknown1() -> ()

protocol P {
  func doSomething()
}

class B : P {
  func doSomething() {
    unknown1()
  }
}

class B2 : B {

}

func doSomething<T : P>(t : T) {
  t.doSomething()
}

var b2 = B2()

doSomething(b2)
