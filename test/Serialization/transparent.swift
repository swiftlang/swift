// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-module -sil-serialize-all -o %t %S/Inputs/def_transparent.swift
// RUN: llvm-bcanalyzer %t/def_transparent.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -sil-link-all -I %t %s | %FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_transparent

// SIL-LABEL: sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// SIL: [[RAW:%.+]] = global_addr @_T011transparent3rawSbv : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @_T015def_transparent15testTransparentS2b1x_tF : $@convention(thin) (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [[FUNC]]({{%.+}}) : $@convention(thin) (Bool) -> Bool
// SIL: store [[RESULT]] to [trivial] [[RAW]] : $*Bool
var raw = testTransparent(x: false)

// SIL: [[TMP:%.+]] = global_addr @_T011transparent3tmps5Int32Vv : $*Int32
// SIL: [[FUNC2:%.+]] = function_ref @_T015def_transparent11testBuiltins5Int32VyF : $@convention(thin) () -> Int32
// SIL: [[RESULT2:%.+]] = apply [[FUNC2]]() : $@convention(thin) () -> Int32
// SIL: store [[RESULT2]] to [trivial] [[TMP]] : $*Int32
var tmp = testBuiltin()

// SIL-LABEL: sil public_external [transparent] [fragile] @_T015def_transparent15testTransparentS2b1x_tF : $@convention(thin) (Bool) -> Bool {
// SIL: bb0(%0 : $Bool):
// SIL: return %0 : $Bool

// SIL-LABEL: sil public_external [transparent] [fragile] @_T015def_transparent11testBuiltins5Int32VyF : $@convention(thin) () -> Int32 {
// SIL: bb0:
// SIL: integer_literal $Builtin.Int32, 300
// SIL: string_literal utf8 "foo"
// SIL: return %{{.*}} : $Int32

// SIL-LABEL: sil public_external [transparent] [fragile] @_T015def_transparent7test_bryyF : $@convention(thin) () -> () {
// SIL: bb{{.*}}:
// SIL: cond_br %{{.*}}, bb{{.*}}, bb{{.*}}
// SIL: bb{{.*}}:
// SIL: br bb{{.*}}
func wrap_br() {
  test_br()
}

// SIL-LABEL: sil public_external [fragile] @_T015def_transparent9do_switchyAA9MaybePairO1u_tF : $@convention(thin) (@owned MaybePair) -> () {
// SIL: bb0(%0 : $MaybePair):
// SIL: retain_value %0 : $MaybePair
// SIL: switch_enum %0 : $MaybePair, case #MaybePair.Neither!enumelt: bb[[CASE1:[0-9]+]], case #MaybePair.Left!enumelt.1: bb[[CASE2:[0-9]+]], case #MaybePair.Right!enumelt.1: bb[[CASE3:[0-9]+]], case #MaybePair.Both!enumelt.1: bb[[CASE4:[0-9]+]]
// SIL: bb[[CASE4]](%{{.*}} : $(Int32, String)):
// SIL: bb[[CASE3]](%{{.*}} : $String):
// SIL: bb[[CASE2]](%{{.*}} : $Int32):
// SIL: bb[[CASE1]]:
func test_switch(u: MaybePair) {
  do_switch(u: u)
}

// SIL-LABEL: sil public_external [transparent] [fragile] @_T015def_transparent7WrapperVACs5Int32V3Val_tcfC : $@convention(method) (Int32, @thin Wrapper.Type) -> Wrapper {
// SIL-LABEL: sil public_external [transparent] [fragile] @_T015def_transparent7WrapperV8getValue{{[_0-9a-zA-Z]*}}F : $@convention(method) (Wrapper) -> Int32 {
// SIL-LABEL: sil public_external [transparent] [fragile] @_T015def_transparent7WrapperV10valueAgains5Int32Vfg : $@convention(method) (Wrapper) -> Int32 {
// SIL-LABEL: sil public_external [transparent] [fragile] @_T015def_transparent7WrapperV13getValueAgain{{[_0-9a-zA-Z]*}}F : $@convention(method) (Wrapper) -> Int32 {
func test_wrapper() {
  var w = Wrapper(Val: 42)
  
  print(w.value, terminator: "")
  print(w.getValue(), terminator: "")
  print(w.valueAgain, terminator: "")
  print(w.getValueAgain(), terminator: "")
}

// SIL-LABEL: sil public_external [transparent] [fragile] @_T015def_transparent17open_existentialsyAA1P_p1p_AA2CP_p2cptF
func test_open_existentials(p: P, cp: CP) {
  // SIL: open_existential_addr immutable_access [[EXIST:%[0-9]+]] : $*P to $*@opened([[N:".*"]]) P
  // SIL: open_existential_ref [[EXIST:%[0-9]+]] : $CP to $@opened([[M:".*"]]) CP
  open_existentials(p: p, cp: cp)
}
