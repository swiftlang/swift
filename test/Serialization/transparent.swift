// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -sil-serialize-all -o %t %S/Inputs/def_transparent.swift
// RUN: llvm-bcanalyzer %t/def_transparent.swiftmodule | FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -sil-link-all -I %t %s | FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_transparent

// SIL-LABEL: sil @main : $@cc(cdecl) @thin (Int32, UnsafeMutablePointer<UnsafeMutablePointer<Int8>>) -> Int32 {
// SIL: [[RAW:%.+]] = global_addr @_Tv11transparent3rawSb : $*Bool
// SIL: [[FUNC:%.+]] = function_ref @_TF15def_transparent15testTransparentFT1xSb_Sb : $@thin (Bool) -> Bool
// SIL: [[RESULT:%.+]] = apply [transparent] [[FUNC]]({{%.+}}) : $@thin (Bool) -> Bool
// SIL: store [[RESULT]] to [[RAW]] : $*Bool
var raw = testTransparent(x: false)

// SIL: [[TMP:%.+]] = global_addr @_Tv11transparent3tmpVSs5Int32 : $*Int32
// SIL: [[FUNC2:%.+]] = function_ref @_TF15def_transparent11testBuiltinFT_VSs5Int32 : $@thin () -> Int32
// SIL: [[RESULT2:%.+]] = apply [transparent] [[FUNC2]]() : $@thin () -> Int32
// SIL: store [[RESULT2]] to [[TMP]] : $*Int32
var tmp = testBuiltin()

func test_partial(i: Int32, j: Int32) {
  calls(i: i, j: j)
}

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF15def_transparent15testTransparentFT1xSb_Sb : $@thin (Bool) -> Bool {
// SIL: bb0(%0 : $Bool):
// SIL: return %0 : $Bool

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF15def_transparent11testBuiltinFT_VSs5Int32 : $@thin () -> Int32 {
// SIL: bb0:
// SIL: integer_literal $Builtin.Int32, 300
// SIL: string_literal utf8 "foo"
// SIL: return %{{.*}} : $Int32

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF15def_transparent7test_brFT_T_ : $@thin () -> () {
// SIL: bb{{.*}}:
// SIL: cond_br %{{.*}}, bb{{.*}}, bb{{.*}}
// SIL: bb{{.*}}:
// SIL: br bb{{.*}}
func wrap_br() {
  test_br()
}

// SIL-LABEL: sil public_external [fragile] @_TF15def_transparent9do_switchFT1uOS_9MaybePair_T_ : $@thin (@owned MaybePair) -> () {
// SIL: bb0(%0 : $MaybePair):
// SIL: retain_value %0 : $MaybePair
// SIL: switch_enum %0 : $MaybePair, case #MaybePair.Neither!enumelt: bb[[CASE1:[0-9]+]], case #MaybePair.Left!enumelt.1: bb[[CASE2:[0-9]+]], case #MaybePair.Right!enumelt.1: bb[[CASE3:[0-9]+]], case #MaybePair.Both!enumelt.1: bb[[CASE4:[0-9]+]]
// SIL: bb[[CASE1]]:
// SIL: bb[[CASE2]](%{{.*}} : $Int32):
// SIL: bb[[CASE3]](%{{.*}} : $String):
// SIL: bb[[CASE4]](%{{.*}} : $(Int32, String)):
func test_switch(u: MaybePair) {
  do_switch(u: u)
}

// SIL-LABEL: sil public_external [transparent] [fragile] @_TFV15def_transparent7WrapperCfMS0_FT3ValVSs5Int32_S0_ : $@thin (Int32, @thin Wrapper.Type) -> Wrapper {
// SIL-LABEL: sil public_external [transparent] [fragile] @_TFV15def_transparent7Wrapper8getValuefS0_FT_VSs5Int32 : $@cc(method) @thin (Wrapper) -> Int32 {
// SIL-LABEL: sil public_external [transparent] [fragile] @_TFV15def_transparent7Wrapperg10valueAgainVSs5Int32 : $@cc(method) @thin (Wrapper) -> Int32 {
// SIL-LABEL: sil public_external [transparent] [fragile] @_TFV15def_transparent7Wrapper13getValueAgainfS0_FT_VSs5Int32 : $@cc(method) @thin (Wrapper) -> Int32 {
func test_wrapper() {
  var w = Wrapper(Val: 42)
  
  print(w.value)
  print(w.getValue())
  print(w.valueAgain)
  print(w.getValueAgain())
}

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF15def_transparent17open_existentialsFT1pPS_1P_2cpPS_2CP__T_
func test_open_existentials(p: P, cp: CP) {
  // SIL: open_existential_addr [[EXIST:%[0-9]+]]#1 : $*P to $*@opened([[N:".*"]]) P
  // SIL: open_existential_ref [[EXIST:%[0-9]+]] : $CP to $@opened([[M:".*"]]) CP
  open_existentials(p: p, cp: cp)
}

// SIL-LABEL: sil shared_external [transparent] [fragile] @_TF15def_transparent16curried_functionFT1xVSs5Int32_FT1yS0__S0_ : $@thin (Int32) -> @owned @callee_owned (Int32) -> Int32 {
// SIL: function_ref @_TF15def_transparent16curried_functionfT1xVSs5Int32_FT1yS0__S0_ : $@thin (Int32, Int32) -> Int32
// SIL: partial_apply %1(%0) : $@thin (Int32, Int32) -> Int32
