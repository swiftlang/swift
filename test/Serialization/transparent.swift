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

// SIL: [[TMP:%.+]] = global_addr @_Tv11transparent3tmpSi : $*Int
// SIL: [[FUNC2:%.+]] = function_ref @_TF15def_transparent11testBuiltinFT_Si : $@thin () -> Int
// SIL: [[RESULT2:%.+]] = apply [transparent] [[FUNC2]]() : $@thin () -> Int
// SIL: store [[RESULT2]] to [[TMP]] : $*Int
var tmp = testBuiltin()

func test_partial(i: Int, j: Int) {
  calls(i: i, j: j)
}

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF15def_transparent15testTransparentFT1xSb_Sb : $@thin (Bool) -> Bool {
// SIL: bb0(%0 : $Bool):
// SIL: return %0 : $Bool

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF15def_transparent11testBuiltinFT_Si : $@thin () -> Int {
// SIL: bb0:
// SIL: integer_literal $Builtin.Word, 300
// SIL: string_literal utf8 "foo"
// SIL: return %{{.*}} : $Int

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
// SIL: bb[[CASE2]](%{{.*}} : $Int):
// SIL: bb[[CASE3]](%{{.*}} : $String):
// SIL: bb[[CASE4]](%{{.*}} : $(Int, String)):
func test_switch(u: MaybePair) {
  do_switch(u: u)
}

// SIL-LABEL: sil public_external [transparent] [fragile] @_TFV15def_transparent7WrapperCfMS0_FT3ValSi_S0_ : $@thin (Int, @thin Wrapper.Type) -> Wrapper {
// SIL-LABEL: sil public_external [transparent] [fragile] @_TFV15def_transparent7Wrapper8getValuefS0_FT_Si : $@cc(method) @thin (Wrapper) -> Int {
// SIL-LABEL: sil public_external [transparent] [fragile] @_TFV15def_transparent7Wrapperg10valueAgainSi : $@cc(method) @thin (Wrapper) -> Int {
// SIL-LABEL: sil public_external [transparent] [fragile] @_TFV15def_transparent7Wrapper13getValueAgainfS0_FT_Si : $@cc(method) @thin (Wrapper) -> Int {
func test_wrapper() {
  var w = Wrapper(Val: 42)
  
  print(w.value)
  print(w.getValue())
  print(w.valueAgain)
  print(w.getValueAgain())
}

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF15def_transparent17open_existentialsFT1pPS_1P_2cpPS_2CP__T_
func test_open_existentials(p: P, cp: CP) {
  // SIL: open_existential [[EXIST:%[0-9]+]]#1 : $*P to $*@opened([[N:".*"]]) P
  // SIL: open_existential_ref [[EXIST:%[0-9]+]] : $CP to $@opened([[M:".*"]]) CP
  open_existentials(p: p, cp: cp)
}

// SIL-LABEL: sil shared_external [transparent] [fragile] @_TF15def_transparent16curried_functionFT1xSi_FT1ySi_Si : $@thin (Int) -> @owned @callee_owned (Int) -> Int {
// SIL: function_ref @_TF15def_transparent16curried_functionfT1xSi_FT1ySi_Si : $@thin (Int, Int) -> Int
// SIL: partial_apply %1(%0) : $@thin (Int, Int) -> Int
