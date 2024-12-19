
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_transparent.swift
// RUN: llvm-bcanalyzer %t/def_transparent.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -module-name transparent -Xllvm -sil-print-types -emit-sil -I %t %s | %FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_transparent

// SIL-LABEL: sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// SIL: [[RAW:%.+]] = global_addr @$s11transparent3rawSbvp : $*Bool
// SIL: [[VALUE:%.+]] = integer_literal $Builtin.Int1, 0
// SIL: [[BOOL:%.+]] = struct $Bool ([[VALUE]] : $Builtin.Int1)
// SIL: store [[BOOL]] to [[RAW]] : $*Bool
var raw = testTransparent(x: false)

// SIL: [[TMP:%.+]] = global_addr @$s11transparent3tmps5Int32V_SStvp : $*(Int32, String)
// SIL: [[TMP1:%.+]] = tuple_element_addr [[TMP]] : $*(Int32, String), 0
// SIL: [[TMP2:%.+]] = tuple_element_addr [[TMP]] : $*(Int32, String), 1
// SIL: [[VALUE:%.+]] = integer_literal $Builtin.Int32, 300
// SIL: [[INT:%.+]] = struct $Int32 ([[VALUE]] : $Builtin.Int32)
// SIL: [[STR:%.*]] = apply %{{.*}} : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String
// SIL: store [[INT]] to [[TMP1]] : $*Int32
// SIL: store [[STR]] to [[TMP2]] : $*String
var tmp = testBuiltin()

// SIL-LABEL: sil public_external [transparent] @$s15def_transparent15testTransparent1xS2b_tF : $@convention(thin) (Bool) -> Bool {
// SIL: bb0(%0 : $Bool):
// SIL: return %0 : $Bool

// SIL-LABEL: sil public_external [transparent] @$s15def_transparent11testBuiltins5Int32V_SStyF : $@convention(thin) () -> (Int32, @owned String) {
// SIL: bb0:
// SIL: integer_literal $Builtin.Int32, 300
// SIL: string_literal utf8 "foo"
// SIL: return %{{.*}} : $(Int32, String)

// SIL-LABEL: sil public_external [transparent] @$s15def_transparent7test_bryyF : $@convention(thin) () -> () {
// SIL: bb{{.*}}:
// SIL: cond_br %{{.*}}, bb{{.*}}, bb{{.*}}
// SIL: bb{{.*}}:
// SIL: br bb{{.*}}
func wrap_br() {
  test_br()
}

// SIL-LABEL: sil public_external [transparent] @$s15def_transparent9do_switch1uyAA9MaybePairO_tF : $@convention(thin) (@guaranteed MaybePair) -> () {
// SIL: bb0(%0 : $MaybePair):
// SIL: switch_enum %0 : $MaybePair, case #MaybePair.Neither!enumelt: bb[[CASE1:[0-9]+]], case #MaybePair.Left!enumelt: bb[[CASE2:[0-9]+]], case #MaybePair.Right!enumelt: bb[[CASE3:[0-9]+]], case #MaybePair.Both!enumelt: bb[[CASE4:[0-9]+]]
// SIL: bb[[CASE4]](%{{.*}} : $(Int32, String)):
// SIL: bb[[CASE3]](%{{.*}} : $String):
// SIL: bb[[CASE2]](%{{.*}} : $Int32):
// SIL: bb[[CASE1]]:
func test_switch(u: MaybePair) {
  do_switch(u: u)
}

// SIL-LABEL: sil public_external [transparent] @$s15def_transparent7WrapperV3ValACs5Int32V_tcfC : $@convention(method) (Int32, @thin Wrapper.Type) -> Wrapper {
// SIL-LABEL: sil public_external [transparent] @$s15def_transparent7WrapperV8getValue{{[_0-9a-zA-Z]*}}F : $@convention(method) (Wrapper) -> Int32 {
// SIL-LABEL: sil public_external [transparent] @$s15def_transparent7WrapperV10valueAgains5Int32Vvg : $@convention(method) (Wrapper) -> Int32 {
// SIL-LABEL: sil public_external [transparent] @$s15def_transparent7WrapperV13getValueAgain{{[_0-9a-zA-Z]*}}F : $@convention(method) (Wrapper) -> Int32 {
func test_wrapper() {
  var w = Wrapper(Val: 42)
  
  print(w.value, terminator: "")
  print(w.getValue(), terminator: "")
  print(w.valueAgain, terminator: "")
  print(w.getValueAgain(), terminator: "")
}

// SIL-LABEL: sil public_external [transparent] @$s15def_transparent17open_existentials1p2cpyAA1P_p_AA2CP_ptF
func test_open_existentials(p: P, cp: CP) {
  // SIL: open_existential_addr immutable_access [[EXIST:%[0-9]+]] : $*any P to $*@opened([[N:".*"]], any P) Self
  // SIL: open_existential_ref [[EXIST:%[0-9]+]] : $any CP to $@opened([[M:".*"]], any CP) Self
  open_existentials(p: p, cp: cp)
}
