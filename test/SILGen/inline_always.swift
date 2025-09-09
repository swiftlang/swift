// RUN: %target-swift-emit-silgen -enable-experimental-feature InlineAlways -parse-as-library %s | %FileCheck %s

// REQUIRES: swift_feature_InlineAlways

// CHECK-LABEL: sil hidden [heuristic_always_inline] [ossa] @$s13inline_always0b1_A7_calleeyyF : $@convention(thin) () -> ()
@inline(__always)
func always_inline_callee() {}

// CHECK-LABEL: sil hidden [always_inline] [ossa] @$s13inline_always0b1_A8_callee2yyF : $@convention(thin) () -> ()
@inline(always)
func always_inline_callee2() {}

// CHECK-LABEL: sil hidden [heuristic_always_inline] [ossa] @$s13inline_always11testClosureyyF
@inline(__always)
func testClosure() {
  // CHECK-LABEL: sil private [heuristic_always_inline] [ossa] @$s13inline_always11testClosureyyFyycfU_
  _ = { }
}
// CHECK-LABEL: sil hidden [always_inline] [ossa] @$s13inline_always12testClosure2yyF
@inline(always)
func testClosure2() {
  // CHECK-LABEL: sil private [always_inline] [ossa] @$s13inline_always12testClosure2yyFyycfU_
  _ = { }
}
protocol AlwaysInline {
  func alwaysInlined()
}

struct AlwaysInlinedMember : AlwaysInline {
  // CHECK-LABEL: sil hidden [heuristic_always_inline] [ossa] @$s13inline_always19AlwaysInlinedMemberV0bD0yyF : $@convention(method) (AlwaysInlinedMember) -> () {
  @inline(__always)
  func alwaysInlined() {}

  @inline(__always)
  var alwaysInlinedVar: Int {
    // CHECK-LABEL: sil hidden [heuristic_always_inline] [ossa] @$s13inline_always19AlwaysInlinedMemberV0bD3VarSivg : $@convention(method) (AlwaysInlinedMember) -> Int
    get { return 0 }

    // CHECK-LABEL: sil hidden [heuristic_always_inline] [ossa] @$s13inline_always19AlwaysInlinedMemberV0bD3VarSivs : $@convention(method) (Int, @inout AlwaysInlinedMember) -> ()
    set { }
  }

  // CHECK-LABEL: sil hidden [heuristic_always_inline] [ossa] @$s13inline_always19AlwaysInlinedMemberV0bD6GetterSivg : $@convention(method) (AlwaysInlinedMember) -> Int
  var alwaysInlinedGetter: Int {
    @inline(__always)
    get { return 0 }
  }
}

// CHECK-LABEL: sil private [transparent] [thunk] [heuristic_always_inline] [ossa] @$s13inline_always19AlwaysInlinedMemberVAA0C6InlineA2aDP0bD0yyFTW : $@convention(witness_method: AlwaysInline) (@in_guaranteed AlwaysInlinedMember) -> () {

struct AlwaysInlinedMember2 : AlwaysInline {
  // CHECK-LABEL: sil hidden [always_inline] [ossa] @$s13inline_always20AlwaysInlinedMember2V0bD0yyF : $@convention(method) (AlwaysInlinedMember2) -> () {
  @inline(always)
  func alwaysInlined() {}

  @inline(always)
  var alwaysInlinedVar: Int {
    // CHECK-LABEL: sil hidden [always_inline] [ossa] @$s13inline_always20AlwaysInlinedMember2V0bD3VarSivg : $@convention(method) (AlwaysInlinedMember2) -> Int
    get { return 0 }

    // CHECK-LABEL: sil hidden [always_inline] [ossa] @$s13inline_always20AlwaysInlinedMember2V0bD3VarSivs : $@convention(method) (Int, @inout AlwaysInlinedMember2) -> ()
    set { }
  }

  // CHECK-LABEL: sil hidden [always_inline] [ossa] @$s13inline_always20AlwaysInlinedMember2V0bD6GetterSivg : $@convention(method) (AlwaysInlinedMember2) -> Int
  var alwaysInlinedGetter: Int {
    @inline(always)
    get { return 0 }
  }
}
// CHECK-LABEL: sil private [transparent] [thunk] [heuristic_always_inline] [ossa] @$s13inline_always20AlwaysInlinedMember2VAA0C6InlineA2aDP0bD0yyFTW : $@convention(witness_method: AlwaysInline) (@in_guaranteed AlwaysInlinedMember2) -> () {
