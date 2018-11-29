// RUN: %target-swift-emit-silgen -parse-as-library -enable-sil-ownership %s | %FileCheck %s

// CHECK-LABEL: sil hidden [always_inline] @$s13inline_always0b1_A7_calleeyyF : $@convention(thin) () -> ()
@inline(__always)
func always_inline_callee() {}

protocol AlwaysInline {
  func alwaysInlined()
}

struct AlwaysInlinedMember : AlwaysInline {
  // CHECK-LABEL: sil hidden [always_inline] @$s13inline_always19AlwaysInlinedMemberV0bD0yyF : $@convention(method) (AlwaysInlinedMember) -> () {
  @inline(__always)
  func alwaysInlined() {}

  @inline(__always)
  var alwaysInlinedVar: Int {
    // CHECK-LABEL: sil hidden [always_inline] @$s13inline_always19AlwaysInlinedMemberV0bD3VarSivg : $@convention(method) (AlwaysInlinedMember) -> Int
    get { return 0 }

    // CHECK-LABEL: sil hidden [always_inline] @$s13inline_always19AlwaysInlinedMemberV0bD3VarSivs : $@convention(method) (Int, @inout AlwaysInlinedMember) -> ()
    set { }
  }

  // CHECK-LABEL: sil hidden [always_inline] @$s13inline_always19AlwaysInlinedMemberV0bD6GetterSivg : $@convention(method) (AlwaysInlinedMember) -> Int
  var alwaysInlinedGetter: Int {
    @inline(__always)
    get { return 0 }
  }
}

// CHECK-LABEL: sil private [transparent] [thunk] [always_inline] @$s13inline_always19AlwaysInlinedMemberVAA0C6InlineA2aDP0bD0yyFTW : $@convention(witness_method: AlwaysInline) (@in_guaranteed AlwaysInlinedMember) -> () {
