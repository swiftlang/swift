// FIXME: crashes under opaque values
// RUN: not --crash %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -disable-availability-checking %s

// RUN: %target-swift-emit-silgen -disable-availability-checking %s | %FileCheck %s

// CHECK-LABEL: sil {{.*}}9withTuple
// CHECK: bb0(%0 : $*(Int, String)):
func withTuple() -> some Any {
  return (1, "hello")
}

struct S {}

// CHECK-LABEL: sil {{.*}}12withMetatype
// CHECK: bb0(%0 : $*@thick S.Type):
func withMetatype() -> some Any {
  return S.self
}

// CHECK-LABEL: sil {{.*}}12withFunction
// CHECK: bb0(%0 : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <()>):
func withFunction() -> some Any {
  let f: () -> () = {}
  return f
}

// CHECK-LABEL: sil {{.*}}30withTupleOfMetatypeAndFunction
// CHECK: bb0(%0 : $*(@thick S.Type, @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <()>)):
func withTupleOfMetatypeAndFunction() -> some Any {
  let f: () -> () = {}
  return (S.self, f)
}
