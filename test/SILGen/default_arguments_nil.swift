// RUN: %target-swift-emit-silgen %s | %FileCheck %s

public func takesAnInt(_: Int? = nil) {}

// CHECK-LABEL: sil [ossa] @$s21default_arguments_nil15callsTakesAnIntyyF : $@convention(thin) () -> () {
// CHECK: [[NIL:%.*]] = enum $Optional<Int>, #Optional.none!enumelt
// CHECK: [[FN:%.*]] = function_ref @$s21default_arguments_nil10takesAnIntyySiSgF
// CHECK: apply [[FN]]([[NIL]]) : $@convention(thin) (Optional<Int>) -> ()
// CHECK: return
public func callsTakesAnInt() {
  takesAnInt()
}