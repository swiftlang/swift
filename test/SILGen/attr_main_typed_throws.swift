// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library %s | %FileCheck %s

struct Err: Error { }

@main
struct ThrowingMain {
  static func main() throws(Err) { }
}

// CHECK: static func $main() throws(Err)

// CHECK-LABEL: sil hidden [ossa] @$s22attr_main_typed_throws12ThrowingMainV5$mainyyAA3ErrVYKFZ : $@convention(method) (@thin ThrowingMain.Type)
// CHECK: try_apply [[FN:%.*]]([[ARG:%.*]]) : $@convention(method) (@thin ThrowingMain.Type) -> @error Err, normal bb1, error bb2
// CHECK: bb1
// CHECK: return
// CHECK: bb2([[ERR:%.*]] : $Err):
// CHECK-NEXT:  throw [[ERR]] : $Err 

// CHECK-LABEL: sil [ossa] @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32
// CHECK: [[MAIN:%.*]] = function_ref @$s22attr_main_typed_throws12ThrowingMainV5$mainyyAA3ErrVYKFZ : $@convention(method) (@thin ThrowingMain.Type) -> @error Err
// CHECK-NEXT: try_apply [[MAIN]]([[ARG:%.*]]) : $@convention(method) (@thin ThrowingMain.Type) -> @error Err, normal [[NORMAL_BB:bb.*]], error [[ERROR_BB:bb[0-9]+]]
// CHECK: [[ERROR_BB]]([[ERR:%.*]] : $Err)
// CHECK-NEXT: [[ERR_STACK:%.*]] = alloc_stack $Err
// CHECK-NEXT:  store [[ERR]] to [trivial] [[ERR_STACK]] : $*Err
// CHECK: [[REPORT_FN:%.*]] = function_ref @$ss17_errorInMainTypedys5NeverOxs5ErrorRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : Error> (@in_guaranteed τ_0_0) -> Never
// CHECK-NEXT: apply [[REPORT_FN]]<Err>([[ERR_STACK]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Error> (@in_guaranteed τ_0_0) -> Never
// CHECK-NEXT: unreachable  
