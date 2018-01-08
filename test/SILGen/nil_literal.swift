// REQUIRES: plus_one_runtime

// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

func takesOptionalFunction(_: (() -> ())?) {}

struct CustomNull : ExpressibleByNilLiteral {
  init(nilLiteral: ()) {}
}

func takesANull(_: CustomNull) {}

// CHECK-LABEL: sil hidden @$S11nil_literal4testyyF : $@convention(thin) () -> ()
func test() {
  // CHECK: [[NIL:%.*]] = enum $Optional<@callee_guaranteed () -> ()>, #Optional.none!enumelt
  // CHECK: br [[DEST_BB:bb.*]]([[NIL]] : $Optional<@callee_guaranteed () -> ()>)
  // CHECK: [[DEST_BB]]([[NIL_ARG:%.*]] : @owned $Optional<@callee_guaranteed () -> ()>):
  // CHECK: [[FN:%.*]] = function_ref @$S11nil_literal21takesOptionalFunctionyyyycSgF
  // CHECK: apply [[FN]]([[NIL_ARG]])
  _ = takesOptionalFunction(nil)

  // CHECK: [[METATYPE:%.*]] = metatype $@thin CustomNull.Type
  // CHECK: [[NIL_FN:%.*]] = function_ref @$S11nil_literal10CustomNullV0A7LiteralACyt_tcfC
  // CHECK: [[NIL:%.*]] = apply [[NIL_FN]]([[METATYPE]])
  // CHECK: [[FN:%.*]] = function_ref @$S11nil_literal10takesANullyyAA10CustomNullVF
  // CHECK: apply [[FN]]([[NIL]])
  _ = takesANull(nil)
}
