
// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

func takesOptionalFunction(_: (() -> ())?) {}

struct CustomNull : ExpressibleByNilLiteral {
  init(nilLiteral: ()) {}
}

func takesANull(_: CustomNull) {}

// CHECK-LABEL: sil hidden @$S11nil_literal4testyyF : $@convention(thin) () -> ()
func test() {
  // CHECK: [[NIL:%.*]] = enum $Optional<@callee_guaranteed () -> ()>, #Optional.none!enumelt
  // CHECK: [[FN:%.*]] = function_ref @$S11nil_literal21takesOptionalFunctionyyyycSgF
  // CHECK: apply [[FN]]([[NIL]])
  _ = takesOptionalFunction(nil)

  // CHECK: [[METATYPE:%.*]] = metatype $@thin CustomNull.Type
  // CHECK: [[NIL_FN:%.*]] = function_ref @$S11nil_literal10CustomNullV0A7LiteralACyt_tcfC
  // CHECK: [[NIL:%.*]] = apply [[NIL_FN]]([[METATYPE]])
  // CHECK: [[FN:%.*]] = function_ref @$S11nil_literal10takesANullyyAA10CustomNullVF
  // CHECK: apply [[FN]]([[NIL]])
  _ = takesANull(nil)
}
