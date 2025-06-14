// RUN: %swift_frontend_plain -S -emit-silgen %s -I %S/Inputs -o - -parse-as-library | %FileCheck %s

import Variadic

public func f() {
  withVaList([0.0]) {
    u_vformatMessage(0, $0)
  }
}

// CHECK: bb0(%0 : $*(), %1 : $CVaListPointer):
// CHECK: %3 = function_ref @$sSC16u_vformatMessageyys5Int32V_s14CVaListPointerVSgtcvg : $@convention(thin) () -> @owned @callee_guaranteed (Int32, Optional<CVaListPointer>) -> ()
// CHECK: %4 = apply %3() : $@convention(thin) () -> @owned @callee_guaranteed (Int32, Optional<CVaListPointer>) -> ()
// CHECK: %5 = integer_literal $Builtin.IntLiteral, 0
// CHECK: %6 = metatype $@thin Int32.Type
// CHECK: %7 = function_ref @$ss5Int32V22_builtinIntegerLiteralABBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: %8 = apply %7(%5, %6) : $@convention(method) (Builtin.IntLiteral, @thin Int32.Type) -> Int32
// CHECK: %9 = enum $Optional<CVaListPointer>, #Optional.some!enumelt, %1
// CHECK: %10 = begin_borrow %4
// CHECK: %11 = apply %10(%8, %9) : $@callee_guaranteed (Int32, Optional<CVaListPointer>) -> ()
