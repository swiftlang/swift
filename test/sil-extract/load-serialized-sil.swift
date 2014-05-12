// RUN: %swift %s -module-name Swift -g -sil-serialize-all -module-link-name swift_stdlib_core -O2 -parse-as-library -parse-stdlib -emit-module -emit-module-path - -o /dev/null | sil-extract -module-name="Swift" -func="_TFVSs1X4testfS_FT_T_" | FileCheck %s

// CHECK: import Builtin
// CHECK: import Swift

// CHECK: struct X {
// CHECK-NEXT:  func test()
// CHECK-NEXT: }

// CHECK: func unknown()

// CHECK: sil public @_TFVSs1XCfMS_FT_S_ : $@thin (@thin X.Type) -> X
// CHECK-NOT: bb0

// CHECK: sil @unknown : $@thin () -> ()

// CHECK-LABEL: sil @_TFVSs1X4testfS_FT_T_ : $@cc(method) @thin (X) -> ()
// CHECK: bb0
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown : $@thin () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: tuple
// CHECK-NEXT: return

@asmname("unknown")
func unknown() -> ()

struct X {
  func test() {
    unknown()
  }
}
