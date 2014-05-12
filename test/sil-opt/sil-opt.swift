// RUN: %swift %s -module-name Swift -g -sil-serialize-all -module-link-name swift_stdlib_core -O2 -parse-as-library -parse-stdlib -emit-module -emit-module-path - -o /dev/null | %sil-opt -verify -module-name="Swift" | FileCheck %s

// CHECK: import Builtin
// CHECK: import Swift

// CHECK: struct X {
// CHECK-NEXT:  func test()
// CHECK-NEXT: }

// CHECK: func unknown()

// CHECK: sil @_TFVSs1XCfMS_FT_S_ : $@thin (@thin X.Type) -> X
// CHECK: bb0
// CHECK-NEXT: struct $X ()
// CHECK-NEXT: return

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
