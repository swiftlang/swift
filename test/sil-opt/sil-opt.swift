// RUN: %target-swift-frontend -primary-file %s -module-name Swift -g -sil-serialize-all -module-link-name swiftCore -O -parse-as-library -parse-stdlib -emit-module -emit-module-path - -o /dev/null | %target-sil-opt -verify -module-name="Swift" | FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -module-name Swift -g -O -parse-as-library -parse-stdlib -emit-sib -o - | %target-sil-opt -verify -module-name="Swift" | FileCheck %s -check-prefix=SIB-CHECK

// CHECK: import Builtin
// CHECK: import Swift
// SIB-CHECK: import Builtin
// SIB-CHECK: import Swift

// CHECK: struct X {
// CHECK-NEXT:  func test()
// CHECK-NEXT:  init
// CHECK-NEXT: }
// SIB-CHECK: struct X {
// SIB-CHECK-NEXT:  func test()
// SIB-CHECK-NEXT:  init
// SIB-CHECK-NEXT: }

// CHECK: func unknown()
// SIB-CHECK: func unknown()

// CHECK: sil hidden [fragile] @_TFVSs1XCfMS_FT_S_ : $@thin (@thin X.Type) -> X
// CHECK: bb0
// CHECK-NEXT: struct $X ()
// CHECK-NEXT: return
// SIB-CHECK: sil hidden @_TFVSs1XCfMS_FT_S_ : $@thin (@thin X.Type) -> X
// SIB-CHECK: bb0
// SIB-CHECK-NEXT: struct $X ()
// SIB-CHECK-NEXT: return

// CHECK: sil @unknown : $@thin () -> ()
// SIB-CHECK: sil @unknown : $@thin () -> ()

// CHECK-LABEL: sil hidden [fragile] @_TFVSs1X4testfS_FT_T_ : $@cc(method) @thin (X) -> ()
// CHECK: bb0
// CHECK-NEXT: function_ref
// CHECK-NEXT: function_ref @unknown : $@thin () -> ()
// CHECK-NEXT: apply
// CHECK-NEXT: tuple
// CHECK-NEXT: return
// SIB-CHECK-LABEL: sil hidden @_TFVSs1X4testfS_FT_T_ : $@cc(method) @thin (X) -> ()
// SIB-CHECK: bb0
// SIB-CHECK-NEXT: function_ref
// SIB-CHECK-NEXT: function_ref @unknown : $@thin () -> ()
// SIB-CHECK-NEXT: apply
// SIB-CHECK-NEXT: tuple
// SIB-CHECK-NEXT: return

@asmname("unknown")
public func unknown() -> ()

struct X {
  func test() {
    unknown()
  }
}
