// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

struct S {}

func noescape_concrete(_ x: (S) -> S) {
  noescape_generic(x)
}

func noescape_generic<T>(_ x: (T) -> T) {
}

// CHECK-LABEL: sil hidden @_TF22noescape_reabstraction17noescape_concreteFFVS_1SS0_T_
// CHECK:         function_ref [[REABSTRACTION_THUNK:@_TTRXFo_dV22noescape_reabstraction1S_dS0__XFo_iS0__iS0__]]

func concrete(_ x: (S) -> S) {
  noescape_generic(x)
}

func generic<T>(_ x: (T) -> T) {
}

// CHECK-LABEL: sil hidden @_TF22noescape_reabstraction8concreteFFVS_1SS0_T_
// CHECK:         function_ref [[REABSTRACTION_THUNK]]
