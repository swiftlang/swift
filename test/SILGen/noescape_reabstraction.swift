// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

struct S {}

func noescape_concrete(@noescape x: S -> S) {
  noescape_generic(x)
}

func noescape_generic<T>(@noescape x: T -> T) {
}

// CHECK-LABEL: sil hidden @_TF22noescape_reabstraction17noescape_concreteFFVS_1SS0_T_
// CHECK:         function_ref [[REABSTRACTION_THUNK:@_TTRXFo_dV22noescape_reabstraction1S_dS0__XFo_iS0__iS0__]]

func concrete(x: S -> S) {
  noescape_generic(x)
}

func generic<T>(x: T -> T) {
}

// CHECK-LABEL: sil hidden @_TF22noescape_reabstraction8concreteFFVS_1SS0_T_
// CHECK:         function_ref [[REABSTRACTION_THUNK]]
