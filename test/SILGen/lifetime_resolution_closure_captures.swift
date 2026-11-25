// RUN: %target-swift-emit-silgen -enable-lifetime-resolution -module-name main %s | %FileCheck %s --check-prefixes=CHECK,LR
// RUN: %target-swift-emit-silgen -module-name main %s | %FileCheck %s --check-prefixes=CHECK,WRAPPER

// A captured `borrowing` or `consuming` parameter is marked `@noImplicitCopy` within a closure and does
// not use the moveonlywrapper type under LifetimeResolution.

class C {}
func useC(_ c: C) {}
func callNonEscaping(_ f: () -> ()) { f() }
func callEscaping(_ f: @escaping () -> ()) { f() }
func useGeneric<T>(_ t: borrowing T) {}

// CHECK-LABEL: sil private [closure] [ossa] {{.*}}@$s4main16borrowingCaptureyyAA1CCFyyXEfU_ :
// LR:          bb0([[C:%.*]] : @noImplicitCopy @closureCapture @guaranteed $C):
// LR-NOT:      copyable_to_moveonlywrapper
// WRAPPER:     bb0([[C:%.*]] : @closureCapture @guaranteed $C):
// WRAPPER:     copyable_to_moveonlywrapper [guaranteed] [[C]]
// CHECK-LABEL: } // end sil function '$s4main16borrowingCaptureyyAA1CCFyyXEfU_'
func borrowingCapture(_ c: borrowing C) {
  callNonEscaping { useC(c) }
}

// CHECK-LABEL: sil private [closure] [ossa] {{.*}}@$s4main27borrowingAddressOnlyCaptureyyxlFyyXEfU_ :
// LR:          bb0({{%.*}} : @noImplicitCopy @closureCapture @guaranteed $T):
// LR-NOT:      copyable_to_moveonlywrapper
// WRAPPER:     bb0([[T:%.*]] : @closureCapture $*T):
// WRAPPER:     copyable_to_moveonlywrapper_addr [[T]]
// CHECK-LABEL: } // end sil function '$s4main27borrowingAddressOnlyCaptureyyxlFyyXEfU_'
func borrowingAddressOnlyCapture<T>(_ t: borrowing T) {
  callNonEscaping { useGeneric(t) }
}

// CHECK-LABEL: sil private [closure] [ossa] {{.*}}@$s4main16consumingCaptureyyAA1CCnFyyXEfU_ :
// LR:          bb0({{%.*}} : @noImplicitCopy @closureCapture $*C):
// LR-NOT:      copyable_to_moveonlywrapper
// WRAPPER:     bb0([[C:%.*]] : @closureCapture $*C):
// WRAPPER:     copyable_to_moveonlywrapper_addr [[C]]
// CHECK-LABEL: } // end sil function '$s4main16consumingCaptureyyAA1CCnFyyXEfU_'
func consumingCapture(_ c: consuming C) {
  callNonEscaping { useC(c) }
}

// CHECK-LABEL: sil private [closure] [ossa] {{.*}}@$s4main24consumingEscapingCaptureyyAA1CCnFyycfU_ :
// LR:          bb0({{%.*}} : @noImplicitCopy @closureCapture @guaranteed ${ var C }):
// LR-NOT:      copyable_to_moveonlywrapper
// WRAPPER:     bb0({{%.*}} : @closureCapture @guaranteed ${ var C }):
// WRAPPER:     copyable_to_moveonlywrapper_addr
// CHECK-LABEL: } // end sil function '$s4main24consumingEscapingCaptureyyAA1CCnFyycfU_'
func consumingEscapingCapture(_ c: consuming C) {
  callEscaping { useC(c) }
}

// CHECK-LABEL: sil private [closure] [ossa] {{.*}}@$s4main14defaultCaptureyyAA1CCFyyXEfU_ :
// CHECK:       bb0({{%.*}} : @closureCapture @guaranteed $C):
// CHECK-LABEL: } // end sil function '$s4main14defaultCaptureyyAA1CCFyyXEfU_'
func defaultCapture(_ c: C) {
  callNonEscaping { useC(c) }
}
