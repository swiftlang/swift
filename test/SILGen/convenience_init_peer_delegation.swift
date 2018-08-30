// RUN: %target-swift-emit-silgen %s | %FileCheck %s

class X {
  init() {
  }

  // Convenience inits must dynamically dispatch designated inits...
  // CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation1XC0A0ACyt_tcfC
  // CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation1XC0A0ACyt_tcfc
  // CHECK:         class_method %7 : $X, #X.init!initializer.1
  convenience init(convenience: ()) {
    self.init()
  }

  // ...but can statically invoke peer convenience inits
  // CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation1XC17doubleConvenienceACyt_tcfC
  // CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation1XC17doubleConvenienceACyt_tcfc
  // CHECK:         function_ref @$S32convenience_init_peer_delegation1XC0A0ACyt_tcfc
  convenience init(doubleConvenience: ()) {
    self.init(convenience: ())
  }

  // CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation1XC8requiredACyt_tcfC
  // CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation1XC8requiredACyt_tcfc
  required init(required: ()) {
  }

  // CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation1XC19requiredConvenienceACyt_tcfC
  // CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation1XC19requiredConvenienceACyt_tcfc
  required convenience init(requiredConvenience: ()) {
    self.init(required: ())
  }

  // Convenience inits must dynamically dispatch required peer convenience inits
  // CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation1XC25requiredDoubleConvenienceACyt_tcfC
  // CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation1XC25requiredDoubleConvenienceACyt_tcfc
  // CHECK:         class_method %7 : $X, #X.init!initializer.1
  required convenience init(requiredDoubleConvenience: ()) {
    self.init(requiredDoubleConvenience: ())
  }
}

// CHECK-LABEL: sil hidden @$S32convenience_init_peer_delegation11invocations2xtyAA1XCm_tF
func invocations(xt: X.Type) {
  // CHECK: function_ref @$S32convenience_init_peer_delegation1XCACycfC
  _ = X()
  // CHECK: function_ref @$S32convenience_init_peer_delegation1XC0A0ACyt_tcfC
  _ = X(convenience: ())
  // CHECK: function_ref @$S32convenience_init_peer_delegation1XC17doubleConvenienceACyt_tcfC
  _ = X(doubleConvenience: ())
  // CHECK: function_ref @$S32convenience_init_peer_delegation1XC8requiredACyt_tcfC
  _ = X(required: ())
  // CHECK: function_ref @$S32convenience_init_peer_delegation1XC19requiredConvenienceACyt_tcfC
  _ = X(requiredConvenience: ())
  // CHECK: function_ref @$S32convenience_init_peer_delegation1XC25requiredDoubleConvenienceACyt_tcfC
  _ = X(requiredDoubleConvenience: ())

  // CHECK: class_method %0 : $@thick X.Type, #X.init!allocator.1
  _ = xt.init(required: ())
  // CHECK: class_method %0 : $@thick X.Type, #X.init!allocator.1
  _ = xt.init(requiredConvenience: ())
  // CHECK: class_method %0 : $@thick X.Type, #X.init!allocator.1
  _ = xt.init(requiredDoubleConvenience: ())
}

// CHECK-LABEL: sil_vtable X
//                -- designated init()
// CHECK-NOT:     @$S32convenience_init_peer_delegation1XCACycfC
// CHECK:         @$S32convenience_init_peer_delegation1XCACycfc

//                -- no unrequired convenience inits
// CHECK-NOT:     @$S32convenience_init_peer_delegation1XC0A0ACyt_tcfC
// CHECK-NOT:     @$S32convenience_init_peer_delegation1XC0A0ACyt_tcfc
// CHECK-NOT:     @$S32convenience_init_peer_delegation1XC17doubleConvenienceACyt_tcfC
// CHECK-NOT:     @$S32convenience_init_peer_delegation1XC17doubleConvenienceACyt_tcfc

//                -- designated init(required:)
// CHECK:         @$S32convenience_init_peer_delegation1XC8requiredACyt_tcfC
// CHECK:         @$S32convenience_init_peer_delegation1XC8requiredACyt_tcfc
//                -- convenience init(requiredConvenience:)
// CHECK:         @$S32convenience_init_peer_delegation1XC19requiredConvenienceACyt_tcfC
// CHECK:         @$S32convenience_init_peer_delegation1XC19requiredConvenienceACyt_tcfc
//                -- convenience init(requiredDoubleConvenience:)
// CHECK:         @$S32convenience_init_peer_delegation1XC25requiredDoubleConvenienceACyt_tcfC
// CHECK:         @$S32convenience_init_peer_delegation1XC25requiredDoubleConvenienceACyt_tcfc
