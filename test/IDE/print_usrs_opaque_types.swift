// This test makes sure we can generate USRs for a variety of declarations using
// opaque result types, even in the presence of errors or unusual generic
// signatures.

// RUN: %target-typecheck-verify-swift -disable-availability-checking
// RUN: %target-swift-ide-test -print-usrs -source-filename %s | %FileCheck -strict-whitespace %s

// CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test0C21UnifyingGenericParams1xQrx_tq_Rszr0_lF
func testUnifyingGenericParams<T, U>(x: T) -> some Collection where T == U {
  // expected-warning@-1 {{same-type requirement makes generic parameters 'U' and 'T' equivalent}}
  return []
}

// CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test0C22UnifyingGenericParams21xQrx_tSlRz7ElementQzRs_r0_lF
func testUnifyingGenericParams2<T, U>(x: T) -> some Collection where T: Collection, U == T.Element {
  return []
}

// CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test0C24ConcretizingGenericParam1xQrSi_tSiRszlF
func testConcretizingGenericParam<T>(x: T) -> some Collection where T == Int {
  // expected-warning@-1 {{same-type requirement makes generic parameter 'T' non-generic}}
  return []
}

struct GenericContext<T> {
  // CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test14GenericContextV0c8UnifyingD6Params1xQrx_tqd__RszlF
  func testUnifyingGenericParams<U>(x: T) -> some Collection where T == U {
    // expected-warning@-1 {{same-type requirement makes generic parameters 'U' and 'T' equivalent}}
    return []
  }

  // CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test14GenericContextV0c8UnifyingD7Params21xQrx_tSlRz7ElementQzRsd__lF
  func testUnifyingGenericParams2<U>(x: T) -> some Collection where T: Collection, U == T.Element {
    return []
  }

  // CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test14GenericContextVyQrxcqd__Rszluip
  subscript<U>(x: T) -> some Collection where T == U {
    // expected-warning@-1 {{same-type requirement makes generic parameters 'U' and 'T' equivalent}}
    // CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test14GenericContextVyQrxcqd__Rszluig
    get {
      return []
    }
  }
}

extension GenericContext where T == Int {
  // CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test14GenericContextVAASiRszlE0c12ConcretizingD5Param1xQrSi_tF
  func testConcretizingGenericParam(x: T) -> some Collection {
    return []
  }
}

struct TooGenericTooContext<T, U> {
}

extension TooGenericTooContext where T == U {
  // CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test010TooGenericD7ContextVAAq_RszrlE0c8UnifyingE6Params1xQrx_tF
  func testUnifyingGenericParams(x: T) -> some Collection {
    return []
  }
}

extension TooGenericTooContext where T: Collection, U == T.Element {
  // CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test010TooGenericD7ContextVAASlRz7ElementQzRs_rlE0c8UnifyingE7Params21xQrx_tF
  func testUnifyingGenericParams2(x: T) -> some Collection {
    return []
  }
}
extension TooGenericTooContext where T == Int {
  // CHECK: [[@LINE+1]]:{{[0-9]+}} s:14swift_ide_test010TooGenericD7ContextVAASiRszrlE0c12ConcretizingE5Param1xQrSi_tF
  func testConcretizingGenericParam(x: T) -> some Collection {
    return []
  }
}

