// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s -import-objc-header %S/Inputs/objc_generic_protocol_conformance.h | %FileCheck --check-prefix=SIL %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir %s -import-objc-header %S/Inputs/objc_generic_protocol_conformance.h | %FileCheck --check-prefix=IR %s

// REQUIRES: objc_interop

protocol P {
  func foo()
}

extension Foo: P {}

// SIL-LABEL: sil hidden [transparent] [thunk] @_TTWuRxs9AnyObjectrGCSo3Foo{{.*}} @pseudogeneric
// IR-LABEL: define hidden void @_TTWuRxs9AnyObjectrGCSo3Foo{{.*}}(%CSo3Foo** noalias nocapture dereferenceable({{4|8}}), %swift.type*{{( %Self)?}}, i8**{{( %SelfWitnessTable)?}})
