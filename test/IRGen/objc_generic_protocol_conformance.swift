// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -primary-file %s -import-objc-header %S/Inputs/objc_generic_protocol_conformance.h | %FileCheck --check-prefix=SIL %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -primary-file %s -import-objc-header %S/Inputs/objc_generic_protocol_conformance.h | %FileCheck --check-prefix=IR %s

// REQUIRES: objc_interop

protocol P {
  func foo()
}

extension Foo: P {}

// SIL-LABEL: sil private [transparent] [thunk] @_T0So3FooCyxG33objc_generic_protocol_conformance1PADs9AnyObjectRzlAdEP3fooyyFTW {{.*}} @pseudogeneric
// IR-LABEL: define internal swiftcc void @_T0So3FooCyxG33objc_generic_protocol_conformance1PADs9AnyObjectRzlAdEP3fooyyFTW(%TSo3FooC** noalias nocapture swiftself dereferenceable({{4|8}}), %swift.type*{{( %Self)?}}, i8**{{( %SelfWitnessTable)?}})
