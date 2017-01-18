// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir -parse-as-library | %FileCheck %s

import c_layout

// CHECK-DAG: @__swift_reflection_version = linkonce_odr hidden constant i16 {{[0-9]+}}

// CHECK-DAG: @_TMRfV28reflection_metadata_imported15HasImportedType = internal constant {{.*}}swift3_fieldmd
// CHECK-DAG: @_TMRbVSC1A = linkonce_odr hidden constant {{.*}}swift3_builtin
// CHECK-DAG: @_TMRbVSC11CrappyColor = linkonce_odr hidden constant {{.*}}swift3_builtin
// CHECK-DAG: @_TMRaVSC11CrappyColors16RawRepresentable8c_layout = linkonce_odr hidden constant {{.*}}swift3_assocty

struct HasImportedType {
  let a: A
  let c: CrappyColor
}

func takesRawRepresentable<T : RawRepresentable>(_: T) {}

func passesRawRepresentable(_ c: CrappyColor) {
  takesRawRepresentable(c)
}
