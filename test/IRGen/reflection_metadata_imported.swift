// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir -parse-as-library | %FileCheck %s

import c_layout

// CHECK-DAG: @__swift_reflection_version = linkonce_odr hidden constant i16 {{[0-9]+}}

// CHECK-DAG: @"$S28reflection_metadata_imported15HasImportedTypeVMF" = internal constant {{.*}}swift4_fieldmd
// CHECK-DAG: @"$SSo1AVMB" = linkonce_odr hidden constant {{.*}}swift4_builtin
// CHECK-DAG: @"$SSo11CrappyColorVMB" = linkonce_odr hidden constant {{.*}}swift4_builtin
// CHECK-DAG: @"$SSo11CrappyColorVs16RawRepresentableSCMA" = linkonce_odr hidden constant {{.*}}swift4_assocty

struct HasImportedType {
  let a: A
  let c: CrappyColor
}

func takesRawRepresentable<T : RawRepresentable>(_: T) {}

func passesRawRepresentable(_ c: CrappyColor) {
  takesRawRepresentable(c)
}
