// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir -parse-as-library | %FileCheck %s

import c_layout

// CHECK-DAG: @__swift_reflection_version = linkonce_odr hidden constant i16 {{[0-9]+}}

// CHECK-DAG: @"$S28reflection_metadata_imported15HasImportedTypeVMF" = internal constant {{.*}}section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DAG: @"$SSo1AVMB" = linkonce_odr hidden constant {{.*}}section "{{[^"]*swift5_builtin|.sw5bltn\$B}}
// CHECK-DAG: @"$SSo11CrappyColorVMB" = linkonce_odr hidden constant {{.*}}section "{{[^"]*swift5_builtin|.sw5bltn\$B}}
// CHECK-DAG: @"$SSo11CrappyColorVSYSCMA" = linkonce_odr hidden constant {{.*}}section "{{[^"]*swift5_assocty|.sw5asty\$B}}

struct HasImportedType {
  let a: A
  let c: CrappyColor
}

func takesRawRepresentable<T : RawRepresentable>(_: T) {}

func passesRawRepresentable(_ c: CrappyColor) {
  takesRawRepresentable(c)
}
