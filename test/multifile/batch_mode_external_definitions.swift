// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
//
// Check that bit-field accessors for external structs are only lazily (and not multiply) emitted:
//
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-neither-field.sil
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D FIELD1 -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-field1.sil
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D FIELD2 -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-field2.sil
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D FIELD1 -primary-file %S/Inputs/batch_mode_external_uses_2.swift -primary-file %S/Inputs/batch_mode_external_uses_1.swift > %t/two-primaries-field1-reorder.sil
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D FIELD2 -primary-file %S/Inputs/batch_mode_external_uses_2.swift -primary-file %S/Inputs/batch_mode_external_uses_1.swift > %t/two-primaries-field2-reorder.sil
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D FIELD1 -D FIELD2 -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-both-fields.sil
//
// RUN: %FileCheck --check-prefix=CHECK-FIELD1-ONLY %s <%t/two-primaries-field1.sil
// RUN: %FileCheck --check-prefix=CHECK-FIELD2-ONLY %s <%t/two-primaries-field2.sil
// RUN: %FileCheck --check-prefix=CHECK-FIELD1-ONLY-REORDER %s <%t/two-primaries-field1-reorder.sil
// RUN: %FileCheck --check-prefix=CHECK-FIELD2-ONLY-REORDER %s <%t/two-primaries-field2-reorder.sil
// RUN: %FileCheck --check-prefix=CHECK-BOTH-FIELDS %s <%t/two-primaries-both-fields.sil
// RUN: %FileCheck --check-prefix=CHECK-NEITHER-FIELD %s <%t/two-primaries-neither-field.sil
//
// CHECK-FIELD1-ONLY: sil_stage canonical
// CHECK-FIELD1-ONLY: use_extern_struct_field_1
// CHECK-FIELD1-ONLY: sil shared{{.*}}extern_struct$field$getter
// CHECK-FIELD1-ONLY-NOT: sil shared{{.*}}extern_struct$field$getter
// CHECK-FIELD1-ONLY: sil_stage canonical
// CHECK-FIELD1-ONLY-NOT: use_extern_struct_field_2
// CHECK-FIELD1-ONLY-NOT: sil shared{{.*}}extern_struct$field$getter
//
// CHECK-FIELD1-ONLY-REORDER: sil_stage canonical
// CHECK-FIELD1-ONLY-REORDER-NOT: use_extern_struct_field_2
// CHECK-FIELD1-ONLY-REORDER-NOT: sil shared{{.*}}extern_struct$field$getter
// CHECK-FIELD1-ONLY-REORDER: sil_stage canonical
// CHECK-FIELD1-ONLY-REORDER: use_extern_struct_field_1
// CHECK-FIELD1-ONLY-REORDER: sil shared{{.*}}extern_struct$field$getter
// CHECK-FIELD1-ONLY-REORDER-NOT: sil shared{{.*}}extern_struct$field$getter
//
// CHECK-FIELD2-ONLY: sil_stage canonical
// CHECK-FIELD2-ONLY-NOT: use_extern_struct_field_1
// CHECK-FIELD2-ONLY-NOT: sil shared{{.*}}extern_struct$field$getter
// CHECK-FIELD2-ONLY: sil_stage canonical
// CHECK-FIELD2-ONLY: use_extern_struct_field_2
// CHECK-FIELD2-ONLY: sil shared{{.*}}extern_struct$field$getter
// CHECK-FIELD2-ONLY-NOT: sil shared{{.*}}extern_struct$field$getter
//
// CHECK-FIELD2-ONLY-REORDER: sil_stage canonical
// CHECK-FIELD2-ONLY-REORDER: use_extern_struct_field_2
// CHECK-FIELD2-ONLY-REORDER: sil shared{{.*}}extern_struct$field$getter
// CHECK-FIELD2-ONLY-REORDER-NOT: sil shared{{.*}}extern_struct$field$getter
// CHECK-FIELD2-ONLY-REORDER: sil_stage canonical
// CHECK-FIELD2-ONLY-REORDER-NOT: use_extern_struct_field_1
// CHECK-FIELD2-ONLY-REORDER-NOT: sil shared{{.*}}extern_struct$field$getter
//
// CHECK-BOTH-FIELDS: sil_stage canonical
// CHECK-BOTH-FIELDS: use_extern_struct_field_1
// CHECK-BOTH-FIELDS: sil shared{{.*}}extern_struct$field$getter
// CHECK-BOTH-FIELDS-NOT: sil shared{{.*}}extern_struct$field$getter
// CHECK-BOTH-FIELDS: sil_stage canonical
// CHECK-BOTH-FIELDS: use_extern_struct_field_2
// CHECK-BOTH-FIELDS: sil shared{{.*}}extern_struct$field$getter
// CHECK-BOTH-FIELDS-NOT: sil shared{{.*}}extern_struct$field$getter
//
// CHECK-NEITHER-FIELD: sil_stage canonical
// CHECK-NEITHER-FIELD-NOT: use_extern_struct_field_1
// CHECK-NEITHER-FIELD-NOT: sil shared{{.*}}extern_struct$field$getter
// CHECK-NEITHER-FIELD: sil_stage canonical
// CHECK-NEITHER-FIELD-NOT: use_extern_struct_field_2
// CHECK-NEITHER-FIELD-NOT: sil shared{{.*}}extern_struct$field$getter
//


// Check that RawRepresentable conformances for external enums are only lazily (and not multiply) emitted:
//
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-neither-rawrep.sil
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D RAWREP1 -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-rawrep1.sil
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D RAWREP2 -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-rawrep2.sil
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D RAWREP1 -primary-file %S/Inputs/batch_mode_external_uses_2.swift -primary-file %S/Inputs/batch_mode_external_uses_1.swift > %t/two-primaries-rawrep1-reorder.sil
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D RAWREP2 -primary-file %S/Inputs/batch_mode_external_uses_2.swift -primary-file %S/Inputs/batch_mode_external_uses_1.swift > %t/two-primaries-rawrep2-reorder.sil
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D RAWREP1 -D RAWREP2 -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-both-rawreps.sil
//
// RUN: %FileCheck --check-prefix=CHECK-RAWREP1-ONLY %s <%t/two-primaries-rawrep1.sil
// RUN: %FileCheck --check-prefix=CHECK-RAWREP2-ONLY %s <%t/two-primaries-rawrep2.sil
// RUN: %FileCheck --check-prefix=CHECK-RAWREP1-ONLY-REORDER %s <%t/two-primaries-rawrep1-reorder.sil
// RUN: %FileCheck --check-prefix=CHECK-RAWREP2-ONLY-REORDER %s <%t/two-primaries-rawrep2-reorder.sil
// RUN: %FileCheck --check-prefix=CHECK-BOTH-RAWREPS %s <%t/two-primaries-both-rawreps.sil
// RUN: %FileCheck --check-prefix=CHECK-NEITHER-RAWREP %s <%t/two-primaries-neither-rawrep.sil
//
// CHECK-RAWREP1-ONLY: sil_stage canonical
// CHECK-RAWREP1-ONLY: take_rawrep_1
// CHECK-RAWREP1-ONLY: sil shared{{.*}}sSo8ext_enumVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW
// CHECK-RAWREP1-ONLY-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-RAWREP1-ONLY: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-RAWREP1-ONLY-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-RAWREP1-ONLY: sil_stage canonical
// CHECK-RAWREP1-ONLY-NOT: take_rawrep_1
// CHECK-RAWREP1-ONLY-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-RAWREP1-ONLY-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
//
// CHECK-RAWREP2-ONLY: sil_stage canonical
// CHECK-RAWREP2-ONLY-NOT: take_rawrep_2
// CHECK-RAWREP2-ONLY-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-RAWREP2-ONLY-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-RAWREP2-ONLY: sil_stage canonical
// CHECK-RAWREP2-ONLY: take_rawrep_2
// CHECK-RAWREP2-ONLY: sil shared{{.*}}sSo8ext_enumVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW
// CHECK-RAWREP2-ONLY-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-RAWREP2-ONLY: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-RAWREP2-ONLY-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
//
// CHECK-RAWREP1-ONLY-REORDER: sil_stage canonical
// CHECK-RAWREP1-ONLY-REORDER-NOT: take_rawrep_1
// CHECK-RAWREP1-ONLY-REORDER-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-RAWREP1-ONLY-REORDER-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-RAWREP1-ONLY-REORDER: sil_stage canonical
// CHECK-RAWREP1-ONLY-REORDER: take_rawrep_1
// CHECK-RAWREP1-ONLY-REORDER: sil shared{{.*}}sSo8ext_enumVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW
// CHECK-RAWREP1-ONLY-REORDER-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-RAWREP1-ONLY-REORDER: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-RAWREP1-ONLY-REORDER-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
//
// CHECK-RAWREP2-ONLY-REORDER: sil_stage canonical
// CHECK-RAWREP2-ONLY-REORDER: take_rawrep_2
// CHECK-RAWREP2-ONLY-REORDER: sil shared{{.*}}sSo8ext_enumVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW
// CHECK-RAWREP2-ONLY-REORDER-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-RAWREP2-ONLY-REORDER: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-RAWREP2-ONLY-REORDER-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-RAWREP2-ONLY-REORDER: sil_stage canonical
// CHECK-RAWREP2-ONLY-REORDER-NOT: take_rawrep_2
// CHECK-RAWREP2-ONLY-REORDER-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-RAWREP2-ONLY-REORDER-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
//
// CHECK-BOTH-RAWREPS: sil_stage canonical
// CHECK-BOTH-RAWREPS: take_rawrep_1
// CHECK-BOTH-RAWREPS: sil shared{{.*}}sSo8ext_enumVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW
// CHECK-BOTH-RAWREPS-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-BOTH-RAWREPS: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-BOTH-RAWREPS-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-BOTH-RAWREPS: sil_stage canonical
// CHECK-BOTH-RAWREPS: take_rawrep_2
// CHECK-BOTH-RAWREPS: sil shared{{.*}}sSo8ext_enumVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW
// CHECK-BOTH-RAWREPS-NOT: sil shared{{.*}}sSo8ext_enumVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW
// CHECK-BOTH-RAWREPS: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-BOTH-RAWREPS-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
//
// CHECK-NEITHER-RAWREP: sil_stage canonical
// CHECK-NEITHER-RAWREP-NOT: take_rawrep_1
// CHECK-NEITHER-RAWREP-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-NEITHER-RAWREP-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable
// CHECK-NEITHER-RAWREP: sil_stage canonical
// CHECK-NEITHER-RAWREP-NOT: take_rawrep_2
// CHECK-NEITHER-RAWREP-NOT: sil shared{{.*}}sSo8ext_enumVSYSCsACP8rawValue0cF0QzvgTW
// CHECK-NEITHER-RAWREP-NOT: sil_witness_table{{.*}}ext_enum: RawRepresentable


// Check external inline functions are only lazily (and not multiply) emitted:
//
// RUN: %target-swift-frontend -emit-ir -import-objc-header %S/Inputs/batch_mode_external_definitions.h -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-neither-func.ir
// RUN: %target-swift-frontend -emit-ir -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D FUNC1 -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-func1.ir
// RUN: %target-swift-frontend -emit-ir -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D FUNC2 -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-func2.ir
// RUN: %target-swift-frontend -emit-ir -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D FUNC1 -primary-file %S/Inputs/batch_mode_external_uses_2.swift -primary-file %S/Inputs/batch_mode_external_uses_1.swift > %t/two-primaries-func1-reorder.ir
// RUN: %target-swift-frontend -emit-ir -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D FUNC2 -primary-file %S/Inputs/batch_mode_external_uses_2.swift -primary-file %S/Inputs/batch_mode_external_uses_1.swift > %t/two-primaries-func2-reorder.ir
// RUN: %target-swift-frontend -emit-ir -import-objc-header %S/Inputs/batch_mode_external_definitions.h -D FUNC1 -D FUNC2 -primary-file %S/Inputs/batch_mode_external_uses_1.swift -primary-file %S/Inputs/batch_mode_external_uses_2.swift > %t/two-primaries-both-funcs.ir
//
// RUN: %FileCheck --check-prefix=CHECK-FUNC1-ONLY %s <%t/two-primaries-func1.ir
// RUN: %FileCheck --check-prefix=CHECK-FUNC2-ONLY %s <%t/two-primaries-func2.ir
// RUN: %FileCheck --check-prefix=CHECK-FUNC1-ONLY-REORDER %s <%t/two-primaries-func1-reorder.ir
// RUN: %FileCheck --check-prefix=CHECK-FUNC2-ONLY-REORDER %s <%t/two-primaries-func2-reorder.ir
// RUN: %FileCheck --check-prefix=CHECK-BOTH-FUNCS %s <%t/two-primaries-both-funcs.ir
// RUN: %FileCheck --check-prefix=CHECK-NEITHER-FUNC %s <%t/two-primaries-neither-func.ir
//
// CHECK-FUNC1-ONLY: ModuleID
// CHECK-FUNC1-ONLY: use_func_1
// CHECK-FUNC1-ONLY: define internal i32 @extern_inline_function
// CHECK-FUNC1-ONLY-NOT: define internal i32 @extern_inline_function
// CHECK-FUNC1-ONLY: ModuleID
// CHECK-FUNC1-ONLY-NOT: use_func_1
// CHECK-FUNC1-ONLY-NOT: define internal i32 @extern_inline_function
//
// CHECK-FUNC2-ONLY: ModuleID
// CHECK-FUNC2-ONLY-NOT: use_func_2
// CHECK-FUNC2-ONLY-NOT: define internal i32 @extern_inline_function
// CHECK-FUNC2-ONLY: ModuleID
// CHECK-FUNC2-ONLY: use_func_2
// CHECK-FUNC2-ONLY: define internal i32 @extern_inline_function
// CHECK-FUNC2-ONLY-NOT: define internal i32 @extern_inline_function
//
// CHECK-FUNC1-ONLY-REORDER: ModuleID
// CHECK-FUNC1-ONLY-REORDER-NOT: use_func_1
// CHECK-FUNC1-ONLY-REORDER-NOT: define internal i32 @extern_inline_function
// CHECK-FUNC1-ONLY-REORDER: ModuleID
// CHECK-FUNC1-ONLY-REORDER: use_func_1
// CHECK-FUNC1-ONLY-REORDER: define internal i32 @extern_inline_function
// CHECK-FUNC1-ONLY-REORDER-NOT: define internal i32 @extern_inline_function
//
// CHECK-FUNC2-ONLY-REORDER: ModuleID
// CHECK-FUNC2-ONLY-REORDER: use_func_2
// CHECK-FUNC2-ONLY-REORDER: define internal i32 @extern_inline_function
// CHECK-FUNC2-ONLY-REORDER-NOT: define internal i32 @extern_inline_function
// CHECK-FUNC2-ONLY-REORDER: ModuleID
// CHECK-FUNC2-ONLY-REORDER-NOT: use_func_2
// CHECK-FUNC2-ONLY-REORDER-NOT: define internal i32 @extern_inline_function
//
// CHECK-BOTH-FUNCS: ModuleID
// CHECK-BOTH-FUNCS: use_func_1
// CHECK-BOTH-FUNCS: define internal i32 @extern_inline_function
// CHECK-BOTH-FUNCS-NOT: define internal i32 @extern_inline_function
// CHECK-BOTH-FUNCS: ModuleID
// CHECK-BOTH-FUNCS: use_func_2
// CHECK-BOTH-FUNCS: define internal i32 @extern_inline_function
// CHECK-BOTH-FUNCS-NOT: define internal i32 @extern_inline_function
//
// CHECK-NEITHER-FUNC: ModuleID
// CHECK-NEITHER-FUNC-NOT: use_func_1
// CHECK-NEITHER-FUNC-NOT: define internal i32 @extern_inline_function
// CHECK-NEITHER-FUNC: ModuleID
// CHECK-NEITHER-FUNC-NOT: use_func_2
// CHECK-NEITHER-FUNC-NOT: define internal i32 @extern_inline_function

