// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | FileCheck %s --check-prefix=FILE_A
// RUN: %target-swift-frontend -emit-silgen %s -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | FileCheck %s --check-prefix=FILE_B
// RUN: %target-swift-frontend -emit-silgen %s %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | FileCheck %s --check-prefix=FILE_C

// FILE_A-NOT: sil [transparent] [thunk] @_TTWV4main5ThingSs9EquatableS_ZFS1_oi2eeuRq_S1__fMq_FTq_q__Sb
// FILE_A-NOT: sil [transparent] [thunk] @_TTWV4main5ThingSs8HashableS_FS1_g9hashValueSi
// FILE_A-NOT: sil_witness_table Thing: Hashable module main
// FILE_A-NOT: sil_witness_table Thing: Equatable module main

// FILE_B-NOT: sil [transparent] [thunk] @_TTWV4main5ThingSs9EquatableS_ZFS1_oi2eeuRq_S1__fMq_FTq_q__Sb
// FILE_B: sil [transparent] [thunk] @_TTWV4main5ThingSs8HashableS_FS1_g9hashValueSi
// FILE_B-NOT: sil [transparent] [thunk] @_TTWV4main5ThingSs9EquatableS_ZFS1_oi2eeuRq_S1__fMq_FTq_q__Sb

// FILE_B-NOT: sil_witness_table Thing: Equatable module main
// FILE_B: sil_witness_table Thing: Hashable module main
// FILE_B-NOT: sil_witness_table Thing: Equatable module main

// FILE_C-NOT: sil [transparent] [thunk] @_TTWV4main5ThingSs8HashableS_FS1_g9hashValueSi
// FILE_C: sil [transparent] [thunk] @_TTWV4main5ThingSs9EquatableS_ZFS1_oi2eeuRq_S1__fMq_FTq_q__Sb
// FILE_C-NOT: sil [transparent] [thunk] @_TTWV4main5ThingSs8HashableS_FS1_g9hashValueSi

// FILE_C-NOT: sil_witness_table Thing: Hashable module main
// FILE_C: sil_witness_table Thing: Equatable module main
// FILE_C-NOT: sil_witness_table Thing: Hashable module main

struct Thing { var value: Int }
