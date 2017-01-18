// -- Try all the permutations of file order possible.

// Abc
// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_A
// aBc
// RUN: %target-swift-frontend -emit-silgen %s -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_B
// abC
// RUN: %target-swift-frontend -emit-silgen %s %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_C

// Bac
// RUN: %target-swift-frontend -emit-silgen -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %s %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_B
// bAc
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -primary-file %s %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_A
// baC
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %s -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_C

// OS X 10.9
// RUN: %target-swift-frontend -emit-silgen -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %s %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_C
// cAb
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -primary-file %s %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_A
// caB
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %s -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_B

// Acb
// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_A
// aCb
// RUN: %target-swift-frontend -emit-silgen %s -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_C
// abC
// RUN: %target-swift-frontend -emit-silgen %s %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_B

// Bca
// RUN: %target-swift-frontend -emit-silgen -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %s -module-name main | %FileCheck %s --check-prefix=FILE_B
// bCa
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %s -module-name main | %FileCheck %s --check-prefix=FILE_C
// bcA
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -primary-file %s -module-name main | %FileCheck %s --check-prefix=FILE_A

// Cba
// RUN: %target-swift-frontend -emit-silgen -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %s -module-name main | %FileCheck %s --check-prefix=FILE_C
// cBa
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %s -module-name main | %FileCheck %s --check-prefix=FILE_B
// cbA
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -primary-file %s -module-name main | %FileCheck %s --check-prefix=FILE_A

// FILE_A-NOT: sil [transparent] [thunk] @_TTWV4main5Things9EquatableS_ZFS1_oi2ee
// FILE_A-NOT: sil [transparent] [thunk] @_TTWV4main5Things8HashableS_FS1_g9hashValueSi
// FILE_A-NOT: sil_witness_table Thing: Hashable module main
// FILE_A-NOT: sil_witness_table Thing: Equatable module main

// FILE_B-NOT: sil [transparent] [thunk] @_TTWV4main5Things9EquatableS_ZFS1_oi2ee
// FILE_B: sil [transparent] [thunk] @_TTWV4main5Things8HashableS_FS1_g9hashValueSi
// FILE_B-NOT: sil [transparent] [thunk] @_TTWV4main5Things9EquatableS_ZFS1_oi2ee

// FILE_B-NOT: sil_witness_table Thing: Equatable module main
// FILE_B: sil_witness_table Thing: Hashable module main
// FILE_B-NOT: sil_witness_table Thing: Equatable module main

// FILE_C-NOT: sil [transparent] [thunk] @_TTWV4main5Things8HashableS_FS1_g9hashValueSi
// FILE_C: sil [transparent] [thunk] @_TTWV4main5Things9EquatableS_ZFS1_oi2ee
// FILE_C-NOT: sil [transparent] [thunk] @_TTWV4main5Things8HashableS_FS1_g9hashValueSi

// FILE_C-NOT: sil_witness_table Thing: Hashable module main
// FILE_C: sil_witness_table Thing: Equatable module main
// FILE_C-NOT: sil_witness_table Thing: Hashable module main

struct Thing { var value: Int }
