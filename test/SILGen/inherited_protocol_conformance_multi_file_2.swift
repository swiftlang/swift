// -- Try all the permutations of file order possible.

// Abc
// RUN: %target-swift-emit-silgen -enable-sil-ownership -primary-file %s %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_A
// aBc
// RUN: %target-swift-emit-silgen -enable-sil-ownership %s -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_B
// abC
// RUN: %target-swift-emit-silgen -enable-sil-ownership %s %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_C

// Bac
// RUN: %target-swift-emit-silgen -enable-sil-ownership -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %s %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_B
// bAc
// RUN: %target-swift-emit-silgen -enable-sil-ownership %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -primary-file %s %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_A
// baC
// RUN: %target-swift-emit-silgen -enable-sil-ownership %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %s -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -module-name main | %FileCheck %s --check-prefix=FILE_C

// OS X 10.9
// RUN: %target-swift-emit-silgen -enable-sil-ownership -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %s %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_C
// cAb
// RUN: %target-swift-emit-silgen -enable-sil-ownership %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -primary-file %s %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_A
// caB
// RUN: %target-swift-emit-silgen -enable-sil-ownership %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %s -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_B

// Acb
// RUN: %target-swift-emit-silgen -enable-sil-ownership -primary-file %s %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_A
// aCb
// RUN: %target-swift-emit-silgen -enable-sil-ownership %s -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_C
// abC
// RUN: %target-swift-emit-silgen -enable-sil-ownership %s %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -module-name main | %FileCheck %s --check-prefix=FILE_B

// Bca
// RUN: %target-swift-emit-silgen -enable-sil-ownership -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %s -module-name main | %FileCheck %s --check-prefix=FILE_B
// bCa
// RUN: %target-swift-emit-silgen -enable-sil-ownership %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %s -module-name main | %FileCheck %s --check-prefix=FILE_C
// bcA
// RUN: %target-swift-emit-silgen -enable-sil-ownership %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -primary-file %s -module-name main | %FileCheck %s --check-prefix=FILE_A

// Cba
// RUN: %target-swift-emit-silgen -enable-sil-ownership -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %s -module-name main | %FileCheck %s --check-prefix=FILE_C
// cBa
// RUN: %target-swift-emit-silgen -enable-sil-ownership %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift -primary-file %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift %s -module-name main | %FileCheck %s --check-prefix=FILE_B
// cbA
// RUN: %target-swift-emit-silgen -enable-sil-ownership %S/Inputs/inherited_protocol_conformance_other_file_2_c.swift %S/Inputs/inherited_protocol_conformance_other_file_2_b.swift -primary-file %s -module-name main | %FileCheck %s --check-prefix=FILE_A

// FILE_A-NOT: sil [transparent] [thunk] @$S4main5ThingVSQAAsADP2eeoi{{[_0-9a-zA-Z]*}}FZTW
// FILE_A-NOT: sil [transparent] [thunk] @$S4main5ThingVSkAAsADP9hashValueSivgTW
// FILE_A-NOT: sil_witness_table Thing: Hashable module main
// FILE_A-NOT: sil_witness_table Thing: Equatable module main

// FILE_B-NOT: sil [transparent] [thunk] @$S4main5ThingVSQAAsADP2eeoi{{[_0-9a-zA-Z]*}}FZTW
// FILE_B: sil private [transparent] [thunk] @$S4main5ThingVSHAASH9hashValueSivgTW
// FILE_B-NOT: sil [transparent] [thunk] @$S4main5ThingVSQAAsADP2eeoi{{[_0-9a-zA-Z]*}}FZTW

// FILE_B-NOT: sil_witness_table hidden Thing: Equatable module main
// FILE_B: sil_witness_table hidden Thing: Hashable module main
// FILE_B-NOT: sil_witness_table hidden Thing: Equatable module main

// FILE_C-NOT: sil [transparent] [thunk] @$S4main5ThingVSkAAsADP9hashValueSivgTW
// FILE_C: sil private [transparent] [thunk] @$S4main5ThingVSQAASQ2eeoiySbx_xtFZTW
// FILE_C-NOT: sil [transparent] [thunk] @$S4main5ThingVSkAAsADP9hashValueSivgTW

// FILE_C-NOT: sil_witness_table hidden Thing: Hashable module main
// FILE_C: sil_witness_table hidden Thing: Equatable module main
// FILE_C-NOT: sil_witness_table hidden Thing: Hashable module main

struct Thing { var value: Int }
