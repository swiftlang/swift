// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/inherited_protocol_conformance_other_file.swift -module-name main | FileCheck %s --check-prefix=THIS_FILE
// RUN: %target-swift-frontend -emit-silgen %s -primary-file %S/Inputs/inherited_protocol_conformance_other_file.swift -module-name main | FileCheck %s --check-prefix=OTHER_FILE

// THIS_FILE-NOT: sil_witness_table {{.*}} B: P
// THIS_FILE-LABEL: sil_witness_table hidden D: R module main
// THIS_FILE-NOT: sil_witness_table {{.*}} B: P
// THIS_FILE-LABEL: sil_witness_table hidden D: Q module main
// THIS_FILE-NOT: sil_witness_table {{.*}} B: P

// OTHER_FILE-LABEL: sil_witness_table hidden B: P module main

class D: B, R {}
