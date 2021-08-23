// RUN: %target-swift-frontend %s -emit-ir -use-jit | %FileCheck %s


class A{}

// Check that only one copy of the type metadata table is emitted.
// CHECK: @"\01l_type_metadata_table"
// CHECK-NOT: @"\01l_type_metadata_table.1"


