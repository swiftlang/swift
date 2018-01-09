// RUN: %target-swift-frontend -parse-as-library -emit-silgen -disable-objc-attr-requires-foundation-module -enable-sil-ownership -import-objc-header %S/Inputs/protocol_imported_type.h -primary-file %s %S/Inputs/protocol_imported_type_helper.swift -o %t.sil -module-name main
// RUN: %FileCheck %s < %t.sil
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t.sil

func test(map: [SomeOptions: Int]) {
  _ = map[.foo]
}

// CHECK-DAG: sil_witness_table shared [serialized] SomeOptions: Equatable module __ObjC
// CHECK-DAG: sil_witness_table shared [serialized] SomeOptions: RawRepresentable module __ObjC
// NEGATIVE-NOT: SomeOptions: Hashable
