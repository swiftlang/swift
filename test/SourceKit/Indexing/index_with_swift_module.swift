// RUN: %empty-directory(%t)
// RUN: %swift -emit-module -o %t/test_module.swiftmodule %S/Inputs/test_module.swift

// RUN: %sourcekitd-test -req=index %s -- %s -I %t | %FileCheck %s

// RUN: %sourcekitd-test -req=index %t/test_module.swiftmodule | %sed_clean > %t.response
// RUN: diff -u %S/Inputs/test_module.index.response %t.response

import test_module

func foo(a: TwoInts) {
}

// CHECK:      key.kind: source.lang.swift.import.module.swift
// CHECK-NEXT: key.name: "Swift"
// CHECK-NEXT: key.filepath: "{{.*[/\\]}}Swift.swiftmodule"
// CHECK-NEXT: key.hash:

// CHECK:      key.kind: source.lang.swift.import.module.swift
// CHECK-NEXT: key.name: "test_module"
// CHECK-NEXT: key.filepath: "{{.*[/\\]}}test_module.swiftmodule"
// CHECK-NEXT: key.hash:

// CHECK:      key.kind: source.lang.swift.ref.module
// CHECK-NEXT: key.name: "test_module"
// CHECK-NEXT: key.usr: "c:@M@test_module"

// CHECK:      key.kind: source.lang.swift.ref.class
// CHECK-NEXT: key.name: "TwoInts"
// CHECK-NEXT: key.usr: "s:11test_module7TwoIntsC"

// RUN: %sourcekitd-test -req=index %S/Inputs/Swift.swiftmodule | %FileCheck %s -check-prefix=CHECK-SWIFT1
// CHECK-SWIFT1-DAG: key.groupname: "Bool"
// CHECK-SWIFT1-DAG: key.groupname: "Collection"
