// RUN: %target-swift-ide-test -dump-importer-lookup-table -source-filename %s -import-objc-header %S/Inputs/swift_name.h > %t.log 2>&1
// RUN: FileCheck %s < %t.log

// CHECK:      Base -> full name mappings:
// CHECK-NEXT:   Bar --> Bar
// CHECK-NEXT:   SomeStruct --> SomeStruct

// CHECK:      Full name -> entry mappings:
// CHECK-NEXT:   Bar:
// CHECK-NEXT:     TU: SNFoo
// CHECK-NEXT:   SomeStruct:
// CHECK-NEXT:     TU: SNSomeStruct
