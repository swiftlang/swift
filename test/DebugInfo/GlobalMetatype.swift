// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

class Foo {}
let x = "Hello World!"

// Test that no static variable for the metatype of Foo is emitted.
// FIXME: This really should be emitted as artificial variable instead.
// CHECK-NOT: DW_TAG_variable{{.*}}_TMnC14GlobalMetatype3Foo
