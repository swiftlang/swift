// RUN: %target-swift-ide-test -print-module -source-filename %s -I %S/Inputs/category-ordering/ -module-to-print=ProtocolThenProperty | FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-PROTO-FIRST
// RUN: %target-swift-ide-test -print-module -source-filename %s -I %S/Inputs/category-ordering/ -module-to-print=PropertyThenProtocol | FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-PROP-FIRST

// REQUIRES: objc_interop

// CHECK-LABEL: class Base {
// CHECK: }

// CHECK-LABEL: protocol Foo {
// CHECK: var foo: AnyObject
// CHECK: }

// CHECK-LABEL: class Sub : Base {
// CHECK: }

// CHECK-PROTO-FIRST-LABEL: extension Sub : Foo {
// CHECK-PROTO-FIRST-NEXT: }

// CHECK-LABEL: extension Sub {
// CHECK: var foo: AnyObject!
// CHECK: }

// CHECK-PROP-FIRST-LABEL: extension Sub : Foo {
// CHECK-PROP-FIRST-NEXT: }

