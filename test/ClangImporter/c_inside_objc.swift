// RUN: %target-swift-ide-test -enable-objc-interop -print-module -module-to-print CInsideObjC -I %S/Inputs/custom-modules -source-filename %s -Xcc -DCLASS | %FileCheck %s
// RUN: %target-swift-ide-test -enable-objc-interop -print-module -module-to-print CInsideObjC -I %S/Inputs/custom-modules -source-filename %s -Xcc -DCATEGORY | %FileCheck %s
// RUN: %target-swift-ide-test -enable-objc-interop -print-module -module-to-print CInsideObjC -I %S/Inputs/custom-modules -source-filename %s -Xcc -DPROTOCOL | %FileCheck %s

// RUN: %target-swift-frontend -enable-objc-interop -typecheck %s -I %S/Inputs/custom-modules -verify -Xcc -DCLASS
// RUN: %target-swift-frontend -enable-objc-interop -typecheck %s -I %S/Inputs/custom-modules -verify -Xcc -DCATEGORY
// RUN: %target-swift-frontend -enable-objc-interop -typecheck %s -I %S/Inputs/custom-modules -verify -Xcc -DPROTOCOL

// CHECK-LABEL: struct AlreadyDeclaredStruct {

// CHECK-LABEL: {{class Wrapper : Base {|extension Wrapper {|protocol Wrapper {}}
// CHECK-NOT: struct
// CHECK: var forward: ForwardDeclaredStruct
// CHECK-NOT: struct
// CHECK: var backward: AlreadyDeclaredStruct
// CHECK-NOT: struct
// CHECK: {{^}$}}

// CHECK-LABEL: func nestedFunc()
// CHECK-LABEL: struct NestedDeclaredStruct {
// CHECK-LABEL: typealias NestedTypedef = Int32
// CHECK-LABEL: let nestedGlobal: Int32

// CHECK-LABEL: struct ForwardDeclaredStruct {

import CInsideObjC

func testTypeLookup(_: AlreadyDeclaredStruct) {}
func testTypeLookup(_: NestedDeclaredStruct) {}
func testTypeLookup(_: ForwardDeclaredStruct) {}
func testTypeLookup(_: NestedTypedef) {}
