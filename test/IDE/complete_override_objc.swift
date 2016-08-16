// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PA -code-completion-keywords=false -F %S/Inputs/mock-sdk > %t.txt
// RUN: %FileCheck %s -check-prefix=CHECK1 < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROPERTY -code-completion-keywords=false -F %S/Inputs/mock-sdk | %FileCheck %s -check-prefix=PROPERTY

// REQUIRES: objc_interop

import Foo

class Sub : FooClassBase {
  #^PA^#
}

// CHECK1: Begin completions, 8 items
// CHECK1: Decl[InstanceMethod]/Super:         override func fooBaseInstanceFunc0() {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func fooBaseInstanceFunc1(_ anObject: Any!) -> FooClassBase! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func fooBaseInstanceFuncOverridden() {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func _internalMeth3() -> Any! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func _internalMeth2() -> Any! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func nonInternalMeth() -> Any! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func _internalMeth1() -> Any! {|}
// CHECK1: Decl[Constructor]/Super:            init!() {|}
// CHECK1: End completions

class Sub : FooClassDerived {
  var #^PROPERTY^#
}
// PROPERTY: Begin completions
// PROPERTY-DAG: Decl[InstanceVar]/Super:            fooProperty1: Int32
// PROPERTY-DAG: Decl[InstanceVar]/Super:            fooProperty2: Int32
// PROPERTY-DAG: Decl[InstanceVar]/Super:            fooProperty3: Int32
// PROPERTY: End completions
