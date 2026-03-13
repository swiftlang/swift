// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PA -code-completion-keywords=false -F %S/Inputs/mock-sdk > %t.txt
// RUN: %FileCheck %s -check-prefix=CHECK1 < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROPERTY -code-completion-keywords=false -F %S/Inputs/mock-sdk | %FileCheck %s -check-prefix=PROPERTY

// REQUIRES: objc_interop

import Foo

class Sub : FooClassBase {
  #^PA^#
}

// CHECK1: Begin completions, 16 items
// CHECK1: Decl[StaticMethod]/Super:           override class func fooBaseInstanceFunc0() {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func fooBaseInstanceFunc0() {|}
// CHECK1: Decl[StaticMethod]/Super:           override class func fooBaseInstanceFunc1(_ anObject: Any!) -> FooClassBase! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func fooBaseInstanceFunc1(_ anObject: Any!) -> FooClassBase! {|}
// CHECK1: Decl[StaticMethod]/Super:           override class func fooBaseInstanceFuncOverridden() {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func fooBaseInstanceFuncOverridden() {|}
// CHECK1: Decl[StaticMethod]/Super:           override class func fooBaseClassFunc0() {|}
// CHECK1: Decl[StaticMethod]/Super:           override class func _internalMeth3() -> Any! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func _internalMeth3() -> Any! {|}
// CHECK1: Decl[StaticMethod]/Super:           override class func _internalMeth2() -> Any! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func _internalMeth2() -> Any! {|}
// CHECK1: Decl[StaticMethod]/Super:           override class func nonInternalMeth() -> Any! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func nonInternalMeth() -> Any! {|}
// CHECK1: Decl[StaticMethod]/Super:           override class func _internalMeth1() -> Any! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func _internalMeth1() -> Any! {|}
// CHECK1: Decl[Constructor]/Super:            override init!() {|}

class Sub : FooClassDerived {
  var #^PROPERTY^#
}
// PROPERTY-DAG: Decl[InstanceVar]/Super/Erase[4]: override var fooProperty1: Int32;
// PROPERTY-DAG: Decl[InstanceVar]/Super/Erase[4]: override var fooProperty2: Int32;
// PROPERTY-DAG: Decl[InstanceVar]/Super/Erase[4]: override var fooProperty3: Int32;
