// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PA -code-completion-keywords=false -F %S/Inputs/mock-sdk > %t.txt
// RUN: FileCheck %s -check-prefix=CHECK1 < %t.txt

import Foo

class Sub : FooClassBase {
  #^PA^#
}

// CHECK1: Begin completions, 8 items
// CHECK1: Decl[InstanceMethod]/Super:         override func fooBaseInstanceFunc0() {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func fooBaseInstanceFunc1(anObject: AnyObject!) -> FooClassBase! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func fooBaseInstanceFuncOverridden() {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func _internalMeth3() -> AnyObject! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func _internalMeth2() -> AnyObject! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func nonInternalMeth() -> AnyObject! {|}
// CHECK1: Decl[InstanceMethod]/Super:         override func _internalMeth1() -> AnyObject! {|}
// CHECK1: Decl[Constructor]/Super:            init!() {|}
// CHECK1: End completions
