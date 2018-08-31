// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUMNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOLNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRECEDENCEGROUPNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPERATORNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LETNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VARNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPEALIASNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNCNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=METHODNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=PROPERTY_LETNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=PROPERTY_VARNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=PROPERTY_TYPEALIASNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=METHODNAME_OVERRIDE | %FileCheck %s --check-prefix=METHODNAME_OVERRIDE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=ASSOCIATEDTYPENAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=METHODNAME_PROTOCOL | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=METHODNAME_CONFORMANCE | %FileCheck %s --check-prefix=METHODNAME_CONFORMANCE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=TYPEALIASNAME_CONFORMANCE | %FileCheck %s --check-prefix=TYPEALIASNAME_CONFORMANCE

// NO_COMPLETIONS-NOT: Begin completions

class #^CLASSNAME^# {}
struct #^STRUCTNAME^#
enum #^ENUMNAME^#
protocol #^PROTOCOLNAME^# {}
precedencegroup #^PRECEDENCEGROUPNAME^#
infix operator #^OPERATORNAME^#
let #^LETNAME^#
var #^VARNAME^#
typealias #^TYPEALIASNAME^#
func #^FUNCNAME^#

class MyCls {
  func foo() {}
  func #^METHODNAME^#
  let #^PROPERTY_LETNAME^#
  var #^PROPERTY_VARNAME^#
  typealias #^PROPERTY_TYPEALIASNAME^#
}

class MySub : MyCls {
  func #^METHODNAME_OVERRIDE^#
// METHODNAME_OVERRIDE: Begin completions, 1 items
// METHODNAME_OVERRIDE-NEXT: Decl[InstanceMethod]/Super: foo() {|}; name=foo()
// METHODNAME_OVERRIDE-NEXT: End completions
}

protocol P {
  associatedtype #^ASSOCIATEDTYPENAME^#
  associatedtype Assoc
  func foo() {}
  func #^METHODNAME_PROTOCOL^#
}

struct MyStruct : P {
  func #^METHODNAME_CONFORMANCE^#
// METHODNAME_CONFORMANCE: Begin completions, 1 items
// METHODNAME_CONFORMANCE-NEXT: Decl[InstanceMethod]/Super: foo() {|}; name=foo()
// METHODNAME_CONFORMANCE-NEXT: End completions
  typealias #^TYPEALIASNAME_CONFORMANCE^#
// TYPEALIASNAME_CONFORMANCE: Begin completions, 1 items
// TYPEALIASNAME_CONFORMANCE-NEXT: Decl[AssociatedType]/Super: Assoc = {#(Type)#}; name=Assoc = Type
// TYPEALIASNAME_CONFORMANCE-NEXT: End completions
}
