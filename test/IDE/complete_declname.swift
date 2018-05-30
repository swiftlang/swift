// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASSNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCTNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUMNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOLNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRECEDENCEGROUPNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPERATORNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=METHODNAME | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=METHODNAME_OVERRIDE | %FileCheck %s --check-prefix=METHODNAME_OVERRIDE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=METHODNAME_PROTOCOL | %FileCheck %s --check-prefix=NO_COMPLETIONS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-keywords=false -code-completion-token=METHODNAME_CONFORMANCE | %FileCheck %s --check-prefix=METHODNAME_CONFORMANCE

// NO_COMPLETIONS-NOT: Begin completions

class #^CLASSNAME^# {}
struct #^STRUCTNAME^#
enum #^ENUMNAME^#
protocol #^PROTOCOLNAME^# {}
precedencegroup #^PRECEDENCEGROUPNAME^#
infix operator #^OPERATORNAME^#

class MyCls {
  func foo() {}
  func #^METHODNAME^#
}

class MySub : MyCls {
  func #^METHODNAME_OVERRIDE^#
// METHODNAME_OVERRIDE: Begin completions, 1 items
// METHODNAME_OVERRIDE-NEXT: Decl[InstanceMethod]/Super: foo() {|}; name=foo()
// METHODNAME_OVERRIDE-NEXT: End completions
}

protocol P {
  func foo() {}
  func #^METHODNAME_PROTOCOL^#
}

struct MyStruct : P {
  func #^METHODNAME_CONFORMANCE^#
// METHODNAME_CONFORMANCE: Begin completions, 1 items
// METHODNAME_CONFORMANCE-NEXT: Decl[InstanceMethod]/Super: foo() {|}; name=foo()
// METHODNAME_CONFORMANCE-NEXT: End completions
}
