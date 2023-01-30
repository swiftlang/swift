// RUN: %sourcekitd-test -req=cursor -pos=%(line + 2):7 %s -- %s == -req=cursor -pos=%(line + 3):7 %s -- %s | %FileCheck %s --check-prefix IN-FUNCTION
func foo() {
  let inFunctionA = 1
  let inFunctionB = "hi"
}

// IN-FUNCTION: source.lang.swift.decl.var.local
// IN-FUNCTION-NEXT: inFunctionA
// IN-FUNCTION: DID REUSE AST CONTEXT: 0
// IN-FUNCTION: source.lang.swift.decl.var.local
// IN-FUNCTION-NEXT: inFunctionB
// IN-FUNCTION: DID REUSE AST CONTEXT: 1

// RUN: %sourcekitd-test -req=cursor -pos=%(line + 3):9 %s -- %s == -req=cursor -pos=%(line + 4):9 %s -- %s | %FileCheck %s --check-prefix IN-INSTANCE-METHOD
struct MyStruct {
  func test() {
    let inInstanceMethod1 = 2
    let inInstanceMethod2 = "hello"
  }
}

// IN-INSTANCE-METHOD: source.lang.swift.decl.var.local
// IN-INSTANCE-METHOD-NEXT: inInstanceMethod1
// IN-INSTANCE-METHOD: DID REUSE AST CONTEXT: 0
// IN-INSTANCE-METHOD: source.lang.swift.decl.var.local
// IN-INSTANCE-METHOD-NEXT: inInstanceMethod2
// IN-INSTANCE-METHOD: DID REUSE AST CONTEXT: 1
