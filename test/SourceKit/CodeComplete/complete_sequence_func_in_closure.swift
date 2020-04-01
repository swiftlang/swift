struct MyValue {
  var value = 1
}
func foo(fn: () -> Void) {}
_ = foo {
  func innerFunc(value: MyValue) {
    value. // HERE
  }
}

struct MyStruct {
  var x = { () -> Int in
    class InnerC {
      init(value: MyValue) {
        value. // HERE
      }
    }
  }()
}

// RUN: %sourcekitd-test \
// RUN:   -req=track-compiles == \
// RUN:   -req=complete -pos=7:11 -repeat-request=2 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=15:15 -repeat-request=2 %s -- %s -parse-as-library \
// RUN:   > %t.response.library
// RUN: %FileCheck --check-prefix=RESULT %s < %t.response.library
// RUN: %FileCheck --check-prefix=LIB_TRACE %s < %t.response.library

// RESULT-LABEL: key.results: [
// RESULT: key.description: "value"
// RESULT-LABEL: key.results: [
// RESULT: key.description: "value"
// RESULT-LABEL: key.results: [
// RESULT: key.description: "value"
// RESULT-LABEL: key.results: [
// RESULT: key.description: "value"

// LIB_TRACE-NOT:  key.description: "completion reusing previous ASTContext (benign diagnostic)"

// RUN: %sourcekitd-test \
// RUN:   -req=track-compiles == \
// RUN:   -req=complete -pos=7:11 -repeat-request=2 %s -- %s == \
// RUN:   -req=complete -pos=15:15 -repeat-request=2 %s -- %s \
// RUN:   > %t.response.script
// RUN: %FileCheck --check-prefix=RESULT %s < %t.response.script
// RUN: %FileCheck --check-prefix=SCRIPT_TRACE %s < %t.response.script

// SCRIPT_TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// SCRIPT_TRACE-NOT: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// SCRIPT_TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// SCRIPT_TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// SCRIPT_TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// SCRIPT_TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// SCRIPT_TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// SCRIPT_TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
