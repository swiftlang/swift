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
// RUN:   -req=complete -pos=7:11 -repeat-request=2 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=15:15 -repeat-request=2 %s -- %s -parse-as-library \
// RUN:   > %t.response.library
// RUN: %FileCheck --check-prefix=RESULT_LIBRARY %s < %t.response.library

// RESULT_LIBRARY-LABEL: key.results: [
// RESULT_LIBRARY: key.description: "value"
// RESULT_LIBRARY-NOT: key.reusingastcontext: 1

// RESULT_LIBRARY-LABEL: key.results: [
// RESULT_LIBRARY: key.description: "value"
// RESULT_LIBRARY-NOT: key.reusingastcontext: 1

// RESULT_LIBRARY-LABEL: key.results: [
// RESULT_LIBRARY: key.description: "value"
// RESULT_LIBRARY-NOT: key.reusingastcontext: 1

// RESULT_LIBRARY-LABEL: key.results: [
// RESULT_LIBRARY: key.description: "value"
// RESULT_LIBRARY-NOT: key.reusingastcontext: 1


// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=7:11 -repeat-request=2 %s -- %s == \
// RUN:   -req=complete -pos=15:15 -repeat-request=2 %s -- %s \
// RUN:   > %t.response.script
// RUN: %FileCheck --check-prefix=RESULT_SCRIPT %s < %t.response.script

// RESULT_SCRIPT-LABEL: key.results: [
// RESULT_SCRIPT: key.description: "value"
// RESULT_SCRIPT-NOT: key.reusingastcontext: 1

// RESULT_SCRIPT-LABEL: key.results: [
// RESULT_SCRIPT: key.description: "value"
// RESULT_SCRIPT: key.reusingastcontext: 1

// RESULT_SCRIPT-LABEL: key.results: [
// RESULT_SCRIPT: key.description: "value"
// RESULT_SCRIPT: key.reusingastcontext: 1

// RESULT_SCRIPT-LABEL: key.results: [
// RESULT_SCRIPT: key.description: "value"
// RESULT_SCRIPT: key.reusingastcontext: 1
