struct MyStruct {
    init() {}
    var value: Int = 1
}

func foo(arg: MyStruct) {
  #if true
  _ = arg./*8:11*/
  #else
  _ = arg./*10:11*/
  #endif
}

struct TestStruct {
  #if true
  func testActive(arg: MyStruct) {
    _ = arg./*17:13*/
  }
  #else
  func testInactive(arg: MyStruct) {
    _ = arg./*21:13*/
  }
  #endif
}

// Test that (1) fast completion happens even in inactive #if blocks, and
// (2) #if in toplevel decls invalidate cached ASTContext

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=8:11 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=10:11 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=17:13 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=21:13 %s -- %s -parse-as-library \
// RUN:   | %FileCheck %s --check-prefix=RESULT

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "value"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "value"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "value"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "value"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1
