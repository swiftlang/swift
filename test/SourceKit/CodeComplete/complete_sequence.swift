class Foo {
  var x: Int
  var y: Int
  func fooMethod() {}
}
struct Bar {
  var a: Int
  var b: Int
  func barMethod() {}
}
func foo(arg: Foo) {
  _ = arg.
}
func bar(arg: Bar) {
  _ = arg.
}

// NOTE: Tests for 'key.codecomplete.reuseastcontex' option.

// Disabled.
// RUN: %sourcekitd-test \
// RUN:   -req=global-config -req-opts=completion_max_astcontext_reuse_count=0 ==\
// RUN:   -req=complete -pos=12:11 %s -- %s == \
// RUN:   -req=complete -pos=15:11 %s -- %s > %t.response
// RUN: %FileCheck --check-prefix=RESULT_SLOW %s < %t.response

// Enabled.
// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=12:11 %s -- %s == \
// RUN:   -req=complete -pos=15:11 %s -- %s > %t.response.reuseastcontext
// RUN: %FileCheck --check-prefix=RESULT_FAST %s < %t.response.reuseastcontext

// Enabled - compiler argument mismatch.
// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=12:11 %s -- %s -DNOTUSED == \
// RUN:   -req=complete -pos=15:11 %s -- -DNOTUSED %s > %t.response.reuseastcontext_argmismatch
// RUN: %FileCheck --check-prefix=RESULT_SLOW  %s < %t.response.reuseastcontext_argmismatch

// RESULT_SLOW-LABEL: key.results: [
// RESULT_SLOW-DAG: key.name: "fooMethod()"
// RESULT_SLOW-DAG: key.name: "self"
// RESULT_SLOW-DAG: key.name: "x"
// RESULT_SLOW-DAG: key.name: "y"
// RESULT_SLOW: ]
// RESULT_SLOW-NOT: key.reusingastcontext: 1 

// RESULT_SLOW-LABEL: key.results: [
// RESULT_SLOW-DAG: key.name: "barMethod()"
// RESULT_SLOW-DAG: key.name: "self"
// RESULT_SLOW-DAG: key.name: "a"
// RESULT_SLOW-DAG: key.name: "b"
// RESULT_SLOW: ]
// RESULT_SLOW-NOT: key.reusingastcontext: 1 


// RESULT_FAST-LABEL: key.results: [
// RESULT_FAST-DAG: key.name: "fooMethod()"
// RESULT_FAST-DAG: key.name: "self"
// RESULT_FAST-DAG: key.name: "x"
// RESULT_FAST-DAG: key.name: "y"
// RESULT_FAST: ]
// RESULT_FAST-NOT: key.reusingastcontext: 1 

// RESULT_FAST-LABEL: key.results: [
// RESULT_FAST-DAG: key.name: "barMethod()"
// RESULT_FAST-DAG: key.name: "self"
// RESULT_FAST-DAG: key.name: "a"
// RESULT_FAST-DAG: key.name: "b"
// RESULT_FAST: ]
// RESULT_FAST: key.reusingastcontext: 1 
