@propertyWrapper
struct TwelveOrLess {
  private var number = 0
  var wrappedValue: Int {
    get { return number }
    set { number = min(newValue, 12) }
  }
}

struct MyStruct {
  @TwelveOrLess var value = 12

  func foo() {}

  func bar(barParam: Int) {

  }
}

// RUN: %sourcekitd-test \
// RUN:   -req=track-compiles == \
// RUN:   -req=complete -pos=16:1 -repeat-request=2 %s -- %s > %t.response
// RUN: %FileCheck --check-prefix=RESULT  %s < %t.response

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "barParam"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "barParam"
// RESULT: ]
// RESULT: key.reusingastcontext: 1
