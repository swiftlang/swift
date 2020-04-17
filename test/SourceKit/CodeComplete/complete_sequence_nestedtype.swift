struct Outer {
  enum Inner {
    case east, west
    static func staticMethod() {}
    func instanceMethod() {}

    func test() {
      Inner.
    }
  }

  func test() {
    Inner.
  }
}

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=8:13 %s -- %s == \
// RUN:   -req=complete -pos=8:13 %s -- %s == \
// RUN:   -req=complete -pos=13:11 %s -- %s \
// RUN:   > %t.response
// RUN: %FileCheck --check-prefix=RESULT  %s < %t.response

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "east"
// RESULT-DAG: key.description: "west"
// RESULT-DAG: key.description: "staticMethod()"
// RESULT-DAG: key.description: "instanceMethod(self: Outer.Inner)"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "east"
// RESULT-DAG: key.description: "west"
// RESULT-DAG: key.description: "staticMethod()"
// RESULT-DAG: key.description: "instanceMethod(self: Outer.Inner)"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "east"
// RESULT-DAG: key.description: "west"
// RESULT-DAG: key.description: "staticMethod()"
// RESULT-DAG: key.description: "instanceMethod(self: Inner)"
// RESULT: ]
// RESULT: key.reusingastcontext: 1
