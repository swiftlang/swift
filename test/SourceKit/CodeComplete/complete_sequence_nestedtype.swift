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
// RUN:   -req=track-compiles == \
// RUN:   -req=complete -pos=8:13 %s -- %s == \
// RUN:   -req=complete -pos=8:13 %s -- %s == \
// RUN:   -req=complete -pos=13:11 %s -- %s \
// RUN:   > %t.response
// RUN: %FileCheck --check-prefix=RESULT  %s < %t.response
// RUN: %FileCheck --check-prefix=TRACE  %s < %t.response

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "east"
// RESULT-DAG: key.description: "west"
// RESULT-DAG: key.description: "staticMethod()"
// RESULT-DAG: key.description: "instanceMethod(self: Outer.Inner)"
// RESULT: ]
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "east"
// RESULT-DAG: key.description: "west"
// RESULT-DAG: key.description: "staticMethod()"
// RESULT-DAG: key.description: "instanceMethod(self: Outer.Inner)"
// RESULT: ]
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "east"
// RESULT-DAG: key.description: "west"
// RESULT-DAG: key.description: "staticMethod()"
// RESULT-DAG: key.description: "instanceMethod(self: Inner)"
// RESULT: ]

// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE-NOT: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
