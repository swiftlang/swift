func test() {
  class C: 
}

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=2:11 -repeat-request=2 %s -- %s -parse-as-library \
// RUN:   | %FileCheck %s

// CHECK: key.results: [
// CHECK:   description: "Int",
