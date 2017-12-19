struct S {
  init(foo: Int) {}
}
func takeS(_: S, other: Int) {}

func test() {
  takeS(S(, other: 2)
}

// RUN: %sourcekitd-test -req=complete -pos=7:11 %s -- %s | %FileCheck %s -check-prefix=CALL_PATTERN
// CALL_PATTERN: key.kind: source.lang.swift.decl.function.constructor

// RUN: %sourcekitd-test -req=complete.open -pos=7:11 %s -- %s | %FileCheck %s -check-prefix=NO_PATTERN
// NO_PATTERN-NOT: key.kind: source.lang.swift.decl.function.constructor

// RUN: %sourcekitd-test -req=complete.open -req-opts=callpatternheuristics=1 -pos=7:11 %s -- %s | %FileCheck %s -check-prefix=NO_PATTERN
// NO_PATTERN-NOT: key.kind: source.lang.swift.decl.function.constructor

// RUN: %sourcekitd-test -req=complete.open -req-opts=callpatternheuristics=0 -pos=7:11 %s -- %s | %FileCheck %s -check-prefix=CALL_PATTERN
// NO_PATTERN-NOT: key.kind: source.lang.swift.decl.function.constructor
