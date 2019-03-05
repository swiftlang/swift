enum E1 {
  case e1
  case e2
  case e3
  case e4
}

func foo1(_ e : E1) -> Int {
  switch e {
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=9:5 -end-pos=9:5 -cursor-action %s -- %s | %FileCheck %s

// CHECK: ACTIONS BEGIN
// CHECK: source.refactoring.kind.expand.switch.cases
// CHECK-NEXT: LSP KIND "refactor"
// CHECK: ACTIONS END

func foo() -> String {
  return bar()
}

// RUN: %sourcekitd-test -req=cursor -pos=21:10 -end-pos=21:15 -cursor-action %s -- %s | %FileCheck %s -check-prefix=CHECK2

// CHECK2: ACTIONS BEGIN
// CHECK2: source.refactoring.kind.extract.expr
// CHECK2-NEXT: LSP KIND "refactor.extract"
// CHECK2: source.refactoring.kind.extract.function
// CHECK2-NEXT: LSP KIND "refactor.extract"
// CHECK2: source.refactoring.kind.extract.expr.repeated
// CHECK2-NEXT: LSP KIND "refactor.extract"
// CHECK2: ACTIONS END

func bar() -> String {
  return "bar"
}

// RUN: %sourcekitd-test -req=cursor -pos=36:10 -end-pos=36:15 -cursor-action %s -- %s | %FileCheck %s -check-prefix=CHECK3

// CHECK3: ACTIONS BEGIN
// CHECK3: source.refactoring.kind.localize.string
// CHECK3-NEXT: LSP KIND "refactor.rewrite"
// CHECK3: ACTIONS END
