// RUN: %target-typecheck-verify-swift -emit-sil -diagnostics-editor-mode

enum E {
  case e1
  case e2
}

func foo1(e : E) {
  switch e { // expected-error{{switch must be exhaustive}}
  // expected-note@-1{{do you want to add missing cases?}}{{3-3=case .e2:\n<#code#>\n}}
  case .e1: return
  }
}

func foo2(i : Int) {
  switch i { // expected-error{{switch must be exhaustive}}
  // expected-note@-1{{do you want to add a default clause?}}{{3-3=default:\n<#code#>\n}}
  case 1: return
  }
}
