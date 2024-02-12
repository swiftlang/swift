// RUN: %target-typecheck-verify-swift -emit-sil -diagnostics-editor-mode

enum E {
  case e1
  case e2
}

func foo1(e : E) {
  // expected-note@+1{{add missing cases}}{{+2:3-3=case .e2:\n<#code#>\n}}
  switch e { // expected-error{{switch must be exhaustive}}
  case .e1: return
  }
}

func foo2(i : Int) {
  // expected-note@+1{{add a default clause}}{{+2:3-3=default:\n<#code#>\n}}
  switch i { // expected-error{{switch must be exhaustive}}
  case 1: return
  }
}
