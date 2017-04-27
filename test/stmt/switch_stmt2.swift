// RUN: %target-typecheck-verify-swift -emit-sil

enum E {
  case e1
  case e2
}

func foo1(e : E) {
  switch e { // expected-error{{switch must be exhaustive, consider adding missing cases}}{{3-3=case .e2: <#code#>\n}}
  case .e1: return
  }
}

func foo2(i : Int) {
  switch i { // expected-error{{switch must be exhaustive, consider adding a default clause}}{{3-3=default: <#code#>\n}}
  case 1: return
  }
}
