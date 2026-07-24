// RUN: %target-swift-frontend -emit-sil %s -I %S/Inputs -verify

enum E1 : Int {
  case e1
  case e2
  case e3
  case e4
}

func foo1(_ e : E1) -> Int {
  switch(e) {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing cases: '.e2', '.e3', '.e4'}} {{+5:3-3=case .e2:\n<#code#>\ncase .e3:\n<#code#>\ncase .e4:\n<#code#>\n}}
  case .e1:
    return 1
  }
}

func foo2(_ i : Int) -> Int {
  switch i {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add a default clause}} {{+5:3-3=default:\n<#code#>\n}}
  case 1:
    return 1
  }
}

func foo3(_ c : Character) -> Character {
  switch c {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add a default clause}} {{+5:3-3=default:\n<#code#>\n}}
  case "a":
    return "a"
  }
}

enum E2 {
  case e1(a: Int, s: Int)
  case e2(a: Int)
  case e3(a: Int)
  case e4(_: Int)
  case e5(_: Int, _: Int)
  case e6(a : Int, _: Int)
  case e7
  case e8(a : Int, Int, Int)
  case e9(Int, Int, Int)
}

func foo4(_ e : E2) -> Int {
  switch e {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing cases: '.e1(a: let a, s: let s)', '.e3(a: let a)', '.e4(_)', '.e5(_, _)', ...}} {{+5:3-3=case .e1(a: let a, s: let s):\n<#code#>\ncase .e3(a: let a):\n<#code#>\ncase .e4(_):\n<#code#>\ncase .e5(_, _):\n<#code#>\ncase .e6(a: let a, _):\n<#code#>\ncase .e7:\n<#code#>\ncase .e8(a: let a, _, _):\n<#code#>\ncase .e9(_, _, _):\n<#code#>\n}}
  case .e2:
    return 1
  }
}

func foo5(_ e : E1) -> Int {
  switch e {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing cases: '.e1', '.e2', '.e3', '.e4'}} {{+5:3-3=case .e1:\n<#code#>\ncase .e2:\n<#code#>\ncase .e3:\n<#code#>\ncase .e4:\n<#code#>\n}}
  case _ where e.rawValue > 0:
    return 1
  }
}

func foo6(_ e : E2) -> Int {
  switch e {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing cases: '.e2(a: let a)', '.e3(a: let a)', '.e4(_)', '.e5(_, _)', '.e6(a: let a, _)', ...}} {{+5:3-3=case .e2(a: let a):\n<#code#>\ncase .e3(a: let a):\n<#code#>\ncase .e4(_):\n<#code#>\ncase .e5(_, _):\n<#code#>\ncase .e6(a: let a, _):\n<#code#>\ncase .e7:\n<#code#>\ncase .e8(a: let a, _, _):\n<#code#>\ncase .e9(_, _, _):\n<#code#>\n}}
  case let .e1(x, y):
    return x + y
  }
}

func foo7(_ e : E2) -> Int {
  switch e {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing cases: '.e2(a: let a)', '.e4(_)', '.e5(_, _)', '.e6(a: let a, _)', '.e7', ...}} {{+6:3-3=case .e2(a: let a):\n<#code#>\ncase .e4(_):\n<#code#>\ncase .e5(_, _):\n<#code#>\ncase .e6(a: let a, _):\n<#code#>\ncase .e7:\n<#code#>\ncase .e8(a: let a, _, _):\n<#code#>\ncase .e9(_, _, _):\n<#code#>\n}}
  case .e2(1): return 0
  case .e1: return 0
  case .e3: return 0
  }
}
