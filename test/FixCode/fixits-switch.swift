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
  // expected-note@-2 {{add missing case: '.e2'}} {{+8:3-3=case .e2:\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '.e3'}} {{+8:3-3=case .e3:\n<#code#>\n}}
  // expected-note@-4 {{add missing case: '.e4'}} {{+8:3-3=case .e4:\n<#code#>\n}}
  // expected-note@-5 {{add missing cases}} {{+8:3-3=case .e2:\n<#code#>\ncase .e3:\n<#code#>\ncase .e4:\n<#code#>\n}}
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
  // expected-note@-2 {{add missing case: '.e1(a: let a, s: let s)'}} {{+13:3-3=case .e1(a: let a, s: let s):\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '.e3(a: let a)'}} {{+13:3-3=case .e3(a: let a):\n<#code#>\n}}
  // expected-note@-4 {{add missing case: '.e4(_)'}} {{+13:3-3=case .e4(_):\n<#code#>\n}}
  // expected-note@-5 {{add missing case: '.e5(_, _)'}} {{+13:3-3=case .e5(_, _):\n<#code#>\n}}
  // expected-note@-6 {{add missing case: '.e6(a: let a, _)'}} {{+13:3-3=case .e6(a: let a, _):\n<#code#>\n}}
  // expected-note@-7 {{add missing case: '.e7'}} {{+13:3-3=case .e7:\n<#code#>\n}}
  // expected-note@-8 {{add missing case: '.e8(a: let a, _, _)'}} {{+13:3-3=case .e8(a: let a, _, _):\n<#code#>\n}}
  // expected-note@-9 {{add missing case: '.e9(_, _, _)'}} {{+13:3-3=case .e9(_, _, _):\n<#code#>\n}}
  // expected-note@-10 {{add missing cases}} {{+13:3-3=case .e1(a: let a, s: let s):\n<#code#>\ncase .e3(a: let a):\n<#code#>\ncase .e4(_):\n<#code#>\ncase .e5(_, _):\n<#code#>\ncase .e6(a: let a, _):\n<#code#>\ncase .e7:\n<#code#>\ncase .e8(a: let a, _, _):\n<#code#>\ncase .e9(_, _, _):\n<#code#>\n}}
  case .e2:
    return 1
  }
}

func foo5(_ e : E1) -> Int {
  switch e {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.e1'}} {{+9:3-3=case .e1:\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '.e2'}} {{+9:3-3=case .e2:\n<#code#>\n}}
  // expected-note@-4 {{add missing case: '.e3'}} {{+9:3-3=case .e3:\n<#code#>\n}}
  // expected-note@-5 {{add missing case: '.e4'}} {{+9:3-3=case .e4:\n<#code#>\n}}
  // expected-note@-6 {{add missing cases}} {{+9:3-3=case .e1:\n<#code#>\ncase .e2:\n<#code#>\ncase .e3:\n<#code#>\ncase .e4:\n<#code#>\n}}
  case _ where e.rawValue > 0:
    return 1
  }
}

func foo6(_ e : E2) -> Int {
  switch e {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.e2(a: let a)'}} {{+13:3-3=case .e2(a: let a):\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '.e3(a: let a)'}} {{+13:3-3=case .e3(a: let a):\n<#code#>\n}}
  // expected-note@-4 {{add missing case: '.e4(_)'}} {{+13:3-3=case .e4(_):\n<#code#>\n}}
  // expected-note@-5 {{add missing case: '.e5(_, _)'}} {{+13:3-3=case .e5(_, _):\n<#code#>\n}}
  // expected-note@-6 {{add missing case: '.e6(a: let a, _)'}} {{+13:3-3=case .e6(a: let a, _):\n<#code#>\n}}
  // expected-note@-7 {{add missing case: '.e7'}} {{+13:3-3=case .e7:\n<#code#>\n}}
  // expected-note@-8 {{add missing case: '.e8(a: let a, _, _)'}} {{+13:3-3=case .e8(a: let a, _, _):\n<#code#>\n}}
  // expected-note@-9 {{add missing case: '.e9(_, _, _)'}} {{+13:3-3=case .e9(_, _, _):\n<#code#>\n}}
  // expected-note@-10 {{add missing cases}} {{+13:3-3=case .e2(a: let a):\n<#code#>\ncase .e3(a: let a):\n<#code#>\ncase .e4(_):\n<#code#>\ncase .e5(_, _):\n<#code#>\ncase .e6(a: let a, _):\n<#code#>\ncase .e7:\n<#code#>\ncase .e8(a: let a, _, _):\n<#code#>\ncase .e9(_, _, _):\n<#code#>\n}}
  case let .e1(x, y):
    return x + y
  }
}

func foo7(_ e : E2) -> Int {
  switch e {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.e2(a: let a)'}} {{+13:3-3=case .e2(a: let a):\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '.e4(_)'}} {{+13:3-3=case .e4(_):\n<#code#>\n}}
  // expected-note@-4 {{add missing case: '.e5(_, _)'}} {{+13:3-3=case .e5(_, _):\n<#code#>\n}}
  // expected-note@-5 {{add missing case: '.e6(a: let a, _)'}} {{+13:3-3=case .e6(a: let a, _):\n<#code#>\n}}
  // expected-note@-6 {{add missing case: '.e7'}} {{+13:3-3=case .e7:\n<#code#>\n}}
  // expected-note@-7 {{add missing case: '.e8(a: let a, _, _)'}} {{+13:3-3=case .e8(a: let a, _, _):\n<#code#>\n}}
  // expected-note@-8 {{add missing case: '.e9(_, _, _)'}} {{+13:3-3=case .e9(_, _, _):\n<#code#>\n}}
  // expected-note@-9 {{add missing cases}} {{+13:3-3=case .e2(a: let a):\n<#code#>\ncase .e4(_):\n<#code#>\ncase .e5(_, _):\n<#code#>\ncase .e6(a: let a, _):\n<#code#>\ncase .e7:\n<#code#>\ncase .e8(a: let a, _, _):\n<#code#>\ncase .e9(_, _, _):\n<#code#>\n}}
  case .e2(1): return 0
  case .e1: return 0
  case .e3: return 0
  }
}
