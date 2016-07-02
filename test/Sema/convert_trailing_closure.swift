// RUN: %target-parse-verify-swift

func bar(v : Int, c1 : () ->Void) {}
func bar1(_ v : () ->Void) {}
func bar2(v : Int, c1 : () ->Void) {}
func bar2(v : Int, c2 : () ->Void) {}
func bar3(v1 : Int, c1 : (Int) ->Void) {}
func bar3(v2 : Int, c1 : (Int) ->Void) {}
func bar4(c1 : (Int, Int, Int) ->Void) {}
func bar4(c2: (Int, Int, Int) ->Void) {}
func bar5(c1: (Int, Int, Int) ->Void) {}
func bar6(c1 : () ->Void, v : Int) {}

class C {
  func bar(v : Int, c1 : () ->Void) {}
  func bar1(_ v : () ->Void) {}
  func bar2(v : Int, c1 : () ->Void) {}
  func bar2(v : Int, c2 : () ->Void) {}
  func bar3(v1 : Int, c1 : (Int) ->Void) {}
  func bar3(v2 : Int, c1 : (Int) ->Void) {}
}

struct S {
  func bar(v : Int, c1 : () ->Void) {}
  func bar1(_ v : () ->Void) {}
  func bar2(v : Int, c1 : () ->Void) {}
  func bar2(v : Int, c2 : () ->Void) {}
  func bar3(v1 : Int, c1 : (Int) ->Void) {}
  func bar3(v2 : Int, c1 : (Int) ->Void) {}
}

func foo(c : C, s : S) {
  bar(v : 3, c1: {}) // expected-note {{use trailing closure to simplify arguments}} {{12-19=) }} {{20-110=}}
  bar1 ({}) // expected-note {{use trailing closure to simplify arguments}} {{8-10=}} {{11-97=}}
  bar2(v : 3, c1: {}) // {{none}}
  bar2(v : 3, c2: {}) // {{none}}
  bar3(v1: 3, c1 : { (i) in }) // expected-note {{use trailing closure to simplify arguments}} {{13-21=) }} {{30-120=}}
  bar3(v2: 3, c1 : { (i) in }) // expected-note {{use trailing closure to simplify arguments}} {{13-21=) }} {{30-120=}}
  bar4(c1 : { (i, j, k) in }) // {{none}}
  bar5(c1 : { (i, j, k) in }) // expected-note {{use trailing closure to simplify arguments}} {{7-14=}} {{29-116=}}
  bar6(c1: {}, v: 3) // {{none}}

  c.bar(v : 3, c1: {}) // expected-note {{use trailing closure to simplify arguments}} {{14-21=) }} {{22-112=}}
  c.bar1({}) // expected-note {{use trailing closure to simplify arguments}} {{9-11=}} {{12-98=}}
  c.bar2(v : 3, c1: {}) // {{none}}
  c.bar2(v : 3, c2: {}) // {{none}}
  c.bar3(v1: 3, c1 : { (i) in }) // expected-note {{use trailing closure to simplify arguments}} {{15-23=) }} {{32-122=}}
  c.bar3(v2: 3, c1 : { (i) in }) // expected-note {{use trailing closure to simplify arguments}} {{15-23=) }} {{32-122=}}

  s.bar(v : 3, c1: {}) // expected-note {{use trailing closure to simplify arguments}} {{14-21=) }} {{22-112=}}
  s.bar1({}) // expected-note {{use trailing closure to simplify arguments}} {{9-11=}} {{12-98=}}
  s.bar2(v : 3, c1: {}) // {{none}}
  s.bar2(v : 3, c2: {}) // {{none}}
  s.bar3(v1: 3, c1 : { (i) in }) // expected-note {{use trailing closure to simplify arguments}} {{15-23=) }} {{32-122=}}
  s.bar3(v2: 3, c1 : { (i) in }) // expected-note {{use trailing closure to simplify arguments}} {{15-23=) }} {{32-122=}}
}
