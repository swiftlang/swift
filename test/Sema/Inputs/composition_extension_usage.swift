struct S {}

extension S : P2 {}

extension S : P1_1 {}

func f() {
  let s = S.init()
  s.p1_1()
}
