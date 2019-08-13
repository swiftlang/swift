struct SR11227_S {}

extension SR11227_S : SR11227_P2 {}

extension SR11227_S : SR11227_P1_1 {}

func f() {
  let s = SR11227_S.init()
  s.sr11227_p1_1()
}
