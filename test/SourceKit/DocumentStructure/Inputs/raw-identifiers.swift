struct `A.B` {
  func `foo bar`(`a b`: Int, c: Int, `$`: String) {}
  func `3four`() {}
  func `baz`(`x`: Int) {}
}
extension `A.B` {}

struct Outer {
  struct Inner {}
}
extension Outer.Inner {}

func + (lhs: `A.B`, rhs: `A.B`) -> `A.B` { lhs }
