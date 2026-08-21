protocol Shape {
  func draw()
}

struct Circle: Shape {
  @_optimize(none)
  func draw() {}
}

func testPeephole() {
  let s: any Shape = Circle()
  s.draw()
}
