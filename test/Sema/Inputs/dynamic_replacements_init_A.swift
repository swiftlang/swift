struct S {
    let i: Int
    init(i: Int) {
        self.i = i
    }

    init(y: Int) {
      self.init(i: y)
    }
}

class A {
  let i: Int
  init(i: Int) {
    self.i = i
  }
  convenience init(c: Int) {
    self.init(i: c)
  }
}

class B : A {
  let b: Int
  init(b: Int, i: Int) {
    self.b = b
    super.init(i: i)
  }

  convenience init(x: Int) {
    self.init(b: x, i: x)
  }
}
