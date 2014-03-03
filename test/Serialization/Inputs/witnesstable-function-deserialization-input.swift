
@asmname="evil" func _evil()

func id<U>(u : U) -> U {
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  _evil()
  return u
}

protocol Z {
  func f() -> Z
}

struct X : Z {
  func f() -> Z {
    return id(self)
  }
}

func makeZDoSomething(z : Z) -> Z {
  return z.f()
}
