class Base {}
class Sub: Base {}

protocol Proto {
  associatedtype Assoc
  func make() -> Assoc
}

private struct Container : Proto {
  func make() -> some Base {
    return Sub()
  }

  func update(arg: Assoc) {}
}
