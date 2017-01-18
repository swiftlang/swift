protocol Fooable {
  associatedtype AssocType
  func foo(x : AssocType)
}

struct X : Fooable {
  func foo(x: Float) {}
}

struct Y : Fooable {
  func foo(x: String) {}
}
