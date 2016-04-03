protocol Fooable {
  associatedtype AssocType
  func foo(x : AssocType)
}

struct X : Fooable {
  func foo(_ x: Float) {}
}

struct Y : Fooable {
  func foo(_ x: String) {}
}
