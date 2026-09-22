protocol P<A, B> {
  associatedtype A
  associatedtype B
}

protocol Q {}

func generic1<T>(_: T.Type) -> Any.Type {
  return (any P<T, Bool> & Q).self
}

func generic2<T>(_: T.Type) -> Any.Type {
  return (any P<Int, T> & Q).self
}

print(generic1(Int.self) == generic2(Bool.self))