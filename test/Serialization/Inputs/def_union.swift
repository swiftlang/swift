enum Basic {
  case Untyped
  case HasType(Int)

  init() {
    self = .Untyped
  }
  func doSomething() {}
}

enum Generic<A> {
  case Left(A)
  case Right(A)
}

protocol Computable {
  func compute()
}

enum Lazy<T> : Computable {
  case Thunk(() -> T)
  case Value(T)

  init(value: T) {
    self = .Value(value)
  }

  func compute() {
//    if (this ~= .Thunk(var fn)) {
//      this = .Value(fn())
//    }
  }
}
