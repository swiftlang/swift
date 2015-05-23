public enum Basic {
  case Untyped
  case HasType(Int)

  public init() {
    self = .Untyped
  }
  public func doSomething() {}
}

public enum Generic<A> {
  case Left(A)
  case Right(A)
}

public protocol Computable {
  func compute()
}

public enum Lazy<T> : Computable {
  case Thunk(() -> T)
  case Value(T)

  public init(value: T) {
    self = .Value(value)
  }

  public func compute() {
//    if (this ~= .Thunk(var fn)) {
//      this = .Value(fn())
//    }
  }
}

public enum Breakfast<Champions> : Int {
  case Eggs
  case Bacon
  case Coffee
}
