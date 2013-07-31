union Basic {
  case Untyped
  case HasType(Int)
  
  constructor() {
    this = .Untyped
  }
  func doSomething() {}
}

union Generic<A> {
  case Left(A)
  case Right(A)
}


protocol Computable {
  func compute()
}

union Lazy<T> : Computable {
  case Thunk(() -> T)
  case Value(T)
  
  func compute() {
//    if (this ~= .Thunk(var fn)) {
//      this = .Value(fn())
//    }
  }
}
