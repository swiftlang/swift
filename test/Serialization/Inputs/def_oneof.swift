oneof Basic {
  Untyped,
  HasType : Int
  
  constructor() {
    this = .Untyped
  }
  func doSomething() {}
}

oneof Generic<A> {
  Left : A,
  Right : A
}


protocol Computable {
  func compute()
}

oneof Lazy<T> : Computable {
  Thunk : () -> T,
  Value : T
  
  func compute() {
//    if (this ~= .Thunk(var fn)) {
//      this = .Value(fn())
//    }
  }
}
