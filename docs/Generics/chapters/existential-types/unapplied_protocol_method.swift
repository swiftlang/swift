protocol Value {
  func clone() -> Self
  static func empty() -> Self
}

func generic<T: Value>(_: T.Type) {
  let fn1 = T.clone
  let staticFn1 = T.empty
}

// What are the types of `fn2' and `fn3'?
let fn2 = (any Cloneable).clone
let fn3 = (any Cloneable & Equatable).clone

// Not allowed:
let staticFn2 = (any Cloneable).staticFn
