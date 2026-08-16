func f<T: Sendable>(_: T) {
  print(T.self)
}

let x: any Sendable = 123
f(x)
