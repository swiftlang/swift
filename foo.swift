
enum SinglePayloadGenericEnumWithDefaultMirror<T, U> {
  case Well
  case Faucet
  case Pipe(T, U)
}

func foo(x: Int, y: [Int], out: (SinglePayloadGenericEnumWithDefaultMirror<Int, [Int]>) -> ()) {
  out(.Well)
  out(.Faucet)
  out(.Pipe(x, y))
}

func bar<T, U>(_ x: SinglePayloadGenericEnumWithDefaultMirror<T, U>) {
  switch x {
  case .Well:
    print("well")
  case .Faucet:
    print("faucet")
  case .Pipe:
    print("pipe")
  }
}

foo(x: 1, y: [1,2,3], out: bar)
