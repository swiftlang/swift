// RUN: %target-typecheck-verify-swift

protocol P1 {}
protocol P2 {}
protocol P3 {}
protocol P4 {}
protocol P5 {}
protocol P6 {}
protocol P7 {}
protocol P8 {}
protocol P9 {}
protocol P10 {}
protocol P11 {}
protocol P12 {}
protocol P13 {}
protocol P14 {}
protocol P15 {}
protocol P16 {}

protocol P {
  associatedtype A
  associatedtype B

  func f<T: P1>(_: T, _: A, _: B)
  func f<T: P2>(_: T, _: A, _: B)
  func f<T: P3>(_: T, _: A, _: B)
  func f<T: P4>(_: T, _: A, _: B)
  func f<T: P5>(_: T, _: A, _: B)
  func f<T: P6>(_: T, _: A, _: B)
  func f<T: P7>(_: T, _: A, _: B)
  func f<T: P8>(_: T, _: A, _: B)
  func f<T: P9>(_: T, _: A, _: B)
  func f<T: P10>(_: T, _: A, _: B)
  func f<T: P11>(_: T, _: A, _: B)
  func f<T: P12>(_: T, _: A, _: B)
  func f<T: P13>(_: T, _: A, _: B)
  func f<T: P14>(_: T, _: A, _: B)
  func f<T: P15>(_: T, _: A, _: B)
  func f<T: P16>(_: T, _: A, _: B)
}

struct G<A>: P {
  func f<T: P1>(_: T, _: A, _: Bool) {}
  func f<T: P2>(_: T, _: A, _: Bool) {}
  func f<T: P3>(_: T, _: A, _: Bool) {}
  func f<T: P4>(_: T, _: A, _: Bool) {}
  func f<T: P5>(_: T, _: A, _: Bool) {}
  func f<T: P6>(_: T, _: A, _: Bool) {}
  func f<T: P7>(_: T, _: A, _: Bool) {}
  func f<T: P8>(_: T, _: A, _: Bool) {}
  func f<T: P9>(_: T, _: A, _: Bool) {}
  func f<T: P10>(_: T, _: A, _: Bool) {}
  func f<T: P11>(_: T, _: A, _: Bool) {}
  func f<T: P12>(_: T, _: A, _: Bool) {}
  func f<T: P13>(_: T, _: A, _: Bool) {}
  func f<T: P14>(_: T, _: A, _: Bool) {}
  func f<T: P15>(_: T, _: A, _: Bool) {}
  func f<T: P16>(_: T, _: A, _: Bool) {}
}

let x: Int.Type = G<Int>.A.self
let y: Bool.Type = G<Int>.B.self
