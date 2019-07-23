// RUN: %target-build-swift -emit-module -o %t %s

public protocol P1 {}
public protocol P2 {}

public protocol P3 {
  static func a()

  func b()
  func b<I: P1>(_: (I) -> Void)

  static func c<I: P1>(_: I)
  static func d()
  static func d<I: P1>(_: ([(I, I)]) -> Void)
  static func d<I: P1>(_: ([I: I]) -> Void)
  static func d<Q: P1>(_: Q)

  static func e<Q: P1, I: P2>(_: Q, _: (I) -> Void)
  static func f<Q: P1, I: P2>(_: Q, _: (I) -> Void)

  func g<I: P1>(_: I)
}

public extension P3 {
  static func a() {}

  func b() {}
  func b<I: P1>(_: (I) -> Void) {}

  static func c<I: P1>(_: I) {}

  static func d() {}
  static func d<I: P1>(_: ([(I, I)]) -> Void) {}
  static func d<I: P1>(_: ([I: I]) -> Void) {}
  static func d<Q: P1>(_: Q) {}

  static func e<Q: P1, I: P2>(_: Q, _: (I) -> Void) {}
  static func f<Q: P1, I: P2>(_: Q, _: (I) -> Void) {}

  func g<I: P1>(_: I) {}
}

struct S: P3 {
}
