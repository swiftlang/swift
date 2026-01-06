// RUN: %target-swift-emit-silgen %s

public class C {
  public func f() -> Self { return self }
  public var v: Self { return self }
  public subscript() -> Self { return self }

  public func g1() {}
}

public protocol P {
  func g2()
}

func f(_ p: any P & C) {
  p.f().g1()
  p.f().g2()

  p.v.g1()
  p.v.g2()

  p[].g1()
  p[].g2()
}

