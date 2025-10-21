// RUN: %target-swift-emit-silgen %s

class C: P1, P2 {
  public func f(_ n: Int) -> Self {
    self.x = n
    self.y = n
    self.z = n
    return self
  }

  public var x: Int? {
    get { fatalError() }
    set { }
  }
}

protocol P1: AnyObject {}
protocol P2 {}

extension P1 {
  public var y: Int? {
    get { fatalError() }
    set { }
  }
}

extension P2 {
  public var z: Int? {
    get { fatalError() }
    nonmutating set { }
  }
}
