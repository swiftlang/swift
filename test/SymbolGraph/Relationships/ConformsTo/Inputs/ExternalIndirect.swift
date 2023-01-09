public protocol EA {}
public protocol EB {}

public typealias EAB = EA & EB

public protocol EP : EAB {
  func foo()
}

public protocol EQ : EP {}

public struct ES { }
