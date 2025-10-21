// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -o /dev/null %s

protocol P {
  associatedtype AT
  func foo() -> AT
}

struct X<C1: Collection, C2: Collection, T>: P
    where C1.Element == C2.Element
{
  func foo() -> some P {
    return self
  }
}
