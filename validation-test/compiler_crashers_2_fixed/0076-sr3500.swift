// RUN: %target-swift-frontend %s -emit-ir

protocol A {
  associatedtype Coordinate: Strideable
  func doSomething(_: Range<Coordinate>) -> Coordinate.Stride
}

extension A where Coordinate == Int {
  func extensionFunc(_ range: Range<Coordinate>) {
    _ = doSomething(range)
  }
}
