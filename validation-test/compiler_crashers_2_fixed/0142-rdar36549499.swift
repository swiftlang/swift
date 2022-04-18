// RUN: %target-swift-frontend %s -emit-ir -o /dev/null
protocol S {
  associatedtype I: IteratorProtocol
  typealias E = I.Element
}

func foo<T: S>(_ s: T) -> Int where T.E == Int {
  return 42
}
