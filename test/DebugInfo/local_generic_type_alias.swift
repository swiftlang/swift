// RUN: %target-swift-frontend -emit-ir -g %s

public func f<T>(t: T) {
  typealias G1<U> = (U)
  typealias G2<U> = (T)
  typealias G3<U> = (T, U)

  var x: G1<Int> = 123
  var y: G2<Int> = t
  var z: G3<Int> = (t, 123)
}
