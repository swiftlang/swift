// RUN:%target-swift-frontend -emit-silgen %s -enable-experimental-feature BorrowAndMutateAccessors

// REQUIRES: swift_feature_BorrowAndMutateAccessors

public final class Klass {}

public struct Wrapper {
  var _k: Klass

  var k: Klass {
    borrow {
      return _k
    }
    mutate {
      return &_k
    }
  }
}

@inline(never)
func blackhole<T>(_ t: T) { }

func inoutTest(_ w: inout Wrapper) {
  let k = w.k
  blackhole(k) 
}
