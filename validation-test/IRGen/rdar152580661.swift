// RUN: %target-swift-frontend %s -target %target-swift-5.9-abi-triple -emit-ir

struct H<T> {
  var packs: [Pack<T>] {
      id(_packs)
  }
  let _packs: [Pack<T>]
}

struct Pack<each T> {}

func id<T>(_ t: T) -> T {
  return t
}
