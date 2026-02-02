// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.9-abi-triple

public protocol P {}

public struct G<T>: P {
  let s1: String
  let s2: String
}

public func f<each T>(t: repeat G<each T>) {
    var ts: [any P] = []
    for x in repeat each t {
        ts.append(x)
    }
}
