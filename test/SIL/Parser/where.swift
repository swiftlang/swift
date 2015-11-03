// RUN: %target-swift-frontend %s -emit-silgen | %target-sil-opt

import Swift
protocol P {
  typealias CodeUnit
  mutating func decode<
    G : IteratorProtocol where G.Element == CodeUnit
  >(inout next: G) -> Int
}
