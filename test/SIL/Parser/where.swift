// RUN: %target-swift-frontend %s -emit-silgen | %target-sil-opt

import Swift
protocol P {
  associatedtype CodeUnit
  mutating func decode<
    G : IteratorProtocol where G.Element == CodeUnit
  >(next: inout G) -> Int
}
