// RUN: %target-swift-frontend %s -emit-silgen | %target-sil-opt

import Swift
protocol P {
  associatedtype CodeUnit
  mutating func decode<
    G : IteratorProtocol
  >(next: inout G) -> Int where G.Element == CodeUnit
}
