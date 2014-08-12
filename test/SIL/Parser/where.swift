// RUN: %swift %s -emit-silgen | sil-opt -verify

import Swift
protocol P {
  typealias CodeUnit
  mutating func decode<
    G : GeneratorType where G.Element == CodeUnit
  >(inout next: G) -> Int
}
