// RUN: %target-swift-frontend %s -emit-silgen | %target-sil-opt

import Swift
protocol P {
  func join<
    S : Sequence
  >(elements: S) -> Self where S.Iterator.Element == Self
}
