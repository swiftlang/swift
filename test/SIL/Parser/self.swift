// RUN: %target-swift-frontend %s -emit-silgen | %target-sil-opt

import Swift
protocol P {
  func join<
    S : Sequence where S.Iterator.Element == Self
  >(elements: S) -> Self
}
