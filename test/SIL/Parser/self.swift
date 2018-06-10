// RUN: %target-swift-frontend %s -emit-silgen -swift-version 3 | %target-sil-opt

import Swift
protocol P {
  func join<
    S : Sequence where S.Iterator.Element == Self
  >(elements: S) -> Self
}
