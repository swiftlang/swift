// RUN: %target-swift-frontend %s -emit-silgen | %target-sil-opt

import Swift
protocol P {
  func join<
    S : SequenceType where S.Iterator.Element == Self
  >(elements: S) -> Self
}
