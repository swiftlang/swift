// RUN: %target-swift-frontend %s -emit-silgen | %target-sil-opt -enable-sil-verify-all

import Swift
protocol P {
  func join<
      S : SequenceType where S.Generator.Element == Self
  >(elements: S) -> Self
}
