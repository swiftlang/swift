// RUN: %swift %s -emit-silgen | sil-opt -verify

import Swift
protocol P {
  func join<
      S : SequenceType where S.Generator.Element == Self
  >(elements: S) -> Self
}
