// RUN: true

import CoreGraphics

protocol OtherPoint {
  typealias FloatType

  var x: FloatType { get set }
  var y: FloatType { get set }
}

extension CGPoint: OtherPoint {}
