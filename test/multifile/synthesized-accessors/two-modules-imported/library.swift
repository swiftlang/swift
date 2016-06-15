// RUN: true

import CoreGraphics

public protocol OtherPoint {
  associatedtype FloatType

  var x: FloatType { get set }
  var y: FloatType { get set }
}

extension CGPoint: OtherPoint {}
