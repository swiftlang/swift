// RUN: true

import CoreGraphics

// Case 1 - witness is imported accessor

protocol OtherPoint {
  associatedtype FloatType

  var x: FloatType { get set }
  var y: FloatType { get set }
}

extension CGPoint: OtherPoint {}

// Case 2 - witness is extension method of imported type

extension CGPoint {
  var z: Float {
    get { return 0.0 }
    set { }
  }
}
