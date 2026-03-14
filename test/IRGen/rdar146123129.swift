// RUN: %target-swift-frontend -emit-ir -O -enable-library-evolution %s
// REQUIRES: objc_interop

import Foundation

extension Measurement: Strideable where UnitType == UnitLength {
    public func advanced(by n: Double) -> Measurement<UnitLength> {
        return Self.init(value: value + n, unit: unit)
    }
    
    public func distance(to other: Measurement<UnitLength>) -> Double {
        return other.value - value
    }
}
public func test(_ gridHalfSizeInLabelSpacingUnits: Measurement<UnitLength>, _ value: Double, _ orientation: Bool) {
  let origin = Measurement(value: 0, unit: UnitLength.meters)

  @inline(never)
  func addLabelMeshForLine(startingAt startPositionMeasurement: Measurement<UnitLength>) {
      print(startPositionMeasurement)
  }

  for startPositionMeasurement in stride(from: origin, through: gridHalfSizeInLabelSpacingUnits, by: value) {
      addLabelMeshForLine(startingAt: startPositionMeasurement)
  }
}
