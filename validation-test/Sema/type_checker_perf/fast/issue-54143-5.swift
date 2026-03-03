// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100
// REQUIRES: objc_interop

import Foundation

struct UnitRange<Unit> where Unit: Dimension {
    var range: ClosedRange<Int> {
        return min...max
    }

    let min: Int
    let max: Int
    let unit: Unit

    func convert(to targetUnit: Unit) -> UnitRange<Unit> {
        func convertedValue(from value: Int) -> Int {
            let rounded = Measurement(value: Double(value), unit: unit)
                .converted(to: targetUnit)
                .value
                .rounded()
            return Int(rounded)
        }
        return UnitRange(
            min: convertedValue(from: min),
            max: convertedValue(from: max),
            unit: targetUnit
        )
    }
}
