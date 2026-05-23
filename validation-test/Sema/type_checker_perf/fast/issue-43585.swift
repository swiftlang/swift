// RUN: %target-typecheck-verify-swift -solver-scope-threshold=200

// https://github.com/swiftlang/swift/issues/43585

struct HalfOpenInterval<T> {
    let start: T
    let end: T
}

extension BinaryInteger {
    func mappedFromRange<T: BinaryFloatingPoint>(srcRange: CountableRange<Self>,
                                                 dstInterval: HalfOpenInterval<T>) -> T {
        let sc = T(self - srcRange.startIndex) + 0.5
        let scale = (dstInterval.end - dstInterval.start) / T(srcRange.count)
        return dstInterval.start + sc * scale
    }
}

extension BinaryFloatingPoint {
    func clampedBetween(min minValue: Self, max maxValue: Self) -> Self {
        precondition(minValue <= maxValue)
        return min(maxValue, max(self, minValue))
    }

    func smoothersteppedBetween(min: Self, max: Self) -> Self {
        // This is Ken Perlin's smootherstep function (See Wikipedia smoothstep).
        precondition(min <= max)
        let a = (self - min) / (max - min)
        let b = a.clampedBetween(min: 0, max: 1)
        return b * b * b * (b * (b * 6 - 15) + 10) // As written in C/C++ ref impl.
    }
}
