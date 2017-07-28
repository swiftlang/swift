//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import ModelIO
import simd

@available(OSX, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLSkinDeformer {
    public func jointBindTransforms() -> [float4x4] {
        let jointCount = jointPaths.count
        var jointBindTransforms = [float4x4](repeating: float4x4(), count: jointCount)
        copyJointBindTransforms(into: &jointBindTransforms[0], maxCount: jointCount)
        return jointBindTransforms
    }
}

@available(OSX, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedValue {
    @nonobjc public func getTimes() -> [Double] {
        var times = [Double](repeating: 0, count: Int(timeSampleCount))
        copyTimes(into: &times[0], maxCount: timeSampleCount)
        return times
    }
}

@available(OSX, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedMatrix4x4 {
    public func getFloat4x4Array() -> [float4x4] {
        var values = [float4x4](repeating: float4x4(), count: Int(timeSampleCount))
        copyFloat4x4Array(into: &values[0], maxCount: timeSampleCount)
        return values
    }

    public func getDouble4x4Array() -> [double4x4] {
        var values = [double4x4](repeating: double4x4(), count: Int(timeSampleCount))
        copyDouble4x4Array(into: &values[0], maxCount: timeSampleCount)
        return values
    }
}

@available(OSX, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedScalarArray {
    @nonobjc public func getFloatArray(atTime time: TimeInterval) -> [Float] {
        var values = [Float](repeating: 0, count: Int(elementsCount))
        copyFloat(into: &values[0], maxCount: elementsCount, atTime: time)
        return values
    }

    @nonobjc public func getDoubleArray(atTime time: TimeInterval) -> [Double] {
        var values = [Double](repeating: 0, count: Int(elementsCount))
        copyDouble(into: &values[0], maxCount: elementsCount, atTime: time)
        return values
    }

    @nonobjc public func getFloatArrays() -> [Float] {
        let count = elementsCount * timeSampleCount
        var values = [Float](repeating: 0, count: Int(count))
        copyFloat(into: &values[0], maxCount: count)
        return values
    }

    @nonobjc public func getDoubleArrays() -> [Double] {
        let count = elementsCount * timeSampleCount
        var values = [Double](repeating: 0, count: Int(count))
        copyDouble(into: &values[0], maxCount: count)
        return values
    }
}

@available(OSX, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedScalar {
    @nonobjc public func getFloatArray() -> [Float] {
        var values = [Float](repeating: 0, count: Int(timeSampleCount))
        copyFloatArray(into: &values[0], maxCount: timeSampleCount)
        return values
    }

    @nonobjc public func getDoubleArray() -> [Double] {
        var values = [Double](repeating: 0, count: Int(timeSampleCount))
        copyDoubleArray(into: &values[0], maxCount: timeSampleCount)
        return values
    }
}

@available(OSX, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector2 {
    public func getFloat2Array() -> [float2] {
        var values = [float2](repeating: float2(), count: Int(timeSampleCount))
        copyFloat2Array(into: &values[0], maxCount: timeSampleCount)
        return values
    }

    public func getDouble2Array() -> [double2] {
        var values = [double2](repeating: double2(), count: Int(timeSampleCount))
        copyDouble2Array(into: &values[0], maxCount: timeSampleCount)
        return values
    }
}

@available(OSX, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector3 {
    public func getFloat3Array() -> [float3] {
        var values = [float3](repeating: float3(), count: Int(timeSampleCount))
        copyFloat3Array(into: &values[0], maxCount: timeSampleCount)
        return values
    }

    public func getDouble3Array() -> [double3] {
        var values = [double3](repeating: double3(), count: Int(timeSampleCount))
        copyDouble3Array(into: &values[0], maxCount: timeSampleCount)
        return values
    }
}

@available(OSX, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector4 {
    public func getFloat4Array() -> [float4] {
        var values = [float4](repeating: float4(), count: timeSampleCount)
        copyFloat4Array(into: &values[0], maxCount: timeSampleCount)
        return values
    }

    public func getDouble4Array() -> [double4] {
        var values = [double4](repeating: double4(), count: timeSampleCount)
        copyDouble4Array(into: &values[0], maxCount: timeSampleCount)
        return values
    }
}
