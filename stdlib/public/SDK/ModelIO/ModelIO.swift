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

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLMatrix4x4Array {
    @nonobjc public var float4x4Array: [float4x4] {
        get {
            let count = elementCount
            var values = [float4x4](repeating: float4x4(), count: Int(count))
            __getFloat4x4Array(&values[0], maxCount: count)
            return values
        }
        set(array) {
            __setFloat4x4(array, count: array.count)
        }
    }
    
    @nonobjc public var double4x4Array: [double4x4] {
        get {
            let count = elementCount
            var values = [double4x4](repeating: double4x4(), count: Int(count))
            __getDouble4x4Array(&values[0], maxCount: count)
            return values
        }
        set(array) {
            __setDouble4x4(array, count: array.count)
        }
    }
}



@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedValue {
    @nonobjc public var times: [TimeInterval] {
        get {
            var times = [TimeInterval](repeating: 0, count: Int(timeSampleCount))
            __getTimes(&times[0], maxCount: timeSampleCount)
            return times
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedScalarArray {
    @nonobjc public func set(floatArray array:[Float], atTime time: TimeInterval){
        __setFloat(array, count: array.count, atTime: time)
    }

    @nonobjc public func set(doubleArray array:[Double], atTime time: TimeInterval){
        __setDouble(array, count: array.count, atTime: time)
    }
    
    @nonobjc public func floatArray(atTime time: TimeInterval) -> [Float] {
        var values = [Float](repeating: 0, count: Int(elementCount))
        __getFloat(&values[0], maxCount: elementCount, atTime: time)
        return values
    }

    @nonobjc public func doubleArray(atTime time: TimeInterval) -> [Double] {
        var values = [Double](repeating: 0, count: Int(elementCount))
        __getDouble(&values[0], maxCount: elementCount, atTime: time)
        return values
    }

    @nonobjc public func reset(floatArray array:[Float], atTimes times: [TimeInterval]){
        __reset(with: array, count: array.count, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(doubleArray array:[Double], atTimes times: [TimeInterval]){
        __reset(with: array, count: array.count, atTimes: times, count: times.count)
    }

    @nonobjc public var floatArray: [Float] {
        get {
            let count = elementCount * timeSampleCount
            var values = [Float](repeating: 0, count: Int(count))
            __getFloat(&values[0], maxCount: count)
            return values
        }
    }

    @nonobjc public var doubleArray: [Double] {
        get {
            let count = elementCount * timeSampleCount
            var values = [Double](repeating: 0, count: Int(count))
            __getDouble(&values[0], maxCount: count)
            return values
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector3Array {
    @nonobjc public func set(float3Array array:[float3], atTime time: TimeInterval){
        __setFloat3(array, count: array.count, atTime: time)
    }
    
    @nonobjc public func set(double3Array array:[double3], atTime time: TimeInterval){
        __setDouble3(array, count: array.count, atTime: time)
    }
    
    @nonobjc public func float3Array(atTime time: TimeInterval) -> [float3] {
        var values = [float3](repeating: float3(), count: Int(elementCount))
        __getFloat3Array(&values[0], maxCount: elementCount, atTime: time)
        return values
    }
    
    @nonobjc public func double3Array(atTime time: TimeInterval) -> [double3] {
        var values = [double3](repeating: double3(), count: Int(elementCount))
        __getDouble3Array(&values[0], maxCount: elementCount, atTime: time)
        return values
    }
    
    @nonobjc public func reset(float3Array array:[float3], atTimes times: [TimeInterval]){
        __reset(withFloat3Array: array, count: array.count, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(double3Array array:[double3], atTimes times: [TimeInterval]){
        __reset(withDouble3Array: array, count: array.count, atTimes: times, count: times.count)
    }
    
    @nonobjc public var float3Array: [float3] {
        get {
            let count = elementCount * timeSampleCount
            var values = [float3](repeating: float3(), count: Int(count))
            __getFloat3Array(&values[0], maxCount: count)
            return values
        }
    }
    
    @nonobjc public var double3Array: [double3] {
        get {
            let count = elementCount * timeSampleCount
            var values = [double3](repeating: double3(), count: Int(count))
            __getDouble3Array(&values[0], maxCount: count)
            return values
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedQuaternionArray {
    @nonobjc public func set(floatQuaternionArray array:[simd_quatf], atTime time: TimeInterval){
        __setFloat(array, count: array.count, atTime: time)
    }
    
    @nonobjc public func set(doubleQuaternionArray array:[simd_quatd], atTime time: TimeInterval){
        __setDouble(array, count: array.count, atTime: time)
    }
    
    @nonobjc public func floatQuaternionArray(atTime time: TimeInterval) -> [simd_quatf] {
        var values = [simd_quatf](repeating: simd_quatf(), count: Int(elementCount))
        __getFloat(&values[0], maxCount: elementCount, atTime: time)
        return values
    }

    @nonobjc public func doubleQuaternionArray(atTime time: TimeInterval) -> [simd_quatd] {
        var values = [simd_quatd](repeating: simd_quatd(), count: Int(elementCount))
        __getDouble(&values[0], maxCount: elementCount, atTime: time)
        return values
    }

    @nonobjc public func reset(floatQuaternionArray array:[simd_quatf], atTimes times: [TimeInterval]){
        __reset(withFloat: array, count: array.count, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(doubleQuaternionArray array:[simd_quatd], atTimes times: [TimeInterval]){
        __reset(withDouble: array, count: array.count, atTimes: times, count: times.count)
    }
    
    @nonobjc public var floatQuaternionArray : [simd_quatf] {
        get {
            let count = elementCount * timeSampleCount
            var values = [simd_quatf](repeating: simd_quatf(), count: Int(count))
            __getFloat(&values[0], maxCount: count)
            return values
        }
    }

    @nonobjc public var doubleQuaternionArray: [simd_quatd] {
        get {
            let count = elementCount * timeSampleCount
            var values = [simd_quatd](repeating: simd_quatd(), count: Int(count))
            __getDouble(&values[0], maxCount: count)
            return values
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedScalar {
    @nonobjc public func reset(floatArray array:[Float], atTimes times: [TimeInterval]){
        __reset(withFloatArray: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(doubleArray array:[Double], atTimes times: [TimeInterval]){
        __reset(withDoubleArray: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public var floatArray: [Float] {
        get {
            var values = [Float](repeating: 0, count: Int(timeSampleCount))
            __getFloatArray(&values[0], maxCount: timeSampleCount)
            return values
        }
    }

    @nonobjc public var doubleArray: [Double] {
        get {
            var values = [Double](repeating: 0, count: Int(timeSampleCount))
            __getDoubleArray(&values[0], maxCount: timeSampleCount)
            return values
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector2 {
    @nonobjc public func reset(float2Array array:[float2], atTimes times: [TimeInterval]){
        __reset(withFloat2Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(double2Array array:[double2], atTimes times: [TimeInterval]){
        __reset(withDouble2Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public var float2Array: [float2] {
        get {
            var values = [float2](repeating: float2(), count: Int(timeSampleCount))
            __getFloat2Array(&values[0], maxCount: timeSampleCount)
            return values
        }
    }

    @nonobjc public var double2Array: [double2] {
        get {
            var values = [double2](repeating: double2(), count: Int(timeSampleCount))
            __getDouble2Array(&values[0], maxCount: timeSampleCount)
            return values
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector3 {
    @nonobjc public func reset(float3Array array:[float3], atTimes times: [TimeInterval]){
        __reset(withFloat3Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(double3Array array:[double3], atTimes times: [TimeInterval]){
        __reset(withDouble3Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public var float3Array: [float3] {
        get {
            var values = [float3](repeating: float3(), count: Int(timeSampleCount))
            __getFloat3Array(&values[0], maxCount: timeSampleCount)
            return values
        }
    }

    @nonobjc public var double3Array: [double3] {
        get {
            var values = [double3](repeating: double3(), count: Int(timeSampleCount))
            __getDouble3Array(&values[0], maxCount: timeSampleCount)
            return values
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector4 {
    @nonobjc public func reset(float4Array array:[float4], atTimes times: [TimeInterval]){
        __reset(withFloat4Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(double4Array array:[double4], atTimes times: [TimeInterval]){
        __reset(withDouble4Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public var float4Array: [float4] {
        get {
            var values = [float4](repeating: float4(), count: Int(timeSampleCount))
            __getFloat4Array(&values[0], maxCount: timeSampleCount)
            return values
        }
    }

    @nonobjc public var double4Array: [double4] {
        get {
            var values = [double4](repeating: double4(), count: Int(timeSampleCount))
            __getDouble4Array(&values[0], maxCount: timeSampleCount)
            return values
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedMatrix4x4 {
    @nonobjc public func reset(float4x4Array array:[float4x4], atTimes times: [TimeInterval]){
        __reset(withFloat4x4Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(double4Array array:[double4x4], atTimes times: [TimeInterval]){
        __reset(withDouble4x4Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public var float4x4Array: [float4x4] {
        get {
            var values = [float4x4](repeating: float4x4(), count: Int(timeSampleCount))
            __getFloat4x4Array(&values[0], maxCount: timeSampleCount)
            return values
        }
    }
    
    @nonobjc public var double4x4Array: [double4x4] {
        get {
            var values = [double4x4](repeating: double4x4(), count: Int(timeSampleCount))
            __getDouble4x4Array(&values[0], maxCount: timeSampleCount)
            return values
        }
    }
}

