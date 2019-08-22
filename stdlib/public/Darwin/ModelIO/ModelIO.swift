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

extension Array {
    fileprivate init(
        unsafeUninitializedCount count: Int,
        initializedWith initializer: (UnsafeMutablePointer<Element>) -> Void
    ) {
        self.init(unsafeUninitializedCapacity: count) { buffer, initializedCount in
            initializer(buffer.baseAddress!)
            initializedCount = count
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLMatrix4x4Array {
    @nonobjc public var float4x4Array: [float4x4] {
        get {
            let count = elementCount
            return [float4x4](unsafeUninitializedCount: count) { ptr in
                __getFloat4x4Array(ptr, maxCount: count)
            }
        }
        set(array) {
            __setFloat4x4(array, count: array.count)
        }
    }
    
    @nonobjc public var double4x4Array: [double4x4] {
        get {
            let count = elementCount
            return [double4x4](unsafeUninitializedCount: count) { ptr in
                __getDouble4x4Array(ptr, maxCount: count)
            }
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
            return [TimeInterval](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getTimes(ptr, maxCount: timeSampleCount)
            }
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
        return [Float](unsafeUninitializedCount: elementCount) { ptr in
            __getFloat(ptr, maxCount: elementCount, atTime: time)
        }
    }

    @nonobjc public func doubleArray(atTime time: TimeInterval) -> [Double] {
        return [Double](unsafeUninitializedCount: elementCount) { ptr in
            __getDouble(ptr, maxCount: elementCount, atTime: time)
        }
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
            return [Float](unsafeUninitializedCount: count) { ptr in
                __getFloat(ptr, maxCount: count)
            }
        }
    }

    @nonobjc public var doubleArray: [Double] {
        get {
            let count = elementCount * timeSampleCount
            return [Double](unsafeUninitializedCount: count) { ptr in
                __getDouble(ptr, maxCount: count)
            }
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector3Array {
    @nonobjc public func set(float3Array array:[SIMD3<Float>], atTime time: TimeInterval){
        __setFloat3(array, count: array.count, atTime: time)
    }
    
    @nonobjc public func set(double3Array array:[SIMD3<Double>], atTime time: TimeInterval){
        __setDouble3(array, count: array.count, atTime: time)
    }
    
    @nonobjc public func float3Array(atTime time: TimeInterval) -> [SIMD3<Float>] {
        return [SIMD3<Float>](unsafeUninitializedCount: elementCount) { ptr in
            __getFloat3Array(ptr, maxCount: elementCount, atTime: time)
        }
    }
    
    @nonobjc public func double3Array(atTime time: TimeInterval) -> [SIMD3<Double>] {
        return [SIMD3<Double>](unsafeUninitializedCount: elementCount) { ptr in
            __getDouble3Array(ptr, maxCount: elementCount, atTime: time)
        }
    }
    
    @nonobjc public func reset(float3Array array:[SIMD3<Float>], atTimes times: [TimeInterval]){
        __reset(withFloat3Array: array, count: array.count, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(double3Array array:[SIMD3<Double>], atTimes times: [TimeInterval]){
        __reset(withDouble3Array: array, count: array.count, atTimes: times, count: times.count)
    }
    
    @nonobjc public var float3Array: [SIMD3<Float>] {
        get {
            let count = elementCount * timeSampleCount
            return [SIMD3<Float>](unsafeUninitializedCount: count) { ptr in
                __getFloat3Array(ptr, maxCount: count)
            }
        }
    }
    
    @nonobjc public var double3Array: [SIMD3<Double>] {
        get {
            let count = elementCount * timeSampleCount
            return [SIMD3<Double>](unsafeUninitializedCount: count) { ptr in
                __getDouble3Array(ptr, maxCount: count)
            }
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
        return [simd_quatf](unsafeUninitializedCount: elementCount) { ptr in
            __getFloat(ptr, maxCount: elementCount, atTime: time)
        }
    }

    @nonobjc public func doubleQuaternionArray(atTime time: TimeInterval) -> [simd_quatd] {
        return [simd_quatd](unsafeUninitializedCount: elementCount) { ptr in
            __getDouble(ptr, maxCount: elementCount, atTime: time)
        }
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
            return [simd_quatf](unsafeUninitializedCount: count) { ptr in
                __getFloat(ptr, maxCount: count)
            }
        }
    }

    @nonobjc public var doubleQuaternionArray: [simd_quatd] {
        get {
            let count = elementCount * timeSampleCount
            return [simd_quatd](unsafeUninitializedCount: count) { ptr in
                __getDouble(ptr, maxCount: count)
            }
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
            return [Float](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getFloatArray(ptr, maxCount: timeSampleCount)
            }
        }
    }

    @nonobjc public var doubleArray: [Double] {
        get {
            return [Double](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getDoubleArray(ptr, maxCount: timeSampleCount)
            }
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector2 {
    @nonobjc public func reset(float2Array array:[SIMD2<Float>], atTimes times: [TimeInterval]){
        __reset(withFloat2Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(double2Array array:[SIMD2<Double>], atTimes times: [TimeInterval]){
        __reset(withDouble2Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public var float2Array: [SIMD2<Float>] {
        get {
            return [SIMD2<Float>](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getFloat2Array(ptr, maxCount: timeSampleCount)
            }
        }
    }

    @nonobjc public var double2Array: [SIMD2<Double>] {
        get {
            return [SIMD2<Double>](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getDouble2Array(ptr, maxCount: timeSampleCount)
            }
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector3 {
    @nonobjc public func reset(float3Array array:[SIMD3<Float>], atTimes times: [TimeInterval]){
        __reset(withFloat3Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(double3Array array:[SIMD3<Double>], atTimes times: [TimeInterval]){
        __reset(withDouble3Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public var float3Array: [SIMD3<Float>] {
        get {
            return [SIMD3<Float>](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getFloat3Array(ptr, maxCount: timeSampleCount)
            }
        }
    }

    @nonobjc public var double3Array: [SIMD3<Double>] {
        get {
            return [SIMD3<Double>](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getDouble3Array(ptr, maxCount: timeSampleCount)
            }
        }
    }
}

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension MDLAnimatedVector4 {
    @nonobjc public func reset(float4Array array:[SIMD4<Float>], atTimes times: [TimeInterval]){
        __reset(withFloat4Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public func reset(double4Array array:[SIMD4<Double>], atTimes times: [TimeInterval]){
        __reset(withDouble4Array: array, atTimes: times, count: times.count)
    }
    
    @nonobjc public var float4Array: [SIMD4<Float>] {
        get {
            return [SIMD4<Float>](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getFloat4Array(ptr, maxCount: timeSampleCount)
            }
        }
    }

    @nonobjc public var double4Array: [SIMD4<Double>] {
        get {
            return [SIMD4<Double>](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getDouble4Array(ptr, maxCount: timeSampleCount)
            }
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
            return [float4x4](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getFloat4x4Array(ptr, maxCount: timeSampleCount)
            }
        }
    }
    
    @nonobjc public var double4x4Array: [double4x4] {
        get {
            return [double4x4](unsafeUninitializedCount: timeSampleCount) { ptr in
                __getDouble4x4Array(ptr, maxCount: timeSampleCount)
            }
        }
    }
}
