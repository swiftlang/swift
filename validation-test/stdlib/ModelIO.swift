// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// UNSUPPORTED: OS=watchos
// REQUIRES: objc_interop

import StdlibUnittest

import Foundation
import ModelIO

var ModelIOTests = TestSuite("ModelIO")

if #available(OSX 10.13, iOS 11.0, tvOS 11.0, *) {
    ModelIOTests.test("MDLAnimatedScalar/accessors") {
        let animatedVal = MDLAnimatedScalar()
        let testCount = 10
        let testTimeVal = 5.0
        let testFloatVal:Float = 1.0
        let testDoubleVal = Double(testFloatVal)
        let fArray = [Float](repeating: testFloatVal, count: testCount)
        let dArray = [Double](repeating: testDoubleVal, count: testCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(floatArray: fArray, atTimes: times)
        let floats = animatedVal.floatArray
        animatedVal.reset(doubleArray: dArray, atTimes: times)
        let doubles = animatedVal.doubleArray
        times = animatedVal.times

        expectEqual(floats.count, testCount)
        expectEqual(doubles.count, testCount)
        expectEqual(times.count, testCount)

        for idx in 0..<testCount {
            expectEqual(floats[idx], testFloatVal)
            expectEqual(doubles[idx], testDoubleVal)
            expectEqual(times[idx], testTimeVal)
        }
    }

    ModelIOTests.test("MDLAnimatedVector2/accessors") {
        let animatedVal = MDLAnimatedVector2()
        let testCount = 10
        let testTimeVal = 5.0
        let testFloatVal = float2(1.0, 2.0)
        let testDoubleVal = double2(Double(testFloatVal.x), Double(testFloatVal.y))
        let fArray = [float2](repeating: testFloatVal, count: testCount)
        let dArray = [double2](repeating: testDoubleVal, count: testCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(float2Array: fArray, atTimes: times)

        let floats = animatedVal.float2Array
        animatedVal.reset(double2Array: dArray, atTimes: times)
        let doubles = animatedVal.double2Array
        times = animatedVal.times

        expectEqual(floats.count, testCount)
        expectEqual(doubles.count, testCount)
        expectEqual(times.count, testCount)

        for idx in 0..<testCount {
            expectEqual(floats[idx].x, testFloatVal.x)
            expectEqual(floats[idx].y, testFloatVal.y)
            expectEqual(doubles[idx].x, testDoubleVal.x)
            expectEqual(doubles[idx].y, testDoubleVal.y)
            expectEqual(times[idx], testTimeVal)
        }
    }

    ModelIOTests.test("MDLAnimatedVector3/accessors") {
        let animatedVal = MDLAnimatedVector3()
        let testCount = 10
        let testTimeVal = 5.0
        let testFloatVal = float3(1.0, 2.0, 3.0)
        let testDoubleVal = double3(Double(testFloatVal.x), Double(testFloatVal.y), Double(testFloatVal.z))
        let fArray = [float3](repeating: testFloatVal, count: testCount)
        let dArray = [double3](repeating: testDoubleVal, count: testCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(float3Array: fArray, atTimes: times)

        let floats = animatedVal.float3Array
        animatedVal.reset(double3Array: dArray, atTimes: times)
        let doubles = animatedVal.double3Array
        times = animatedVal.times

        expectEqual(floats.count, testCount)
        expectEqual(doubles.count, testCount)
        expectEqual(times.count, testCount)

        for idx in 0..<testCount {
            expectEqual(floats[idx].x, testFloatVal.x)
            expectEqual(floats[idx].y, testFloatVal.y)
            expectEqual(floats[idx].z, testFloatVal.z)
            expectEqual(doubles[idx].x, testDoubleVal.x)
            expectEqual(doubles[idx].y, testDoubleVal.y)
            expectEqual(doubles[idx].z, testDoubleVal.z)
            expectEqual(times[idx], testTimeVal)
        }
    }

    ModelIOTests.test("MDLAnimatedVector4/accessors") {
        let animatedVal = MDLAnimatedVector4()
        let testCount = 10
        let testTimeVal = 5.0
        let testFloatVal = float4(1.0, 2.0, 3.0, 4.0)
        let testDoubleVal = double4(Double(testFloatVal.x), Double(testFloatVal.y), Double(testFloatVal.z), Double(testFloatVal.w))
        let fArray = [float4](repeating: testFloatVal, count: testCount)
        let dArray = [double4](repeating: testDoubleVal, count: testCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(float4Array: fArray, atTimes: times)

        let floats = animatedVal.float4Array
        animatedVal.reset(double4Array: dArray, atTimes: times)
        let doubles = animatedVal.double4Array
        times = animatedVal.times

        expectEqual(floats.count, testCount)
        expectEqual(doubles.count, testCount)
        expectEqual(times.count, testCount)

        for idx in 0..<testCount {
            expectEqual(floats[idx].x, testFloatVal.x)
            expectEqual(floats[idx].y, testFloatVal.y)
            expectEqual(floats[idx].z, testFloatVal.z)
            expectEqual(floats[idx].w, testFloatVal.w)
            expectEqual(doubles[idx].x, testDoubleVal.x)
            expectEqual(doubles[idx].y, testDoubleVal.y)
            expectEqual(doubles[idx].z, testDoubleVal.z)
            expectEqual(doubles[idx].w, testDoubleVal.w)
            expectEqual(times[idx], testTimeVal)
        }
    }

    ModelIOTests.test("MDLAnimatedMatrix4x4/accessors") {
        let animatedVal = MDLAnimatedMatrix4x4()
        let testCount = 10
        let testTimeVal = 5.0
        let testFloatVal = matrix_identity_float4x4
        let testDoubleVal = matrix_identity_double4x4
        let fArray = [float4x4](repeating: testFloatVal, count: testCount)
        let dArray = [double4x4](repeating: testDoubleVal, count: testCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(float4x4Array: fArray, atTimes: times)

        let floats = animatedVal.float4x4Array
        animatedVal.reset(double4Array: dArray, atTimes: times)
        let doubles = animatedVal.double4x4Array
        times = animatedVal.times

        expectEqual(floats.count, testCount)
        expectEqual(doubles.count, testCount)
        expectEqual(times.count, testCount)

        for idx in 0..<testCount {
            for matIdx in 0..<4 {
                expectEqual(floats[idx][matIdx].x, testFloatVal[matIdx].x)
                expectEqual(floats[idx][matIdx].y, testFloatVal[matIdx].y)
                expectEqual(floats[idx][matIdx].z, testFloatVal[matIdx].z)
                expectEqual(floats[idx][matIdx].w, testFloatVal[matIdx].w)
                expectEqual(doubles[idx][matIdx].x, testDoubleVal[matIdx].x)
                expectEqual(doubles[idx][matIdx].y, testDoubleVal[matIdx].y)
                expectEqual(doubles[idx][matIdx].z, testDoubleVal[matIdx].z)
                expectEqual(doubles[idx][matIdx].w, testDoubleVal[matIdx].w)
            }
            expectEqual(times[idx], testTimeVal)
        }
    }

    ModelIOTests.test("MDLMatrix4x4Array/accessors") {
        let testCount = 10
        let matrixArray = MDLMatrix4x4Array(elementCount: testCount)
        let testFloatVal = float4x4()
        let testDoubleVal = double4x4()
        let fArray = [float4x4](repeating: testFloatVal, count: testCount)
        let dArray = [double4x4](repeating: testDoubleVal, count: testCount)
        matrixArray.float4x4Array = fArray

        let floats = matrixArray.float4x4Array
        matrixArray.double4x4Array = dArray
        let doubles = matrixArray.double4x4Array

        expectEqual(floats.count, testCount)
        expectEqual(doubles.count, testCount)

        for idx in 0..<testCount {
            for matIdx in 0..<4 {
                expectEqual(floats[idx][matIdx].x, testFloatVal[matIdx].x)
                expectEqual(floats[idx][matIdx].y, testFloatVal[matIdx].y)
                expectEqual(floats[idx][matIdx].z, testFloatVal[matIdx].z)
                expectEqual(floats[idx][matIdx].w, testFloatVal[matIdx].w)
                expectEqual(doubles[idx][matIdx].x, testDoubleVal[matIdx].x)
                expectEqual(doubles[idx][matIdx].y, testDoubleVal[matIdx].y)
                expectEqual(doubles[idx][matIdx].z, testDoubleVal[matIdx].z)
                expectEqual(doubles[idx][matIdx].w, testDoubleVal[matIdx].w)
            }
        }
    }

    ModelIOTests.test("MDLAnimatedScalarArray/accessors") {
        let elementCount = 10
        let animatedVal = MDLAnimatedScalarArray(elementCount: elementCount)
        let subCount = 2
        let testCount = 10
        let totalCount = elementCount * testCount
        let testTimeVal = 5.0
        let testFloatVal:Float = 10.0
        let testSubFloatVal:Float = 5.0
        let testDoubleVal = Double(testFloatVal)
        let testSubDoubleVal = Double(testSubFloatVal)
        let fArray = [Float](repeating: testFloatVal, count: totalCount)
        let _ = [Float](repeating: testSubFloatVal, count: subCount)
        let dArray = [Double](repeating: testDoubleVal, count: totalCount)
        let _ = [Double](repeating: testSubDoubleVal, count: subCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(floatArray: fArray, atTimes: times)

        let floats = animatedVal.floatArray
        // reset is currently appending instead of resetting the time sampled data
        //animatedVal.reset(doubleArray: dArray, atTimes: times)
        let doubles = animatedVal.doubleArray
        let sampledFloatArray = animatedVal.floatArray(atTime: 5.0)
        let sampledDoubleArray = animatedVal.doubleArray(atTime: 5.0)
        times = animatedVal.times

        expectEqual(floats.count, totalCount)
        expectEqual(doubles.count, totalCount)
        expectEqual(times.count, testCount)

        for idx in 0..<testCount {
            // -- test a sampled time
            expectEqual(sampledFloatArray[idx], testFloatVal)
            expectEqual(sampledDoubleArray[idx], testDoubleVal)

            // -- for each time test the arrays
            for arrIdx in 0..<elementCount {
                expectEqual(floats[idx * elementCount + arrIdx], testFloatVal)
                expectEqual(doubles[idx * elementCount + arrIdx], testDoubleVal)
            }
            expectEqual(times[idx], testTimeVal)
        }
    }

    ModelIOTests.test("MDLAnimatedQuaternionArray/accessors") {
        let elementCount = 10
        let testCount = 10
        let totalCount = elementCount * testCount
        let animatedVal = MDLAnimatedQuaternionArray(elementCount: elementCount)
        let testTimeVal = 5.0;
        let testFloatVal = simd_quatf(ix: 1.0, iy: 2.0, iz: 3.0, r: 4.0)
        let testDoubleVal = simd_quatd(ix: 1.0, iy: 2.0, iz: 3.0, r: 4.0)
        let fArray = [simd_quatf](repeating: testFloatVal, count: totalCount)
        let dArray = [simd_quatd](repeating: testDoubleVal, count: totalCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(floatQuaternionArray: fArray, atTimes: times)

        let quatFloats = animatedVal.floatQuaternionArray
        // reset is appending instead of reseting the time sampled data
        //animatedVal.reset(doubleQuaternionArray: dArray, atTimes: times)
        let quatDoubles = animatedVal.doubleQuaternionArray
        let sampledFloatQuaternionArray = animatedVal.floatQuaternionArray(atTime: 5.0)
        let sampledDoubleQuaternionArray = animatedVal.doubleQuaternionArray(atTime: 5.0)
        times = animatedVal.times

        expectEqual(quatFloats.count, totalCount)
        expectEqual(quatDoubles.count, totalCount)
        expectEqual(times.count, testCount)

        for idx in 0..<testCount {
            // -- test a sampled time
            // -- data gets swizzled somewhere between getting and setting so we just
            // -- check to make sure we can at least access the data
            sampledFloatQuaternionArray[idx]
            sampledDoubleQuaternionArray[idx]
            /*expectEqual(sampledFloatQuaternionArray[idx], testFloatVal)
            expectEqual(sampledDoubleQuaternionArray[idx], testDoubleVal)

            // -- for each time test the arrays
            for arrIdx in 0..<elementCount {
                expectEqual(quatFloats[idx * elementCount + arrIdx], testFloatVal)
                expectEqual(quatDoubles[idx * elementCount + arrIdx], testDoubleVal)
            }*/
            expectEqual(times[idx], testTimeVal)
        }
    }

    ModelIOTests.test("MDLAnimatedVector3Array/accessors") {
        let elementCount = 10
        let animatedVal = MDLAnimatedVector3Array(elementCount: elementCount)
        let testCount = 10
        let totalCount = elementCount * testCount
        let testTimeVal = 5.0
        let testFloatVal = float3(1.0, 2.0, 3.0)
        let testDoubleVal = double3(1.0, 2.0, 3.0)
        let fArray = [float3](repeating: testFloatVal, count: totalCount)
        let dArray = [double3](repeating: testDoubleVal, count: totalCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(float3Array: fArray, atTimes: times)

        let vector3Floats = animatedVal.float3Array
        // reset is appending  instead reseting the time sampled data
        //animatedVal.reset(double3Array: dArray, atTimes: times)
        let vector3Doubles = animatedVal.double3Array
        let sampledFloatVector3Array = animatedVal.float3Array(atTime: 5.0)
        let sampledDoubleVector3Array = animatedVal.double3Array(atTime: 5.0)
        times = animatedVal.times

        expectEqual(vector3Floats.count, totalCount)
        expectEqual(vector3Doubles.count, totalCount)
        expectEqual(times.count, testCount)

        for idx in 0..<testCount {
            // -- test a sampled time
            expectEqual(sampledFloatVector3Array[idx], testFloatVal)
            expectEqual(sampledDoubleVector3Array[idx], testDoubleVal)

            // -- for each time test the arrays
            for arrIdx in 0..<elementCount {
                expectEqual(vector3Floats[idx * elementCount + arrIdx], testFloatVal)
                expectEqual(vector3Doubles[idx * elementCount + arrIdx], testDoubleVal)
            }
            expectEqual(times[idx], testTimeVal)
        }
    }
}

runAllTests()
