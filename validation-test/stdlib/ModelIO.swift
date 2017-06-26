// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// UNSUPPORTED: OS=watchos
// REQUIRES: objc_interop

import StdlibUnittest

import Foundation
import ModelIO

var ModelIOTests = TestSuite("ModelIO")

if #available(OSX 10.13, iOS 11.0, tvOS 11.0, *) {
    ModelIOTests.test("MDLSkinDeformer.jointBindTransforms()") {
        let jointPaths = ["Aa", "Bb", "Cc"]
        let count = 3
        let jointTransforms = [matrix_float4x4](repeating: matrix_identity_float4x4, count: count)
        let meshBindTransform = matrix_identity_float4x4
        let skinDeformer = MDLSkinDeformer(jointPaths: jointPaths,
                                           jointBindTransforms: jointTransforms,
                                           count: count,
                                           meshBindTransform: meshBindTransform)
        let jointBindTransforms = skinDeformer.jointBindTransforms()

        expectEqual(jointBindTransforms.count, count)
        for (bindIdx, jointBindTransform) in jointBindTransforms.enumerated() {
            for idx in 0..<4 {
                expectEqual(jointBindTransform[idx].x, jointTransforms[bindIdx][idx].x)
                expectEqual(jointBindTransform[idx].y, jointTransforms[bindIdx][idx].y)
                expectEqual(jointBindTransform[idx].z, jointTransforms[bindIdx][idx].z)
                expectEqual(jointBindTransform[idx].w, jointTransforms[bindIdx][idx].w)
            }
        }
    }

    ModelIOTests.test("MDLAnimatedScalar/accessors") {
        let animatedVal = MDLAnimatedScalar()
        let testCount = 10
        let testTimeVal = 5.0
        let testFloatVal:Float = 1.0
        let testDoubleVal = Double(testFloatVal)
        let fArray = [Float](repeating: testFloatVal, count: testCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(withFloatArray: fArray, atTimes: times, count: testCount)

        let floats = animatedVal.getFloatArray()
        let doubles = animatedVal.getDoubleArray()
        times = animatedVal.getTimes()

        expectEqual(floats.count, testCount)
        expectEqual(doubles.count, testCount)
        expectEqual(times.count, testCount)

        for idx in 0..<testCount {
            expectEqual(floats[idx], testFloatVal)
            expectEqual(doubles[idx], testDoubleVal)
            expectEqual(times[idx], testTimeVal)
        }
    }

    ModelIOTests.test("MDLAnimatedScalar/accessors") {
        let animatedVal = MDLAnimatedScalar()
        let testCount = 10
        let testTimeVal = 5.0
        let testFloatVal:Float = 1.0
        let testDoubleVal = Double(testFloatVal)
        let fArray = [Float](repeating: testFloatVal, count: testCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(withFloatArray: fArray, atTimes: times, count: testCount)

        let floats = animatedVal.getFloatArray()
        let doubles = animatedVal.getDoubleArray()
        times = animatedVal.getTimes()

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
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(withFloat2Array: fArray, atTimes: times, count: testCount)

        let floats = animatedVal.getFloat2Array()
        let doubles = animatedVal.getDouble2Array()
        times = animatedVal.getTimes()

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
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(withFloat3Array: fArray, atTimes: times, count: testCount)

        let floats = animatedVal.getFloat3Array()
        let doubles = animatedVal.getDouble3Array()
        times = animatedVal.getTimes()

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
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(withFloat4Array: fArray, atTimes: times, count: testCount)

        let floats = animatedVal.getFloat4Array()
        let doubles = animatedVal.getDouble4Array()
        times = animatedVal.getTimes()

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
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(withFloat4x4Array: fArray, atTimes: times, count: testCount)

        let floats = animatedVal.getFloat4x4Array()
        let doubles = animatedVal.getDouble4x4Array()
        times = animatedVal.getTimes()

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

    ModelIOTests.test("MDLAnimatedScalarArray/accessors") {
        let elementsCount = 10
        let animatedVal = MDLAnimatedScalarArray(name: "test", elementsCount: elementsCount)
        let testCount = 10
        let totalCount = elementsCount * testCount
        let testTimeVal = 5.0
        let testFloatVal:Float = 10.0
        let testDoubleVal = Double(testFloatVal)
        let fArray = [Float](repeating: testFloatVal, count: totalCount)
        var times = [TimeInterval](repeating: testTimeVal, count: testCount)
        animatedVal.reset(with: fArray, count: totalCount, atTimes: times, count: testCount)

        let floats = animatedVal.getFloatArrays()
        let doubles = animatedVal.getDoubleArrays()
        times = animatedVal.getTimes()

        expectEqual(floats.count, totalCount)
        expectEqual(doubles.count, totalCount)
        expectEqual(times.count, testCount)

        for idx in 0..<testCount {
            for arrIdx in 0..<elementsCount {
                expectEqual(floats[idx * elementsCount + arrIdx], testFloatVal)
                expectEqual(doubles[idx * elementsCount + arrIdx], testDoubleVal)
            }
            expectEqual(times[idx], testTimeVal)
        }
    }
}

runAllTests()
