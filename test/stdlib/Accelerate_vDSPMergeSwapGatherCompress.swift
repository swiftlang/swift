// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPMergeSwapGatherCompressTests = TestSuite("Accelerate_vDSPMergeSwapGatherCompress")

//===----------------------------------------------------------------------===//
//
//  vDSP Merge, Swap, Gather, and Compress Tests.
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    //===----------------------------------------------------------------------===//
    // MARK: Merge
    //===----------------------------------------------------------------------===//
    
    Accelerate_vDSPMergeSwapGatherCompressTests.test("vDSP/TaperedMergeSingle") {
        let a: [Float] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        let b: [Float] = [20, 40, 60, 80, 100, 120, 140, 160, 180, 200]
        
        var result = [Float](repeating: 0, count: 10)
        vDSP.taperedMerge(a, b, result: &result)
        
        let returnedResult = vDSP.taperedMerge(a, b)
        
        var legacyResult = [Float](repeating: -1, count: 10)
        
        vDSP_vtmerg(a, 1,
                    b, 1,
                    &legacyResult, 1,
                    10)
        
        expectTrue(result.elementsEqual(returnedResult),
                   "`result` not equal `returnedResult`")
        
        expectTrue(result.elementsEqual(legacyResult),
                   "`result` not equal `legacyResult`")
    }
    
    Accelerate_vDSPMergeSwapGatherCompressTests.test("vDSP/TaperedMergeDouble") {
        let a: [Double] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        let b: [Double] = [20, 40, 60, 80, 100, 120, 140, 160, 180, 200]
        
        var result = [Double](repeating: 0, count: 10)
        vDSP.taperedMerge(a, b, result: &result)
        
        let returnedResult = vDSP.taperedMerge(a, b)
        
        var legacyResult = [Double](repeating: -1, count: 10)
        
        vDSP_vtmergD(a, 1,
                     b, 1,
                     &legacyResult, 1,
                     10)
        
        expectTrue(result.elementsEqual(returnedResult),
                   "`result` not equal `returnedResult`")
        
        expectTrue(result.elementsEqual(legacyResult),
                   "`result` not equal `legacyResult`")
    }
    
    //===----------------------------------------------------------------------===//
    // MARK: Swap
    //===----------------------------------------------------------------------===//
    
    Accelerate_vDSPMergeSwapGatherCompressTests.test("vDSP/SwapElementsSingle") {
        var a: [Float] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        var b: [Float] = [20, 40, 60, 80, 100, 120, 140, 160, 180, 200]
        
        var legacyA: [Float] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        var legacyB: [Float] = [20, 40, 60, 80, 100, 120, 140, 160, 180, 200]
        
        vDSP.swapElements(&a, &b)
        
        vDSP_vswap(&legacyA, 1,
                   &legacyB, 1,
                   10)
        
        expectTrue(a.elementsEqual(legacyA),
                   "`a` not equal `legacyA`")
        
        expectTrue(b.elementsEqual(legacyB),
                   "`a` not equal `legacyB`")
    }
    
    Accelerate_vDSPMergeSwapGatherCompressTests.test("vDSP/SwapElementsDouble") {
        var a: [Double] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        var b: [Double] = [20, 40, 60, 80, 100, 120, 140, 160, 180, 200]
        
        var legacyA: [Double] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        var legacyB: [Double] = [20, 40, 60, 80, 100, 120, 140, 160, 180, 200]
        
        vDSP.swapElements(&a, &b)
        
        vDSP_vswapD(&legacyA, 1,
                    &legacyB, 1,
                    10)
        
        expectTrue(a.elementsEqual(legacyA),
                   "`a` not equal `legacyA`")
        
        expectTrue(b.elementsEqual(legacyB),
                   "`a` not equal `legacyB`")
    }
    
    //===----------------------------------------------------------------------===//
    // MARK: Gather
    //===----------------------------------------------------------------------===//
    
    Accelerate_vDSPMergeSwapGatherCompressTests.test("vDSP/GatherSingle") {
        let vector: [Float] = [20, 40, 60, 80, 100, 120, 140, 160, 180, 200]
        let indices: [UInt] = [1, 3, 7, 10]
        
        var result = [Float](repeating: 0, count: 4)
        
        vDSP.gather(vector, indices: indices, result: &result)
        
        let returnedResult = vDSP.gather(vector, indices: indices)
        
        var legacyResult = [Float](repeating: -1, count: 4)
        
        vDSP_vgathr(vector, indices, 1, &legacyResult, 1, 4)
        
        expectTrue(result.elementsEqual(returnedResult),
                   "`result` not equal `returnedResult`")
        
        expectTrue(result.elementsEqual(legacyResult),
                   "`result` not equal `legacyResult`")
    }
    
    Accelerate_vDSPMergeSwapGatherCompressTests.test("vDSP/GatherDouble") {
        let vector: [Double] = [20, 40, 60, 80, 100, 120, 140, 160, 180, 200]
        let indices: [UInt] = [1, 3, 7, 10]
        
        var result = [Double](repeating: 0, count: 4)
        
        vDSP.gather(vector, indices: indices, result: &result)
        
        let returnedResult = vDSP.gather(vector, indices: indices)
        
        var legacyResult = [Double](repeating: -1, count: 4)
        
        vDSP_vgathrD(vector, indices, 1, &legacyResult, 1, 4)
        
        expectTrue(result.elementsEqual(returnedResult),
                   "`result` not equal `returnedResult`")
        
        expectTrue(result.elementsEqual(legacyResult),
                   "`result` not equal `legacyResult`")
    }
    
    //===----------------------------------------------------------------------===//
    // MARK: Compress
    //===----------------------------------------------------------------------===//
    
    Accelerate_vDSPMergeSwapGatherCompressTests.test("vDSP/CompressSingle") {
        let vector: [Float] =       [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120]
        let gatingVector: [Float] = [Float.leastNormalMagnitude,
                                     Float.greatestFiniteMagnitude,
                                     1,
                                     0,
                                     -1,
                                     0.001,
                                     0,
                                     Float.infinity,
                                     0.0000000,
                                     -0,
                                     9999,
                                     0,
                                     -0.00001]
        
        let nonZeroCount = gatingVector.filter { $0 != 0 }.count
        
        let xxx = vDSP.compress(vector,
                                gatingVector: gatingVector,
                                nonZeroGatingCount: nil)
        
        let yyy = vDSP.compress(vector,
                                gatingVector: gatingVector,
                                nonZeroGatingCount: nonZeroCount)
        
        var zzz = [Float](repeating: -1,
                          count: vector.count)
        
        vDSP.compress(vector,
                      gatingVector: gatingVector,
                      result: &zzz)
        
        expectTrue(xxx.elementsEqual(yyy) && xxx.elementsEqual(zzz[0 ..< nonZeroCount]))
        
        var legacyResult = [Float](repeating: -1,
                                   count: vector.count)
        
        vDSP_vcmprs(vector, 1,
                    gatingVector, 1,
                    &legacyResult, 1,
                    vDSP_Length(vector.count))
        
        expectTrue(xxx.elementsEqual(legacyResult[0 ..< nonZeroCount]))
    }
    
    Accelerate_vDSPMergeSwapGatherCompressTests.test("vDSP/CompressDouble") {
        let vector: [Double] =       [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120]
        let gatingVector: [Double] = [Double.leastNormalMagnitude,
                                      Double.greatestFiniteMagnitude,
                                      1,
                                      0,
                                      -1,
                                      0.001,
                                      0,
                                      Double.infinity,
                                      0.0000000,
                                      -0,
                                      9999,
                                      0,
                                      -0.00001]
        
        let nonZeroCount = gatingVector.filter { $0 != 0 }.count
        
        let xxx = vDSP.compress(vector,
                                gatingVector: gatingVector,
                                nonZeroGatingCount: nil)
        
        let yyy = vDSP.compress(vector,
                                gatingVector: gatingVector,
                                nonZeroGatingCount: nonZeroCount)
        
        var zzz = [Double](repeating: -1,
                           count: vector.count)
        
        vDSP.compress(vector,
                      gatingVector: gatingVector,
                      result: &zzz)
        
        expectTrue(xxx.elementsEqual(yyy) && xxx.elementsEqual(zzz[0 ..< nonZeroCount]))
        
        var legacyResult = [Double](repeating: -1,
                                    count: vector.count)
        
        vDSP_vcmprsD(vector, 1,
                     gatingVector, 1,
                     &legacyResult, 1,
                     vDSP_Length(vector.count))
        
        expectTrue(xxx.elementsEqual(legacyResult[0 ..< nonZeroCount]))
    }
}

runAllTests()
