// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPElementwiseArithmeticTests = TestSuite("Accelerate_vDSPElementwiseArithmetic")

//===----------------------------------------------------------------------===//
//
//  vDSP Single Precision Arithmetic
//
//===----------------------------------------------------------------------===//

func lround(_ x: Float) -> Int {
    return lround(Double(x))
}

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    var result = [Float.nan]
    
    var vectorA = [Float.nan]
    var vectorB = [Float.nan]
    var vectorC = [Float.nan]
    
    var scalarA = Float.nan
    var scalarB = Float.nan
    
    // c[i] = a[i] + b
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsadd") {
        scalarA = 100
        vectorB = [500]
        
        vDSP.add(scalarA,
                 vectorB,
                 result: &result)
        
        let returnedResult = vDSP.add(scalarA,
                                      vectorB)
        
        expectEqual(lround(result.first!), 600)
        expectEqual(lround(returnedResult.first!), 600)
    }
    
    // c[i] = a[i] + b[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vadd") {
        vectorA = [750]
        vectorB = [250]
        
        vDSP.add(vectorA,
                 vectorB,
                 result: &result)
        
        let returnedResult = vDSP.add(vectorA,
                                      vectorB)
        
        expectEqual(lround(result.first!), 1000)
        expectEqual(lround(returnedResult.first!), 1000)
    }
    
    // c[i] = a[i] - b[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsub") {
        vectorA = [5]
        vectorB = [30]
        
        vDSP.subtract(vectorA,
                      vectorB,
                      result: &result)
        
        let returnedResult = vDSP.subtract(vectorA,
                                           vectorB)
        
        expectEqual(lround(result.first!), -25)
        expectEqual(lround(returnedResult.first!), -25)
    }
    
    // c[i] = a[i] * b
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsmul") {
        scalarA = 100
        vectorB = [3]
        
        vDSP.multiply(scalarA,
                      vectorB,
                      result: &result)
        
        let returnedResult = vDSP.multiply(scalarA,
                                           vectorB)
        
        expectEqual(lround(result.first!), 300)
        expectEqual(lround(returnedResult.first!), 300)
    }
    
    // a[i] * b[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmul") {
        vectorA = [6]
        vectorB = [10]
        
        vDSP.multiply(vectorA,
                      vectorB,
                      result: &result)
        
        let returnedResult = vDSP.multiply(vectorA,
                                           vectorB)
        
        expectEqual(lround(result.first!), 60)
        expectEqual(lround(returnedResult.first!), 60)
    }
    
    // c[i] = a[i] / b
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsdiv") {
        scalarA = 4
        vectorB = [100]
        
        vDSP.divide(vectorB,
                    scalarA,
                    result: &result)
        
        let returnedResult = vDSP.divide(vectorB,
                                         scalarA)
        
        expectEqual(lround(result.first!), 25)
        expectEqual(lround(returnedResult.first!), 25)
    }
    
    // c[i] = a / b[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/svdiv") {
        scalarA = 200
        vectorB = [4]
        
        vDSP.divide(scalarA,
                    vectorB,
                    result: &result)
        
        let returnedResult = vDSP.divide(scalarA,
                                         vectorB)
        
        expectEqual(lround(result.first!), 50)
        expectEqual(lround(returnedResult.first!), 50)
    }
    
    // c[i] = a[i] / b[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vdiv") {
        vectorA = [600]
        vectorB = [3]
        
        vDSP.divide(vectorA,
                    vectorB,
                    result: &result)
        
        let returnedResult = vDSP.divide(vectorA,
                                         vectorB)
        
        expectEqual(lround(result.first!), 200)
        expectEqual(lround(returnedResult.first!), 200)
    }
    
    // o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vaddsub") {
        vectorA = [16]
        vectorB = [4]
        var subtractResult = [ Float.nan ]
        
        vDSP.addSubtract(vectorA,
                         vectorB,
                         addResult: &result,
                         subtractResult: &subtractResult)
        
        expectEqual(lround(result.first!), 20)
        expectEqual(lround(subtractResult.first!), 12)
        
    }
    
    // d[i] = (a[i] + b[i]) * c
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vasm") {
        vectorA = [4]
        vectorB = [16]
        scalarA = 4
        
        vDSP.multiply(addition: (vectorA, vectorB),
                      scalarA,
                      result: &result)
        
        let returnedResult = vDSP.multiply(addition: (vectorA, vectorB),
                                           scalarA)
        
        expectEqual(lround(result.first!), 80)
        expectEqual(lround(returnedResult.first!), 80)
    }
    
    // d[i] = (a[i] + b[i]) * c[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vam") {
        vectorA = [4]
        vectorB = [16]
        vectorC = [5]
        
        vDSP.multiply(addition: (vectorA, vectorB),
                      vectorC,
                      result: &result)
        
        let returnedResult = vDSP.multiply(addition: (vectorA, vectorB),
                                           vectorC)
        
        expectEqual(lround(result.first!), 100)
        expectEqual(lround(returnedResult.first!), 100)
    }
    
    // d[i] = (a[i] - b[i]) * c
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsbsm") {
        vectorA = [20]
        vectorB = [5]
        scalarA = 2
        
        vDSP.multiply(subtraction: (vectorA, vectorB),
                      scalarA,
                      result: &result)
        
        let returnedResult = vDSP.multiply(subtraction: (vectorA, vectorB),
                                           scalarA)
        
        expectEqual(lround(result.first!), 30)
        expectEqual(lround(returnedResult.first!), 30)
    }
    
    // d[i] = (a[i] - b[i]) * c[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsbm") {
        vectorA = [20]
        vectorB = [5]
        vectorC = [2]
        
        vDSP.multiply(subtraction: (vectorA, vectorB),
                      vectorC,
                      result: &result)
        
        let returnedResult = vDSP.multiply(subtraction: (vectorA, vectorB),
                                           vectorC)
        
        expectEqual(lround(result.first!), 30)
        expectEqual(lround(returnedResult.first!), 30)
    }
    
    // a[i]*b[i] + c
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmsa") {
        vectorA = [18]
        vectorB = [10]
        scalarA = 20
        
        vDSP.add(multiplication: (vectorA, vectorB),
                 scalarA,
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, vectorB),
                                      scalarA)
        
        expectEqual(lround(result.first!), 200)
        expectEqual(lround(returnedResult.first!), 200)
    }
    
    // (a[i] * b) + c[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsma") {
        vectorA = [18]
        scalarB = 10
        vectorC = [120]
        
        vDSP.add(multiplication: (vectorA, scalarB),
                 vectorC,
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, scalarB),
                                      vectorC)
        
        expectEqual(lround(result.first!), 300)
        expectEqual(lround(returnedResult.first!), 300)
    }
    
    // d[i] = (a[i] * b[i]) + c[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vma") {
        vectorA = [5]
        vectorB = [20]
        vectorC = [6]
        
        vDSP.add(multiplication: (vectorA, vectorB),
                 vectorC,
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, vectorB),
                                      vectorC)
        
        expectEqual(lround(result.first!), 106)
        expectEqual(lround(returnedResult.first!), 106)
    }
    
    // d[i] = (a[i] * b[i]) - c[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmsb") {
        vectorA = [25]
        vectorB = [2]
        vectorC = [50]
        
        vDSP.subtract(multiplication: (vectorB, vectorC),
                      vectorA,
                      result: &result)
        
        let returnedResult = vDSP.subtract(multiplication: (vectorB, vectorC),
                                           vectorA)
        
        expectEqual(lround(result.first!), 75)
        expectEqual(lround(returnedResult.first!), 75)
    }
    
    // e[i] = (a[i] * b) + (c[i] * d)
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsmsma") {
        vectorA = [5]
        scalarA = 100
        vectorB = [50]
        scalarB = 4
        
        vDSP.add(multiplication: (vectorA, scalarA),
                 multiplication: (vectorB, scalarB),
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, scalarA),
                                      multiplication: (vectorB, scalarB))
        
        expectEqual(lround(result.first!), 700)
        expectEqual(lround(returnedResult.first!), 700)
    }
    
    // E[n] = A[n]*B[n] + C[n]*D[n]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmma") {
        vectorA = [2]
        vectorB = [10]
        vectorC = [3]
        let vectorD: [Float] = [20]
        
        vDSP.add(multiplication: (vectorA, vectorB),
                 multiplication: (vectorC, vectorD),
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, vectorB),
                                      multiplication: (vectorC, vectorD))
        
        expectEqual(lround(result.first!), 80)
        expectEqual(lround(returnedResult.first!), 80)
    }
    
    // e[i] = (a[i] + b[i]) * (c[i] + d[i])
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vaam") {
        vectorA = [2]
        vectorB = [8]
        vectorC = [3]
        let vectorD: [Float] = [17]
        
        vDSP.multiply(addition: (vectorA, vectorB),
                      addition: (vectorC, vectorD),
                      result: &result)
        
        let returnedResult = vDSP.multiply(addition: (vectorA, vectorB),
                                           addition: (vectorC, vectorD))
        
        expectEqual(lround(result.first!), 200)
        expectEqual(lround(returnedResult.first!), 200)
    }
    
    // e[i] = (a[i] * b[i]) - (c[i] * d[i])
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmmsb") {
        vectorA = [2]
        vectorB = [3]
        vectorC = [5]
        let vectorD: [Float] = [15]
        
        vDSP.subtract(multiplication: (vectorA, vectorB),
                      multiplication: (vectorC, vectorD),
                      result: &result)
        
        let returnedResult = vDSP.subtract(multiplication: (vectorA, vectorB),
                                           multiplication: (vectorC, vectorD))
        
        expectEqual(lround(result.first!), -69)
        expectEqual(lround(returnedResult.first!), -69)
    }
    
    // e[i] = (a[i] - b[i]) * (c[i] - d[i])
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsbsbm") {
        vectorA = [12]
        vectorB = [2]
        vectorC = [20]
        let vectorD: [Float] = [3]
        
        vDSP.multiply(subtraction: (vectorA, vectorB),
                      subtraction: (vectorC, vectorD),
                      result: &result)
        
        let returnedResult = vDSP.multiply(subtraction: (vectorA, vectorB),
                                           subtraction: (vectorC, vectorD))
        
        expectEqual(lround(result.first!), 170)
        expectEqual(lround(returnedResult.first!), 170)
    }
    
    // e[i] = (a[i] + b[i]) * (c[i] - d[i])
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vasbm") {
        vectorA = [16]
        vectorB = [4]
        vectorC = [102]
        let vectorD: [Float] = [2]
        
        vDSP.multiply(addition: (vectorA, vectorB),
                      subtraction: (vectorC, vectorD),
                      result: &result)
        
        let returnedResult = vDSP.multiply(addition: (vectorA, vectorB),
                                           subtraction: (vectorC, vectorD))
        
        expectEqual(lround(result.first!), 2000)
        expectEqual(lround(returnedResult.first!), 2000)
    }
    
    // d[n] = a[n]*b + c
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsmsa") {
        vectorA = [12]
        scalarA = 2
        scalarB = 6
        
        vDSP.add(multiplication: (vectorA, scalarA),
                 scalarB,
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, scalarA),
                                      scalarB)
        
        expectEqual(lround(result.first!), 30)
        expectEqual(lround(returnedResult.first!), 30)
    }
    
    // D[n] = A[n]*B - C[n];
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsmsb") {
        vectorA = [10]
        scalarB = 5
        vectorC = [25]
        
        vDSP.subtract(multiplication: (vectorA, scalarB),
                      vectorC,
                      result: &result)
        
        let returnedResult = vDSP.subtract(multiplication: (vectorA, scalarB),
                                           vectorC)
        
        expectEqual(lround(result.first!), 25)
        expectEqual(lround(returnedResult.first!), 25)
    }
}


//===----------------------------------------------------------------------===//
//
//  vDSP Double Precision Arithmetic
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    var result = [Double.nan]
    
    var vectorA = [Double.nan]
    var vectorB = [Double.nan]
    var vectorC = [Double.nan]
    
    var scalarA = Double.nan
    var scalarB = Double.nan
    
    // c[i] = a[i] + b
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsaddD") {
        scalarA = 100
        vectorB = [500]
        
        vDSP.add(scalarA,
                 vectorB,
                 result: &result)
        
        let returnedResult = vDSP.add(scalarA,
                                      vectorB)
        
        expectEqual(lround(result.first!), 600)
        expectEqual(lround(returnedResult.first!), 600)
    }
    
    // c[i] = a[i] + b[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vaddD") {
        vectorA = [750]
        vectorB = [250]
        
        vDSP.add(vectorA,
                 vectorB,
                 result: &result)
        
        let returnedResult = vDSP.add(vectorA,
                                      vectorB)
        
        expectEqual(lround(result.first!), 1000)
        expectEqual(lround(returnedResult.first!), 1000)
    }
    
    // c[i] = a[i] - b[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsubD") {
        vectorA = [5]
        vectorB = [30]
        
        vDSP.subtract(vectorA,
                      vectorB,
                      result: &result)
        
        let returnedResult = vDSP.subtract(vectorA,
                                            vectorB)
        
        expectEqual(lround(result.first!), -25)
        expectEqual(lround(returnedResult.first!), -25)
    }
    
    // c[i] = a[i] * b
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsmulD") {
        scalarA = 100
        vectorB = [3]
        
        vDSP.multiply(scalarA,
                      vectorB,
                      result: &result)
        
        let returnedResult = vDSP.multiply(scalarA,
                                           vectorB)
        
        expectEqual(lround(result.first!), 300)
        expectEqual(lround(returnedResult.first!), 300)
    }
    
    // a[i] * b[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmulD") {
        vectorA = [6]
        vectorB = [10]
        
        vDSP.multiply(vectorA,
                      vectorB,
                      result: &result)
        
        let returnedResult = vDSP.multiply(vectorA,
                                           vectorB)
        
        expectEqual(lround(result.first!), 60)
        expectEqual(lround(returnedResult.first!), 60)
    }
    
    // c[i] = a[i] / b
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsdivD") {
        scalarA = 4
        vectorB = [100]
        
        vDSP.divide(vectorB,
                    scalarA,
                    result: &result)
        
        let returnedResult = vDSP.divide(vectorB,
                                         scalarA)
        
        expectEqual(lround(result.first!), 25)
        expectEqual(lround(returnedResult.first!), 25)
    }
    
    // c[i] = a / b[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/svdivD") {
        scalarA = 200
        vectorB = [4]
        
        vDSP.divide(scalarA,
                    vectorB,
                    result: &result)
        
        let returnedResult = vDSP.divide(scalarA,
                                         vectorB)
        
        expectEqual(lround(result.first!), 50)
        expectEqual(lround(returnedResult.first!), 50)
    }
    
    // c[i] = a[i] / b[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vdivD") {
        vectorA = [600]
        vectorB = [3]
        
        vDSP.divide(vectorA,
                    vectorB,
                    result: &result)
        
        let returnedResult = vDSP.divide(vectorA,
                                         vectorB)
        
        expectEqual(lround(result.first!), 200)
        expectEqual(lround(returnedResult.first!), 200)
    }
    
    // o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vaddsubD") {
        vectorA = [16]
        vectorB = [4]
        var subtractResult = [ Double.nan ]
        
        vDSP.addSubtract(vectorA,
                         vectorB,
                         addResult: &result,
                         subtractResult: &subtractResult)
        
        expectEqual(lround(result.first!), 20)
        expectEqual(lround(subtractResult.first!), 12)
    }
    
    // d[i] = (a[i] + b[i]) * c
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vasmD") {
        vectorA = [4]
        vectorB = [16]
        scalarA = 4
        
        vDSP.multiply(addition: (vectorA, vectorB),
                      scalarA,
                      result: &result)
        
        let returnedResult = vDSP.multiply(addition: (vectorA, vectorB),
                                           scalarA)
        
        expectEqual(lround(result.first!), 80)
        expectEqual(lround(returnedResult.first!), 80)
    }
    
    // d[i] = (a[i] + b[i]) * c[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vamD") {
        vectorA = [4]
        vectorB = [16]
        vectorC = [5]
        
        vDSP.multiply(addition: (vectorA, vectorB),
                      vectorC,
                      result: &result)
        
        let returnedResult = vDSP.multiply(addition: (vectorA, vectorB),
                                           vectorC)
        
        expectEqual(lround(result.first!), 100)
        expectEqual(lround(returnedResult.first!), 100)
    }
    
    // d[i] = (a[i] - b[i]) * c
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsbsmD") {
        vectorA = [20]
        vectorB = [5]
        scalarA = 2
        
        vDSP.multiply(subtraction: (vectorA, vectorB),
                      scalarA,
                      result: &result)
        
        let returnedResult = vDSP.multiply(subtraction: (vectorA, vectorB),
                                           scalarA)
        
        expectEqual(lround(result.first!), 30)
        expectEqual(lround(returnedResult.first!), 30)
    }
    
    // d[i] = (a[i] - b[i]) * c[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsbmD") {
        vectorA = [20]
        vectorB = [5]
        vectorC = [2]
        
        vDSP.multiply(subtraction: (vectorA, vectorB),
                      vectorC,
                      result: &result)
        
        let returnedResult = vDSP.multiply(subtraction: (vectorA, vectorB),
                                           vectorC)
        
        expectEqual(lround(result.first!), 30)
        expectEqual(lround(returnedResult.first!), 30)
    }
    
    // a[i]*b[i] + c
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmsaD") {
        vectorA = [18]
        vectorB = [10]
        scalarA = 20
        
        vDSP.add(multiplication: (vectorA, vectorB),
                 scalarA,
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, vectorB),
                                      scalarA)
        
        expectEqual(lround(result.first!), 200)
        expectEqual(lround(returnedResult.first!), 200)
    }
    
    // (a[i] * b) + c[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsmaD") {
        vectorA = [18]
        scalarB = 10
        vectorC = [120]
        
        vDSP.add(multiplication: (vectorA, scalarB),
                 vectorC,
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, scalarB),
                                      vectorC)
        
        expectEqual(lround(result.first!), 300)
        expectEqual(lround(returnedResult.first!), 300)
    }
    
    // d[i] = (a[i] * b[i]) + c[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmaD") {
        vectorA = [5]
        vectorB = [20]
        vectorC = [6]
        
        vDSP.add(multiplication: (vectorA, vectorB),
                 vectorC,
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, vectorB),
                                      vectorC)
        
        expectEqual(lround(result.first!), 106)
        expectEqual(lround(returnedResult.first!), 106)
    }
    
    // d[i] = (a[i] * b[i]) - c[i]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmsbD") {
        vectorA = [25]
        vectorB = [2]
        vectorC = [50]
        
        vDSP.subtract(multiplication: (vectorB, vectorC),
                      vectorA,
                      result: &result)
        
        let returnedResult = vDSP.subtract(multiplication: (vectorB, vectorC),
                                           vectorA)
        
        expectEqual(lround(result.first!), 75)
        expectEqual(lround(returnedResult.first!), 75)
    }
    
    // e[i] = (a[i] * b) + (c[i] * d)
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsmsmaD") {
        vectorA = [5]
        scalarA = 100
        vectorB = [50]
        scalarB = 4
        
        vDSP.add(multiplication: (vectorA, scalarA),
                 multiplication: (vectorB, scalarB),
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, scalarA),
                                      multiplication: (vectorB, scalarB))
        
        expectEqual(lround(result.first!), 700)
        expectEqual(lround(returnedResult.first!), 700)
    }
    
    // E[n] = A[n]*B[n] + C[n]*D[n]
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmmaD") {
        vectorA = [2]
        vectorB = [10]
        vectorC = [3]
        let vectorD: [Double] = [20]
        
        vDSP.add(multiplication: (vectorA, vectorB),
                 multiplication: (vectorC, vectorD),
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, vectorB),
                                      multiplication: (vectorC, vectorD))
        
        expectEqual(lround(result.first!), 80)
        expectEqual(lround(returnedResult.first!), 80)
    }
    
    // e[i] = (a[i] + b[i]) * (c[i] + d[i])
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vaamD") {
        vectorA = [2]
        vectorB = [8]
        vectorC = [3]
        let vectorD: [Double] = [17]
        
        vDSP.multiply(addition: (vectorA, vectorB),
                      addition: (vectorC, vectorD),
                      result: &result)
        
        let returnedResult = vDSP.multiply(addition: (vectorA, vectorB),
                                           addition: (vectorC, vectorD))
        
        expectEqual(lround(result.first!), 200)
        expectEqual(lround(returnedResult.first!), 200)
    }
    
    // e[i] = (a[i] * b[i]) - (c[i] * d[i])
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vmmsbD") {
        vectorA = [2]
        vectorB = [3]
        vectorC = [5]
        let vectorD: [Double] = [15]
        
        vDSP.subtract(multiplication: (vectorA, vectorB),
                      multiplication: (vectorC, vectorD),
                      result: &result)
        
        let returnedResult = vDSP.subtract(multiplication: (vectorA, vectorB),
                                           multiplication: (vectorC, vectorD))
        
        expectEqual(lround(result.first!), -69)
        expectEqual(lround(returnedResult.first!), -69)
    }
    
    // e[i] = (a[i] - b[i]) * (c[i] - d[i])
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsbsbmD") {
        vectorA = [12]
        vectorB = [2]
        vectorC = [20]
        let vectorD: [Double] = [3]
        
        vDSP.multiply(subtraction: (vectorA, vectorB),
                      subtraction: (vectorC, vectorD),
                      result: &result)
        
        let returnedResult = vDSP.multiply(subtraction: (vectorA, vectorB),
                                           subtraction: (vectorC, vectorD))
        
        expectEqual(lround(result.first!), 170)
        expectEqual(lround(returnedResult.first!), 170)
    }
    
    // e[i] = (a[i] + b[i]) * (c[i] - d[i])
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vasbmD") {
        vectorA = [16]
        vectorB = [4]
        vectorC = [102]
        let vectorD: [Double] = [2]
        
        vDSP.multiply(addition: (vectorA, vectorB),
                      subtraction: (vectorC, vectorD),
                      result: &result)
        
        let returnedResult = vDSP.multiply(addition: (vectorA, vectorB),
                                           subtraction: (vectorC, vectorD))
        
        expectEqual(lround(result.first!), 2000)
        expectEqual(lround(returnedResult.first!), 2000)
    }
    
    // d[n] = a[n]*b + c
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsmsaD") {
        vectorA = [12]
        scalarA = 2
        scalarB = 6
        
        vDSP.add(multiplication: (vectorA, scalarA),
                 scalarB,
                 result: &result)
        
        let returnedResult = vDSP.add(multiplication: (vectorA, scalarA),
                                      scalarB)
        
        expectEqual(lround(result.first!), 30)
        expectEqual(lround(returnedResult.first!), 30)
    }
    
    // D[n] = A[n]*B - C[n];
    Accelerate_vDSPElementwiseArithmeticTests.test("vDSP/vsmsbD") {
        vectorA = [10]
        scalarB = 5
        vectorC = [25]
        
        vDSP.subtract(multiplication: (vectorA, scalarB),
                      vectorC,
                      result: &result)
        
        let returnedResult = vDSP.subtract(multiplication: (vectorA, scalarB),
                                           vectorC)
        
        expectEqual(lround(result.first!), 25)
        expectEqual(lround(returnedResult.first!), 25)
    }
}


runAllTests()
