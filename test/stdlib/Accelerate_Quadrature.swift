// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var AccelerateQuadratureTests = TestSuite("Accelerate_Quadrature")

//===----------------------------------------------------------------------===//
//
//  Quadrature Tests
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    func vectorExp(x: UnsafeBufferPointer<Double>,
                   y: UnsafeMutableBufferPointer<Double>) {
        let radius: Double = 12.5
        for i in 0 ..< x.count {
            y[i] = sqrt(radius * radius - pow(x[i] - radius, 2))
        }
    }
    
    AccelerateQuadratureTests.test("Quadrature/QNG") {
        var diameter: Double = 25
        
        // New API: Scalar
        let quadrature = Quadrature(integrator: .nonAdaptive,
                                    absoluteTolerance: 1.0e-8,
                                    relativeTolerance: 1.0e-2)
        
        let result = quadrature.integrate(over: 0.0 ... diameter) { x in
            let radius = diameter * 0.5
            return sqrt(radius * radius - pow(x - radius, 2))
        }
        
        // New API: Vectorized
        let vQuadrature = Quadrature(integrator: .nonAdaptive,
                                     absoluteTolerance: 1.0e-8,
                                     relativeTolerance: 1.0e-2)
        
        let vRresult = vQuadrature.integrate(over: 0.0 ... diameter,
                                             integrand: vectorExp)
        
        // Legacy API
        var integrateFunction: quadrature_integrate_function = {
            return quadrature_integrate_function(
                fun: { (arg: UnsafeMutableRawPointer?,
                    n: Int,
                    x: UnsafePointer<Double>,
                    y: UnsafeMutablePointer<Double>
                    ) in
                    
                    guard let diameter = arg?.load(as: Double.self) else {
                        return
                    }
                    
                    let r = diameter * 0.5
                    
                    (0 ..< n).forEach { i in
                        y[i] = sqrt(r * r - pow(x[i] - r, 2))
                    }
            },
                fun_arg: &diameter)
        }()
        
        var options = quadrature_integrate_options(integrator: QUADRATURE_INTEGRATE_QNG,
                                                   abs_tolerance: 1.0e-8,
                                                   rel_tolerance: 1.0e-2,
                                                   qag_points_per_interval: 0,
                                                   max_intervals: 0)
        
        var status = QUADRATURE_SUCCESS
        var legacyEstimatedAbsoluteError: Double = 0
        
        let legacyResult = quadrature_integrate(&integrateFunction,
                                                0.0,
                                                diameter,
                                                &options,
                                                &status,
                                                &legacyEstimatedAbsoluteError,
                                                0,
                                                nil)
        
        switch result {
        case .success(let integralResult, let estimatedAbsoluteError):
            expectEqual(integralResult, legacyResult)
            expectEqual(estimatedAbsoluteError, legacyEstimatedAbsoluteError)
            switch vRresult {
            case .success(let vIntegralResult, let vEstimatedAbsoluteError):
                expectEqual(integralResult, vIntegralResult)
                expectEqual(estimatedAbsoluteError, vEstimatedAbsoluteError)
            case .failure(_):
                expectationFailure("Vectorized non-adaptive integration failed.", trace: "",
                                   stackTrace: SourceLocStack())
            }
        case .failure( _):
            expectationFailure("Non-adaptive integration failed.", trace: "",
                               stackTrace: SourceLocStack())
        }
    }
    
    AccelerateQuadratureTests.test("Quadrature/QAGS") {
        var diameter: Double = 25
        
        // New API
        let quadrature = Quadrature(integrator: .adaptiveWithSingularities(maxIntervals: 11),
                                    absoluteTolerance: 1.0e-8,
                                    relativeTolerance: 1.0e-2)
        
        let result = quadrature.integrate(over: 0.0 ... diameter) { x in
            let radius = diameter * 0.5
            return sqrt(radius * radius - pow(x - radius, 2))
        }
        
        // New API: Vectorized
        let vQuadrature = Quadrature(integrator: .adaptiveWithSingularities(maxIntervals: 11),
                                     absoluteTolerance: 1.0e-8,
                                     relativeTolerance: 1.0e-2)
        
        let vRresult = vQuadrature.integrate(over: 0.0 ... diameter,
                                             integrand: vectorExp)
        
        // Legacy API
        var integrateFunction: quadrature_integrate_function = {
            return quadrature_integrate_function(
                fun: { (arg: UnsafeMutableRawPointer?,
                    n: Int,
                    x: UnsafePointer<Double>,
                    y: UnsafeMutablePointer<Double>
                    ) in
                    
                    guard let diameter = arg?.load(as: Double.self) else {
                        return
                    }
                    
                    let r = diameter * 0.5
                    
                    (0 ..< n).forEach { i in
                        y[i] = sqrt(r * r - pow(x[i] - r, 2))
                    }
            },
                fun_arg: &diameter)
        }()
        
        var options = quadrature_integrate_options(integrator: QUADRATURE_INTEGRATE_QAGS,
                                                   abs_tolerance: 1.0e-8,
                                                   rel_tolerance: 1.0e-2,
                                                   qag_points_per_interval: 0,
                                                   max_intervals: 11)
        
        var status = QUADRATURE_SUCCESS
        var legacyEstimatedAbsoluteError = Double(0)
        
        let legacyResult = quadrature_integrate(&integrateFunction,
                                                0,
                                                diameter,
                                                &options,
                                                &status,
                                                &legacyEstimatedAbsoluteError,
                                                0,
                                                nil)
        
        switch result {
        case .success(let integralResult, let estimatedAbsoluteError):
            expectEqual(integralResult, legacyResult)
            expectEqual(estimatedAbsoluteError, legacyEstimatedAbsoluteError)
            switch vRresult {
            case .success(let vIntegralResult, let vEstimatedAbsoluteError):
                expectEqual(integralResult, vIntegralResult)
                expectEqual(estimatedAbsoluteError, vEstimatedAbsoluteError)
            case .failure(_):
                expectationFailure("Vectorized adaptive with singularities integration failed.", trace: "",
                                   stackTrace: SourceLocStack())
            }
        case .failure( _):
            expectationFailure("Adaptive with singularities integration failed.", trace: "",
                               stackTrace: SourceLocStack())
        }
    }
    
    AccelerateQuadratureTests.test("Quadrature/QAG") {
        var diameter: Double = 25
        
        // New API
        let quadrature = Quadrature(integrator: .adaptive(pointsPerInterval: .sixtyOne,
                                                          maxIntervals: 7),
                                    absoluteTolerance: 1.0e-8,
                                    relativeTolerance: 1.0e-2)
        
        let result = quadrature.integrate(over: 0.0 ... diameter) { x in
            let radius: Double = diameter * 0.5
            return sqrt(radius * radius - pow(x - radius, 2))
        }
        
        // New API: Vectorized
        let vQuadrature = Quadrature(integrator: .adaptive(pointsPerInterval: .sixtyOne,
                                                           maxIntervals: 7),
                                     absoluteTolerance: 1.0e-8,
                                     relativeTolerance: 1.0e-2)
        
        let vRresult = vQuadrature.integrate(over: 0.0 ... diameter,
                                             integrand: vectorExp)
        
        // Legacy API
        var integrateFunction: quadrature_integrate_function = {
            return quadrature_integrate_function(
                fun: { (arg: UnsafeMutableRawPointer?,
                    n: Int,
                    x: UnsafePointer<Double>,
                    y: UnsafeMutablePointer<Double>
                    ) in
                    
                    guard let diameter = arg?.load(as: Double.self) else {
                        return
                    }
                    
                    let r = diameter * 0.5
                    
                    (0 ..< n).forEach { i in
                        y[i] = sqrt(r * r - pow(x[i] - r, 2))
                    }
            },
                fun_arg: &diameter)
        }()
        
        var options = quadrature_integrate_options(integrator: QUADRATURE_INTEGRATE_QAG,
                                                   abs_tolerance: 1.0e-8,
                                                   rel_tolerance: 1.0e-2,
                                                   qag_points_per_interval: 61,
                                                   max_intervals: 7)
        
        var status = QUADRATURE_SUCCESS
        var legacyEstimatedAbsoluteError = Double(0)
        
        let legacyResult = quadrature_integrate(&integrateFunction,
                                                0,
                                                diameter,
                                                &options,
                                                &status,
                                                &legacyEstimatedAbsoluteError,
                                                0,
                                                nil)
        
        switch result {
        case .success(let integralResult, let estimatedAbsoluteError):
            expectEqual(integralResult, legacyResult)
            expectEqual(estimatedAbsoluteError, legacyEstimatedAbsoluteError)
            switch vRresult {
            case .success(let vIntegralResult, let vEstimatedAbsoluteError):
                expectEqual(integralResult, vIntegralResult)
                expectEqual(estimatedAbsoluteError, vEstimatedAbsoluteError)
            case .failure(_):
                expectationFailure("Vectorized adaptive integration failed.", trace: "",
                                   stackTrace: SourceLocStack())
            }
        case .failure( _):
            expectationFailure("Adaptive integration failed.", trace: "",
                               stackTrace: SourceLocStack())
        }
    }
    
    AccelerateQuadratureTests.test("Quadrature/ToleranceProperties") {
        var quadrature = Quadrature(integrator: .qng,
                                    absoluteTolerance: 1,
                                    relativeTolerance: 2)
        
        expectEqual(quadrature.absoluteTolerance, 1)
        expectEqual(quadrature.relativeTolerance, 2)
        
        quadrature.absoluteTolerance = 101
        quadrature.relativeTolerance = 102
        
        expectEqual(quadrature.absoluteTolerance, 101)
        expectEqual(quadrature.relativeTolerance, 102)
    }
    
    AccelerateQuadratureTests.test("Quadrature/QAGPointsPerInterval") {
        expectEqual(Quadrature.QAGPointsPerInterval.fifteen.points, 15)
        expectEqual(Quadrature.QAGPointsPerInterval.twentyOne.points, 21)
        expectEqual(Quadrature.QAGPointsPerInterval.thirtyOne.points, 31)
        expectEqual(Quadrature.QAGPointsPerInterval.fortyOne.points, 41)
        expectEqual(Quadrature.QAGPointsPerInterval.fiftyOne.points, 51)
        expectEqual(Quadrature.QAGPointsPerInterval.sixtyOne.points, 61)
    }
    
    AccelerateQuadratureTests.test("Quadrature/ErrorDescription") {
        let a = Quadrature.Error(quadratureStatus: QUADRATURE_ERROR)
        expectEqual(a.errorDescription, "Generic error.")
        
        let b = Quadrature.Error(quadratureStatus: QUADRATURE_INVALID_ARG_ERROR)
        expectEqual(b.errorDescription, "Invalid Argument.")
        
        let c = Quadrature.Error(quadratureStatus: QUADRATURE_INTERNAL_ERROR)
        expectEqual(c.errorDescription, "This is a bug in the Quadrature code, please file a bug report.")
        
        let d = Quadrature.Error(quadratureStatus: QUADRATURE_INTEGRATE_MAX_EVAL_ERROR)
        expectEqual(d.errorDescription, "The requested accuracy limit could not be reached with the allowed number of evals/subdivisions.")
        
        let e = Quadrature.Error(quadratureStatus: QUADRATURE_INTEGRATE_BAD_BEHAVIOUR_ERROR)
        expectEqual(e.errorDescription, "Extremely bad integrand behaviour, or excessive roundoff error occurs at some points of the integration interval.")
    }
}

runAllTests()
