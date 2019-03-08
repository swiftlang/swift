// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var AccelerateTests = TestSuite("Accelerate")

if #available(iOS 10.0, OSX 10.12, tvOS 10.0, watchOS 4.0, *) {
    
    AccelerateTests.test("BNNS/ImageStackDescriptor") {
        var succeed = BNNSImageStackDescriptor(width: 0, height: 0, channels: 0,
                                               row_stride: 0, image_stride: 0,
                                               data_type: .int8)
        expectEqual(succeed.data_scale, 1)
        expectEqual(succeed.data_bias, 0)
        succeed = BNNSImageStackDescriptor(width: 0, height: 0, channels: 0,
                                           row_stride: 0, image_stride: 0,
                                           data_type: .int16,
                                           data_scale: 0.5, data_bias: 0.5)
        expectEqual(succeed.data_scale, 0.5)
        expectEqual(succeed.data_bias, 0.5)
        expectCrashLater()
        //  indexed8 is not allowed as an imageStack data type.
        let _ = BNNSImageStackDescriptor(width: 0, height: 0, channels: 0,
                                         row_stride: 0, image_stride: 0,
                                         data_type: .indexed8)
    }
    
    AccelerateTests.test("BNNS/VectorDescriptor") {
        var succeed = BNNSVectorDescriptor(size: 0, data_type: .int8)
        expectEqual(succeed.data_scale, 1)
        expectEqual(succeed.data_bias, 0)
        succeed = BNNSVectorDescriptor(size: 0, data_type: .int8,
                                       data_scale: 0.5, data_bias: 0.5)
        expectEqual(succeed.data_scale, 0.5)
        expectEqual(succeed.data_bias, 0.5)
        expectCrashLater()
        //  indexed8 is not allowed as a vector data type.
        let _ = BNNSVectorDescriptor(size: 0, data_type: .indexed8)
    }
    
    AccelerateTests.test("BNNS/LayerData") {
        //  The zero layer should have data == nil.
        expectEqual(BNNSLayerData.zero.data, nil)
        var succeed = BNNSLayerData(data: nil, data_type: .int8)
        expectEqual(succeed.data_scale, 1)
        expectEqual(succeed.data_bias, 0)
        succeed = BNNSLayerData(data: nil, data_type: .int8, data_scale: 0.5,
                                data_bias: 0.5, data_table: nil)
        expectEqual(succeed.data_scale, 0.5)
        expectEqual(succeed.data_bias, 0.5)
        var table: [Float] = [1.0]
        succeed = BNNSLayerData.indexed8(data: nil, data_table: &table)
        expectCrashLater()
        // indexed8 requires a non-nil data table.
        let _ = BNNSLayerData(data: nil, data_type: .indexed8)
    }
    
    AccelerateTests.test("BNNS/Activation") {
        expectEqual(BNNSActivation.identity.function, .identity)
        let id = BNNSActivation(function: .identity)
        expectTrue(id.alpha.isNaN)
        expectTrue(id.beta.isNaN)
    }
    
}

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    AccelerateTests.test("Quadrature/QNG") {
        var diameter: Double = 25
        
        // New API
        let quadrature = Quadrature(integrator: .qng,
                                    absoluteTolerance: 1.0e-8,
                                    relativeTolerance: 1.0e-2)
        
        let result = quadrature.integrate(over: 0.0 ... diameter) { x in
            let radius = diameter * 0.5
            return sqrt(radius * radius - pow(x - radius, 2))
        }
        
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
        case .failure( _):
            expectTrue(false)
        }
    }
    
    AccelerateTests.test("Quadrature/QAGS") {
        var diameter: Double = 25
        
        // New API
        let quadrature = Quadrature(integrator: .qags(maxIntervals: 11),
                                    absoluteTolerance: 1.0e-8,
                                    relativeTolerance: 1.0e-2)
        
        let result = quadrature.integrate(over: 0.0 ... diameter) { x in
            let radius = diameter * 0.5
            return sqrt(radius * radius - pow(x - radius, 2))
        }
        
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
        case .failure( _):
            expectTrue(false)
        }
    }
    
    AccelerateTests.test("Quadrature/QAG") {
        var diameter: Double = 25
        
        // New API
        let quadrature = Quadrature(integrator: .qag(pointsPerInterval: .sixtyOne, maxIntervals: 7),
                                    absoluteTolerance: 1.0e-8,
                                    relativeTolerance: 1.0e-2)
        
        let result = quadrature.integrate(over: 0.0 ... diameter) { x in
            let radius: Double = diameter * 0.5
            return sqrt(radius * radius - pow(x - radius, 2))
        }
        
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
        case .failure( _):
            expectTrue(false)
        }
    }
    
    AccelerateTests.test("Quadrature/ToleranceProperties") {
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
}

runAllTests()
