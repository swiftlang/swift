//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// MARK: Quadrature

/// A structure that approximates the definite integral of a function over a finite interval.
///
/// The following code is an example of using a `Quadrature` structure to calculate the
/// area under a curve defined by `y = sqrt(radius * radius - pow(x - radius, 2))`:
///
///
///     let quadrature = Quadrature(integrator: .qags(maxIntervals: 10),
///                                 absoluteTolerance: 1.0e-8,
///                                 relativeTolerance: 1.0e-2)
///
///     let result = quadrature.integrate(over: 0.0 ... 25.0) { x in
///         let radius: Double = 12.5
///         return sqrt(radius * radius - pow(x - radius, 2))
///     }
///
///     switch result {
///     case .success(let integralResult, let estimatedAbsoluteError):
///         print("quadrature success:", integralResult,
///                                      estimatedAbsoluteError)
///     case .failure(let error):
///         print("quadrature error:", error.errorDescription)
///     }
///
/// Alternatively, you can integrate over a function that uses vectors for its
/// source and destination. For example:
///
///     func vectorExp(x: UnsafeBufferPointer<Double>,
///                    y: UnsafeMutableBufferPointer<Double>) {
///         let radius: Double = 12.5
///         for i in 0 ..< x.count {
///             y[i] = sqrt(radius * radius - pow(x[i] - radius, 2))
///         }
///     }
///
///     let vRresult = quadrature.integrate(over: 0.0 ... diameter,
///                                         integrand: vectorExp)
@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public struct Quadrature {
    
    private var integrateOptions = quadrature_integrate_options()
    
    /// Initializes and returns a quadrature instance.
    ///
    /// - Parameter integrator: An enumeration specifying the integration algorithm and relevant properties.
    /// - Parameter absoluteTolerance: Requested absolute tolerance on the result.
    /// - Parameter relativeTolerance: Requested relative tolerance on the result.
    public init(integrator: Integrator,
                absoluteTolerance: Double = 1.0e-8,
                relativeTolerance: Double = 1.0e-2){
        
        integrateOptions.abs_tolerance = absoluteTolerance
        integrateOptions.rel_tolerance = relativeTolerance
        
        switch integrator {
        case .qng:
            integrateOptions.integrator = QUADRATURE_INTEGRATE_QNG
        case .qag(let pointsPerInterval, let maxIntervals):
            integrateOptions.integrator = QUADRATURE_INTEGRATE_QAG
            integrateOptions.qag_points_per_interval = pointsPerInterval.points
            integrateOptions.max_intervals = maxIntervals
        case .qags(let maxIntervals):
            integrateOptions.integrator = QUADRATURE_INTEGRATE_QAGS
            integrateOptions.max_intervals = maxIntervals
        }
    }
    
    /// The quadrature instance's requested absolute tolerance on the result.
    public var absoluteTolerance: Double {
        set {
            integrateOptions.abs_tolerance = newValue
        }
        get {
            return integrateOptions.abs_tolerance
        }
    }
    
    /// The quadrature instance's requested relative tolerance on the result.
    public var relativeTolerance: Double {
        set {
            integrateOptions.rel_tolerance = newValue
        }
        get {
            return integrateOptions.rel_tolerance
        }
    }
    
    /// Performs the integration over the supplied function.
    ///
    /// - Parameter interval: The lower and upper bounds of the integration interval.
    /// - Parameter integrand: The function to integrate. The input value is `x` that's within the interval over which the integrand is being integrated, and the output value is the corresponding value `y = integrand(x)` at those points.
    public func integrate(over interval: ClosedRange<Double>,
                          integrand: (_ input: UnsafeBufferPointer<Double>, _ result: UnsafeMutableBufferPointer<Double>) -> ()) ->
        Result<(integralResult: Double, estimatedAbsoluteError: Double), Error>{
            
            var status = QUADRATURE_SUCCESS
            var estimatedAbsoluteError: Double = 0
            var result: Double = 0
            
            var callback: quadrature_integrate_function!
            
            withoutActuallyEscaping(integrand) {escapableIntegrand in
                withUnsafePointer(to: escapableIntegrand) {
                    let integrandPointer = UnsafeMutableRawPointer(mutating: $0)
                    
                    callback = quadrature_integrate_function(
                        fun: { (arg: UnsafeMutableRawPointer?,
                            n: Int,
                            x: UnsafePointer<Double>,
                            y: UnsafeMutablePointer<Double>
                            ) in
                            
                            guard let integrand = arg?.load(as: ((UnsafeBufferPointer<Double>, UnsafeMutableBufferPointer<Double>) ->()).self) else {
                                return
                            }
                            
                            integrand(UnsafeBufferPointer(start: x, count: n),
                                      UnsafeMutableBufferPointer(start: y, count: n))
                    },
                        fun_arg: integrandPointer)
                    
                    withUnsafePointer(to: self.integrateOptions) { options in
                        result = quadrature_integrate(&callback,
                                                      interval.lowerBound,
                                                      interval.upperBound,
                                                      options,
                                                      &status,
                                                      &estimatedAbsoluteError,
                                                      0,
                                                      nil)
                    }
                }
            }
            
            if status == QUADRATURE_SUCCESS {
                return .success((integralResult: result,
                                 estimatedAbsoluteError: estimatedAbsoluteError))
            } else {
                return .failure(Error(quadratureStatus: status))
            }
    }
    
    /// Performs the integration over the supplied function.
    ///
    /// - Parameter interval: The lower and upper bounds of the integration interval.
    /// - Parameter integrand: The function to integrate. The input value is `x` that's within the interval over which the integrand is being integrated, and the output value is the corresponding value `y = integrand(x)` at those points.
    public func integrate(over interval: ClosedRange<Double>,
                          integrand: (Double) -> Double) ->
        Result<(integralResult: Double, estimatedAbsoluteError: Double), Error> {
            
            var status = QUADRATURE_SUCCESS
            var estimatedAbsoluteError: Double = 0
            var result: Double = 0
            
            var callback: quadrature_integrate_function!
            
            withoutActuallyEscaping(integrand) {escapableIntegrand in
                withUnsafePointer(to: escapableIntegrand) {
                    let integrandPointer = UnsafeMutableRawPointer(mutating: $0)
                    
                    callback = quadrature_integrate_function(
                        fun: { (arg: UnsafeMutableRawPointer?,
                            n: Int,
                            x: UnsafePointer<Double>,
                            y: UnsafeMutablePointer<Double>
                            ) in
                            
                            guard let integrand = arg?.load(as: ((Double) -> Double).self) else {
                                return
                            }
                            
                            (0 ..< n).forEach { i in
                                y[i] = integrand(x[i])
                            }
                    },
                        fun_arg: integrandPointer)
                    
                    withUnsafePointer(to: self.integrateOptions) { options in
                        result = quadrature_integrate(&callback,
                                                      interval.lowerBound,
                                                      interval.upperBound,
                                                      options,
                                                      &status,
                                                      &estimatedAbsoluteError,
                                                      0,
                                                      nil)
                    }
                }
            }
            
            if status == QUADRATURE_SUCCESS {
                return .success((integralResult: result,
                                 estimatedAbsoluteError: estimatedAbsoluteError))
            } else {
                return .failure(Error(quadratureStatus: status))
            }
    }
    
    public enum Integrator {
        /// Simple non-adaptive automatic integrator using Gauss-Kronrod-Patterson quadrature coefficients.
        /// Evaluates 21, or 43, or 87 points in the interval until the requested accuracy is reached.
        case qng
        
        /// Simple non-adaptive automatic integrator using Gauss-Kronrod-Patterson quadrature coefficients.
        /// Evaluates 21, or 43, or 87 points in the interval until the requested accuracy is reached.
        public static let nonAdaptive = Integrator.qng
        
        /// Simple globally adaptive integrator.
        /// Allows selection of the number of Gauss-Kronrod points used in each subinterval, and the max number of subintervals.
        case qag(pointsPerInterval: QAGPointsPerInterval, maxIntervals: Int)
        
        /// Simple globally adaptive integrator.
        /// Allows selection of the number of Gauss-Kronrod points used in each subinterval, and the max number of subintervals.
        public static func adaptive(pointsPerInterval: QAGPointsPerInterval, maxIntervals: Int) -> Integrator  {
            return Integrator.qag(pointsPerInterval: pointsPerInterval, maxIntervals: maxIntervals)
        }
        
        /// Global adaptive quadrature based on 21-point or 15-point (if at least one bound is infinite) Gauss–Kronrod quadrature within each subinterval, with acceleration by Peter Wynn's epsilon algorithm.
        /// If at least one of the interval bounds is infinite, this is equivalent to the QUADPACK QAGI routine. Otherwise, this is equivalent to the QUADPACK QAGS routine.
        case qags(maxIntervals: Int)
        
        /// Global adaptive quadrature based on 21-point or 15-point (if at least one bound is infinite) Gauss–Kronrod quadrature within each subinterval, with acceleration by Peter Wynn's epsilon algorithm.
        /// If at least one of the interval bounds is infinite, this is equivalent to the QUADPACK QAGI routine. Otherwise, this is equivalent to the QUADPACK QAGS routine.
        public static func adaptiveWithSingularities(maxIntervals: Int) -> Integrator  {
            return Integrator.qags(maxIntervals: maxIntervals)
        }
    }
    
    public struct QAGPointsPerInterval {
        public let points: Int
        private init(points: Int) { self.points = points }
        
        public static let fifteen = QAGPointsPerInterval(points: 15)
        public static let twentyOne = QAGPointsPerInterval(points: 21)
        public static let thirtyOne = QAGPointsPerInterval(points: 31)
        public static let fortyOne = QAGPointsPerInterval(points: 41)
        public static let fiftyOne = QAGPointsPerInterval(points: 51)
        public static let sixtyOne = QAGPointsPerInterval(points: 61)
    }
    
    public enum Error: Swift.Error {
        case generic
        case invalidArgument
        case `internal`
        case integrateMaxEval
        case badIntegrandBehaviour
        
        public init(quadratureStatus: quadrature_status) {
            switch quadratureStatus {
            case QUADRATURE_ERROR:
                self = .generic
            case QUADRATURE_INVALID_ARG_ERROR:
                self = .invalidArgument
            case QUADRATURE_INTERNAL_ERROR:
                self = .internal
            case QUADRATURE_INTEGRATE_MAX_EVAL_ERROR:
                self = .integrateMaxEval
            case QUADRATURE_INTEGRATE_BAD_BEHAVIOUR_ERROR:
                self = .badIntegrandBehaviour
            default:
                self = .internal
            }
        }
        
        public var errorDescription: String {
            switch self {
            case .generic:
                return "Generic error."
            case .invalidArgument:
                return "Invalid Argument."
            case .internal:
                return "This is a bug in the Quadrature code, please file a bug report."
            case .integrateMaxEval:
                return "The requested accuracy limit could not be reached with the allowed number of evals/subdivisions."
            case .badIntegrandBehaviour:
                return "Extremely bad integrand behaviour, or excessive roundoff error occurs at some points of the integration interval."
            }
        }
    }
}
