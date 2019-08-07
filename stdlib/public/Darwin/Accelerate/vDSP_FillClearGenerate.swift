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

/// Types that support vectorized window generation.
@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public protocol vDSP_FloatingPointGeneratable: BinaryFloatingPoint {
}
extension Float: vDSP_FloatingPointGeneratable {}
extension Double: vDSP_FloatingPointGeneratable {}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    /// Fill vector with specified scalar value, single-precision.
    ///
    /// - Parameter vector: The vector to fill.
    /// - Parameter value: The fill value.
    @inlinable
    public static func fill<V>(_ vector: inout V,
                               with value: Float)
        where V: AccelerateMutableBuffer,
        V.Element == Float {
            
            let n = vDSP_Length(vector.count)
            
            vector.withUnsafeMutableBufferPointer { v in
                withUnsafePointer(to: value) {
                    vDSP_vfill($0,
                               v.baseAddress!, 1,
                               n)
                }
            }
    }
    
    /// Fill vector with specified scalar value, double-precision.
    ///
    /// - Parameter vector: The vector to fill.
    /// - Parameter value: The fill value.
    @inlinable
    public static func fill<V>(_ vector: inout V,
                               with value: Double)
        where V: AccelerateMutableBuffer,
        V.Element == Double {
            
            let n = vDSP_Length(vector.count)
            
            vector.withUnsafeMutableBufferPointer { v in
                withUnsafePointer(to: value) {
                    vDSP_vfillD($0,
                                v.baseAddress!, 1,
                                n)
                }
            }
    }
    
    /// Fill vector with zeros, single-precision.
    ///
    /// - Parameter vector: The vector to fill.
    @inlinable
    public static func clear<V>(_ vector: inout V)
        where V: AccelerateMutableBuffer,
        V.Element == Float {
            
            let n = vDSP_Length(vector.count)
            
            vector.withUnsafeMutableBufferPointer { v in
                vDSP_vclr(v.baseAddress!, 1,
                          n)
            }
    }
    
    /// Fill vector with zeros, double-precision.
    ///
    /// - Parameter vector: The vector to fill.
    @inlinable
    public static func clear<V>(_ vector: inout V)
        where V: AccelerateMutableBuffer,
        V.Element == Double {
            
            let n = vDSP_Length(vector.count)
            
            vector.withUnsafeMutableBufferPointer { v in
                vDSP_vclrD(v.baseAddress!, 1,
                           n)
            }
    }
    
    /// Enum specifying window sequence.
    public enum WindowSequence {
        /// Creates a normalized Hanning window.
        case hanningNormalized
        
        /// Creates a denormalized Hanning window.
        case hanningDenormalized
        
        /// Creates a Hamming window.
        case hamming
        
        /// Creates a Blackman window.
        case blackman
    }
    
    /// Creates an array containing the specified window.
    ///
    /// - Parameter ofType: Specifies single- or double-precision.
    /// - Parameter sequence: Specifies the window sequence.
    /// - Parameter count: The number of elements in the array.
    /// - Parameter isHalfWindow: When true, creates a window with only the first `(N+1)/2` points.
    /// - Returns: An array containing the specified window.
    @inlinable
    public static func window<T: vDSP_FloatingPointGeneratable>(ofType: T.Type,
                                                                usingSequence sequence: WindowSequence,
                                                                count: Int,
                                                                isHalfWindow: Bool) -> [T] {
        
        precondition(count > 0)
        
        if T.self == Float.self {
            
            let result = Array<Float>(unsafeUninitializedCapacity: count) {
                buffer, initializedCount in
                
                formWindow(usingSequence: sequence,
                           result: &buffer,
                           isHalfWindow: isHalfWindow)
                
                initializedCount = count
            }
            return result as! [T]
            
        } else if T.self == Double.self {
            
            let result = Array<Double>(unsafeUninitializedCapacity: count) {
                buffer, initializedCount in
                
                formWindow(usingSequence: sequence,
                           result: &buffer,
                           isHalfWindow: isHalfWindow)
                
                initializedCount = count
            }
            return result as! [T]
            
        } else {
            fatalError("This operation only supports `Float` and `Double` types.")
        }
    }
    
    /// Fills a supplied array with the specified window, single-precision.
    ///
    /// - Parameter sequence: Specifies the window sequence.
    /// - Parameter result: Output values.
    /// - Parameter isHalfWindow: When true, creates a window with only the first `(N+1)/2` points.
    public static func formWindow<V>(usingSequence sequence: WindowSequence,
                                     result: inout V,
                                     isHalfWindow: Bool)
        where V: AccelerateMutableBuffer,
        V.Element == Float {
            
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { v in
                switch sequence {
                case .hanningNormalized:
                    vDSP_hann_window(v.baseAddress!,
                                     n,
                                     Int32(vDSP_HANN_NORM) |
                                        Int32(isHalfWindow ?  vDSP_HALF_WINDOW : 0))
                case .hanningDenormalized:
                    vDSP_hann_window(v.baseAddress!,
                                     n,
                                     Int32(vDSP_HANN_DENORM) |
                                        Int32(isHalfWindow ?  vDSP_HALF_WINDOW : 0))
                case .hamming:
                    vDSP_hamm_window(v.baseAddress!,
                                     n,
                                     Int32(isHalfWindow ?  vDSP_HALF_WINDOW : 0))
                case .blackman:
                    vDSP_blkman_window(v.baseAddress!,
                                       n,
                                       Int32(isHalfWindow ?  vDSP_HALF_WINDOW : 0))
                }
            }
    }
    
    /// Fills a supplied array with the specified window, double-precision.
    ///
    /// - Parameter sequence: Specifies the window sequence.
    /// - Parameter result: Output values.
    /// - Parameter isHalfWindow: When true, creates a window with only the first `(N+1)/2` points.
    public static func formWindow<V>(usingSequence sequence: WindowSequence,
                                     result: inout V,
                                     isHalfWindow: Bool)
        where V: AccelerateMutableBuffer,
        V.Element == Double {
            
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { v in
                switch sequence {
                case .hanningNormalized:
                    vDSP_hann_windowD(v.baseAddress!,
                                      n,
                                      Int32(vDSP_HANN_NORM) |
                                        Int32(isHalfWindow ?  vDSP_HALF_WINDOW : 0))
                case .hanningDenormalized:
                    vDSP_hann_windowD(v.baseAddress!,
                                      n,
                                      Int32(vDSP_HANN_DENORM) |
                                        Int32(isHalfWindow ?  vDSP_HALF_WINDOW : 0))
                case .hamming:
                    vDSP_hamm_windowD(v.baseAddress!,
                                      n,
                                      Int32(isHalfWindow ?  vDSP_HALF_WINDOW : 0))
                case .blackman:
                    vDSP_blkman_windowD(v.baseAddress!,
                                        n,
                                        Int32(isHalfWindow ?  vDSP_HALF_WINDOW : 0))
                }
            }
    }
    
    // MARK: Ramps
    
    //===----------------------------------------------------------------------===//
    //  withInitialValue and increment
    //===----------------------------------------------------------------------===//
    
    /// Returns an array containing monotonically incrementing or decrementing values, single-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements.
    /// - Parameter count: The number of elements in the array.
    /// - Returns: An array containing the specified ramp.
    @inlinable
    public static func ramp(withInitialValue initialValue: Float,
                            increment: Float,
                            count: Int) -> [Float] {
        
        precondition(count > 0)
        
        let result = Array<Float>(unsafeUninitializedCapacity: count) {
            buffer, initializedCount in
            
            formRamp(withInitialValue: initialValue,
                     increment: increment,
                     result: &buffer)
            
            initializedCount = count
        }
        
        return result
    }
    
    /// Fills a supplied array with monotonically incrementing or decrementing values, single-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements.
    /// - Parameter result: Output values.
    @inlinable
    public static func formRamp<V>(withInitialValue initialValue: Float,
                                   increment: Float,
                                   result: inout V)
        where V: AccelerateMutableBuffer,
        V.Element == Float {
            
            let n = vDSP_Length(result.count)
            
            withUnsafePointer(to: initialValue) { a in
                withUnsafePointer(to: increment) { b in
                    result.withUnsafeMutableBufferPointer { c in
                        vDSP_vramp(a,
                                   b,
                                   c.baseAddress!, 1,
                                   n)
                    }
                }
            }
    }
    
    /// Returns an array containing monotonically incrementing or decrementing values, double-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements.
    /// - Parameter count: The number of elements in the array.
    /// - Returns: An array containing the specified ramp.
    @inlinable
    public static func ramp(withInitialValue initialValue: Double,
                            increment: Double,
                            count: Int) -> [Double] {
        
        precondition(count > 0)
        
        let result = Array<Double>(unsafeUninitializedCapacity: count) {
            buffer, initializedCount in
            
            formRamp(withInitialValue: initialValue,
                     increment: increment,
                     result: &buffer)
            
            initializedCount = count
        }
        
        return result
    }
    
    /// Fills a supplied array with monotonically incrementing or decrementing values, double-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements.
    /// - Parameter result: Output values.
    @inlinable
    public static func formRamp<V>(withInitialValue initialValue: Double,
                                   increment: Double,
                                   result: inout V)
        where V: AccelerateMutableBuffer,
        V.Element == Double {
            
            let n = vDSP_Length(result.count)
            
            withUnsafePointer(to: initialValue) { a in
                withUnsafePointer(to: increment) { b in
                    result.withUnsafeMutableBufferPointer { c in
                        vDSP_vrampD(a,
                                    b,
                                    c.baseAddress!, 1,
                                    n)
                    }
                }
            }
    }
    
    //===----------------------------------------------------------------------===//
    //  range
    //===----------------------------------------------------------------------===//
    
    /// Returns an array containing monotonically incrementing or decrementing values within a specified range, single-precision.
    ///
    /// - Parameter range: Specifies range of the ramp..
    /// - Parameter count: The number of elements in the array.
    /// - Returns: An array containing the specified ramp.
    @inlinable
    public static func ramp(in range: ClosedRange<Float>,
                            count: Int) -> [Float] {
        
        precondition(count > 0)
        
        let result = Array<Float>(unsafeUninitializedCapacity: count) {
            buffer, initializedCount in
            
            formRamp(in: range,
                     result: &buffer)
            
            initializedCount = count
        }
        
        return result
    }
    
    /// Fills a supplied array with monotonically incrementing or decrementing values within a specified range, single-precision.
    ///
    /// - Parameter range: Specifies range of the ramp.
    /// - Parameter result: Output values.
    @inlinable
    public static func formRamp<V>(in range: ClosedRange<Float>,
                                   result: inout V)
        where V: AccelerateMutableBuffer,
        V.Element == Float {
            
            let n = vDSP_Length(result.count)
            
            withUnsafePointer(to: range.lowerBound) { a in
                withUnsafePointer(to: range.upperBound) { b in
                    result.withUnsafeMutableBufferPointer { c in
                        vDSP_vgen(a,
                                  b,
                                  c.baseAddress!, 1,
                                  n)
                    }
                }
            }
    }
    
    /// Returns an array containing monotonically incrementing or decrementing values within a specified range, double-precision.
    ///
    /// - Parameter range: Specifies range of the ramp..
    /// - Parameter count: The number of elements in the array.
    /// - Returns: An array containing the specified ramp.
    @inlinable
    public static func ramp(in range: ClosedRange<Double>,
                            count: Int) -> [Double] {
        
        precondition(count > 0)
        
        let result = Array<Double>(unsafeUninitializedCapacity: count) {
            buffer, initializedCount in
            
            formRamp(in: range,
                     result: &buffer)
            
            initializedCount = count
        }
        
        return result
    }
    
    /// Fills a supplied array with monotonically incrementing or decrementing values within a specified range, double-precision.
    ///
    /// - Parameter range: Specifies range of the ramp.
    /// - Parameter result: Output values.
    @inlinable
    public static func formRamp<V>(in range: ClosedRange<Double>,
                                   result: inout V)
        where V: AccelerateMutableBuffer,
        V.Element == Double {
            
            let n = vDSP_Length(result.count)
            
            withUnsafePointer(to: range.lowerBound) { a in
                withUnsafePointer(to: range.upperBound) { b in
                    result.withUnsafeMutableBufferPointer { c in
                        vDSP_vgenD(a,
                                   b,
                                   c.baseAddress!, 1,
                                   n)
                    }
                }
            }
    }
    
    //===----------------------------------------------------------------------===//
    //  initialValue, multiplyingBy, and increment
    //===----------------------------------------------------------------------===//
    
    /// Returns an array containing monotonically incrementing or decrementing values, multiplying by a source vector, single-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value. Modified on return to hold the next value (including accumulated errors) so that the ramp function can be continued smoothly.
    /// - Parameter multiplyingBy: Input values multiplied by the ramp function.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements
    /// - Parameter count: The number of elements in the array.
    /// - Returns: An array containing the specified ramp.
    @inlinable
    public static func ramp<U>(withInitialValue initialValue: inout Float,
                               multiplyingBy vector: U,
                               increment: Float) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                formRamp(withInitialValue: &initialValue,
                         multiplyingBy: vector,
                         increment: increment,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Fills a supplied array with monotonically incrementing or decrementing values, multiplying by a source vector, single-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value. Modified on return to hold the next value (including accumulated errors) so that the ramp function can be continued smoothly.
    /// - Parameter multiplyingBy: Input values multiplied by the ramp function.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements.
    /// - Parameter result: Output values.
    @inlinable
    public static func formRamp<U,V>(withInitialValue initialValue: inout Float,
                                     multiplyingBy vector: U,
                                     increment: Float,
                                     result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    withUnsafePointer(to: increment) { step in
                        vDSP_vrampmul(src.baseAddress!, 1,
                                      &initialValue,
                                      step,
                                      dest.baseAddress!, 1,
                                      n)
                    }
                }
            }
    }
    
    /// Returns an array containing monotonically incrementing or decrementing values, multiplying by a source vector, double-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value. Modified on return to hold the next value (including accumulated errors) so that the ramp function can be continued smoothly.
    /// - Parameter multiplyingBy: Input values multiplied by the ramp function.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements
    /// - Parameter count: The number of elements in the array.
    /// - Returns: An array containing the specified ramp.
    @inlinable
    public static func ramp<U>(withInitialValue initialValue: inout Double,
                               multiplyingBy vector: U,
                               increment: Double) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                formRamp(withInitialValue: &initialValue,
                         multiplyingBy: vector,
                         increment: increment,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Fills a supplied array with monotonically incrementing or decrementing values, multiplying by a source vector, double-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value. Modified on return to hold the next value (including accumulated errors) so that the ramp function can be continued smoothly.
    /// - Parameter multiplyingBy: Input values multiplied by the ramp function.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements.
    /// - Parameter result: Output values.
    @inlinable
    public static func formRamp<U,V>(withInitialValue initialValue: inout Double,
                                     multiplyingBy vector: U,
                                     increment: Double,
                                     result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    withUnsafePointer(to: increment) { step in
                        vDSP_vrampmulD(src.baseAddress!, 1,
                                       &initialValue,
                                       step,
                                       dest.baseAddress!, 1,
                                       n)
                    }
                }
            }
    }
    
    //===----------------------------------------------------------------------===//
    //  stereo
    //===----------------------------------------------------------------------===//
    
    /// Returns two arraya containing monotonically monotonically incrementing or decrementing values, multiplying by a source vector, stereo, single-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value. Modified on return to hold the next value (including accumulated errors) so that the ramp function can be continued smoothly.
    /// - Parameter multiplierOne: Input values multiplied by the ramp function.
    /// - Parameter multiplierTwo: Input values multiplied by the ramp function.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements.
    /// - Returns: A tuple of two arrays containing the specified ramps.
    @inlinable
    public static func stereoRamp<U>(withInitialValue initialValue: inout Float,
                                     multiplyingBy multiplierOne: U, _ multiplierTwo: U,
                                     increment: Float) -> (firstOutput:[Float], secondOutput: [Float])
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = multiplierOne.count
            
            var firstOutput: Array<Float>!
            
            let secondOutput = Array<Float>(unsafeUninitializedCapacity: n) {
                secondBuffer, secondInitializedCount in
                
                firstOutput = Array<Float>(unsafeUninitializedCapacity: n) {
                    firstBuffer, firstInitializedCount in
                    
                    formStereoRamp(withInitialValue: &initialValue,
                                   multiplyingBy: multiplierOne, multiplierTwo,
                                   increment: increment,
                                   results: &firstBuffer, &secondBuffer)
                    
                    firstInitializedCount = n
                }
                
                secondInitializedCount = n
            }
            
            return (firstOutput: firstOutput,
                    secondOutput: secondOutput)
    }
    
    /// Fills a supplied array with monotonically incrementing or decrementing values, multiplying by a source vector, stereo, single-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value. Modified on return to hold the next value (including accumulated errors) so that the ramp function can be continued smoothly.
    /// - Parameter multiplierOne: Input values multiplied by the ramp function.
    /// - Parameter multiplierTwo: Input values multiplied by the ramp function.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements.
    /// - Parameter resultOne: Output values.
    /// - Parameter resultTwo: Output values.
    @inlinable
    public static func formStereoRamp<U,V>(withInitialValue initialValue: inout Float,
                                           multiplyingBy multiplierOne: U, _ multiplierTwo: U,
                                           increment: Float,
                                           results resultOne: inout V, _ resultTwo: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(multiplierOne.count == multiplierTwo.count)
            precondition(resultOne.count == resultTwo.count)
            precondition(multiplierOne.count == resultOne.count)
            let n = vDSP_Length(resultTwo.count)
            
            resultOne.withUnsafeMutableBufferPointer { o0 in
                resultTwo.withUnsafeMutableBufferPointer { o1 in
                    multiplierOne.withUnsafeBufferPointer { i0 in
                        multiplierTwo.withUnsafeBufferPointer { i1 in
                            withUnsafePointer(to: increment) { step in
                                vDSP_vrampmul2(i0.baseAddress!,
                                               i1.baseAddress!, 1,
                                               &initialValue,
                                               step,
                                               o0.baseAddress!,
                                               o1.baseAddress!, 1,
                                               n)
                            }
                        }
                    }
                }
            }
    }
    
    /// Returns two arraya containing monotonically monotonically incrementing or decrementing values, multiplying by a source vector, stereo, double-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value. Modified on return to hold the next value (including accumulated errors) so that the ramp function can be continued smoothly.
    /// - Parameter multiplierOne: Input values multiplied by the ramp function.
    /// - Parameter multiplierTwo: Input values multiplied by the ramp function.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements.
    /// - Returns: A tuple of two arrays containing the specified ramps.
    @inlinable
    public static func stereoRamp<U>(withInitialValue initialValue: inout Double,
                                     multiplyingBy multiplierOne: U, _ multiplierTwo: U,
                                     increment: Double) -> (firstOutput:[Double], secondOutput: [Double])
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = multiplierOne.count
            
            var firstOutput: Array<Double>!
            
            let secondOutput = Array<Double>(unsafeUninitializedCapacity: n) {
                secondBuffer, secondInitializedCount in
                
                firstOutput = Array<Double>(unsafeUninitializedCapacity: n) {
                    firstBuffer, firstInitializedCount in
                    
                    formStereoRamp(withInitialValue: &initialValue,
                                   multiplyingBy: multiplierOne, multiplierTwo,
                                   increment: increment,
                                   results: &firstBuffer, &secondBuffer)
                    
                    firstInitializedCount = n
                }
                
                secondInitializedCount = n
            }
            
            return (firstOutput: firstOutput,
                    secondOutput: secondOutput)
    }
    
    /// Fills a supplied array with monotonically incrementing or decrementing values, multiplying by a source vector, stereo, double-precision.
    ///
    /// - Parameter initialValue: Specifies the initial value. Modified on return to hold the next value (including accumulated errors) so that the ramp function can be continued smoothly.
    /// - Parameter multiplierOne: Input values multiplied by the ramp function.
    /// - Parameter multiplierTwo: Input values multiplied by the ramp function.
    /// - Parameter increment: The increment (or decrement if negative) between consecutive elements.
    /// - Parameter resultOne: Output values.
    /// - Parameter resultTwo: Output values.
    @inlinable
    public static func formStereoRamp<U,V>(withInitialValue initialValue: inout Double,
                                           multiplyingBy multiplierOne: U, _ multiplierTwo: U,
                                           increment: Double,
                                           results resultOne: inout V, _ resultTwo: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(multiplierOne.count == multiplierTwo.count)
            precondition(resultOne.count == resultTwo.count)
            precondition(multiplierOne.count == resultOne.count)
            let n = vDSP_Length(resultTwo.count)
            
            resultOne.withUnsafeMutableBufferPointer { o0 in
                resultTwo.withUnsafeMutableBufferPointer { o1 in
                    multiplierOne.withUnsafeBufferPointer { i0 in
                        multiplierTwo.withUnsafeBufferPointer { i1 in
                            withUnsafePointer(to: increment) { step in
                                vDSP_vrampmul2D(i0.baseAddress!,
                                                i1.baseAddress!, 1,
                                                &initialValue,
                                                step,
                                                o0.baseAddress!,
                                                o1.baseAddress!, 1,
                                                n)
                            }
                        }
                    }
                }
            }
    }
    
    //===----------------------------------------------------------------------===//
    //  Generation by extrapolation and interpolation.
    //===----------------------------------------------------------------------===//
    
    /// Fills a single-precision vector by extrapolation and interpolation.
    ///
    /// - Parameter values: The values to interpolate, for example, intensities, magnitudes, or some other measurable quantities.
    /// - Parameter indices: The monotonically increasing indices to `result` for the corresponding element in `values`.
    /// - Parameter result: Output values.
    @inlinable
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func linearInterpolate<T, U, V>(values: T,
                                                  atIndices indices: U,
                                                  result: inout V)
        where
        T: AccelerateBuffer, U: AccelerateBuffer, V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(indices.count == values.count,
                         "The number of elements in `values` must equal the number of elements in `indices`.")
            
            let n = vDSP_Length(result.count)
            let m = vDSP_Length(indices.count)
            
            result.withUnsafeMutableBufferPointer { r in
                values.withUnsafeBufferPointer { v in
                    indices.withUnsafeBufferPointer { i in
                        vDSP_vgenp(v.baseAddress!, 1,
                                   i.baseAddress!, 1,
                                   r.baseAddress!, 1,
                                   n,
                                   m)
                    }
                }
            }
    }
    
    /// Returns a single-precision vector generated by extrapolation and interpolation.
    ///
    /// - Parameter values: The values to interpolate, for example, intensities, magnitudes, or some other measurable quantities.
    /// - Parameter indices: The monotonically increasing indices to the result for the corresponding element in `values`.
    ///
    /// - Returns: An array containing the extrapolation and interpolation result with a length of `max(indices) + 1`.
    @inlinable
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func linearInterpolate<T, U>(values: T,
                                               atIndices indices: U) -> [Float]
        where
        T: AccelerateBuffer, U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let resultCount = 1 + Int(Accelerate.vDSP.maximum(indices))
            
            let result = Array<Float>(unsafeUninitializedCapacity: resultCount) {
                buffer, initializedCount in
                
                vDSP.linearInterpolate(values: values,
                                       atIndices: indices,
                                       result: &buffer)
                
                initializedCount = resultCount
            }
            
            return result
    }
    
    /// Fills a double-precision vector by extrapolation and interpolation.
    ///
    /// - Parameter values: The values to interpolate, for example, intensities, magnitudes, or some other measurable quantities.
    /// - Parameter indices: The monotonically increasing indices to `result` for the corresponding element in `values`.
    /// - Parameter result: Output values.
    @inlinable
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func linearInterpolate<T, U, V>(values: T,
                                                  atIndices indices: U,
                                                  result: inout V)
        where
        T: AccelerateBuffer, U: AccelerateBuffer, V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(indices.count == values.count,
                         "The number of elements in `values` must equal the number of elements in `indices`.")
            
            let n = vDSP_Length(result.count)
            let m = vDSP_Length(indices.count)
            
            result.withUnsafeMutableBufferPointer { r in
                values.withUnsafeBufferPointer { v in
                    indices.withUnsafeBufferPointer { i in
                        vDSP_vgenpD(v.baseAddress!, 1,
                                    i.baseAddress!, 1,
                                    r.baseAddress!, 1,
                                    n,
                                    m)
                    }
                }
            }
    }
    
    /// Returns a double-precision vector generated by extrapolation and interpolation.
    ///
    /// - Parameter values: The values to interpolate, for example, intensities, magnitudes, or some other measurable quantities.
    /// - Parameter indices: The monotonically increasing indices to the result for the corresponding element in `values`.
    ///
    /// - Returns: An array containing the extrapolation and interpolation result with a length of `max(indices) + 1`.
    @inlinable
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func linearInterpolate<T, U>(values: T,
                                               atIndices indices: U) -> [Double]
        where
        T: AccelerateBuffer, U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let resultCount = 1 + Int(Accelerate.vDSP.maximum(indices))
            
            let result = Array<Double>(unsafeUninitializedCapacity: resultCount) {
                buffer, initializedCount in
                
                vDSP.linearInterpolate(values: values,
                                       atIndices: indices,
                                       result: &buffer)
                
                initializedCount = resultCount
            }
            
            return result
    }
    
    //===----------------------------------------------------------------------===//
    //  Generation by table lookup.
    //===----------------------------------------------------------------------===//
    
    /// Generates a single-precision vector by table lookup interpolation.
    ///
    /// - Parameter lookupTable: The lookup table.
    /// - Parameter offsets: The offsets into the lookup table.
    /// - Parameter scale: The scale factor.
    /// - Parameter baseOffset: The base offset.
    /// - Parameter result: Output values.
    @inlinable
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func linearInterpolate<T, U, V>(lookupTable: T,
                                                  withOffsets offsets: U,
                                                  scale: Float = 1,
                                                  baseOffset: Float = 0,
                                                  result: inout V)
        where
        T: AccelerateBuffer, U: AccelerateBuffer, V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(offsets.count == result.count,
                         "The number of elements in `offsets` must equal the number of elements in `result`.")
            
            let m = vDSP_Length(lookupTable.count)
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { r in
                lookupTable.withUnsafeBufferPointer { lut in
                    offsets.withUnsafeBufferPointer { o in
                        vDSP_vtabi(o.baseAddress!, 1,
                                   [scale],
                                   [baseOffset],
                                   lut.baseAddress!, m,
                                   r.baseAddress!, 1, n)
                    }
                }
            }
    }
    
    /// Returns a single-precision vector generated by table lookup interpolation.
    ///
    /// - Parameter lookupTable: The lookup table.
    /// - Parameter offsets: The offsets into the lookup table.
    /// - Parameter scale: The scale factor.
    /// - Parameter baseOffset: The base offset.
    ///
    /// - Returns: An array containing the result with a length of `offsets.count`.
    @inlinable
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func linearInterpolate<T, U>(lookupTable: T,
                                               withOffsets offsets: U,
                                               scale: Float = 1,
                                               baseOffset: Float = 0) -> [Float]
        where
        T: AccelerateBuffer, U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: offsets.count) {
                buffer, initializedCount in
                
                vDSP.linearInterpolate(lookupTable: lookupTable,
                                       withOffsets: offsets,
                                       scale: scale,
                                       baseOffset: baseOffset,
                                       result: &buffer)
                
                initializedCount = offsets.count
            }
            
            return result
    }

    /// Generates a double-precision vector by table lookup interpolation.
    ///
    /// - Parameter lookupTable: The lookup table.
    /// - Parameter offsets: The offsets into the lookup table.
    /// - Parameter scale: The scale factor.
    /// - Parameter baseOffset: The base offset.
    /// - Parameter result: Output values.
    @inlinable
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func linearInterpolate<T, U, V>(lookupTable: T,
                                                  withOffsets offsets: U,
                                                  scale: Double = 1,
                                                  baseOffset: Double = 0,
                                                  result: inout V)
        where
        T: AccelerateBuffer, U: AccelerateBuffer, V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(offsets.count == result.count,
                         "The number of elements in `offsets` must equal the number of elements in `result`.")
            
            let m = vDSP_Length(lookupTable.count)
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { r in
                lookupTable.withUnsafeBufferPointer { lut in
                    offsets.withUnsafeBufferPointer { o in
                        vDSP_vtabiD(o.baseAddress!, 1,
                                   [scale],
                                   [baseOffset],
                                   lut.baseAddress!, m,
                                   r.baseAddress!, 1, n)
                    }
                }
            }
    }
    
    /// Returns a double-precision vector generated by table lookup interpolation.
    ///
    /// - Parameter lookupTable: The lookup table.
    /// - Parameter offsets: The offsets into the lookup table.
    /// - Parameter scale: The scale factor.
    /// - Parameter baseOffset: The base offset.
    ///
    /// - Returns: An array containing the result with a length of `offsets.count`.
    @inlinable
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func linearInterpolate<T, U>(lookupTable: T,
                                               withOffsets offsets: U,
                                               scale: Double = 1,
                                               baseOffset: Double = 0) -> [Double]
        where
        T: AccelerateBuffer, U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: offsets.count) {
                buffer, initializedCount in
                
                vDSP.linearInterpolate(lookupTable: lookupTable,
                                       withOffsets: offsets,
                                       scale: scale,
                                       baseOffset: baseOffset,
                                       result: &buffer)
                
                initializedCount = offsets.count
            }
            
            return result
    }
}
