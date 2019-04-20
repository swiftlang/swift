import Accelerate

extension vDSP {
    
    /// FIR filtering with decimation and antialiasing; single-precision.
    ///
    /// - Parameter source: Single-precision input vector.
    /// - Parameter decimationFactor: The integer factor by which to divide the sampling rate.
    /// - Parameter filter: Filter to use during the downsampling operation.
    /// - Returns: Single-precision output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func downsample<T, U>(_ source: U,
                                        decimationFactor: Int,
                                        filter: T) -> [Float]
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        T.Element == Float,
        U.Element == Float {
            
            let n = (source.count - filter.count) / decimationFactor + 1
            
            let result = Array<Float>(unsafeUninitializedCapacity: n) {
                buffer, initializedCount in
                
                downsample(source,
                           decimationFactor: decimationFactor,
                           filter: filter,
                           result: &buffer)
                
                initializedCount = n
            }
            
            return result
    }
    
    /// FIR filtering with decimation and antialiasing; single-precision.
    ///
    /// - Parameter source: Single-precision input vector.
    /// - Parameter decimationFactor: The integer factor by which to divide the sampling rate.
    /// - Parameter filter: Filter to use during the downsampling operation.
    /// - Parameter result: Single-precision output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func downsample<T, U, V>(_ source: U,
                                           decimationFactor: Int,
                                           filter: T,
                                           result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float,
        U.Element == Float,
        V.Element == Float {
            
            let p = filter.count
            let n = result.count
            
            precondition(source.count == decimationFactor * (n - 1) + p)
            
            result.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    filter.withUnsafeBufferPointer { f in
                        
                        vDSP_desamp(src.baseAddress!,
                                    decimationFactor,
                                    f.baseAddress!,
                                    dest.baseAddress!,
                                    vDSP_Length(n),
                                    vDSP_Length(p))
                    }
                }
            }
    }
    
    /// FIR filtering with decimation and antialiasing; double-precision.
    ///
    /// - Parameter source: Double-precision input vector.
    /// - Parameter decimationFactor: The integer factor by which to divide the sampling rate.
    /// - Parameter filter: Filter to use during the downsampling operation.
    /// - Returns: Double-precision output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func downsample<T, U>(_ source: U,
                                        decimationFactor: Int,
                                        filter: T) -> [Double]
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        T.Element == Double,
        U.Element == Double {
            let n = (source.count - filter.count) / decimationFactor + 1
            
            let result = Array<Double>(unsafeUninitializedCapacity: n) {
                buffer, initializedCount in
                
                downsample(source,
                           decimationFactor: decimationFactor,
                           filter: filter,
                           result: &buffer)
                
                initializedCount = n
            }
            
            return result
    }
    
    /// FIR filtering with decimation and antialiasing; double-precision.
    ///
    /// - Parameter source: Double-precision input vector.
    /// - Parameter decimationFactor: The integer factor by which to divide the sampling rate.
    /// - Parameter filter: Filter to use during the downsampling operation.
    /// - Parameter result: Double-precision output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func downsample<T, U, V>(_ source: U,
                                           decimationFactor: Int,
                                           filter: T,
                                           result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double,
        U.Element == Double,
        V.Element == Double {
            
            let p = filter.count
            let n = result.count
            
            precondition(source.count == decimationFactor * (n - 1) + p)
            
            result.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    filter.withUnsafeBufferPointer { f in
                        
                        vDSP_desampD(src.baseAddress!,
                                     decimationFactor,
                                     f.baseAddress!,
                                     dest.baseAddress!,
                                     vDSP_Length(n),
                                     vDSP_Length(p))
                    }
                }
            }
    }
    
}
