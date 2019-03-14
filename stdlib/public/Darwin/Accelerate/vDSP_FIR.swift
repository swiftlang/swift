import Accelerate

extension vDSP {
    
    /// FIR filtering with decimation and antialiasing; single-precision.
    ///
    /// - Parameter source: Single-precision input vector.
    /// - Parameter decimationFaction: The integer factor by which to divide the sampling rate.
    /// - Parameter filter: Filter to use during the downsampling operation.
    /// - Parameter result: Single-precision output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func downsample<T, U, V>(_ source: U,
                                           decimationFaction: Int,
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
            
            precondition(source.count == decimationFaction * (n - 1) + p)
            
            result.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    filter.withUnsafeBufferPointer { f in
                        
                        vDSP_desamp(src.baseAddress!,
                                    decimationFaction,
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
    /// - Parameter decimationFaction: The integer factor by which to divide the sampling rate.
    /// - Parameter filter: Filter to use during the downsampling operation.
    /// - Parameter result: Double-precision output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func downsample<T, U, V>(_ source: U,
                                           decimationFaction: Int,
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
            
            precondition(source.count == decimationFaction * (n - 1) + p)
            
            result.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    filter.withUnsafeBufferPointer { f in
                        
                        vDSP_desampD(src.baseAddress!,
                                     decimationFaction,
                                     f.baseAddress!,
                                     dest.baseAddress!,
                                     vDSP_Length(n),
                                     vDSP_Length(p))
                    }
                }
            }
    }
    
}
