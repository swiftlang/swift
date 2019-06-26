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

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    public enum FourierTransformDirection {
        case forward
        case inverse
        
        public var dftDirection: vDSP_DFT_Direction {
            switch self {
            case .forward:
                return .FORWARD
            case .inverse:
                return .INVERSE
            }
        }
        
        public var fftDirection: FFTDirection {
            switch self {
            case .forward:
                return FFTDirection(kFFTDirection_Forward)
            case .inverse:
                return FFTDirection(kFFTDirection_Inverse)
            }
        }
    }
}
