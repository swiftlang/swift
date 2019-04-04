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

//===----------------------------------------------------------------------===//
//
//  vImage_Error
//
//===----------------------------------------------------------------------===//

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension vImage {
    
    /// Error codes returned by vImage operations.
    public enum Error: Int, Swift.Error {
        case noError                      =    0
        case roiLargerThanInputBuffer     =    -21766
        case invalidKernelSize            =    -21767
        case invalidEdgeStyle             =    -21768
        case invalidOffset_X              =    -21769
        case invalidOffset_Y              =    -21770
        case memoryAllocationError        =    -21771
        case nullPointerArgument          =    -21772
        case invalidParameter             =    -21773
        case bufferSizeMismatch           =    -21774
        case unknownFlagsBit              =    -21775
        case internalError                =    -21776
        case invalidRowBytes              =    -21777
        case invalidImageFormat           =    -21778
        case colorSyncIsAbsent            =    -21779
        case outOfPlaceOperationRequired  =    -21780
        case invalidImageObject           =    -21781
        case invalidCVImageFormat         =    -21782
        case unsupportedConversion        =    -21783
        case coreVideoIsAbsent            =    -21784
        
        public init(vImageError: vImage_Error) {
            self = Error(rawValue: vImageError) ?? .internalError
        }
    }
    
}
