//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

//===----------------------------------------------------------------------===//
// TextChecking
//===----------------------------------------------------------------------===//

extension NSTextCheckingResult.CheckingType {
    public static var allSystemTypes : NSTextCheckingResult.CheckingType {
        return NSTextCheckingResult.CheckingType(rawValue: 0xffffffff)
    }
    
    public static var allCustomTypes : NSTextCheckingResult.CheckingType {
        return NSTextCheckingResult.CheckingType(rawValue: 0xffffffff << 32)
    }
    
    public static var allTypes : NSTextCheckingResult.CheckingType {
        return NSTextCheckingResult.CheckingType(rawValue: UInt64.max)
    }
}
