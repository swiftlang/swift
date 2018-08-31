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

public extension Progress {
    @available(macOS 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
    public var estimatedTimeRemaining: TimeInterval? {
        get {
            guard let v = self.__estimatedTimeRemaining else { return nil }
            return v.doubleValue as TimeInterval
        }
        set {
            guard let nv = newValue else {
                self.__estimatedTimeRemaining = nil
                return
            }
            let v = NSNumber(value: nv)
            self.__estimatedTimeRemaining = v
        }
    }
    
    @available(macOS 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
    public var throughput: Int? {
        get {
            guard let v = self.__throughput else { return nil }
            return v.intValue
        }
        set {
            guard let nv = newValue else {
                self.__throughput = nil
                return
            }
            let v = NSNumber(value: nv)
            self.__throughput = v
        }
    }
    
    @available(macOS 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
    public var fileTotalCount: Int? {
        get {
            guard let v = self.__fileTotalCount else { return nil }
            return v.intValue
        }
        set {
            guard let nv = newValue else {
                self.__fileTotalCount = nil
                return
            }
            let v = NSNumber(value: nv)
            self.__fileTotalCount = v
        }
    }
    
    @available(macOS 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
    public var fileCompletedCount: Int? {
        get {
            guard let v = self.__fileCompletedCount else { return nil }
            return v.intValue
        }
        set {
            guard let nv = newValue else {
                self.__fileCompletedCount = nil
                return
            }
            let v = NSNumber(value: nv)
            self.__fileCompletedCount = v
        }
    }
    
    public func performAsCurrent<ReturnType>(withPendingUnitCount unitCount: Int64, using work: () throws -> ReturnType) rethrows -> ReturnType {
        becomeCurrent(withPendingUnitCount: unitCount)
        defer { resignCurrent() }
        return try work()
    }
}
