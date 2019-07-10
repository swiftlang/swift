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

@_exported import CryptoTokenKit

import Foundation

@available(macOS 10.10, *)
extension TKSmartCard {
  public func send(ins: UInt8, p1: UInt8, p2: UInt8, data: Data? = nil,
    le: Int? = nil, reply: @escaping (Data?, UInt16, Error?) -> Void) {

    self.__sendIns(ins, p1: p1, p2: p2, data: data,
      le: le.map { NSNumber(value: $0) }, reply: reply)
  }

  @available(macOS 10.12, *)
  public func send(ins: UInt8, p1: UInt8, p2: UInt8, data: Data? = nil,
    le: Int? = nil) throws -> (sw: UInt16, response: Data) {

    var sw: UInt16 = 0
    let response = try self.__sendIns(ins, p1: p1, p2: p2, data: data,
      le: le.map { NSNumber(value: $0) }, sw: &sw)
    return (sw: sw, response: response)
  }

  @available(macOS 10.12, *)
  public func withSession<T>(_ body: @escaping () throws -> T) throws -> T {
    var result: T?
    try self.__inSession(executeBlock: {
      (errorPointer: NSErrorPointer) -> Bool in
      do {
        result = try body()
        return true
      } catch let error as NSError {
        errorPointer?.pointee = error
        return false
      }
    })

    // it is safe to force unwrap the result here, as the self.__inSession
    // function rethrows the errors which happened inside the block
    return result!
  }
}
