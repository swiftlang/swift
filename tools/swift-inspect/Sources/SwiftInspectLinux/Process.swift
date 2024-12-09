//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import LinuxSystemHeaders

public class Process {
  public enum ProcessError: Error {
    case processVmReadFailure(pid: pid_t, address: UInt64, size: UInt64)
    case malformedString(address: UInt64)
  }

  let pid: pid_t
  let elfFile: ElfFile

  public init(_ pid: pid_t) throws {
    self.pid = pid
    let executableFilePath = "/proc/\(pid)/exe"
    self.elfFile = try ElfFile(filePath: executableFilePath)
  }

  // read a struct of type T from the target process
  public func readStruct<T>(address: UInt64) throws -> T {
    let result: [T] = try readArray(address: address, upToCount: 1)
    return result.first!
  }

  // read a null-terminated string from the target process
  public func readString(address: UInt64, encoding: String.Encoding = .utf8) throws -> String {
    let rawBytes = try readRawString(address: address)
    guard let result = String(bytes: rawBytes, encoding: encoding) else {
      throw ProcessError.malformedString(address: address)
    }

    return result
  }

  // read bytes from the remote process until a zero-byte is encountered; the
  // zero-byte is not included in the result
  public func readRawString(address: UInt64) throws -> [UInt8] {
    var readAddress: UInt64 = address
    let chunkSize: UInt = 64
    var result: [UInt8] = []

    while true {
      let chunk: [UInt8] = try readArray(address: readAddress, upToCount: chunkSize)

      if let nullIndex = chunk.firstIndex(of: 0) {
        result.append(contentsOf: chunk.prefix(nullIndex))
        break
      }

      result.append(contentsOf: chunk)
      readAddress += UInt64(chunkSize)
    }

    return result
  }

  // read an array of type T elements from the target process
  public func readArray<T>(address: UInt64, upToCount: UInt) throws -> [T] {
    guard upToCount > 0 else { return [] }
    let maxSize = upToCount * UInt(MemoryLayout<T>.stride)
    let array: [T] = Array(unsafeUninitializedCapacity: Int(upToCount)) { buffer, initCount in
      var local = iovec(iov_base: buffer.baseAddress!, iov_len: Int(maxSize))
      var remote = iovec(
        iov_base: UnsafeMutableRawPointer(bitPattern: UInt(address)), iov_len: Int(maxSize))
      let bytesRead = process_vm_readv(self.pid, &local, 1, &remote, 1, 0)
      initCount = bytesRead / MemoryLayout<T>.stride
    }

    guard array.count > 0 else {
      throw ProcessError.processVmReadFailure(pid: self.pid, address: address, size: UInt64(maxSize))
    }

    return array
  }
}
