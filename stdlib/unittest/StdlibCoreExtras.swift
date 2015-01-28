//===--- StdlibCoreExtras.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Darwin
import Foundation

//
// These APIs don't really belong in a unit testing library, but they are
// useful in tests, and stdlib does not have such facilities yet.
//

/// Convert the given numeric value to a hexidecimal string.
public func asHex<T : IntegerType>(x: T) -> String {
  return "0x" + String(x.toIntMax(), radix: 16)
}

/// Convert the given sequence of numeric values to a string representing
/// their hexidecimal values.
public func asHex<
  S: SequenceType
where
  S.Generator.Element : IntegerType
>(x: S) -> String {
  return "[ " + ", ".join(lazy(x).map { asHex($0) }) + " ]"
}

/// Compute the prefix sum of `seq`.
func scan<
  S : SequenceType, U
>(seq: S, initial: U, combine: (U, S.Generator.Element) -> U) -> _UnitTestArray<U> {
  var result = _UnitTestArray<U>()
  result.reserveCapacity(underestimateCount(seq))
  var runningResult = initial
  for element in seq {
    runningResult = combine(runningResult, element)
    result.append(runningResult)
  }
  return result
}

public func _stdlib_randomShuffle<T>(a: _UnitTestArray<T>) -> _UnitTestArray<T> {
  var result = a
  for var i = a.count - 1; i != 0; --i {
    // FIXME: 32 bits are not enough in general case!
    let j = Int(rand32(exclusiveUpperBound: UInt32(i + 1)))
    swap(&result[i], &result[j])
  }
  return result
}

public func _stdlib_gather<T>(a: _UnitTestArray<T>, idx: _UnitTestArray<Int>) -> _UnitTestArray<T> {
  var result = _UnitTestArray<T>()
  result.reserveCapacity(a.count)
  for i in 0..<a.count {
    result.append(a[idx[i]])
  }
  return result
}

public func _stdlib_scatter<T>(a: _UnitTestArray<T>, idx: _UnitTestArray<Int>) -> _UnitTestArray<T> {
  var result = a
  for i in 0..<a.count {
    result[idx[i]] = a[i]
  }
  return result
}

func findSubstring(string: String, substring: String) -> String.Index? {
  if substring.isEmpty {
    return string.startIndex
  }
  return string.rangeOfString(substring)?.startIndex
}

func withArrayOfCStrings<R>(
  args: _UnitTestArray<String>, body: (Array<UnsafeMutablePointer<CChar>>) -> R
) -> R {

  let argsLengths = _UnitTestArray(map(args) { count($0.utf8) + 1 })
  let argsOffsets = [ 0 ] + scan(argsLengths, 0, +)
  let argsBufferSize = argsOffsets.last!

  var argsBuffer = _UnitTestArray<UInt8>()
  argsBuffer.reserveCapacity(argsBufferSize)
  for arg in args {
    argsBuffer.extend(arg.utf8)
    argsBuffer.append(0)
  }

  return argsBuffer.withUnsafeBufferPointer {
    (argsBuffer) in
    let ptr = UnsafeMutablePointer<CChar>(argsBuffer.baseAddress)
    var cStrings = map(argsOffsets) { ptr + $0 }
    cStrings[cStrings.count - 1] = nil
    return body(cStrings)
  }
}

public struct _Stderr : OutputStreamType {
  public init() {}

  public mutating func write(string: String) {
    for c in string.utf8 {
      putc(Int32(c), stderr)
    }
  }
}

struct _FDOutputStream : OutputStreamType {
  let fd: CInt

  mutating func write(string: String) {
    let utf8 = string.nulTerminatedUTF8
    utf8.withUnsafeBufferPointer {
      (utf8) -> () in
      var writtenBytes: size_t = 0
      let bufferSize = size_t(utf8.count - 1)
      while writtenBytes != bufferSize {
        let result = Darwin.write(
          self.fd, UnsafePointer(utf8.baseAddress + Int(writtenBytes)),
          bufferSize - writtenBytes)
        if result < 0 {
          fatalError("write() returned an error")
        }
        writtenBytes += size_t(result)
      }
    }
  }
}

func _stdlib_mkstemps(inout template: String, suffixlen: CInt) -> CInt {
  var utf8 = template.nulTerminatedUTF8
  let (fd, fileName) = utf8.withUnsafeMutableBufferPointer {
    (utf8) -> (CInt, String) in
    let fd = mkstemps(UnsafeMutablePointer(utf8.baseAddress), suffixlen)
    let fileName = String.fromCString(UnsafePointer(utf8.baseAddress))!
    return (fd, fileName)
  }
  template = fileName
  return fd
}

public func createTemporaryFile(
  fileNamePrefix: String, fileNameSuffix: String, contents: String
) -> String {
  var fileName = NSTemporaryDirectory().stringByAppendingPathComponent(
    fileNamePrefix + "XXXXXX" + fileNameSuffix)
  let fd = _stdlib_mkstemps(
    &fileName, CInt(count(fileNameSuffix.utf8)))
  if fd < 0 {
    fatalError("mkstemps() returned an error")
  }
  var stream = _FDOutputStream(fd: fd)
  stream.write(contents)
  if close(fd) != 0 {
    fatalError("close() return an error")
  }
  return fileName
}

/// An atomic counter for contended applications.
///
/// This is a struct to prevent extra retains/releases.  You are required to
/// call `deinit()` to release the owned memory.
public struct _stdlib_ShardedAtomicCounter {
  // Using an array causes retains/releases, which create contention on the
  // reference count.
  // FIXME: guard every element against false sharing.
  var _shardsPtr: UnsafeMutablePointer<Int>
  var _shardsCount: Int

  public init() {
    let hardwareConcurrency = _stdlib_getHardwareConcurrency()
    let count = max(8, hardwareConcurrency * hardwareConcurrency)
    let shards = UnsafeMutablePointer<Int>.alloc(count)
    for var i = 0; i != count; i++ {
      (shards + i).initialize(0)
    }
    self._shardsPtr = shards
    self._shardsCount = count
  }

  public func `deinit`() {
    self._shardsPtr.destroy(self._shardsCount)
    self._shardsPtr.dealloc(self._shardsCount)
  }

  public func add(operand: Int, randomInt: Int) {
    let shardIndex = Int(UInt(bitPattern: randomInt) % UInt(self._shardsCount))
    _swift_stdlib_atomicFetchAddInt(
      object: self._shardsPtr + shardIndex, operand: operand)
  }

  // FIXME: non-atomic as a whole!
  public func getSum() -> Int {
    var result = 0
    let shards = self._shardsPtr
    let count = self._shardsCount
    for var i = 0; i != count; i++ {
      result += _swift_stdlib_atomicLoadInt(object: shards + i)
    }
    return result
  }

  public struct PRNG {
    var _state: Int

    public init() {
      _state = Int(Int32(bitPattern: StdlibUnittest.rand32()))
    }

    public mutating func randomInt() -> Int {
      var result = 0
      for var i = 0; i != Int._sizeInBits; ++i {
        result = (result << 1) | (_state & 1)
        _state = (_state >> 1) ^ (-(_state & 1) & Int(bitPattern: 0xD0000001))
      }
      return result
    }
  }
}

