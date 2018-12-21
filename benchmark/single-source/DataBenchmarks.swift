//===--- DataBenchmarks.swift ---------------------------------------------===//
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

import TestsUtils
import Foundation

let d: [BenchmarkCategory] =  [.validation, .api, .Data]
let skip: [BenchmarkCategory] = [.validation, .api, .Data, .skip]

public let DataBenchmarks = [
  BenchmarkInfo(name: "DataCreateEmpty",
    runFunction: run_createEmpty, tags: d),
  BenchmarkInfo(name: "DataCreateSmall",
    runFunction: run_createSmall, tags: d),
  BenchmarkInfo(name: "DataCreateMedium",
    runFunction: run_createMedium, tags: d),
  BenchmarkInfo(name: "DataCreateLarge",
    runFunction: run_createLarge, tags: skip),

  BenchmarkInfo(name: "DataCreateEmptyArray",
    runFunction: run_createEmptyArray, tags: d),
  BenchmarkInfo(name: "DataCreateSmallArray",
    runFunction: run_createSmallArray, tags: d),
  BenchmarkInfo(name: "DataCreateMediumArray",
    runFunction: run_createMediumArray, tags: d),

  BenchmarkInfo(name: "DataSubscriptSmall",
    runFunction: run_SubscriptSmall, tags: d),
  BenchmarkInfo(name: "DataSubscriptMedium",
    runFunction: run_SubscriptMedium, tags: d),
  BenchmarkInfo(name: "DataSubscriptLarge",
    runFunction: run_SubscriptLarge, tags: skip),

  BenchmarkInfo(name: "DataCountSmall",
    runFunction: run_CountSmall, tags: d),
  BenchmarkInfo(name: "DataCountMedium",
    runFunction: run_CountMedium, tags: d),
  BenchmarkInfo(name: "DataCountLarge",
    runFunction: run_CountLarge, tags: skip),

  BenchmarkInfo(name: "DataSetCountSmall",
    runFunction: run_SetCountSmall, tags: d),
  BenchmarkInfo(name: "DataSetCountMedium",
    runFunction: run_SetCountMedium, tags: d),
  BenchmarkInfo(name: "DataSetCountLarge",
    runFunction: run_SetCountLarge, tags: skip),

  BenchmarkInfo(name: "DataAccessBytesSmall",
    runFunction: run_AccessBytesSmall, tags: d),
  BenchmarkInfo(name: "DataAccessBytesMedium",
    runFunction: run_AccessBytesMedium, tags: d),
  BenchmarkInfo(name: "DataAccessBytesLarge",
    runFunction: run_AccessBytesLarge, tags: skip),

  BenchmarkInfo(name: "DataMutateBytesSmall",
    runFunction: run_MutateBytesSmall, tags: d),
  BenchmarkInfo(name: "DataMutateBytesMedium",
    runFunction: run_MutateBytesMedium, tags: d),
  BenchmarkInfo(name: "DataMutateBytesLarge",
    runFunction: run_MutateBytesLarge, tags: skip),

  BenchmarkInfo(name: "DataCopyBytesSmall",
    runFunction: run_CopyBytesSmall, tags: d),
  BenchmarkInfo(name: "DataCopyBytesMedium",
    runFunction: run_CopyBytesMedium, tags: d),
  BenchmarkInfo(name: "DataCopyBytesLarge",
    runFunction: run_CopyBytesLarge, tags: skip),

  BenchmarkInfo(name: "DataAppendBytesSmall",
    runFunction: run_AppendBytesSmall, tags: d),
  BenchmarkInfo(name: "DataAppendBytesMedium",
    runFunction: run_AppendBytesMedium, tags: d),
  BenchmarkInfo(name: "DataAppendBytesLarge",
    runFunction: run_AppendBytesLarge, tags: skip),

  BenchmarkInfo(name: "DataAppendArray",
    runFunction: run_AppendArray, tags: d),

  BenchmarkInfo(name: "DataReset",
    runFunction: run_Reset, tags: d),

  BenchmarkInfo(name: "DataReplaceSmall",
    runFunction: run_ReplaceSmall, tags: d),
  BenchmarkInfo(name: "DataReplaceMedium",
    runFunction: run_ReplaceMedium, tags: d),
  BenchmarkInfo(name: "DataReplaceLarge",
    runFunction: run_ReplaceLarge, tags: d),

  BenchmarkInfo(name: "DataReplaceSmallBuffer",
    runFunction: run_ReplaceSmallBuffer, tags: d),
  BenchmarkInfo(name: "DataReplaceMediumBuffer",
    runFunction: run_ReplaceMediumBuffer, tags: d),
  BenchmarkInfo(name: "DataReplaceLargeBuffer",
    runFunction: run_ReplaceLargeBuffer, tags: skip),

  BenchmarkInfo(name: "DataAppendSequence",
    runFunction: run_AppendSequence, tags: d),

  BenchmarkInfo(name: "DataAppendDataSmallToSmall",
    runFunction: run_AppendDataSmallToSmall, tags: d),
  BenchmarkInfo(name: "DataAppendDataSmallToMedium",
    runFunction: run_AppendDataSmallToMedium, tags: d),
  BenchmarkInfo(name: "DataAppendDataSmallToLarge",
    runFunction: run_AppendDataSmallToLarge, tags: d),
  BenchmarkInfo(name: "DataAppendDataMediumToSmall",
    runFunction: run_AppendDataMediumToSmall, tags: d),
  BenchmarkInfo(name: "DataAppendDataMediumToMedium",
    runFunction: run_AppendDataMediumToMedium, tags: d),
  BenchmarkInfo(name: "DataAppendDataMediumToLarge",
    runFunction: run_AppendDataMediumToLarge, tags: skip),
  BenchmarkInfo(name: "DataAppendDataLargeToSmall",
    runFunction: run_AppendDataLargeToSmall, tags: d),
  BenchmarkInfo(name: "DataAppendDataLargeToMedium",
    runFunction: run_AppendDataLargeToMedium, tags: d),
  BenchmarkInfo(name: "DataAppendDataLargeToLarge",
    runFunction: run_AppendDataLargeToLarge, tags: skip),

  BenchmarkInfo(name: "DataToStringEmpty",
    runFunction: run_DataToStringEmpty, tags: d, legacyFactor: 50),
  BenchmarkInfo(name: "DataToStringSmall",
    runFunction: run_DataToStringSmall, tags: d, legacyFactor: 50),
  BenchmarkInfo(name: "DataToStringMedium",
    runFunction: run_DataToStringMedium, tags: d, legacyFactor: 50),

  BenchmarkInfo(name: "StringToDataEmpty",
    runFunction: run_StringToDataEmpty, tags: d, legacyFactor: 50),
  BenchmarkInfo(name: "StringToDataSmall",
    runFunction: run_StringToDataSmall, tags: d, legacyFactor: 50),
  BenchmarkInfo(name: "StringToDataMedium",
    runFunction: run_StringToDataMedium, tags: d, legacyFactor: 50),
]

enum SampleKind {
  case small
  case medium
  case large
  case veryLarge
  case immutableBacking
}

func sampleData(size: Int) -> Data {
  var data = Data(count: size)
  data.withUnsafeMutableBytes { getRandomBuf(baseAddress: $0, count: size) }
  return data
}

#if os(Linux)
import Glibc
#endif

@inline(__always)
func getRandomBuf(_ arg: UnsafeMutableBufferPointer<UInt8>) {
  #if os(Linux)
  let fd = open("/dev/urandom", O_RDONLY)
  defer { if (fd >= 0) { close(fd) } }
  if fd >= 0 {
    read(fd, arg.baseAddress, arg.count)
  }
  #else
  arc4random_buf(arg.baseAddress, arg.count)
  #endif
}

@inline(__always)
func getRandomBuf(baseAddress: UnsafeMutablePointer<UInt8>, count: Int) {
  #if os(Linux)
  let fd = open("/dev/urandom", O_RDONLY)
  defer { if (fd >= 0) { close(fd) } }
  if fd >= 0 {
    read(fd, baseAddress, count)
  }
  #else
  arc4random_buf(baseAddress, count)
  #endif
}

func sampleBridgedNSData() -> Data {
  let count = 1033
  var bytes = [UInt8](repeating: 0, count: count)
  bytes.withUnsafeMutableBufferPointer {
    getRandomBuf($0)
  }
  let data = NSData(bytes: bytes, length: count)
  return Data(referencing: data)
}

func sampleData(_ type: SampleKind) -> Data {
  switch type {
  case .small: return sampleData(size: 11)
  case .medium: return sampleData(size: 1033)
  case .large: return sampleData(size: 40980)
  case .veryLarge: return sampleData(size: 1024 * 1024 * 1024 + 128)
  case .immutableBacking: return sampleBridgedNSData()
  }

}

func benchmark_AccessBytes(_ N: Int, _ data: Data) {
  for _ in 0..<10000*N {
    data.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) in
      // Ensure that the compiler does not optimize away this call
      blackHole(ptr.pointee)
    }
  }
}

func benchmark_MutateBytes(_ N: Int, _ data_: Data) {
  for _ in 0..<10000*N {
    var data = data_
    data.withUnsafeMutableBytes { (ptr: UnsafeMutablePointer<UInt8>) in
      // Mutate a byte
      ptr.pointee = 42
    }
  }
}

func benchmark_CopyBytes(_ N: Int, _ data: Data) {
  let amount = data.count
  var buffer = UnsafeMutablePointer<UInt8>.allocate(capacity: amount)
  defer { buffer.deallocate() }
  for _ in 0..<10000*N {
    data.copyBytes(to: buffer, from: 0..<amount)
  }
}

func benchmark_AppendBytes(_ N: Int, _ count: Int, _ data_: Data) {
  let bytes = malloc(count).assumingMemoryBound(to: UInt8.self)
  defer { free(bytes) }
  for _ in 0..<10000*N {
    var data = data_
    data.append(bytes, count: count)
  }
}

func benchmark_AppendArray(_ N: Int, _ count: Int, _ data_: Data) {
  var bytes = [UInt8](repeating: 0, count: count)
  bytes.withUnsafeMutableBufferPointer {
    getRandomBuf($0)
  }
  for _ in 0..<10000*N {
    var data = data_
    data.append(contentsOf: bytes)
  }
}

func benchmark_AppendSequence(_ N: Int, _ count: Int, _ data_: Data) {
  let bytes = repeatElement(UInt8(0xA0), count: count)
  for _ in 0..<10000*N {
    var data = data_
    data.append(contentsOf: bytes)
  }
}

func benchmark_Reset(_ N: Int, _ range: Range<Data.Index>, _ data_: Data) {
  for _ in 0..<10000*N {
    var data = data_
    data.resetBytes(in: range)
  }
}

func benchmark_Replace(_ N: Int, _ range: Range<Data.Index>, _ data_: Data, _ replacement: Data) {
  for _ in 0..<10000*N {
    var data = data_
    data.replaceSubrange(range, with: replacement)
  }
}

func benchmark_ReplaceBuffer(_ N: Int, _ range: Range<Data.Index>, _ data_: Data, _ replacement: UnsafeBufferPointer<UInt8>) {
  for _ in 0..<10000*N {
    var data = data_
    data.replaceSubrange(range, with: replacement)
  }
}

func benchmark_AppendData(_ N: Int, _ lhs: Data, _ rhs: Data) {
  var data = lhs
  for _ in 0..<10000*N {
    data = lhs
    data.append(rhs)
  }
}

@inline(never)
public func run_SubscriptSmall(_ N: Int) {
  let data = sampleData(.small)
  let index = 1
  for _ in 0..<10000*N {
    // Ensure that the compiler does not optimize away this call
    blackHole(data[index])
  }
}

@inline(never)
public func run_SubscriptMedium(_ N: Int) {
  let data = sampleData(.medium)
  let index = 521
  for _ in 0..<10000*N {
    // Ensure that the compiler does not optimize away this call
    blackHole(data[index])
  }
}

@inline(never)
public func run_SubscriptLarge(_ N: Int) {
  let data = sampleData(.veryLarge)
  let index = 521
  for _ in 0..<10000*N {
    // Ensure that the compiler does not optimize away this call
    blackHole(data[index])
  }
}

@inline(never)
public func run_CountSmall(_ N: Int) {
  let data = sampleData(.small)
  for _ in 0..<10000*N {
    // Ensure that the compiler does not optimize away this call
    blackHole(data.count)
  }
}

@inline(never)
public func run_CountMedium(_ N: Int) {
  let data = sampleData(.medium)
  for _ in 0..<10000*N {
    // Ensure that the compiler does not optimize away this call
    blackHole(data.count)
  }
}

@inline(never)
public func run_CountLarge(_ N: Int) {
  let data = sampleData(.veryLarge)
  for _ in 0..<10000*N {
    // Ensure that the compiler does not optimize away this call
    blackHole(data.count)
  }
}

@inline(never)
public func run_SetCountSmall(_ N: Int) {
  var data = sampleData(.small)
  let count = data.count + 3
  let orig = data.count
  for _ in 0..<10000*N {
    data.count = count
    data.count = orig
  }
}

@inline(never)
public func run_SetCountMedium(_ N: Int) {
  var data = sampleData(.medium)
  let count = data.count + 100
  let orig = data.count
  for _ in 0..<10000*N {
    data.count = count
    data.count = orig
  }
}

@inline(never)
public func run_SetCountLarge(_ N: Int) {
  var data = sampleData(.large)
  let count = data.count + 100
  let orig = data.count
  for _ in 0..<10000*N {
    data.count = count
    data.count = orig
  }
}

@inline(never)
public func run_AccessBytesSmall(_ N: Int) {
  let data = sampleData(.small)
  benchmark_AccessBytes(N, data)
}


@inline(never)
public func run_AccessBytesMedium(_ N: Int) {
  let data = sampleData(.medium)
  benchmark_AccessBytes(N, data)
}

@inline(never)
public func run_AccessBytesLarge(_ N: Int) {
  let data = sampleData(.veryLarge)
  benchmark_AccessBytes(N, data)
}

@inline(never)
public func run_MutateBytesSmall(_ N: Int) {
  let data = sampleData(.small)
  benchmark_MutateBytes(N, data)
}

@inline(never)
public func run_MutateBytesMedium(_ N: Int) {
  let data = sampleData(.medium)
  benchmark_MutateBytes(N, data)
}

@inline(never)
public func run_MutateBytesLarge(_ N: Int) {
  let data = sampleData(.veryLarge)
  benchmark_MutateBytes(N, data)
}

@inline(never)
public func run_CopyBytesSmall(_ N: Int) {
  let data = sampleData(.small)
  benchmark_CopyBytes(N, data)
}

@inline(never)
public func run_CopyBytesMedium(_ N: Int) {
  let data = sampleData(.medium)
  benchmark_CopyBytes(N, data)
}

@inline(never)
public func run_CopyBytesLarge(_ N: Int) {
  let data = sampleData(.large)
  benchmark_CopyBytes(N, data)
}

@inline(never)
public func run_AppendBytesSmall(_ N: Int) {
  let data = sampleData(.small)
  benchmark_AppendBytes(N, 3, data)
}

@inline(never)
public func run_AppendBytesMedium(_ N: Int) {
  let data = sampleData(.medium)
  benchmark_AppendBytes(N, 809, data)
}

@inline(never)
public func run_AppendBytesLarge(_ N: Int) {
  let data = sampleData(.veryLarge)
  benchmark_AppendBytes(N, 809, data)
}

@inline(never)
public func run_AppendArray(_ N: Int) {
  let data = sampleData(.medium)
  benchmark_AppendArray(N, 809, data)
}

@inline(never)
public func run_Reset(_ N: Int) {
  let data = sampleData(.medium)
  benchmark_Reset(N, 431..<809, data)
}

@inline(never)
public func run_ReplaceSmall(_ N: Int) {
  let data = sampleData(.medium)
  let replacement = sampleData(.small)
  benchmark_Replace(N, 431..<809, data, replacement)
}

@inline(never)
public func run_ReplaceMedium(_ N: Int) {
  let data = sampleData(.medium)
  let replacement = sampleData(.medium)
  benchmark_Replace(N, 431..<809, data, replacement)
}

@inline(never)
public func run_ReplaceLarge(_ N: Int) {
  let data = sampleData(.medium)
  let replacement = sampleData(.large)
  benchmark_Replace(N, 431..<809, data, replacement)
}

@inline(never)
public func run_ReplaceSmallBuffer(_ N: Int) {
  let data = sampleData(.medium)
  let replacement = sampleData(.small)
  let sz = replacement.count
  replacement.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) in
    benchmark_ReplaceBuffer(N, 431..<809, data, UnsafeBufferPointer(start: ptr, count: sz))
  }
}

@inline(never)
public func run_ReplaceMediumBuffer(_ N: Int) {
  let data = sampleData(.medium)
  let replacement = sampleData(.medium)
  let sz = replacement.count
  replacement.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) in
    benchmark_ReplaceBuffer(N, 431..<809, data, UnsafeBufferPointer(start: ptr, count: sz))
  }
}

@inline(never)
public func run_ReplaceLargeBuffer(_ N: Int) {
  let data = sampleData(.medium)
  let replacement = sampleData(.large)
  let sz = replacement.count
  replacement.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) in
    benchmark_ReplaceBuffer(N, 431..<809, data, UnsafeBufferPointer(start: ptr, count: sz))
  }
}

@inline(never)
public func run_AppendSequence(_ N: Int) {
  let data = sampleData(.medium)
  benchmark_AppendSequence(N, 809, data)
}

@inline(never)
public func run_AppendDataSmallToSmall(_ N: Int) {
  let data = sampleData(.small)
  let other = sampleData(.small)
  benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataSmallToMedium(_ N: Int) {
  let data = sampleData(.medium)
  let other = sampleData(.small)
  benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataSmallToLarge(_ N: Int) {
  let data = sampleData(.large)
  let other = sampleData(.small)
  benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataMediumToSmall(_ N: Int) {
  let data = sampleData(.small)
  let other = sampleData(.medium)
  benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataMediumToMedium(_ N: Int) {
  let data = sampleData(.medium)
  let other = sampleData(.medium)
  benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataMediumToLarge(_ N: Int) {
  let data = sampleData(.large)
  let other = sampleData(.medium)
  benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataLargeToSmall(_ N: Int) {
  let data = sampleData(.small)
  let other = sampleData(.large)
  benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataLargeToMedium(_ N: Int) {
  let data = sampleData(.medium)
  let other = sampleData(.large)
  benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataLargeToLarge(_ N: Int) {
  let data = sampleData(.large)
  let other = sampleData(.large)
  benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_createEmpty(_ N: Int) {
  for _ in 0..<100000 * N {
    blackHole(Data())
  }
}

@inline(never)
public func run_createSmall(_ N: Int) {
  for _ in 0..<100000 * N {
    blackHole(sampleData(.small))
  }
}

@inline(never)
public func run_createMedium(_ N: Int) {
  for _ in 0..<10000 * N {
    blackHole(sampleData(.medium))
  }
}

@inline(never)
public func run_createLarge(_ N: Int) {
  blackHole(sampleData(.veryLarge))
}

@inline(never)
public func run_createEmptyArray(_ N: Int) {
  for _ in 0..<100000 * N {
    blackHole(Data([]))
  }
}

@inline(never)
public func run_createSmallArray(_ N: Int) {
  for _ in 0..<100000 * N {
    blackHole(Data([0, 1, 2, 3, 4, 5, 6]))
  }
}

@inline(never)
public func run_createMediumArray(_ N: Int) {
  for _ in 0..<10000 * N {
    blackHole(Data([0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5, 6]))
  }
}

@inline(never)
public func run_DataToStringEmpty(_ N: Int) {
  let d = Data()
  for _ in 0..<200 * N {
    let s = String(decoding: d, as: UTF8.self)
    blackHole(s)
  }
}

@inline(never)
public func run_DataToStringSmall(_ N: Int) {
  let d = Data([0x0D, 0x0A])
  for _ in 0..<200 * N {
    let s = String(decoding: d, as: UTF8.self)
    blackHole(s)
  }
}

@inline(never)
public func run_DataToStringMedium(_ N: Int) {
  let d = Data([0x0D, 0x0A, 0x0D, 0x0A, 0x0D, 0x0A, 0x0D, 0x0A, 0x0D, 0x0A, 0x0D, 0x0A, 0x0D, 0x0A, 0x0D, 0x0A, 0x0D, 0x0A])
  for _ in 0..<200 * N {
    let s = String(decoding: d, as: UTF8.self)
    blackHole(s)
  }
}

@inline(never)
public func run_StringToDataEmpty(_ N: Int) {
  let s = ""
  for _ in 0..<200 * N {
    let d = Data(s.utf8)
    blackHole(d)
  }
}

@inline(never)
public func run_StringToDataSmall(_ N: Int) {
  let s = "\r\n"
  for _ in 0..<200 * N {
    let d = Data(s.utf8)
    blackHole(d)
  }
}

@inline(never)
public func run_StringToDataMedium(_ N: Int) {
  let s = "\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n"
  for _ in 0..<200 * N {
    let d = Data(s.utf8)
    blackHole(d)
  }
}
