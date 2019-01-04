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

public let DataBenchmarks = [
  BenchmarkInfo(name: "DataCreateEmpty",
    runFunction: { for _ in 0..<$0*100_000 { blackHole(Data()) } }, tags: d),
  BenchmarkInfo(name: "DataCreateSmall",
    runFunction: { for _ in 0..<$0*100_000 { blackHole(sampleData(.small)) } },
    tags: d),
  BenchmarkInfo(name: "DataCreateMedium",
    runFunction: { for _ in 0..<$0*10_000 { blackHole(sampleData(.medium)) } },
    tags: d),

  BenchmarkInfo(name: "DataCreateEmptyArray",
    runFunction: { for _ in 0..<$0*100_000 { blackHole(Data([])) } }, tags: d),
  BenchmarkInfo(name: "DataCreateSmallArray",
    runFunction: { for _ in 0..<$0*100_000 { blackHole(Data(
      [0, 1, 2, 3, 4, 5, 6])) } }, tags: d),
  BenchmarkInfo(name: "DataCreateMediumArray",
    runFunction: { for _ in 0..<$0*10_000 { blackHole(Data([
      0, 1, 2, 3, 4, 5, 6,
      0, 1, 2, 3, 4, 5, 6,
      0, 1, 2, 3, 4, 5, 6,
      0, 1, 2, 3, 4, 5, 6,
    ])) } }, tags: d),

  BenchmarkInfo(name: "DataSubscriptSmall",
    runFunction: { for _ in 0..<$0*10_000 { blackHole(small[1]) } }, tags: d),
  BenchmarkInfo(name: "DataSubscriptMedium",
    runFunction: { for _ in 0..<$0*10_000 { blackHole(medium[521]) } }, tags: d),

  BenchmarkInfo(name: "DataCountSmall",
    runFunction: { count($0, data: small) }, tags: d),
  BenchmarkInfo(name: "DataCountMedium",
    runFunction: { count($0, data: medium) }, tags: d),

  BenchmarkInfo(name: "DataSetCountSmall",
    runFunction: { setCount($0, data: small, extra: 3) }, tags: d),
  BenchmarkInfo(name: "DataSetCountMedium",
    runFunction: { setCount($0, data: medium, extra: 100) }, tags: d),

  BenchmarkInfo(name: "DataAccessBytesSmall",
    runFunction: { withUnsafeBytes($0, data: small) }, tags: d),
  BenchmarkInfo(name: "DataAccessBytesMedium",
    runFunction: { withUnsafeBytes($0, data: medium) }, tags: d),

  BenchmarkInfo(name: "DataMutateBytesSmall",
    runFunction: { withUnsafeMutableBytes($0, data: small) }, tags: d),
  BenchmarkInfo(name: "DataMutateBytesMedium",
    runFunction: { withUnsafeMutableBytes($0, data: medium) }, tags: d),

  BenchmarkInfo(name: "DataCopyBytesSmall",
    runFunction: { copyBytes($0, data: small) }, tags: d),
  BenchmarkInfo(name: "DataCopyBytesMedium",
    runFunction: { copyBytes($0, data: medium) }, tags: d),

  BenchmarkInfo(name: "DataAppendBytesSmall",
    runFunction: { append($0, bytes: 3, to: small) }, tags: d),
  BenchmarkInfo(name: "DataAppendBytesMedium",
    runFunction: { append($0, bytes: 809, to: medium) }, tags: d),

  BenchmarkInfo(name: "DataAppendArray",
    runFunction: { append($0, arraySize: 809, to: medium) }, tags: d),

  BenchmarkInfo(name: "DataReset",
    runFunction: { resetBytes($0, in: 431..<809, data: medium) }, tags: d),

  BenchmarkInfo(name: "DataReplaceSmall", runFunction: {
    replace($0, data: medium, subrange:431..<809, with: small) }, tags: d),
  BenchmarkInfo(name: "DataReplaceMedium", runFunction: {
    replace($0, data: medium, subrange:431..<809, with: medium) }, tags: d),
  BenchmarkInfo(name: "DataReplaceLarge", runFunction: {
    replace($0, data: medium, subrange:431..<809, with: large) }, tags: d),

  BenchmarkInfo(name: "DataReplaceSmallBuffer", runFunction: {
    replaceBuffer($0, data: medium, subrange:431..<809, with: small) },
    tags: d),
  BenchmarkInfo(name: "DataReplaceMediumBuffer", runFunction: {
    replaceBuffer($0, data: medium, subrange:431..<809, with: medium) },
    tags: d),
  BenchmarkInfo(name: "DataReplaceLargeBuffer", runFunction: {
    replaceBuffer($0, data: medium, subrange:431..<809, with: large) },
    tags: d),

  BenchmarkInfo(name: "DataAppendSequence",
    runFunction: { append($0, sequenceLength: 809, to: medium) }, tags: d),

  BenchmarkInfo(name: "DataAppendDataSmallToSmall",
    runFunction: { append($0, data: small, to: small) }, tags: d),
  BenchmarkInfo(name: "DataAppendDataSmallToMedium",
    runFunction: { append($0, data: small, to: medium) }, tags: d),
  BenchmarkInfo(name: "DataAppendDataSmallToLarge",
    runFunction: { append($0, data: small, to: large) }, tags: d),
  BenchmarkInfo(name: "DataAppendDataMediumToSmall",
    runFunction: { append($0, data: medium, to: small) }, tags: d),
  BenchmarkInfo(name: "DataAppendDataMediumToMedium",
    runFunction: { append($0, data: medium, to: medium) }, tags: d),
  BenchmarkInfo(name: "DataAppendDataMediumToLarge",
    runFunction: { append($0, data: medium, to: large) }, tags: d),
  BenchmarkInfo(name: "DataAppendDataLargeToSmall",
    runFunction: { append($0, data: large, to: small) }, tags: d),
  BenchmarkInfo(name: "DataAppendDataLargeToMedium",
    runFunction: { append($0, data: large, to: medium) }, tags: d),
  BenchmarkInfo(name: "DataAppendDataLargeToLarge",
    runFunction: { append($0, data: large, to: large) }, tags: d),

  BenchmarkInfo(name: "DataToStringEmpty",
    runFunction: { string($0, from: emptyData) }, tags: d, legacyFactor: 50),
  BenchmarkInfo(name: "DataToStringSmall",
    runFunction: { string($0, from: smallData) }, tags: d, legacyFactor: 50),
  BenchmarkInfo(name: "DataToStringMedium",
    runFunction: { string($0, from: mediumData) }, tags: d, legacyFactor: 50),

  BenchmarkInfo(name: "StringToDataEmpty",
    runFunction: { data($0, from: emptyString) }, tags: d, legacyFactor: 50),
  BenchmarkInfo(name: "StringToDataSmall",
    runFunction: { data($0, from: smallString) }, tags: d, legacyFactor: 50),
  BenchmarkInfo(name: "StringToDataMedium",
    runFunction: { data($0, from: mediumString) }, tags: d, legacyFactor: 50),
]

let emptyString = ""
let smallString = "\r\n"
let mediumString = "\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n"
let emptyData = Data()
let smallData = Data(smallString.utf8)
let mediumData = Data(mediumString.utf8)

let small = sampleData(.small)
let medium = sampleData(.medium)
let large = sampleData(.large)

enum SampleKind {
  case small
  case medium
  case large
  case veryLarge
  case immutableBacking
}

func fillBuffer(_ buffer: UnsafeMutableBufferPointer<UInt8>) {
  for i in buffer.indices {
    buffer[i] = UInt8(truncatingIfNeeded: i)
  }
}

func sampleData(size: Int) -> Data {
  var data = Data(count: size)
  data.withUnsafeMutableBytes { (bytes: UnsafeMutablePointer<UInt8>) -> () in
    for i in 0..<size {
      bytes[i] = UInt8(truncatingIfNeeded: i)
    }
  }
  return data
}

func sampleBridgedNSData() -> Data {
  let count = 1033
  var bytes = [UInt8](repeating: 0, count: count)
  bytes.withUnsafeMutableBufferPointer {
    fillBuffer($0)
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

@inline(never)
func withUnsafeBytes(_ N: Int, data: Data) {
  for _ in 0..<10000*N {
    data.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) in
      blackHole(ptr.pointee)
    }
  }
}

@inline(never)
func withUnsafeMutableBytes(_ N: Int, data: Data) {
  for _ in 0..<10000*N {
    var copy = data
    copy.withUnsafeMutableBytes { (ptr: UnsafeMutablePointer<UInt8>) in
      // Mutate a byte
      ptr.pointee = 42
    }
  }
}

@inline(never)
func copyBytes(_ N: Int, data: Data) {
  let amount = data.count
  var buffer = UnsafeMutablePointer<UInt8>.allocate(capacity: amount)
  defer { buffer.deallocate() }
  for _ in 0..<10000*N {
    data.copyBytes(to: buffer, from: 0..<amount)
  }
}

@inline(never)
func append(_ N: Int, bytes count: Int, to data: Data) {
  let bytes = malloc(count).assumingMemoryBound(to: UInt8.self)
  defer { free(bytes) }
  for _ in 0..<10000*N {
    var copy = data
    copy.append(bytes, count: count)
  }
}

@inline(never)
func append(_ N: Int, arraySize: Int, to data: Data) {
  var bytes = [UInt8](repeating: 0, count: arraySize)
  bytes.withUnsafeMutableBufferPointer {
    fillBuffer($0)
  }
  for _ in 0..<10000*N {
    var copy = data
    copy.append(contentsOf: bytes)
  }
}

@inline(never)
func append(_ N: Int, sequenceLength: Int, to data: Data) {
  let bytes = repeatElement(UInt8(0xA0), count: sequenceLength)
  for _ in 0..<10000*N {
    var copy = data
    copy.append(contentsOf: bytes)
  }
}

@inline(never)
func resetBytes(_ N: Int, in range: Range<Data.Index>, data: Data) {
  for _ in 0..<10000*N {
    var copy = data
    copy.resetBytes(in: range)
  }
}

@inline(never)
func replace(
  _ N: Int,
  data: Data,
  subrange range: Range<Data.Index>,
  with replacement: Data
) {
  for _ in 0..<10000*N {
    var copy = data
    copy.replaceSubrange(range, with: replacement)
  }
}

@inline(never)
func replaceBuffer(
  _ N: Int,
  data: Data,
  subrange range: Range<Data.Index>,
  with replacement: Data
) {
  replacement.withUnsafeBytes { (bytes: UnsafePointer<UInt8>) in
    let buffer = UnsafeBufferPointer(start: bytes, count: replacement.count)

    for _ in 0..<10000*N {
      var copy = data
      copy.replaceSubrange(range, with: buffer)
    }
  }
}

@inline(never)
func append(_ N: Int, data: Data, to target: Data) {
  var copy: Data
  for _ in 0..<10000*N {
    copy = target
    copy.append(data)
  }
}

@inline(never)
public func count(_ N: Int, data: Data) {
  for _ in 0..<10000*N {
    blackHole(data.count)
  }
}

@inline(never)
public func setCount(_ N: Int, data: Data, extra: Int) {
  var copy = data
  let count = data.count + extra
  let orig = data.count
  for _ in 0..<10000*N {
    copy.count = count
    copy.count = orig
  }
}

@inline(never)
public func string(_ N: Int, from data: Data) {
  for _ in 0..<200 * N {
    blackHole(String(decoding: data, as: UTF8.self))
  }
}

@inline(never)
public func data(_ N: Int, from string: String) {
  for _ in 0..<200 * N {
    blackHole(Data(string.utf8))
  }
}
