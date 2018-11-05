//===--- DataBenchmarks.swift ---------------------------------------------------===//
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

public let DataBenchmarks = [
    BenchmarkInfo(name: "DataSubscript", runFunction: run_Subscript, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataCount", runFunction: run_Count, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataSetCount", runFunction: run_SetCount, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAccessBytes", runFunction: run_AccessBytes, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataMutateBytes", runFunction: run_MutateBytes, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataCopyBytes", runFunction: run_CopyBytes, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendBytes", runFunction: run_AppendBytes, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendArray", runFunction: run_AppendArray, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataReset", runFunction: run_Reset, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataReplaceSmall", runFunction: run_ReplaceSmall, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataReplaceMedium", runFunction: run_ReplaceMedium, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataReplaceLarge", runFunction: run_ReplaceLarge, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataReplaceSmallBuffer", runFunction: run_ReplaceSmallBuffer, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataReplaceMediumBuffer", runFunction: run_ReplaceMediumBuffer, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataReplaceLargeBuffer", runFunction: run_ReplaceLargeBuffer, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendSequence", runFunction: run_AppendSequence, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendDataSmallToSmall", runFunction: run_AppendDataSmallToSmall, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendDataSmallToMedium", runFunction: run_AppendDataSmallToMedium, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendDataSmallToLarge", runFunction: run_AppendDataSmallToLarge, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendDataMediumToSmall", runFunction: run_AppendDataMediumToSmall, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendDataMediumToMedium", runFunction: run_AppendDataMediumToMedium, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendDataMediumToLarge", runFunction: run_AppendDataMediumToLarge, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendDataLargeToSmall", runFunction: run_AppendDataLargeToSmall, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendDataLargeToMedium", runFunction: run_AppendDataLargeToMedium, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "DataAppendDataLargeToLarge", runFunction: run_AppendDataLargeToLarge, tags: [.validation, .api, .Data]),
]

let small = sampleData(.small)
let medium = sampleData(.medium)

enum SampleKind {
    case small
    case medium
    case large
    case veryLarge
    case string
    case immutableBacking
}

func sampleData(size: Int) -> Data {
    var data = Data(count: size)
    data.withUnsafeMutableBytes { getRandomBuf(baseAddress: $0, count: size) }
    return data
}

func sampleString() -> Data {
    let bytes: [UInt8] = [
        0x4c,0x6f,0x72,0x65, 0x6d,0x20,0x69,0x70,  0x73,0x75,0x6d,0x20, 0x64,0x6f,0x6c,0x6f,
        0x72,0x20,0x73,0x69, 0x74,0x20,0x61,0x6d,  0x65,0x74,0x2c,0x20, 0x63,0x6f,0x6e,0x73,
        0x65,0x63,0x74,0x65, 0x74,0x75,0x72,0x20,  0x61,0x64,0x69,0x70, 0x69,0x73,0x69,0x63,
        0x69,0x6e,0x67,0x20, 0x65,0x6c,0x69,0x74,  0x2c,0x20,0x73,0x65, 0x64,0x20,0x64,0x6f,
        0x20,0x65,0x69,0x75, 0x73,0x6d,0x6f,0x64,  0x0a,0x74,0x65,0x6d, 0x70,0x6f,0x72,0x20,
        0x69,0x6e,0x63,0x69, 0x64,0x69,0x64,0x75,  0x6e,0x74,0x20,0x75, 0x74,0x20,0x6c,0x61,
        0x62,0x6f,0x72,0x65, 0x20,0x65,0x74,0x20,  0x64,0x6f,0x6c,0x6f, 0x72,0x65,0x20,0x6d,
        0x61,0x67,0x6e,0x61, 0x20,0x61,0x6c,0x69,  0x71,0x75,0x61,0x2e, 0x20,0x55,0x74,0x20,
        0x65,0x6e,0x69,0x6d, 0x20,0x61,0x64,0x20,  0x6d,0x69,0x6e,0x69, 0x6d,0x20,0x76,0x65,
        0x6e,0x69,0x61,0x6d, 0x2c,0x0a,0x71,0x75,  0x69,0x73,0x20,0x6e, 0x6f,0x73,0x74,0x72,
        0x75,0x64,0x20,0x65, 0x78,0x65,0x72,0x63,  0x69,0x74,0x61,0x74, 0x69,0x6f,0x6e,0x20,
        0x75,0x6c,0x6c,0x61, 0x6d,0x63,0x6f,0x20,  0x6c,0x61,0x62,0x6f, 0x72,0x69,0x73,0x20,
        0x6e,0x69,0x73,0x69, 0x20,0x75,0x74,0x20,  0x61,0x6c,0x69,0x71, 0x75,0x69,0x70,0x20,
        0x65,0x78,0x20,0x65, 0x61,0x20,0x63,0x6f,  0x6d,0x6d,0x6f,0x64, 0x6f,0x0a,0x63,0x6f,
        0x6e,0x73,0x65,0x71, 0x75,0x61,0x74,0x2e,  0x20,0x44,0x75,0x69, 0x73,0x20,0x61,0x75,
        0x74,0x65,0x20,0x69, 0x72,0x75,0x72,0x65,  0x20,0x64,0x6f,0x6c, 0x6f,0x72,0x20,0x69,
        0x6e,0x20,0x72,0x65, 0x70,0x72,0x65,0x68,  0x65,0x6e,0x64,0x65, 0x72,0x69,0x74,0x20,
        0x69,0x6e,0x20,0x76, 0x6f,0x6c,0x75,0x70,  0x74,0x61,0x74,0x65, 0x20,0x76,0x65,0x6c,
        0x69,0x74,0x20,0x65, 0x73,0x73,0x65,0x0a,  0x63,0x69,0x6c,0x6c, 0x75,0x6d,0x20,0x64,
        0x6f,0x6c,0x6f,0x72, 0x65,0x20,0x65,0x75,  0x20,0x66,0x75,0x67, 0x69,0x61,0x74,0x20,
        0x6e,0x75,0x6c,0x6c, 0x61,0x20,0x70,0x61,  0x72,0x69,0x61,0x74, 0x75,0x72,0x2e,0x20,
        0x45,0x78,0x63,0x65, 0x70,0x74,0x65,0x75,  0x72,0x20,0x73,0x69, 0x6e,0x74,0x20,0x6f,
        0x63,0x63,0x61,0x65, 0x63,0x61,0x74,0x20,  0x63,0x75,0x70,0x69, 0x64,0x61,0x74,0x61,
        0x74,0x20,0x6e,0x6f, 0x6e,0x0a,0x70,0x72,  0x6f,0x69,0x64,0x65, 0x6e,0x74,0x2c,0x20,
        0x73,0x75,0x6e,0x74, 0x20,0x69,0x6e,0x20,  0x63,0x75,0x6c,0x70, 0x61,0x20,0x71,0x75,
        0x69,0x20,0x6f,0x66, 0x66,0x69,0x63,0x69,  0x61,0x20,0x64,0x65, 0x73,0x65,0x72,0x75,
        0x6e,0x74,0x20,0x6d, 0x6f,0x6c,0x6c,0x69,  0x74,0x20,0x61,0x6e, 0x69,0x6d,0x20,0x69,
        0x64,0x20,0x65,0x73, 0x74,0x20,0x6c,0x61,  0x62,0x6f,0x72,0x75, 0x6d,0x2e,0x0a,0x00]
    return Data(bytes: bytes)
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
    case .string: return sampleString()
    case .immutableBacking: return sampleBridgedNSData()
    }
}

func benchmark_AccessBytes(_ N: Int, _ data: Data) {
    for _ in 1...10000*N {
        data.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) in
            // Ensure that the compiler does not optimize away this call
            blackHole(ptr.pointee)
        }
    }
}

func benchmark_MutateBytes(_ N: Int, _ data_: Data) {
    for _ in 1...10000*N {
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
    for _ in 1...10000*N {
        data.copyBytes(to: buffer, count: amount)
    }
}

func benchmark_AppendBytes(_ N: Int, _ count: Int, _ data_: Data) {
    let bytes = malloc(count).assumingMemoryBound(to: UInt8.self)
    defer { free(bytes) }
    for _ in 1...10000*N {
        var data = data_
        data.append(bytes, count: count)
    }
}

func benchmark_AppendArray(_ N: Int, _ count: Int, _ data_: Data) {
    var bytes = [UInt8](repeating: 0, count: count)
    bytes.withUnsafeMutableBufferPointer {
        getRandomBuf($0)
    }
    for _ in 1...10000*N {
        var data = data_
        data.append(contentsOf: bytes)
    }
}

func benchmark_AppendSequence(_ N: Int, _ count: Int, _ data_: Data) {
    let bytes = repeatElement(UInt8(0xA0), count: count)
    for _ in 1...10000*N {
        var data = data_
        data.append(contentsOf: bytes)
    }
}

func benchmark_Reset(_ N: Int, _ range: Range<Data.Index>, _ data_: Data) {
    for _ in 1...10000*N {
        var data = data_
        data.resetBytes(in: range)
    }
}

func benchmark_Replace(_ N: Int, _ range: Range<Data.Index>, _ data_: Data, _ replacement: Data) {
    for _ in 1...10000*N {
        var data = data_
        data.replaceSubrange(range, with: replacement)
    }
}

func benchmark_ReplaceBuffer(_ N: Int, _ range: Range<Data.Index>, _ data_: Data, _ replacement: UnsafeBufferPointer<UInt8>) {
    for _ in 1...10000*N {
        var data = data_
        data.replaceSubrange(range, with: replacement)
    }
}

func benchmark_AppendData(_ N: Int, _ lhs: Data, _ rhs: Data) {
    var data = lhs
    for _ in 1...10000*N {
        data = lhs
        data.append(rhs)
    }
}

@inline(never)
public func run_Subscript(_ N: Int) {
    let data = medium
    let index = 521
    for _ in 1...10000*N {
        // Ensure that the compiler does not optimize away this call
        blackHole(data[index])
    }
}

@inline(never)
public func run_Count(_ N: Int) {
    let data = medium
    for _ in 1...10000*N {
        // Ensure that the compiler does not optimize away this call
        blackHole(data.count)
    }
}

@inline(never)
public func run_SetCount(_ N: Int) {
    let data = medium
    let count = data.count + 100
    var otherData = data
    let orig = data.count
    for _ in 1...10000*N {
        otherData.count = count
        otherData.count = orig
    }
}

@inline(never)
public func run_AccessBytes(_ N: Int) {
    let data = medium
    benchmark_AccessBytes(N, data)
}

@inline(never)
public func run_MutateBytes(_ N: Int) {
    let data = medium
    benchmark_MutateBytes(N, data)
}

@inline(never)
public func run_CopyBytes(_ N: Int) {
    let data = medium
    benchmark_CopyBytes(N, data)
}

@inline(never)
public func run_AppendBytes(_ N: Int) {
    let data = medium
    benchmark_AppendBytes(N, 809, data)
}

@inline(never)
public func run_AppendArray(_ N: Int) {
    let data = medium
    benchmark_AppendArray(N, 809, data)
}

@inline(never)
public func run_Reset(_ N: Int) {
    let data = medium
    benchmark_Reset(N, 431..<809, data)
}

@inline(never)
public func run_ReplaceSmall(_ N: Int) {
    let data = medium
    let replacement = small
    benchmark_Replace(N, 431..<809, data, replacement)
}

@inline(never)
public func run_ReplaceMedium(_ N: Int) {
    let data = medium
    let replacement = medium
    benchmark_Replace(N, 431..<809, data, replacement)
}

@inline(never)
public func run_ReplaceLarge(_ N: Int) {
    let data = medium
    let replacement = sampleData(.large)
    benchmark_Replace(N, 431..<809, data, replacement)
}

@inline(never)
public func run_ReplaceSmallBuffer(_ N: Int) {
    let data = medium
    let replacement = small
    let sz = replacement.count
    replacement.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) in
        benchmark_ReplaceBuffer(N, 431..<809, data, UnsafeBufferPointer(start: ptr, count: sz))
    }
}

@inline(never)
public func run_ReplaceMediumBuffer(_ N: Int) {
    let data = medium
    let replacement = medium
    let sz = replacement.count
    replacement.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) in
        benchmark_ReplaceBuffer(N, 431..<809, data, UnsafeBufferPointer(start: ptr, count: sz))
    }
}

@inline(never)
public func run_ReplaceLargeBuffer(_ N: Int) {
    let data = medium
    let replacement = sampleData(.large)
    let sz = replacement.count
    replacement.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) in
        benchmark_ReplaceBuffer(N, 431..<809, data, UnsafeBufferPointer(start: ptr, count: sz))
    }
}

@inline(never)
public func run_AppendSequence(_ N: Int) {
    let data = medium
    benchmark_AppendSequence(N, 809, data)
}

@inline(never)
public func run_AppendDataSmallToSmall(_ N: Int) {
    let data = small
    let other = small
    benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataSmallToMedium(_ N: Int) {
    let data = medium
    let other = small
    benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataSmallToLarge(_ N: Int) {
    let data = sampleData(.large)
    let other = small
    benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataMediumToSmall(_ N: Int) {
    let data = small
    let other = medium
    benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataMediumToMedium(_ N: Int) {
    let data = medium
    let other = medium
    benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataMediumToLarge(_ N: Int) {
    let data = sampleData(.large)
    let other = medium
    benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataLargeToSmall(_ N: Int) {
    let data = small
    let other = sampleData(.large)
    benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataLargeToMedium(_ N: Int) {
    let data = medium
    let other = sampleData(.large)
    benchmark_AppendData(N, data, other)
}

@inline(never)
public func run_AppendDataLargeToLarge(_ N: Int) {
    let data = sampleData(.large)
    let other = sampleData(.large)
    benchmark_AppendData(N, data, other)
}
