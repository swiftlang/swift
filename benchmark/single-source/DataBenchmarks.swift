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
    BenchmarkInfo(name: "Subscript", runFunction: run_Subscript, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "Count", runFunction: run_Count, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "SetCount", runFunction: run_SetCount, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AccessBytes", runFunction: run_AccessBytes, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "MutateBytes", runFunction: run_MutateBytes, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "CopyBytes", runFunction: run_CopyBytes, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendBytes", runFunction: run_AppendBytes, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendArray", runFunction: run_AppendArray, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "Reset", runFunction: run_Reset, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "ReplaceSmall", runFunction: run_ReplaceSmall, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "ReplaceMedium", runFunction: run_ReplaceMedium, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "ReplaceLarge", runFunction: run_ReplaceLarge, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "ReplaceSmallCountable", runFunction: run_ReplaceSmallCountable, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "ReplaceMediumCountable", runFunction: run_ReplaceMediumCountable, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "ReplaceLargeCountable", runFunction: run_ReplaceLargeCountable, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "ReplaceSmallBuffer", runFunction: run_ReplaceSmallBuffer, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "ReplaceMediumBuffer", runFunction: run_ReplaceMediumBuffer, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "ReplaceLargeBuffer", runFunction: run_ReplaceLargeBuffer, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendSequence", runFunction: run_AppendSequence, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendDataSmallToSmall", runFunction: run_AppendDataSmallToSmall, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendDataSmallToMedium", runFunction: run_AppendDataSmallToMedium, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendDataSmallToLarge", runFunction: run_AppendDataSmallToLarge, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendDataMediumToSmall", runFunction: run_AppendDataMediumToSmall, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendDataMediumToMedium", runFunction: run_AppendDataMediumToMedium, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendDataMediumToLarge", runFunction: run_AppendDataMediumToLarge, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendDataLargeToSmall", runFunction: run_AppendDataLargeToSmall, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendDataLargeToMedium", runFunction: run_AppendDataLargeToMedium, tags: [.validation, .api, .Data]),
    BenchmarkInfo(name: "AppendDataLargeToLarge", runFunction: run_AppendDataLargeToLarge, tags: [.validation, .api, .Data]),
]

enum SampleKind {
    case small
    case medium
    case large
    case string
    case immutableBacking
}

func sampleData(size: Int) -> Data {
    var data = Data(count: size)
    data.withUnsafeMutableBytes { arc4random_buf($0, size) }
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

func sampleBridgedNSData() -> Data {
    let count = 1033
    var bytes = [UInt8](repeating: 0, count: count)
    bytes.withUnsafeMutableBufferPointer {
        arc4random_buf($0.baseAddress, $0.count)
    }
    let data = NSData(bytes: bytes, length: count)
    return Data(referencing: data)
}

func sampleData(_ type: SampleKind) -> Data {
    switch type {
    case .small: return sampleData(size: 11)
    case .medium: return sampleData(size: 1033)
    case .large: return sampleData(size: 40980)
    case .string: return sampleString()
    case .immutableBacking: return sampleBridgedNSData()
    default: fatalError()
    }
    
}

func benchmark_Subscript(_ N: Int, _ data: Data, _ index: Data.Index) {
    for _ in 1...N {
        _ = data[index]
    }
}

func benchmark_Count(_ N: Int, _ data: Data) {
    for _ in 1...N {
        _ = data.count
    }
}

func benchmark_SetCount(_ N: Int, _ data_: Data, _ count: Int) {
    var data = data_
    let orig = data.count
    for _ in 1...N {
        data.count = count
        data.count = orig
    }
}

func benchmark_AccessBytes(_ N: Int, _ data: Data) {
    for _ in 1...N {
        data.withUnsafeBytes { (ptr: UnsafePointer<UInt8>) in
            // do nothing here
        }
    }
}

func benchmark_MutateBytes(_ N: Int, _ data_: Data) {
    
    for _ in 1...N {
        var data = data_
        data.withUnsafeMutableBytes { (ptr: UnsafeMutablePointer<UInt8>) in
            // do nothing here
        }
    }
}

func benchmark_CopyBytes(_ N: Int, _ data: Data) {
    let amount = data.count
    var buffer = UnsafeMutablePointer<UInt8>.allocate(capacity: amount)
    defer { buffer.deallocate() }
    for _ in 1...N {
        data.copyBytes(to: buffer, count: amount)
    }
}

func benchmark_StringConversion(_ N: Int, _ data: Data) {
    for _ in 1...N {
        _ = String(data: data, encoding: .utf8)
    }
}

func benchmark_AppendBytes(_ N: Int, _ count: Int, _ data_: Data) {
    let bytes = malloc(count).assumingMemoryBound(to: UInt8.self)
    defer { free(bytes) }
    for _ in 1...N {
        var data = data_
        data.append(bytes, count: count)
    }
}

func benchmark_AppendArray(_ N: Int, _ count: Int, _ data_: Data) {
    var bytes = [UInt8](repeating: 0, count: count)
    bytes.withUnsafeMutableBufferPointer {
        arc4random_buf($0.baseAddress, $0.count)
    }
    for _ in 1...N {
        var data = data_
        data.append(contentsOf: bytes)
    }
}

func benchmark_AppendSequence(_ N: Int, _ count: Int, _ data_: Data) {
    let bytes = repeatElement(UInt8(0xA0), count: count)
    for _ in 1...N {
        var data = data_
        data.append(contentsOf: bytes)
    }
}

func benchmark_Reset(_ N: Int, _ range: Range<Data.Index>, _ data_: Data) {
    for _ in 1...N {
        var data = data_
        data.resetBytes(in: range)
    }
}

func benchmark_Replace(_ N: Int, _ range: Range<Data.Index>, _ data_: Data, _ replacement: Data) {
    for _ in 1...N {
        var data = data_
        data.replaceSubrange(range, with: replacement)
    }
}

func benchmark_ReplaceCountable(_ N: Int, _ range: CountableRange<Data.Index>, _ data_: Data, _ replacement: Data) {
    for _ in 1...N {
        var data = data_
        data.replaceSubrange(range, with: replacement)
    }
}

func benchmark_ReplaceBuffer(_ N: Int, _ range: Range<Data.Index>, _ data_: Data, _ replacement: UnsafeBufferPointer<UInt8>) {
    for _ in 1...N {
        var data = data_
        data.replaceSubrange(range, with: replacement)
    }
}

func benchmark_AppendData(_ N: Int, _ lhs: Data, _ rhs: Data) {
    var data = lhs
    for _ in 1...N {
        data = lhs
        data.append(rhs)
    }
}

@inline(never)
public func run_Subscript(_ N: Int) {
    let data = sampleData(.medium)
    let index = 521
    benchmark_Subscript(N, data, index)
}

@inline(never)
public func run_Count(_ N: Int) {
    let data = sampleData(.medium)
    benchmark_Count(N, data)
}

@inline(never)
public func run_SetCount(_ N: Int) {
    let data = sampleData(.medium)
    let count = data.count + 100
    benchmark_SetCount(N, data, count)
}

@inline(never)
public func run_AccessBytes(_ N: Int) {
    let data = sampleData(.medium)
    benchmark_AccessBytes(N, data)
}

@inline(never)
public func run_MutateBytes(_ N: Int) {
    let data = sampleData(.medium)
    benchmark_MutateBytes(N, data)
}

@inline(never)
public func run_CopyBytes(_ N: Int) {
    let data = sampleData(.medium)
    benchmark_CopyBytes(N, data)
}

@inline(never)
public func run_AppendBytes(_ N: Int) {
    let data = sampleData(.medium)
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
public func run_ReplaceSmallCountable(_ N: Int) {
    let data = sampleData(.medium)
    let replacement = sampleData(.small)
    benchmark_ReplaceCountable(N, 431..<809, data, replacement)
}

@inline(never)
public func run_ReplaceMediumCountable(_ N: Int) {
    let data = sampleData(.medium)
    let replacement = sampleData(.medium)
    benchmark_ReplaceCountable(N, 431..<809, data, replacement)
}

@inline(never)
public func run_ReplaceLargeCountable(_ N: Int) {
    let data = sampleData(.medium)
    let replacement = sampleData(.large)
    benchmark_ReplaceCountable(N, 431..<809, data, replacement)
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
