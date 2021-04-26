//===--- NSStringConversion.swift -----------------------------------------===//
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

// <rdar://problem/19003201>
#if canImport(Darwin)

import TestsUtils
import Foundation
import Darwin
import _Concurrency

@available(macOS 9999, *)
public let AsyncSequenceFoundation = [
  BenchmarkInfo(name: "LoopbackLinesNaive",
                runFunction: run_loopbackLines_naive,
                tags: [.concurrency]
                )
]

//Some bits copy-pasted from Swift System
@available(macOS 9999, *)
struct FileDescriptor: RawRepresentable, Hashable, Codable {
  /// The raw C file handle.
  let rawValue: CInt
  
  /// Creates a strongly-typed file handle from a raw C file handle.
  init(rawValue: CInt) { self.rawValue = rawValue }
  
  func _read(
    into buffer: UnsafeMutableRawBufferPointer,
    retryOnInterrupt: Bool = true
  ) throws -> Result<Int, Errno> {
    valueOrErrno(retryOnInterrupt: retryOnInterrupt) {
      system_read(self.rawValue, buffer.baseAddress, buffer.count)
    }
  }
  
  func read(
    into buffer: UnsafeMutableRawBufferPointer,
    retryOnInterrupt: Bool = true
  ) throws -> Int {
    try _read(into: buffer, retryOnInterrupt: retryOnInterrupt).get()
  }
  
}

@available(macOS 9999, *)
var system_errno: CInt {
  get { Darwin.errno }
  set { Darwin.errno = newValue }
}

@available(macOS 9999, *)
var _EINTR: CInt { 4 }

@available(macOS 9999, *)
struct Errno: RawRepresentable, Error, Hashable, Codable {
  /// The raw C error number.
  let rawValue: CInt
  
  /// Creates a strongly typed error number from a raw C error number.
  init(rawValue: CInt) { self.rawValue = rawValue }
  
  static var current: Errno {
    get { Errno(rawValue: system_errno) }
    set { system_errno = newValue.rawValue }
  }
  
  
  static var interrupted: Errno { Errno(rawValue: _EINTR) }
}

@available(macOS 9999, *)
func valueOrErrno<I: FixedWidthInteger>(
  _ i: I
) -> Result<I, Errno> {
  i == -1 ? .failure(Errno.current) : .success(i)
}

@available(macOS 9999, *)
func system_read(
  _ fd: Int32, _ buf: UnsafeMutableRawPointer!, _ nbyte: Int
) -> Int {
  return read(fd, buf, nbyte)
}

@available(macOS 9999, *)
func valueOrErrno<I: FixedWidthInteger>(
  retryOnInterrupt: Bool, _ f: () -> I
) -> Result<I, Errno> {
  repeat {
    switch valueOrErrno(f()) {
    case .success(let r): return .success(r)
    case .failure(let err):
      guard retryOnInterrupt && err == .interrupted else { return .failure(err) }
      break
    }
  } while true
}

//end of copy-pasted System bits

@available(macOS 9999, *)
actor IOActor {
  func createNSError(posixErrno: Errno, fileDescriptor: FileDescriptor, isReading: Bool = true) -> Error {
    return NSError(domain: NSPOSIXErrorDomain, code: Int(posixErrno.rawValue), userInfo: [:])
  }
  
  func read(from fd: FileDescriptor, into buffer: UnsafeMutableRawBufferPointer) async throws -> Int {
    if buffer.count == 0 { return 0 }
    
    do {
      return try fd.read(into: buffer)
    } catch (let err as Errno) {
      throw createNSError(posixErrno:err, fileDescriptor: fd)
    }
  }
  
  func createFileHandle(reading url: URL) async throws -> FileHandle {
    return try FileHandle(forReadingFrom: url)
  }
  
  static let `default` = IOActor()
}

@available(macOS 9999, *)
extension FileHandle {
  
  struct NaiveAsyncBytes: AsyncSequence {
    typealias Element = UInt8
    typealias AsyncIterator = FileHandle.NaiveAsyncBytes.Iterator
    var handle: FileHandle
    
    internal init(file: FileHandle) {
      handle = file
    }
    
    func makeAsyncIterator() -> Iterator {
      return Iterator(file: handle)
    }
    
    actor Iterator : AsyncSequence, AsyncIteratorProtocol {
      typealias Element = UInt8
      typealias AsyncIterator = FileHandle.NaiveAsyncBytes.Iterator
      var handle: FileHandle
      var buffer = UnsafeMutableRawBufferPointer.allocate(byteCount: 16384, alignment: MemoryLayout<AnyObject>.alignment)
      var bufferRange = 0..<0
      var descriptor: FileDescriptor
      var finished = false
      
      internal init(file: FileHandle) {
        handle = file
        descriptor = FileDescriptor(rawValue: handle.fileDescriptor)
      }
      
      @inline(never)
      func reloadBufferAndNext() async throws -> UInt8? {
        try Task.checkCancellation()
        if finished {
          return nil
        }
        bufferRange = 0..<0
        do {
          let readCount = try await IOActor.default.read(from: descriptor, into: buffer)
          if readCount == 0 {
            finished = true
          }
          bufferRange = 0 ..< readCount
        } catch {
          finished = true
          throw error
        }
        return try await next()
      }
      
      @inline(__always)
      func next() async throws -> UInt8? {
        if !bufferRange.isEmpty {
          defer {
            bufferRange = (bufferRange.lowerBound + MemoryLayout<UInt8>.stride) ..< bufferRange.upperBound
          }
          return buffer.load(fromByteOffset: bufferRange.lowerBound, as: UInt8.self)
        }
        return try await reloadBufferAndNext()
      }
      
      @actorIndependent
      func makeAsyncIterator() -> Iterator {
        return self
      }
      
      deinit {
        buffer.deallocate()
      }
    }
  }
  
  var naiveBytes: NaiveAsyncBytes {
    return NaiveAsyncBytes(file: self)
  }
}

@available(macOS 9999, *)
struct NaiveAsyncLineSequence<Base: AsyncSequence>: AsyncSequence, AsyncIteratorProtocol where Base.Element == UInt8 {
  typealias AsyncIterator = NaiveAsyncLineSequence<Base>
  typealias Element = String
  
  var remaining: NaiveAsyncCharacterSequence<Base>.AsyncIterator
  var inProgressResult = ""
  
  mutating func next() async throws -> String? {
    while let char = try await remaining.next() {
      if char.isNewline {
        defer {
          inProgressResult = ""
        }
        if inProgressResult.isEmpty {
          continue
        }
        return inProgressResult
      }
      inProgressResult.append(char)
    }
    if inProgressResult.isEmpty {
      return nil
    }
    defer {
      inProgressResult = ""
    }
    return inProgressResult
  }
  
  func makeAsyncIterator() -> NaiveAsyncLineSequence<Base> {
    return self
  }
  
  internal init(underlyingSequence: Base) {
    remaining = NaiveAsyncCharacterSequence(underlyingSequence: underlyingSequence).makeAsyncIterator()
  }
}

@available(macOS 9999, *)
struct NaiveAsyncUnicodeScalarSequence<Base: AsyncSequence>: AsyncSequence, AsyncIteratorProtocol where Base.Element == UInt8 {
  typealias AsyncIterator = NaiveAsyncUnicodeScalarSequence<Base>
  typealias Element = UnicodeScalar
  
  var remaining: Base.AsyncIterator
  var current = [UInt8]()
  
  mutating func nextMultiByte(_ first: UInt8) async throws
  -> UnicodeScalar? {
    current.append(first)
    
    while let next = try await remaining.next() {
      if UTF8.isContinuation(next) {
        current.append(next)
      } else {
        defer {
          current.removeAll(keepingCapacity: true)
          current.append(next)
        }
        return String(decoding: current, as: UTF8.self).unicodeScalars.first
      }
    }
    return String(decoding: current, as: UTF8.self).unicodeScalars.first
  }
  
  @inline(__always)
  mutating func next() async throws -> UnicodeScalar? {
    if let byte = current.count > 0 ? current.removeFirst() : try await remaining.next() {
      if UTF8.isASCII(byte) {
        return UnicodeScalar(byte)
      }
      return try await nextMultiByte(byte)
    }
    return nil
  }
  
  func makeAsyncIterator() -> NaiveAsyncUnicodeScalarSequence<Base> {
    return self
  }
  
  internal init(underlyingSequence: Base) {
    remaining = underlyingSequence.makeAsyncIterator()
  }
}

@available(macOS 9999, *)
struct NaiveAsyncCharacterSequence<Base: AsyncSequence>: AsyncSequence, AsyncIteratorProtocol where Base.Element == UInt8 {
  typealias AsyncIterator = NaiveAsyncCharacterSequence<Base>
  typealias Element = Character
  
  var remaining: NaiveAsyncUnicodeScalarSequence<Base>.AsyncIterator
  var accumulator = ""
  
  @inline(__always)
  mutating func next() async throws -> Character? {
    while let scalar = try await remaining.next() {
      accumulator.unicodeScalars.append(scalar)
      if accumulator.count > 1 {
        return accumulator.removeFirst()
      }
    }
    return accumulator.count > 0 ? accumulator.removeFirst() : nil
  }
  
  func makeAsyncIterator() -> NaiveAsyncCharacterSequence<Base> {
    return self
  }
  
  internal init(underlyingSequence: Base) {
    remaining = NaiveAsyncUnicodeScalarSequence(underlyingSequence: underlyingSequence).makeAsyncIterator()
  }
}

@available(macOS 9999, *)
extension AsyncSequence where Self.Element == UInt8 {
  var naiveLines: NaiveAsyncLineSequence<Self> {
    NaiveAsyncLineSequence(underlyingSequence: self)
  }
  
  var naiveCharacters: NaiveAsyncCharacterSequence<Self> {
    NaiveAsyncCharacterSequence(underlyingSequence: self)
  }
  
  var naiveUnicodeScalars: NaiveAsyncUnicodeScalarSequence<Self> {
    NaiveAsyncUnicodeScalarSequence(underlyingSequence: self)
  }
}

/*
 This version doesn't use custom executors, and uses a simplistic implementation
 of Async{Line, Character, UnicodeScalar}Sequence
 */
@available(macOS 9999, *)
public func run_loopbackLines_naive(_ N: Int) async {
  let pipe = Pipe()
  let reader = pipe.fileHandleForReading
  let writer = pipe.fileHandleForWriting
  //short string because we're not using the fast line breaker, and want to
  //focus on AsyncSequence overhead
  let lines = "a\nb".data(using: .utf8)!
  
  let handle = Task.runDetached {
    for _ in 0..<N {
      writer.write(lines)
    }
    try! writer.close()
  }
  
  do {
    for try await line in reader.naiveBytes.naiveLines {
      blackHole(line)
    }
    try reader.close()
  } catch {
    
  }
  //wait for everything before moving to the next iteration
  let _ = try! await handle.get()
}

#endif
