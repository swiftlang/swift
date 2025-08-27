// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/Library.swift -g -enable-experimental-feature Embedded -enable-experimental-feature Lifetimes -c -parse-as-library -o %t/Library.o -emit-module
// RUN: %target-swift-frontend -I %t %t/Application.swift -g -enable-experimental-feature Embedded -enable-experimental-feature Lifetimes -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Lifetimes

//--- Library.swift
@safe public struct UniqueBuffer<Element: ~Copyable>: ~Copyable {
    @usableFromInline
    let buffer: UnsafeMutableBufferPointer<Element>

    private init(_uninitializedCount count: Int) {
      buffer = UnsafeMutableBufferPointer.allocate(capacity: count)
    }

    @inline(__always)
    @_alwaysEmitIntoClient
    deinit {
        buffer.deinitialize().deallocate()
    }

    /// Allocate a new buffer with `count` elements and call the given `body` function to produce an element
    /// for each entry.
    public init<E>(count: Int, body: (Int) throws(E) -> Element) throws(E) {
        self.init(_uninitializedCount: count)

        for i in 0..<count {
            do throws(E) {
                buffer[i] = try body(i)
            } catch {
                // The closure threw an error. We need to deinitialize every element we've initialized up to this point.
                for j in 0 ..< i {
                    buffer.deinitializeElement(at: j)
                }

                throw error
            }
        }
    }

    /// The number of elements in the buffer.
    public var count: Int { buffer.count }

    /// Access the ith element in the buffer.
    public subscript(_ i: Index) -> Element {
        unsafeAddress {
            precondition(i >= 0 && i < count)
            return UnsafePointer(buffer.baseAddress! + i)
        }

        unsafeMutableAddress {
            precondition(i >= 0 && i < count)
            return buffer.baseAddress! + i
        }
    }

    /// Index into this data structure.
    public typealias Index = Int

    /// Indices into this buffer.
    public var indices: Range<Int> { 0..<count }

    /// Produce a span covering all of the elements in the buffer.
    public var span: Span<Element> {
        @_lifetime(borrow self)
        borrowing get {
            buffer.span
        }
    }

    /// Produce a mutable span covering all of the elements in the buffer.
    public var mutableSpan: MutableSpan<Element> {
        @_lifetime(&self)
        mutating get {
            buffer.mutableSpan
        }
    }

    /// Run the body closure with an unsafe buffer pointer referencing the storage of this unique buffer.
    /// 
    /// Clients should prefer the `mutableSpan` property, which provides memory safety.
    @unsafe public mutating func withUnsafeMutableBufferPointer<T, E>(_ body: (UnsafeMutableBufferPointer<Element>) throws(E) -> T) throws(E) -> T {
        try body(buffer)
    }
}

extension UniqueBuffer {
    /// Allocate a buffer with `count` elements, all of which are a copy of `Element`.
    public init(repeating element: Element, count: Int) {
        self.init(_uninitializedCount: count)
        buffer.initialize(repeating: element)
    }

    /// Allocate a buffer that contains a copy of the elements in the given collection.
    public init(_ collection: some Collection<Element>) {
        self.init(_uninitializedCount: collection.count)
        _ = buffer.initialize(fromContentsOf: collection)
    }
}

public enum BufferWrapper: ~Copyable {
case buffer(UniqueBuffer<Int>)
case empty
}

extension BufferWrapper {
  public init(repeating: Int, count: Int) {
    self = .buffer(UniqueBuffer<Int>(repeating: 17, count: 15))
  }

  public var count: Int {
    switch self {
    case .buffer(let unique): unique.count
    case .empty: 0
    }
  }

  public subscript(index: Int) -> Int {
    switch self {
    case .buffer(let unique): unique[index]
    case .empty: fatalError("boom")
    }
  }
}

public struct BufferOfWrappers: ~Copyable {
  let inner: UniqueBuffer<BufferWrapper>

  public init() {
    inner = UniqueBuffer(count: 17) { index in
      .empty
    }
  }

  public func countEm() -> Int {
    return inner.count
  }
}

//--- Application.swift
import Library

func test() {
  let bufferWrapper = BufferWrapper(repeating: 17, count: 16)
  var sum = 0
  for i in 0..<bufferWrapper.count {
    sum += bufferWrapper[i]
  }
  print(bufferWrapper.count)
  print(sum)

  let anotherBuffer = BufferOfWrappers()
  print(anotherBuffer.countEm())
}

test()
// CHECK: 15
// CHECK: 17

