// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-feature -Xfrontend Lifetimes -disable-availability-checking) | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

// Ensure we don't crash

struct MyBuffer: ~Copyable {
    private let buffer: UnsafeMutableRawBufferPointer

    var length: Int { buffer.count }
    func contents() -> UnsafeMutableRawPointer {
        buffer.baseAddress!
    }

    init(buffer: UnsafeMutableRawBufferPointer)
    {
        self.buffer = buffer
    }

    init<T>(of type: T.Type) where T: BitwiseCopyable {
        self.buffer = UnsafeMutableRawBufferPointer.allocate(byteCount: MemoryLayout<T>.size, alignment: MemoryLayout<T>.alignment)
    }
}

extension RawSpan {
    @_lifetime(borrow buffer)
    init(buffer: borrowing MyBuffer) {
        let span = unsafe RawSpan(_unsafeStart: buffer.contents(), byteCount: buffer.length)
        self = unsafe _overrideLifetime(span, borrowing: buffer)
    }
}

extension MutableRawSpan {
    @_lifetime(borrow buffer)
    init(buffer: borrowing MyBuffer) {
        let span = unsafe MutableRawSpan(_unsafeStart: buffer.contents(), byteCount: buffer.length)
        self = unsafe _overrideLifetime(span, borrowing: buffer)
    }
}

extension OutputRawSpan {
    @_lifetime(borrow buffer)
    init(buffer: borrowing MyBuffer) {
        let raw = unsafe UnsafeMutableRawBufferPointer(start: buffer.contents(), count: buffer.length)
        let span = unsafe OutputRawSpan(buffer: raw, initializedCount: 0)
        self = unsafe _overrideLifetime(span, borrowing: buffer)
    }
}

extension Span where Element: BitwiseCopyable {
    @_lifetime(borrow buffer)
    init(buffer: borrowing MyBuffer) {
        precondition(buffer.length.isMultiple(of: MemoryLayout<Element>.size), "buffer size must be a multiple of the element size.")
        let span = unsafe buffer.rawSpan._unsafeView(as: Element.self)
        assert(span.count == buffer.length / MemoryLayout<Element>.size, "Extraction of contiguous storage failed.")
        self = unsafe _overrideLifetime(span, borrowing: buffer)
    }
}

extension MutableSpan where Element: BitwiseCopyable {
    @_lifetime(borrow buffer)
    init(buffer: borrowing MyBuffer) {
        precondition(buffer.length.isMultiple(of: MemoryLayout<Element>.size), "buffer buffer size (\(buffer.length)) must be a multiple of the element size \(MemoryLayout<Element>.size).")
        var raw = buffer.mutableRawSpan
        let span = unsafe raw._unsafeMutableView(as: Element.self)
        assert(span.count == buffer.length / MemoryLayout<Element>.size, "Extraction of contiguous storage failed.")
        self = unsafe _overrideLifetime(span, borrowing: buffer)
    }
}

extension MyBuffer {
    var rawSpan: RawSpan { .init(buffer: self) }
    var mutableRawSpan: MutableRawSpan { .init(buffer: self) }
    var outputRawSpan: OutputRawSpan { .init(buffer: self) }

    @_lifetime(borrow self)
    func span<T>(of type: T.Type) -> Span<T> where T: BitwiseCopyable { .init(buffer: self) }

    @_lifetime(borrow self)
    func mutableSpan<T>(of type: T.Type) -> MutableSpan<T> where T: BitwiseCopyable { .init(buffer: self) }
}

struct MyData: BitwiseCopyable {
    var storage: InlineArray<262144, UInt32>
}

func fillBuffer(buffer: borrowing MyBuffer)
{
    var mutableSpan = buffer.mutableSpan(of: MyData.self)
    for i in 0..<262144 {
        mutableSpan[0].storage[i] = UInt32(1 + i)
    }
}

func accessFirstElement(buffer: borrowing MyBuffer)
{
    if buffer.length > 4 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        // CHECK: nextValue: 2
        print("nextValue: \(nextValue)")
    }
    if buffer.length <= 4 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    }
}

func calculateNextValue(value: UInt32) -> UInt32
{
    return value + 1
}

func explodeStack(buffer: borrowing MyBuffer)
{
    if buffer.length <= 1024 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    } else if buffer.length <= 1020 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    } else if buffer.length <= 1016 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    } else if buffer.length <= 1012 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    } else if buffer.length <= 1008 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    } else if buffer.length <= 1004 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    } else if buffer.length <= 1000 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    } else if buffer.length <= 996 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    } else if buffer.length <= 992 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    } else if buffer.length <= 988 {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        print("nextValue: \(nextValue)")
    } else {
        let span = buffer.span(of: MyData.self)
        let firstElement = span[0].storage[0]
        let nextValue = calculateNextValue(value: firstElement)
        // CHECK: nextValue: 2
        print("nextValue: \(nextValue)")
    }
}

//MARK: - main

func main() {
    let myBuffer = MyBuffer(of: MyData.self)
    fillBuffer(buffer: myBuffer)

    accessFirstElement(buffer: myBuffer)

    explodeStack(buffer: myBuffer)
}

main()
