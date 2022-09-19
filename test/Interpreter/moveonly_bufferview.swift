// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-move-only) | %FileCheck %s

// REQUIRES: executable_test

@_moveOnly
public struct BufferView<T> {
    var ptr: UnsafeMutableBufferPointer<T>

    deinit {}
}

extension BufferView : Sequence {
    public typealias Iterator = UnsafeMutableBufferPointer<T>.Iterator
    public typealias Element = UnsafeMutableBufferPointer<T>.Element

    public func makeIterator() -> Self.Iterator {
        return ptr.makeIterator()
    }
}

extension Array {
    public mutating func withBufferView<U>(_ f: (BufferView<Element>) -> U) -> U {
        return withUnsafeMutableBufferPointer {
            return f(BufferView(ptr: $0))
        }
    }
}

func testBufferView(_ x: __owned [Int]) {
    var y = _move x
    // CHECK: 1
    // CHECK: 2
    // CHECK: 3
    y.withBufferView {
        for x in $0 {
            print(x)
        }
    }
}

func main() {
    testBufferView([1,2,3])
}

main()
