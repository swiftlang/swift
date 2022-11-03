// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-move-only) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none

// Until we get support for emitting value witnesses for deinits, do not run
// this with optimizations.

@_moveOnly
public struct BufferView<T> {
    var ptr: UnsafeBufferPointer<T>

    deinit {}
}

extension BufferView : Sequence {
    public typealias Iterator = UnsafeBufferPointer<T>.Iterator
    public typealias Element = UnsafeBufferPointer<T>.Element

    public func makeIterator() -> Self.Iterator {
        return ptr.makeIterator()
    }
}

extension Array {
    public mutating func withBufferView<U>(_ f: (BufferView<Element>) -> U) -> U {
        return withUnsafeBufferPointer {
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

@inline(never)
func getBool() -> Bool { return true }

func testConditionalBufferView(_ x: __owned [Int]) {
    (_move x).withUnsafeBufferPointer {
        let y = BufferView(ptr: $0)
        // CHECK: 4
        // CHECK: 5
        // CHECK: 6
        if getBool() {
            for z in y {
                print(z)
            }
        }
    }
}

func main() {
    testBufferView([1,2,3])
    testConditionalBufferView([4,5,6])
}

main()
