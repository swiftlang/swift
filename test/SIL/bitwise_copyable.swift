// RUN: %target-swift-frontend                         \
// RUN:     %s                                         \
// RUN:     -emit-sil                                  \
// RUN:     -enable-experimental-feature ValueGenerics \
// RUN:     -enable-builtin-module \
// RUN:     -disable-availability-checking \
// RUN:     -O

// REQUIRES: swift_feature_ValueGenerics

// Force verification of TypeLowering's isTrivial.

import Builtin

@frozen
public struct Vector<let Count: Int, Element: ~Copyable>: ~Copyable {
    private var storage: Builtin.FixedArray<Count, Element>

    public init(_ valueForIndex: (Int) -> Element) {
        storage = Builtin.emplace { rawPointer in
            let base = UnsafeMutablePointer<Element>(rawPointer)
            for i in 0..<Count {
                (base + i).initialize(to: valueForIndex(i))
            }
        }
    }

    public subscript(i: Int) -> Element {
        _read {
            assert(i >= 0 && i < Count)
            let rawPointer = Builtin.addressOfBorrow(self)
            let base = UnsafePointer<Element>(rawPointer)
            yield ((base + i).pointee)
        }

        _modify {
            assert(i >= 0 && i < Count)
            let rawPointer = Builtin.addressof(&self)
            let base = UnsafeMutablePointer<Element>(rawPointer)
            yield (&(base + i).pointee)
        }
    }
}
extension Vector: Copyable where Element: Copyable {
    public init(repeating value: Element) {
        self.init { _ in value }
    }
}
extension Vector: BitwiseCopyable where Element: BitwiseCopyable {}

func main() {
    var v = Vector<4, String>(repeating: "x") // OK
    //v[0] = 1
    //v[1] = 2
    //v[2] = 3
    //v[3] = 4
    print("break here")
}
main()
