// RUN: %target-run-simple-swift(-target %module-target-future -enable-experimental-feature BuiltinModule -Xfrontend -disable-experimental-parser-round-trip) | %FileCheck %s
// RUN: %target-swift-frontend -target %module-target-future -enable-experimental-feature BuiltinModule -disable-experimental-parser-round-trip -emit-module %s -o %t/Vector.swiftmodule

// REQUIRES: executable_test
// REQUIRES: swift_feature_BuiltinModule
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME: miscompile/verifier error after optimization

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

extension Vector: CustomDebugStringConvertible {
    public var debugDescription: String {
        var result = "["
        if Count > 0 {
            result += String(reflecting: self[0])

            for i in 1..<Count {
                result += ", " + String(reflecting: self[i])
            }
        }
        result += "]"
        return result
    }
}

class Canary: CustomDebugStringConvertible {
    var value: String

    init(value: String) { self.value = value }

    var debugDescription: String {
        "<\(self.value)>"
    }

    deinit { print("deinit <\(self.value)>") }
}

func main() {
    var v = Vector<4, Canary>(repeating: Canary(value: ""))

    // CHECK: [<>, <>, <>, <>]
    print(v)

    v[0] = Canary(value: "hello")
    v[1] = Canary(value: "world")
    v[2] = Canary(value: "goodbye")
    v[3] = Canary(value: "moon")
    // CHECK-NEXT: deinit <>

    // CHECK-NEXT: [<hello>, <world>, <goodbye>, <moon>]
    print(v)

    do {
        var w = v
        w[1] = Canary(value: "sun")
        w[3] = Canary(value: "earth")
        // CHECK-NEXT: [<hello>, <sun>, <goodbye>, <earth>]
        print(w)
    }
    // CHECK-NEXT: deinit <sun>
    // CHECK-NEXT: deinit <earth>

    // CHECK-NEXT: [<hello>, <world>, <goodbye>, <moon>]
    print(v)

    // CHECK-NEXT: deinit <hello>
    // CHECK-NEXT: deinit <world>
    // CHECK-NEXT: deinit <goodbye>
    // CHECK-NEXT: deinit <moon>
}
main()
