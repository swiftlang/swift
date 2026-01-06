// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// This test used to fail on 32 bit platforms. It would crash on the second
// swift-frontend invocation.

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -enable-experimental-feature Embedded -parse-as-library -disable-objc-interop -wmo -O
// RUN: %target-swift-frontend -I %t -c -o %t/Client.o %t/Client.swift -enable-experimental-feature Embedded -parse-as-library -disable-objc-interop -wmo -O

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

//--- Lib.swift

public struct S {
    public var a, b, c, d: Float
    public init(a: Float, b: Float, c: Float, d: Float) {
        self.a = a; self.b = b; self.c = c; self.d = d
    }
}

public var setFn: ((OpaquePointer, S) -> Void)? = nil
public var getFn: ((OpaquePointer) -> S)? = nil

open class C {
    public var ptr: OpaquePointer
    public init() { ptr = OpaquePointer(bitPattern: 1)! }
    public var value: S? {
        get { getFn.unsafelyUnwrapped(ptr) }
        set {
            if let newValue { setFn.unsafelyUnwrapped(ptr, newValue) }
        }
    }
}

//--- Client.swift

import Lib

final class D: C {
    override init() {
        super.init()
        value = S(a: 0, b: 0, c: 0, d: 0)
    }
}
