// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -experimental-skip-non-inlinable-function-bodies-without-types %s -emit-module-path %t/skip_local_function_bodies.swiftmodule

public protocol P {
    init()
}

extension P {
    public func f() {
        typealias T = Self

        func g(_ x: T) {}
	g(self)
    }
}

