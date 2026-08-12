// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-frontend -emit-module -verify %s -emit-module-path /dev/null
// RUN: %target-swift-frontend -emit-module -verify -experimental-skip-non-inlinable-function-bodies-without-types %s -emit-module-path /dev/null
// RUN: %target-swift-frontend -emit-module -verify -experimental-skip-non-inlinable-function-bodies %s -emit-module-path /dev/null
// RUN: %target-swift-frontend -emit-module -verify -experimental-skip-all-function-bodies %s -emit-module-path /dev/null

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

func throwsError() throws -> Int { 0 }

extension P {
    public func h() {
        typealias T = Self

        func g(_ x: T) {
            do {
                _ = try throwsError()
            } catch {
            }
        }
        g(self)
    }
}

let globalWithLocalFunc: Void = {
    func g() {
        do {
            _ = try throwsError()
        } catch {
        }
    }
    g()
}()
