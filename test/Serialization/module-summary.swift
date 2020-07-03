// RUN: %swift-frontend -emit-module-summary %s

func foo() {}
func bar(_: Int) {}

public func publicFunc() {}

public struct X {
    func method1() {}
}
