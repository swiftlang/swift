protocol FancyProtocol {
    associatedtype Thing
    func holdPinkyUp(x: Thing)
}

struct Dashing: FancyProtocol {
    func holdPinkyUp(x: String) { print("Dashing: \(x)") }
}
class AnyFancyBoxBase<T>: FancyProtocol {
    func holdPinkyUp(x: T) {
        //never called
        fatalError()
    }
}

final class _FancyBox<Base: FancyProtocol>: AnyFancyBoxBase<Base.Thing> {
    var base: Base
    init(_ base: Base) {
        self.base = base
    }
    override func holdPinkyUp(x: Base.Thing) {
        base.holdPinkyUp(x: x)
    }
}

struct AnyFancy<T>: FancyProtocol {
    var _box: AnyFancyBoxBase<T>
    func holdPinkyUp(x: T) {
        _box.holdPinkyUp(x: x)
    }

    init<U: FancyProtocol>(_ base: U) where U.Thing == T {
        _box = _FancyBox(base)
    }
}

let dashing = Dashing()
var anyFancy = AnyFancy(dashing)
print("\(type(of: anyFancy))")
anyFancy.holdPinkyUp(x: "")

// RUN: %sourcekitd-test -req=cursor -pos=40:3 %s -- %s | %FileCheck %s -check-prefix=CASE1

// CASE1: AnyFancy<String>
