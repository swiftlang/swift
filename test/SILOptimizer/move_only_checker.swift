// RUN: %target-swift-frontend -verify %s -parse-stdlib -emit-sil

import Swift

public class Klass {
    var field: Int?
}

func whatShouldIDo() -> Bool {
    return false
}

public typealias MoveOnlyKlass = @_moveOnly Klass

public func simpleChainTest(_ x: Klass) {
    let x2 = Builtin.move(x)
    let y2 = x2
    let k2 = y2
}

public func simpleNonConsumingUseTest(_ x: Klass) {
    let x2 = Builtin.move(x)
    print(x2)
}

public func useMoveOnlyWithoutEscaping(_ x: @_moveOnly Klass) {
}

public func multipleNonConsumingUseTest(_ x: Klass) {
    let x2 = Builtin.move(x)
    useMoveOnlyWithoutEscaping(x2)
    useMoveOnlyWithoutEscaping(x2)
    print(x2)
}
