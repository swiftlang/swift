// RUN: %target-swift-emit-silgen -enable-experimental-move-only %s | %FileCheck %s
// RUN: %target-swift-emit-sil -enable-experimental-move-only %s | %FileCheck %s

//////////////////
// Declarations //
//////////////////

@_moveOnly
public class Klass {
    var intField: Int
    var klsField: Klass?

    init() {
        klsField = Klass()
        intField = 5
    }
}

public func nonConsumingUseKlass(_ k: Klass) {}

///////////
// Tests //
///////////

// -----------
// Class Tests
//

// CHECK-LABEL: useVarKlass
public func useVarKlassNoErrorSimple() {
    var k = Klass()
    k = Klass()

    nonConsumingUseKlass(k)
    let k2 = k
    let _ = k2
}

/*
public func useVarKlassErrorSimple() {
    var k = Klass()
    let k1 = k
    let _ = k1
    let k2 = k
    let _ = k2

    k = Klass()
    let k3 = k
    let _ = k3
    let k4 = k
    let _ = k4

    k = Klass()
    let k5 = k
    let _ = k5
}
*/
