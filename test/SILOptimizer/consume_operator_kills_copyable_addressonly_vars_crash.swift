// RUN: %target-swift-frontend %s -parse-stdlib -emit-sil -o /dev/null

import Swift

var booleanValue: Bool { true }

public class Klass {
    func doSomething() { print("something") }
}


// Make sure we put the dbg_info after the reinit, not before it. Otherwise this
// test case crashes b/c we are using the value before it is reinited.
public func copyableVarArgTestCCFlowReinitOutOfBlockTest(_ k: inout Klass) {
    k.doSomething()
    if booleanValue {
        let m = consume k
        m.doSomething()
    }
    k = Klass()
    k.doSomething()
}

