// RUN: %target-typecheck-verify-swift
protocol P {
    associatedtype X = Int

    func f1(_ x: X)
    func f2(_ x: X)
}

struct S: P {
    func f1(_ x: X) {}
    func f2(_ x: X) {}
}
 
