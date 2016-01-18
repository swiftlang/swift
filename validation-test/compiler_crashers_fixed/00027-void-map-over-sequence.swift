// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/tmu (Teemu Kurppa)
// rdar://18118173

class a<T : Hashable> {
    var b = [T : Bool]()
    init<S : SequenceType where S.Generator.Element == T>(_ c: S) {
        c.map { self.b[$0] = true }
    }
}
a([1])
