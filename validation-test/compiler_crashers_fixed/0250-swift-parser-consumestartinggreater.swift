// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A {
    typealias B
}
class C<D> {
    init <A: A where A.B == D>(e: A.B) {
    }
}
import Founda
    typealias F
    func g<T where T.E == F>(f: B<T>)
}
struct D : C {
    ias F = Int
    func g<T where T.E == F>(f: B<T>) {
    }
}
func b(c) -> <d>(() -> d) {S<T> : P  convenience init<T>(array: Array<T>}
