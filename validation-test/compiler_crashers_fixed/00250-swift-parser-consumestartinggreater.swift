// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
