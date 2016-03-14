// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func ^(a: Boolean, Bool) -> Bool {
    return !(a)
}
func i(c: () -> ()) {
}
class a protocol A {
       typealias F = Int
    func g<T where T.E == F>(f: B<T>) {
    }
}
protocol a : a {
}
class A : A {
}
class B : C {
}
typealias C = B
class A<T : A> {
}
class c {
    f   var : 
