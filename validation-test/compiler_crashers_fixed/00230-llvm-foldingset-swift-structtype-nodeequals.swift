// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func ^(a: BooleanType, Bool) -> Bool {
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
