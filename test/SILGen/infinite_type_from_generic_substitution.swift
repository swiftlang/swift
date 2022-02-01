// RUN: %target-swift-emit-sil -verify %s

protocol P {
    associatedtype A
}

struct S<T: P> {
    var s: T.A
}

struct SR: P {
    typealias A = S<SR>
}

struct SU<T: P> {
    var x: S<T>
}

func foo(x: SU<SR>) -> SU<SR> { return x }
func bar(x: S<SR>) -> S<SR> { return x }
func bas(x: S<SR>) -> S<SR> { return x.s }

enum E<T: P> {
    case recursive(T.A)
    case blah
}

struct ER: P {
    typealias A = E<ER>
}

enum EU<T: P> {
    case x(E<T>)
}

func zim(x: EU<ER>) -> EU<ER> { return x }
func zang(x: E<ER>) -> E<ER> { return x }
