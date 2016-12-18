// RUN: not %target-swift-frontend %s -typecheck
struct A {
    var a: Int32 { return 0 }
}

struct B {
    var b: Int64 { return 0 }
}

struct C {
    var c: Int64 { return 0 }
}

class S {
    var a: A? = A()
    var b: B? = B()
    var c: C? = C()
    var result: Int64? { return a?.a ?? b?.b ?? c?.c }
}

let s = S()
print(s.result)
