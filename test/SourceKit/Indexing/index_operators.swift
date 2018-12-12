// RUN: %sourcekitd-test -req=index %s -- -Xfrontend -serialize-diagnostics-path -Xfrontend %t.dia %s | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

class ClassA {
    init(){}
}

func +(lhs: ClassA, rhs: ClassA) -> ClassA {
    return lhs
}

struct StructB {
    func method() {
        let a = ClassA()
        let b = a + a
        let c = StructB()
        let d = c - c
    }

    public static func -(lhs: StructB, rhs: StructB) -> StructB {
        return lhs
    }
}
